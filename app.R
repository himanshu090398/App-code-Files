# Load necessary libraries
# Ensure these are installed: install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", "lubridate", "ggplot2", "forecast", "zoo", "DT", "shinycssloaders", "plotly"))

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(zoo) # For as.yearmon
library(DT) # For interactive tables
library(shinycssloaders) # For loading animations
library(plotly) # For interactive plots

# Helper function to generate filter choices
get_filter_choices <- function(data_column, default_all_text, placeholder_value = "N/A") {
    if (is.null(data_column) || length(data_column) == 0) { 
        return(c(default_all_text))
    }
    
    data_column_char <- as.character(data_column) 
    
    unique_values <- unique(data_column_char)
    valid_values <- unique_values[!is.na(unique_values) & 
                                  unique_values != placeholder_value &
                                  trimws(unique_values) != ""]
    
    if (length(valid_values) > 0) {
        return(c(default_all_text, sort(valid_values)))
    } else { 
        return(c(default_all_text))
    }
}

# Helper function to find original column name by keyword matching
get_original_col_name_by_keyword <- function(original_names_vec, normalized_names_vec, concept_keywords_vec) {
    if (is.null(original_names_vec) || length(original_names_vec) == 0 || 
        is.null(normalized_names_vec) || length(normalized_names_vec) == 0) {
        return(NULL)
    }
    for (keyword in concept_keywords_vec) {
        for (i in seq_along(normalized_names_vec)) {
            if (grepl(keyword, normalized_names_vec[i], fixed = FALSE, ignore.case = TRUE)) { 
                return(original_names_vec[i]) 
            }
        }
    }
    return(NULL)
}

# --- NEW: Function to format SARIMA equation ---
format_sarima_equation <- function(model) {
  if (is.null(model) || !inherits(model, "ARIMA")) {
    return("Equation not available.")
  }

  order <- model$arma[c(1, 6, 2, 3, 7, 4, 5)]
  p <- order[1]; d <- order[2]; q <- order[3]
  P <- order[4]; D <- order[5]; Q <- order[6]
  m <- order[7]
  
  coefs <- model$coef
  
  format_poly <- function(coef_names, prefix) {
    poly_coefs <- coefs[grepl(paste0("^", prefix), names(coefs))]
    if (length(poly_coefs) == 0) return("1")
    
    terms <- c()
    for (i in seq_along(poly_coefs)) {
      val <- round(poly_coefs[i], 4)
      lag <- gsub(prefix, "", names(poly_coefs)[i])
      sign <- ifelse(val >= 0, "+", "-")
      terms <- c(terms, paste0(sign, " ", abs(val), "B^", lag))
    }
    return(paste0("(1 ", paste(terms, collapse = " "), ")"))
  }

  phi_poly <- format_poly(names(coefs), "ar")
  Phi_poly <- format_poly(names(coefs), "sar")
  theta_poly <- format_poly(names(coefs), "ma")
  Theta_poly <- format_poly(names(coefs), "sma")
  
  diff_part <- ""
  if (d > 0) diff_part <- paste0(diff_part, "(1 - B)^", d)
  if (D > 0) diff_part <- paste0(diff_part, "(1 - B^", m, ")^", D)
  if (diff_part == "") diff_part <- " " else diff_part <- paste0(diff_part, " ")

  y_t <- "y[t]"
  if (!is.null(model$lambda)) {
    y_t <- paste0("BoxCox(y[t], lambda=", round(model$lambda, 4), ")")
  }
  
  lhs <- paste(Phi_poly, phi_poly, diff_part, y_t, sep=" ")
  rhs <- paste(Theta_poly, theta_poly, "e[t]", sep=" ")
  
  intercept_term <- ""
  if ("intercept" %in% names(coefs)) {
      intercept_term <- paste0(" + ", round(coefs['intercept'], 4))
  }
  if ("drift" %in% names(coefs)) {
      intercept_term <- paste0(" + ", round(coefs['drift'], 4),"*t")
  }
  
  equation <- paste(lhs, "=", rhs, intercept_term)
  
  equation <- gsub("\\s+", " ", equation)
  equation <- gsub("\\(1 \\)", "", equation) # Remove empty polynomials
  
  return(equation)
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "IOCL Sales Forecaster", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("1. Upload & View Data", tabName = "upload_view", icon = icon("upload")),
      menuItem("2. Forecast Sales", tabName = "forecast_sales", icon = icon("chart-line")),
      menuItem("3. Check Accuracy", tabName = "accuracy_check", icon = icon("check-circle")) 
    ),
    conditionalPanel(
      condition = "input.tabs == 'upload_view' || input.tabs == 'forecast_sales' || input.tabs == 'accuracy_check'",
      tags$hr(),
      h4("1. Upload Historical Data", style = "padding-left: 10px;"),
      fileInput("file1", "Upload Historical Sales Files (Max 10)", 
                multiple = TRUE, 
                accept = c(".xlsx", ".xls", ".csv", "text/csv", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                placeholder = "No files selected"
      ),
      uiOutput("file_info"),
      tags$p("Ensure your files have columns like 'Date', 'Sales', and other optional filters.", style = "font-size: 0.9em; padding: 0 10px;")
    ),
    conditionalPanel(
      condition = "input.tabs == 'upload_view' && output.data_loaded", 
      tags$hr(),
      h4("2. Filter Data", style = "padding-left: 10px;"), 
      uiOutput("sales_org_filter_ui"),
      uiOutput("matrl_group_filter_ui"),
      uiOutput("dist_channel_filter_ui"), 
      uiOutput("material_filter_ui")      
    ),
    conditionalPanel(
      condition = "input.tabs == 'forecast_sales' && output.data_loaded", 
      tags$hr(),
      h4("4. Forecasting Controls", style = "padding-left: 10px;"),
      numericInput("forecast_periods", "Forecast Periods (Months):", value = 12, min = 1, max = 60, step = 1),
      actionButton("generate_forecast_btn", "Generate Forecast", icon = icon("cogs"), class = "btn-success", style="margin-left: 10px; margin-top: 10px; width: calc(100% - 20px);")
    ),
    tags$div(
      style = "position: absolute; bottom: 20px; width: 100%; text-align: center; font-size: 0.8em; color: #777;",
      HTML("&copy; 2025 IOCL Sales Forecaster. <br>Powered by R Shiny.")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo { background-color: #0073b7; color: #fff; }
        .skin-blue .main-header .navbar { background-color: #0073b7; }
        .content-wrapper, .right-side { background-color: #ecf0f5; }
        .small-box { border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
        .btn-success { background-color: #28a745; border-color: #28a745; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #0073b7; }
        .shiny-output-error-validation { color: red; font-weight: bold; }
        .dataTables_wrapper .dataTables_paginate .paginate_button { padding: 0.3em 0.8em; }
        .dataTables_wrapper .dataTables_filter input { margin-left: 0.5em; border-radius: 4px; }
        .dataTables_wrapper .dataTables_length select { border-radius: 4px; }
        #file_info { font-style: italic; color: #555; padding-left: 10px;}
        .plot-container { border: 1px solid #ddd; border-radius: 5px; padding: 15px; background-color: #fff; margin-bottom:20px; }
        .table-container { border: 1px solid #ddd; border-radius: 5px; padding: 15px; background-color: #fff; margin-bottom:20px; }
        .model-details-container { border: 1px solid #ddd; border-radius: 5px; padding: 15px; background-color: #fff; margin-bottom:20px; max-height: 400px; overflow-y: auto;}
        .loading-spinner { margin-top: 20px; }
      "))
    ),
    tabItems(
      tabItem(tabName = "upload_view",
              fluidRow(
                column(width = 12,
                       conditionalPanel(
                         condition = "!output.data_loaded",
                         tags$div(class="jumbotron text-center", style="background-color: #fff; padding: 30px; border-radius: 8px;",
                           tags$h3("Welcome to the IOCL Sales Forecaster"),
                           tags$p("Please upload your sales data using the panel on the left to begin."),
                           tags$p(tags$i(class="fa fa-arrow-left"), " Use the 'Upload Historical Data' button.")
                         )
                       ),
                       conditionalPanel(
                         condition = "output.data_loaded",
                         box(
                           title = "3. Monthly Sales Data (Filtered)", status = "primary", solidHeader = TRUE, width = 12,
                           collapsible = TRUE,
                           fluidRow(
                             column(width = 5,
                                    h5("Aggregated Monthly Sales Table:"),
                                    div(class = "table-container",
                                        withSpinner(DTOutput("monthly_sales_table"), type = 6, color = "#0073b7", proxy.height = "200px")
                                    )
                             ),
                             column(width = 7,
                                    h5("Monthly Sales Trend Plot:"),
                                    div(class = "plot-container",
                                        withSpinner(plotlyOutput("monthly_sales_plot"), type = 6, color = "#0073b7", proxy.height = "300px")
                                    )
                             )
                           )
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "forecast_sales",
              fluidRow(
                column(width = 12,
                       conditionalPanel(
                         condition = "!output.data_loaded",
                         tags$div(class="jumbotron text-center", style="background-color: #fff; padding: 30px; border-radius: 8px;",
                           tags$h3("Upload Data to Forecast"),
                           tags$p("Please upload your sales data first to enable forecasting.")
                         )
                       ),
                       conditionalPanel(
                         condition = "output.data_loaded && !output.forecast_generated",
                         tags$div(class="jumbotron text-center", style="background-color: #fff; padding: 30px; border-radius: 8px;",
                           tags$h3("Ready to Forecast"),
                           tags$p("Adjust the 'Forecast Periods' in the sidebar and click 'Generate Forecast'.")
                         )
                       ),
                       conditionalPanel(
                         condition = "output.forecast_generated",
                         box(
                           title = "5. Sales Forecast (Based on Filtered Data)", status = "success", solidHeader = TRUE, width = 12,
                           collapsible = TRUE,
                           fluidRow(
                             column(width = 12,
                                   h5("Sales Forecast Plot (Actual vs. Forecasted):"),
                                   div(class = "plot-container",
                                       withSpinner(plotlyOutput("forecast_plot"), type = 6, color = "#0073b7", proxy.height = "300px")
                                   )
                            )
                           ),
                           fluidRow(
                             column(width = 7,
                                    h5("Forecasted Sales Values:"),
                                    div(class = "table-container",
                                        withSpinner(DTOutput("forecast_table"), type = 6, color = "#0073b7", proxy.height = "300px")
                                    )
                             ),
                             column(width = 5,
                                    h5("SARIMA Model Details:"),
                                    div(class = "model-details-container",
                                        withSpinner(verbatimTextOutput("model_details_text"), type = 6, color = "#0073b7", proxy.height = "300px")
                                    )
                             )
                           )
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "accuracy_check",
              fluidRow(
                column(width = 12,
                       conditionalPanel(
                          condition = "!output.forecast_generated",
                          tags$div(class="jumbotron text-center", style="background-color: #fff; padding: 30px; border-radius: 8px;",
                            tags$h3("Generate a Forecast First"),
                            tags$p("Go to the 'Forecast Sales' tab to generate a forecast before you can check its accuracy.")
                          )
                       ),
                       conditionalPanel(
                         condition = "output.forecast_generated",
                         box(
                           title = "6. Check Forecast Accuracy", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
                           p("Upload a file with the actual sales data for the forecasted period. The file should have the same structure and filters as your historical data."),
                           fileInput("accuracy_file", "Upload Actuals File (.xlsx, .xls, .csv)",
                                     accept = c(".xlsx", ".xls", ".csv")),
                           actionButton("check_accuracy_btn", "Check Accuracy", icon = icon("check"), class = "btn-primary")
                         ),
                         conditionalPanel(
                           condition = "output.accuracy_results_generated",
                           box(
                             title = "Accuracy Results", status = "info", solidHeader = TRUE, width=12,
                             fluidRow(
                               column(width=12,
                                      h5("Forecast vs. Actuals Plot"),
                                      withSpinner(plotlyOutput("accuracy_comparison_plot"))
                               )
                             ),
                             fluidRow(
                               column(width=8,
                                      h5("Comparison Table"),
                                      withSpinner(DTOutput("accuracy_comparison_table"))
                               ),
                               column(width=4,
                                      h5("Accuracy Metrics"),
                                      withSpinner(verbatimTextOutput("accuracy_metrics_text"))
                               )
                             )
                           )
                         )
                       )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  raw_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  filtered_data_reactive <- reactiveVal(NULL)
  forecast_results <- reactiveVal(NULL)
  accuracy_data <- reactiveVal(NULL)
  accuracy_results_output <- reactiveVal(NULL)

  output$data_loaded <- reactive({ !is.null(raw_data()) })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$forecast_generated <- reactive({ !is.null(forecast_results()) })
  outputOptions(output, "forecast_generated", suspendWhenHidden = FALSE)
  
  output$accuracy_results_generated <- reactive({!is.null(accuracy_results_output()) })
  outputOptions(output, "accuracy_results_generated", suspendWhenHidden = FALSE)
  
  output$file_info <- renderUI({
    req(input$file1)
    tags$p(paste("Selected files:", paste(input$file1$name, collapse = ", ")))
  })

  observeEvent(input$file1, {
    inFiles <- input$file1 
    if (is.null(inFiles) || nrow(inFiles) == 0) {
      raw_data(NULL); processed_data(NULL); filtered_data_reactive(NULL); forecast_results(NULL); accuracy_results_output(NULL); accuracy_data(NULL)
      showNotification("No files selected or files removed.", type = "warning", duration = 5)
      return(NULL)
    }
    
    if (nrow(inFiles) > 10) { 
        showNotification("Please upload a maximum of 10 files at a time.", type = "warning", duration = 7)
    }

    all_selected_dfs <- list() 
    error_files <- c() 

    for (i in 1:nrow(inFiles)) {
      current_file_info <- inFiles[i, ]
      current_file_path <- current_file_info$datapath
      current_file_name <- current_file_info$name
      
      tryCatch({
        file_ext <- tools::file_ext(tolower(current_file_name))
        if (file_ext %in% c("xlsx", "xls")) {
          df <- read_excel(current_file_path, sheet = 1, .name_repair = "minimal") 
        } else if (file_ext == "csv") {
          df <- read.csv(current_file_path, stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          stop(paste0("Unsupported file type for ", current_file_name, "."))
        }

        original_column_names <- names(df)
        normalized_column_names_from_file <- tolower(gsub("[^a-zA-Z0-9]", "", original_column_names))
        
        date_keywords <- c("date", "orderdate", "transactiondate", "timestamp") 
        sales_org_keywords <- c("salesorganization", "salesorg", "salesoffice", "sorg", "organization", "branch", "region", "division", "so", "salesdistrictoffice") 
        matrl_group_keywords <- c("matlgroup", "materialgroup", "matrlgroup", "matgroup", "productgroup", "itemgroup", "productcategory", "category", "matrlgrp", "mg", "materialdescription") 
        dist_channel_keywords <- c("distributionchannel", "distchannel", "distrchannel", "dchannel", "channel") 
        material_keywords <- c("material", "materialnumber", "materialcode", "materialid", "itemcode", "productcode") 
        sales_keywords <- c("net", "netsales", "netvalue", "salesintons", "revenue", "salesamount", "salesvalue", "sales", "quantity", "amount", "value", "volume", "tons") 

        temp_original_col_names <- original_column_names
        temp_normalized_col_names <- normalized_column_names_from_file

        find_and_remove_col <- function(orig_names, norm_names, keywords_list) {
            found_orig_name <- get_original_col_name_by_keyword(orig_names, norm_names, keywords_list)
            remaining_orig_names <- orig_names
            remaining_norm_names <- norm_names
            if (!is.null(found_orig_name)) {
                idx <- which(orig_names == found_orig_name)
                if (length(idx) > 0) {
                    idx_to_remove <- idx[1] 
                    remaining_orig_names <- orig_names[-idx_to_remove]
                    remaining_norm_names <- norm_names[-idx_to_remove]
                }
            }
            return(list(found = found_orig_name, orig_names = remaining_orig_names, norm_names = remaining_norm_names))
        }

        res_date <- find_and_remove_col(temp_original_col_names, temp_normalized_col_names, date_keywords)
        actual_date_col_orig <- res_date$found
        temp_original_col_names <- res_date$orig_names
        temp_normalized_col_names <- res_date$norm_names
        
        res_sales_org <- find_and_remove_col(temp_original_col_names, temp_normalized_col_names, sales_org_keywords)
        actual_sales_org_col_orig <- res_sales_org$found
        temp_original_col_names <- res_sales_org$orig_names
        temp_normalized_col_names <- res_sales_org$norm_names

        res_matrl_group <- find_and_remove_col(temp_original_col_names, temp_normalized_col_names, matrl_group_keywords)
        actual_matrl_group_col_orig <- res_matrl_group$found
        temp_original_col_names <- res_matrl_group$orig_names
        temp_normalized_col_names <- res_matrl_group$norm_names
        
        res_dist_channel <- find_and_remove_col(temp_original_col_names, temp_normalized_col_names, dist_channel_keywords)
        actual_dist_channel_col_orig <- res_dist_channel$found
        temp_original_col_names <- res_dist_channel$orig_names
        temp_normalized_col_names <- res_dist_channel$norm_names
        
        res_material <- find_and_remove_col(temp_original_col_names, temp_normalized_col_names, material_keywords)
        actual_material_col_orig <- res_material$found
        temp_original_col_names <- res_material$orig_names
        temp_normalized_col_names <- res_material$norm_names
        
        res_sales <- find_and_remove_col(temp_original_col_names, temp_normalized_col_names, sales_keywords)
        actual_sales_col_orig <- res_sales$found
        
        if (is.null(actual_date_col_orig) || is.null(actual_sales_col_orig)) {
          stop(paste0("Essential columns ('Date' and 'Sales') not found in file: ", current_file_name))
        }
        
        new_df_list_single_file <- list()
        raw_dates <- df[[actual_date_col_orig]]
        parsed_dates <- tryCatch({ as.Date(raw_dates) }, warning = function(w) NULL, error = function(e) NULL)
        if(is.null(parsed_dates) || all(is.na(parsed_dates))) {
            if(is.numeric(raw_dates) && all(raw_dates > 10000 & raw_dates < 60000, na.rm = TRUE)) { 
                parsed_dates <- as.Date(raw_dates, origin = "1899-12-30")
            } else { 
                parsed_dates <- parse_date_time(as.character(raw_dates), orders = c("mdy", "dmy", "ymd", "ym", "my", "Ymd HMS", "mdy HMS", "dmy HMS")) 
            }
        }
        if(all(is.na(parsed_dates))) stop(paste0("Could not parse the Date column in file: ", current_file_name))
        new_df_list_single_file$Date <- as.Date(parsed_dates)

        new_df_list_single_file$Sales_Organization <- if (!is.null(actual_sales_org_col_orig)) as.character(df[[actual_sales_org_col_orig]]) else rep("N/A", nrow(df))
        new_df_list_single_file$Material_Group <- if (!is.null(actual_matrl_group_col_orig)) as.character(df[[actual_matrl_group_col_orig]]) else rep("N/A", nrow(df))
        new_df_list_single_file$Distribution_Channel <- if (!is.null(actual_dist_channel_col_orig)) as.character(df[[actual_dist_channel_col_orig]]) else rep("N/A", nrow(df))
        new_df_list_single_file$Material <- if (!is.null(actual_material_col_orig)) as.character(df[[actual_material_col_orig]]) else rep("N/A", nrow(df))
        new_df_list_single_file$Sales <- as.numeric(gsub("[^0-9\\.\\-]", "", as.character(df[[actual_sales_col_orig]]))) 
        
        single_selected_df <- as.data.frame(new_df_list_single_file, stringsAsFactors = FALSE)
        
        single_selected_df <- single_selected_df %>% 
          mutate(across(where(is.character), trimws)) %>%
          filter(!is.na(Date) & !is.na(Sales))

        if (nrow(single_selected_df) > 0) {
            all_selected_dfs[[length(all_selected_dfs) + 1]] <- single_selected_df
        }

      }, error = function(e) {
        message(paste0("Error processing file ", current_file_name, ": ", e$message))
        error_files <- c(error_files, current_file_name)
      })
    } 

    if (length(all_selected_dfs) > 0) {
      combined_df <- bind_rows(all_selected_dfs) 
      if (nrow(combined_df) == 0) { stop("No valid data rows found across all uploaded files.") }
      
      raw_data(combined_df)
      showNotification(paste("Successfully loaded and processed", length(all_selected_dfs), "file(s)."), type = "message", duration = 5)
      if (length(error_files) > 0) {
          showNotification(paste("Failed to process:", paste(error_files, collapse=", ")), type = "warning", duration = 10)
      }
      
      updateSelectInput(session, "filter_sales_org", choices = get_filter_choices(combined_df$Sales_Organization, "All Sales Organizations"), selected = "All Sales Organizations")
      updateSelectInput(session, "filter_matrl_group", choices = get_filter_choices(combined_df$Material_Group, "All Material Groups"), selected = "All Material Groups")
      updateSelectInput(session, "filter_dist_channel", choices = get_filter_choices(combined_df$Distribution_Channel, "All Distribution Channels"), selected = "All Distribution Channels") 
      updateSelectInput(session, "filter_material", choices = get_filter_choices(combined_df$Material, "All Materials"), selected = "All Materials") 
      
      forecast_results(NULL); accuracy_results_output(NULL) 
    } else {
      raw_data(NULL); processed_data(NULL); filtered_data_reactive(NULL); forecast_results(NULL); accuracy_results_output(NULL); accuracy_data(NULL)
      showNotification(paste("No files were successfully processed. Failed files:", paste(error_files, collapse=", ")), type = "error", duration = 15)
    }
  }) 

  output$sales_org_filter_ui <- renderUI({
    req(raw_data()) 
    df <- raw_data()
    choices <- get_filter_choices(df$Sales_Organization, "All Sales Organizations")
    current_selection <- isolate(input$filter_sales_org)
    selected_val <- if (!is.null(current_selection) && current_selection %in% choices) current_selection else choices[1]
    selectInput("filter_sales_org", "Filter by Sales Organization:", choices = choices, selected = selected_val)
  })

  output$matrl_group_filter_ui <- renderUI({
    req(raw_data()) 
    df <- raw_data()
    choices <- get_filter_choices(df$Material_Group, "All Material Groups")
    current_selection <- isolate(input$filter_matrl_group)
    selected_val <- if (!is.null(current_selection) && current_selection %in% choices) current_selection else choices[1]
    selectInput("filter_matrl_group", "Filter by Material Group:", choices = choices, selected = selected_val)
  })
  
  output$dist_channel_filter_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    choices <- get_filter_choices(df$Distribution_Channel, "All Distribution Channels")
    current_selection <- isolate(input$filter_dist_channel)
    selected_val <- if (!is.null(current_selection) && current_selection %in% choices) current_selection else choices[1]
    selectInput("filter_dist_channel", "Filter by Distribution Channel:", choices = choices, selected = selected_val)
  })

  output$material_filter_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    choices <- get_filter_choices(df$Material, "All Materials")
    current_selection <- isolate(input$filter_material)
    selected_val <- if (!is.null(current_selection) && current_selection %in% choices) current_selection else choices[1]
    selectInput("filter_material", "Filter by Material:", choices = choices, selected = selected_val)
  })

  observe({
    df_orig <- raw_data() 
    if (is.null(df_orig) || nrow(df_orig) == 0) { 
      filtered_data_reactive(NULL); processed_data(NULL)
      return()
    }
    
    df_filtered <- df_orig 

    if (!is.null(input$filter_sales_org) && input$filter_sales_org != "All Sales Organizations") df_filtered <- df_filtered %>% filter(Sales_Organization == input$filter_sales_org)
    if (!is.null(input$filter_matrl_group) && input$filter_matrl_group != "All Material Groups") df_filtered <- df_filtered %>% filter(Material_Group == input$filter_matrl_group)
    if (!is.null(input$filter_dist_channel) && input$filter_dist_channel != "All Distribution Channels") df_filtered <- df_filtered %>% filter(Distribution_Channel == input$filter_dist_channel)
    if (!is.null(input$filter_material) && input$filter_material != "All Materials") df_filtered <- df_filtered %>% filter(Material == input$filter_material)
    
    if (nrow(df_filtered) == 0) {
        showNotification("No data matches the current filter selection.", type = "warning", duration = 5)
        cols_to_keep <- c("Date", "Sales_Organization", "Material_Group", "Distribution_Channel", "Material", "Sales")
        empty_df <- df_filtered[0, intersect(names(df_filtered), cols_to_keep), drop = FALSE] 
        filtered_data_reactive(empty_df) 
        processed_data(data.frame(Month_Year = as.yearmon(as.Date(character())), Total_Sales = numeric(), Month_Year_Display = character())) 
        return()
    }

    filtered_data_reactive(df_filtered) 

    monthly_df <- df_filtered %>%
      mutate(Month_Year = as.yearmon(Date)) %>% 
      group_by(Month_Year) %>%
      summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Month_Year) %>%
      mutate(Month_Year_Display = format(as.Date(Month_Year), "%Y-%m")) 

    if (nrow(monthly_df) == 0) {
        showNotification("No data to aggregate for the current filter selection.", type = "warning", duration = 5)
        processed_data(data.frame(Month_Year = as.yearmon(as.Date(character())), Total_Sales = numeric(), Month_Year_Display = character()))
        return()
    }
    processed_data(monthly_df)
  })

  output$monthly_sales_table <- renderDT({
    req(processed_data())
    df <- processed_data()
    if (nrow(df) == 0 || all(is.na(df$Total_Sales))) return(datatable(data.frame(Message = "No data to display for current filters."), options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE)))
    display_df <- df %>%
        select(Month_Year_Display, Total_Sales) %>%
        rename(`Month-Year` = Month_Year_Display, `Total Sales` = Total_Sales) %>%
        mutate(`Total Sales` = round(`Total Sales`, 2))
    datatable(display_df, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, columnDefs = list(list(className = 'dt-center', targets = '_all'))), rownames = FALSE, caption = "Aggregated monthly sales based on current filters.")
  })

  output$monthly_sales_plot <- renderPlotly({
    req(processed_data())
    df <- processed_data()
    if (nrow(df) < 2 || all(is.na(df$Total_Sales))) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "Not enough data points to plot trend for current filters.", size = 5) + theme_void()
        return(ggplotly(p))
    }
    p <- ggplot(df, aes(x = as.Date(Month_Year), y = Total_Sales)) +
      geom_line(color = "#0073b7", size = 1) + geom_point(color = "#0073b7", size = 2) +
      labs(title = "Monthly Sales Trend", x = "Month-Year", y = "Total Sales") +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") + 
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5, face = "bold"), panel.grid.minor = element_blank()) +
      scale_y_continuous(labels = scales::comma) 
    ggplotly(p, tooltip = c("x", "y")) %>% layout(hovermode = 'x unified')
  })

  observeEvent(input$generate_forecast_btn, {
    req(processed_data(), input$forecast_periods)
    monthly_sales_data <- processed_data()
    min_obs_forecast <- 24 
    
    if (nrow(monthly_sales_data) < min_obs_forecast || all(is.na(monthly_sales_data$Total_Sales))) { 
      showNotification(paste0("Not enough historical data for robust forecasting. Requires at least ", min_obs_forecast, " months of aggregated data after filtering."), type = "warning", duration = 10)
      forecast_results(NULL); return()
    }
    
    monthly_sales_data_cleaned <- monthly_sales_data %>% filter(!is.na(Total_Sales)) %>% arrange(Month_Year)
    if (nrow(monthly_sales_data_cleaned) < min_obs_forecast) {
       showNotification(paste0("Not enough non-NA historical data for robust forecasting. Requires at least ", min_obs_forecast, " months."), type = "warning", duration = 10)
      forecast_results(NULL); return()
    }
    
    start_year <- year(min(as.Date(monthly_sales_data_cleaned$Month_Year)))
    start_month <- month(min(as.Date(monthly_sales_data_cleaned$Month_Year)))
    sales_ts <- ts(monthly_sales_data_cleaned$Total_Sales, start = c(start_year, start_month), frequency = 12) 
    
    if (var(sales_ts, na.rm = TRUE) == 0 && !all(is.na(sales_ts))) { 
        showNotification("Sales data has zero variance after filtering. Forecasting a flat line equal to the mean.", type = "warning", duration = 10)
        fc_mean <- rep(mean(sales_ts, na.rm = TRUE), input$forecast_periods)
        last_actual_month_year <- max(monthly_sales_data_cleaned$Month_Year)
        future_month_years <- seq(as.Date(last_actual_month_year) %m+% months(1), by = "month", length.out = input$forecast_periods)
        forecast_df <- data.frame(
            Month_Year = as.yearmon(future_month_years), Forecasted_Sales = fc_mean,
            Lower_80 = fc_mean, Upper_80 = fc_mean, Lower_95 = fc_mean, Upper_95 = fc_mean 
        ) %>% mutate(Month_Year_Display = format(as.Date(Month_Year), "%Y-%m"))
        forecast_results(list(actual = monthly_sales_data_cleaned, forecast = forecast_df, model_details = "Flat forecast due to zero variance in input data.", model_equation = "y[t] = mean")) 
        return() 
    }

    sales_ts <- tsclean(sales_ts, replace.missing = TRUE) 

    showNotification("Generating robust ARIMAX forecast... This may take a moment.", type = "message", id="forecast_progress", duration = NULL)
    
    fit <- NULL 
    fc <- NULL  

    tryCatch({
      h <- input$forecast_periods
      xreg <- NULL
      future_xreg <- NULL

      if (frequency(sales_ts) > 1 && length(sales_ts) >= frequency(sales_ts)) { 
          K_fourier <- min(6, floor(frequency(sales_ts)/2 -1 )) 
          if(K_fourier < 1) K_fourier <- 1 
          
          xreg <- fourier(sales_ts, K = K_fourier) 
          future_xreg <- fourier(sales_ts, K = K_fourier, h = h)
      } else {
          message("Frequency of time series is <= 1 or not enough data for Fourier terms. Proceeding without xreg.")
      }
      
      fit <- auto.arima(sales_ts, xreg = xreg, lambda = "auto", seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
      
      if(is.null(fit)){ stop("ARIMA model estimation failed.") }
      
      fc <- forecast(fit, h = h, xreg = future_xreg)
      
      if(is.null(fc)){ stop("Forecast generation failed after model estimation.") }

      forecast_df <- data.frame(
        Month_Year = as.yearmon(time(fc$mean)),
        Forecasted_Sales = as.numeric(fc$mean),
        Lower_80 = as.numeric(fc$lower[,1]), Upper_80 = as.numeric(fc$upper[,1]), 
        Lower_95 = as.numeric(fc$lower[,2]), Upper_95 = as.numeric(fc$upper[,2])  
      ) %>% mutate(Month_Year_Display = format(as.Date(Month_Year), "%Y-%m"))
      
      model_equation_str <- format_sarima_equation(fit)
      
      model_details_str <- ""
      if (!is.null(fit)) {
          model_details_str <- paste("Model: ", fit$method, "\n")
          model_details_str <- paste0(model_details_str, "Equation: ", model_equation_str, "\n")
          if(!is.null(fit$lambda)) {
            model_details_str <- paste0(model_details_str, "Box-Cox lambda: ", round(fit$lambda, 4), "\n")
          }
          model_details_str <- paste0(model_details_str, "\nCoefficients:\n")
          coef_printout <- utils::capture.output(print(summary(fit)$coef))
          model_details_str <- paste0(model_details_str, paste(coef_printout, collapse = "\n"))
          model_details_str <- paste0(model_details_str, "\n\nAIC: ", round(fit$aic,2), 
                                      " | AICc: ", round(fit$aicc,2), 
                                      " | BIC: ", round(fit$bic,2))
      } else {
          model_details_str <- "No ARIMAX model could be estimated."
      }
      
      forecast_results(list(actual = monthly_sales_data_cleaned, forecast = forecast_df, model_details = model_details_str)) 
      removeNotification("forecast_progress")
      showNotification("Forecast generated successfully!", type = "message", duration = 5)

    }, error = function(e) {
      forecast_results(NULL); removeNotification("forecast_progress")
      showNotification(paste("Error during forecasting:", e$message), type = "error", duration = 10)
      message("Error during forecasting: ", e$message) 
    })
  })

  output$forecast_plot <- renderPlotly({
    req(forecast_results())
    res <- forecast_results()
    actual_df_plot_source <- res$actual 
    forecast_df_source <- res$forecast 

    actual_df_plot <- actual_df_plot_source %>% 
        mutate(Date = as.Date(Month_Year)) %>%
        rename(Actual_Sales = Total_Sales) 

    last_actual_date <- if (nrow(actual_df_plot) > 0) {
        max(actual_df_plot$Date, na.rm = TRUE)
    } else {
        p <- ggplot() + annotate("text", x=1,y=1, label="No actual data to plot for forecast comparison.") + theme_void()
        return(ggplotly(p))
    }

    forecast_df_plot_filtered <- data.frame() 
    if (!is.null(forecast_df_source) && nrow(forecast_df_source) > 0) {
        forecast_df_plot_filtered <- forecast_df_source %>%
          mutate(Date_forecast = as.Date(Month_Year)) %>% 
          filter(Date_forecast > last_actual_date)
    } 
    
    plot_data_actual <- actual_df_plot %>% 
        select(Date, Sales = Actual_Sales) %>% 
        mutate(Type = "Actual")
    
    plot_data_forecast <- forecast_df_plot_filtered %>% 
        select(Date = Date_forecast, Sales = Forecasted_Sales, Lower_80, Upper_80, Lower_95, Upper_95) %>% 
        mutate(Type = "Forecast")

    fig <- plot_ly() %>%
      add_lines(data = plot_data_actual, x = ~Date, y = ~Sales, name = "Actual Sales",
                line = list(color = "#0073b7"),
                hoverinfo = "text", text = ~paste("Date:", format(Date, "%Y-%m"), "<br>Actual Sales:", scales::comma(round(Sales,0)))) %>%
      add_markers(data = plot_data_actual, x = ~Date, y = ~Sales, name = "Actual Sales",
                  marker = list(color = "#0073b7"), showlegend = FALSE,
                  hoverinfo = "text", text = ~paste("Date:", format(Date, "%Y-%m"), "<br>Actual Sales:", scales::comma(round(Sales,0))))
    
    if(nrow(plot_data_forecast) > 0) {
      fig <- fig %>%
        add_lines(data = plot_data_forecast, x = ~Date, y = ~Sales, name = "Forecasted Sales",
                  line = list(color = "orange"),
                  hoverinfo = "text", text = ~paste("Date:", format(Date, "%Y-%m"), "<br>Forecast:", scales::comma(round(Sales,0)))) %>%
        add_markers(data = plot_data_forecast, x = ~Date, y = ~Sales, name = "Forecasted Sales",
                    marker = list(color = "orange"), showlegend = FALSE,
                    hoverinfo = "text", text = ~paste("Date:", format(Date, "%Y-%m"), "<br>Forecast:", scales::comma(round(Sales,0)))) %>%
        add_ribbons(data = plot_data_forecast, x = ~Date, ymin = ~Lower_95, ymax = ~Upper_95, name = "95% Confidence Interval",
                    line = list(color = 'rgba(255, 165, 0, 0.2)'), fillcolor = 'rgba(255, 165, 0, 0.2)',
                    hoverinfo = "text", text = ~paste("Date:", format(Date, "%Y-%m"), "<br>95% CI:", scales::comma(round(Lower_95,0)), "-", scales::comma(round(Upper_95,0)))) %>%
        add_ribbons(data = plot_data_forecast, x = ~Date, ymin = ~Lower_80, ymax = ~Upper_80, name = "80% Confidence Interval",
                    line = list(color = 'rgba(255, 165, 0, 0.3)'), fillcolor = 'rgba(255, 165, 0, 0.3)',
                    hoverinfo = "text", text = ~paste("Date:", format(Date, "%Y-%m"), "<br>80% CI:", scales::comma(round(Lower_80,0)), "-", scales::comma(round(Upper_80,0))))
    }
      
    fig <- fig %>% layout(title = "Actual vs. Forecasted Sales",
             xaxis = list(title = "Month-Year", type = 'date', tickformat = "%Y-%m", dtick = "M3", automargin = TRUE), 
             yaxis = list(title = "Sales", tickformat = ",", automargin = TRUE), 
             hovermode = "x unified",
             legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.3)) 
    
    fig
  })

  output$forecast_table <- renderDT({
    req(forecast_results())
    
    actual_df_plot_source <- forecast_results()$actual 
    forecast_df_source <- forecast_results()$forecast

    last_actual_date <- if (nrow(actual_df_plot_source %>% mutate(Date = as.Date(Month_Year))) > 0) {
        max(as.Date(actual_df_plot_source$Month_Year), na.rm = TRUE)
    } else {
        Sys.Date() - 1e6 
    }
    
    forecast_df_for_table <- data.frame() 
    if (!is.null(forecast_df_source) && nrow(forecast_df_source) > 0) {
        forecast_df_for_table <- forecast_df_source %>%
          mutate(Date = as.Date(Month_Year)) %>%
          filter(Date > last_actual_date) %>%
          select(Month_Year_Display, Forecasted_Sales, Lower_80, Upper_80, Lower_95, Upper_95) %>%
          rename(`Month-Year` = Month_Year_Display,
                 `Forecasted Sales` = Forecasted_Sales,
                 `Lower 80% CI` = Lower_80,
                 `Upper 80% CI` = Upper_80,
                 `Lower 95% CI` = Lower_95,
                 `Upper 95% CI` = Upper_95) %>%
          mutate(across(where(is.numeric), ~round(., 2)))
    }

    if(nrow(forecast_df_for_table) == 0){
        return(datatable(data.frame(Message = "No future forecast data to display in table."), options = list(searching = FALSE, lengthChange = FALSE)))
    }

    datatable(forecast_df_for_table,
              options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))),
              rownames = FALSE,
              caption = "Forecasted sales values (future periods only) with confidence intervals."
    )
  })

  output$model_details_text <- renderPrint({
    req(forecast_results())
    res <- forecast_results()
    if (!is.null(res$model_details) && res$model_details != "Flat forecast due to zero variance in input data." && res$model_details != "No ARIMA model could be estimated.") {
      cat(res$model_details)
    } else if (!is.null(res$model_details)) { 
      cat(res$model_details)
    } else {
      cat("Model details are not available.")
    }
  })
  
  observeEvent(input$accuracy_file, {
      req(input$accuracy_file)
      showNotification("Actuals file received. Processing...", type="message", duration=4)
      
      tryCatch({
          df <- read_excel(input$accuracy_file$datapath, sheet = 1)
          
          filtered_df <- df
          if (!is.null(input$filter_sales_org) && input$filter_sales_org != "All Sales Organizations") filtered_df <- filtered_df %>% filter(`Sales Organization` == input$filter_sales_org)
          if (!is.null(input$filter_matrl_group) && input$filter_matrl_group != "All Material Groups") filtered_df <- filtered_df %>% filter(`Matl Group` == input$filter_matrl_group)

          monthly_actuals <- filtered_df %>%
            mutate(Date = as.Date(Date),
                   Month_Year = as.yearmon(Date),
                   Sales = as.numeric(gsub("[^0-9\\.\\-]", "", as.character(Net)))) %>%
            group_by(Month_Year) %>%
            summarise(Actual_Sales = sum(Sales, na.rm=TRUE), .groups='drop')
            
          accuracy_data(monthly_actuals)
          showNotification("Actuals file processed. Click 'Check Accuracy' to compare.", type="message", duration=5)
      }, error = function(e) {
          showNotification(paste("Error processing actuals file:", e$message), type="error", duration=10)
      })
  })

  observeEvent(input$check_accuracy_btn, {
      req(forecast_results(), accuracy_data())
      
      forecast_df <- forecast_results()$forecast
      actuals_df <- accuracy_data()
      
      comparison_df <- forecast_df %>%
          inner_join(actuals_df, by = "Month_Year")
          
      if(nrow(comparison_df) == 0){
          showNotification("No matching time periods found between forecast and uploaded actuals.", type="warning", duration=8)
          accuracy_results_output(NULL)
          return()
      }
      
      comparison_df <- comparison_df %>%
          mutate(
              Percent_Difference = ifelse(Actual_Sales == 0, NA, ((Actual_Sales - Forecasted_Sales) / Actual_Sales) * 100)
          )

      acc_metrics <- forecast::accuracy(comparison_df$Forecasted_Sales, comparison_df$Actual_Sales)
      
      accuracy_results_output(list(
          plot_data = comparison_df,
          metrics = acc_metrics
      ))
      showNotification("Accuracy check complete.", type="message", duration=5)
  })
  
  output$accuracy_comparison_plot <- renderPlotly({
      req(accuracy_results_output())
      plot_data <- accuracy_results_output()$plot_data %>% mutate(Date = as.Date(Month_Year))
      
      plot_ly(data = plot_data, x = ~Date) %>%
        add_lines(y = ~Actual_Sales, name = "Actual Sales", line = list(color = "black", dash="solid")) %>%
        add_lines(y = ~Forecasted_Sales, name = "Forecasted Sales", line = list(color = "orange", dash="dot")) %>%
        add_ribbons(ymin = ~Lower_95, ymax = ~Upper_95, name = "95% Confidence Interval",
                    line = list(color = 'rgba(255, 165, 0, 0.2)'), fillcolor = 'rgba(255, 165, 0, 0.2)') %>%
        layout(title = "Forecast vs. Actuals Comparison",
               xaxis = list(title="Month-Year"),
               yaxis = list(title="Sales"),
               legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2))
  })
  
  output$accuracy_metrics_text <- renderPrint({
      req(accuracy_results_output())
      metrics <- accuracy_results_output()$metrics
      metrics_to_show <- as.data.frame(metrics)
      rownames(metrics_to_show) <- "Metrics"
      cat("Forecast Accuracy Metrics:\n")
      print(metrics_to_show)
  })

  output$accuracy_comparison_table <- renderDT({
    req(accuracy_results_output())
    comparison_data <- accuracy_results_output()$plot_data
    
    display_df <- comparison_data %>%
      select(Month_Year_Display, Actual_Sales, Forecasted_Sales, Percent_Difference) %>%
      rename(
        `Month-Year` = Month_Year_Display,
        `Actual Sales` = Actual_Sales,
        `Forecasted Sales` = Forecasted_Sales,
        `% Difference` = Percent_Difference
      ) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
      
    datatable(display_df, options = list(pageLength=5, scrollX=TRUE), rownames=FALSE,
              caption="Side-by-side comparison of actual and forecasted values.")
  })

}

shinyApp(ui = ui, server = server)



