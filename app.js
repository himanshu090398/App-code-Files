// Global variables
let processedData = [];
let historicalData = [];
let uploadedFiles = [];
let indentData = [];
let chartInstances = {};

// Sample data for testing, now includes previousDaySale
const sampleData = [
    {
        location: "Aonla",
        material: "MS",
        materialCode: "16700",
        stock: 7.864,
        ullage: 4.76,
        ullageTW: 0.095,
        dailyAvgSale: 0.262,
        previousDaySale: 0.250,
        daysCover: 30,
        extractedDate: "22-4-2025"
    },
    {
        location: "Aonla", 
        material: "HSD",
        materialCode: "50700",
        stock: 11.547,
        ullage: 2.901,
        ullageTW: 0.058,
        dailyAvgSale: 0.825,
        previousDaySale: 0.815,
        daysCover: 14,
        extractedDate: "22-4-2025"
    },
    {
        location: "Banthra",
        material: "MS", 
        materialCode: "16700",
        stock: 3.824,
        ullage: 0.594,
        ullageTW: 0.012,
        dailyAvgSale: 0.273,
        previousDaySale: 0.260,
        daysCover: 14,
        extractedDate: "22-4-2025"
    },
    {
        location: "Banthra",
        material: "HSD",
        materialCode: "50700", 
        stock: 16.359,
        ullage: 4.47,
        ullageTW: 0.089,
        dailyAvgSale: 0.818,
        previousDaySale: 0.800,
        daysCover: 20,
        extractedDate: "22-4-2025"
    },
    {
        location: "Gonda",
        material: "MS",
        materialCode: "16700",
        stock: 1.138,
        ullage: 2.931,
        ullageTW: 0.059,
        dailyAvgSale: 0.142,
        previousDaySale: 0.135,
        daysCover: 8,
        extractedDate: "22-4-2025"
    },
    {
        location: "Gonda",
        material: "HSD",
        materialCode: "50700",
        stock: 6.547,
        ullage: 6.857,
        ullageTW: 0.137,
        dailyAvgSale: 0.818,
        previousDaySale: 0.810,
        daysCover: 8,
        extractedDate: "22-4-2025"
    },
    {
        location: "Jammu",
        material: "MS",
        materialCode: "16700",
        stock: 5.498,
        ullage: 0.467,
        ullageTW: 0.009,
        dailyAvgSale: 0.687,
        previousDaySale: 0.680,
        daysCover: 8,
        extractedDate: "22-4-2025"
    },
    {
        location: "Jammu",
        material: "HSD",
        materialCode: "50700",
        stock: 14.241,
        ullage: 5.192,
        ullageTW: 0.104,
        dailyAvgSale: 1.424,
        previousDaySale: 1.410,
        daysCover: 10,
        extractedDate: "22-4-2025"
    },
    {
        location: "Jhansi",
        material: "MS",
        materialCode: "16700",
        stock: 3.001,
        ullage: 0.413,
        ullageTW: 0.008,
        dailyAvgSale: 0.2,
        previousDaySale: 0.195,
        daysCover: 15,
        extractedDate: "22-4-2025"
    },
    {
        location: "Jhansi",
        material: "HSD",
        materialCode: "50700",
        stock: 10.937,
        ullage: 3.337,
        ullageTW: 0.067,
        dailyAvgSale: 1.094,
        previousDaySale: 1.080,
        daysCover: 10,
        extractedDate: "22-4-2025"
    },
    {
        location: "Jodhpur",
        material: "MS",
        materialCode: "16700",
        stock: 7.246,
        ullage: 7.212,
        ullageTW: 0.144,
        dailyAvgSale: 0.725,
        previousDaySale: 0.715,
        daysCover: 10,
        extractedDate: "22-4-2025"
    },
    {
        location: "Jodhpur",
        material: "HSD",
        materialCode: "50700",
        stock: 27.023,
        ullage: 9.27,
        ullageTW: 0.185,
        dailyAvgSale: 2.456,
        previousDaySale: 2.450,
        daysCover: 11,
        extractedDate: "22-4-2025"
    },
    {
        location: "Lal Kuan",
        material: "MS",
        materialCode: "16700",
        stock: 0.972,
        ullage: 2.076,
        ullageTW: 0.042,
        dailyAvgSale: 0.243,
        previousDaySale: 0.230,
        daysCover: 4,
        extractedDate: "22-4-2025"
    },
    {
        location: "Lal Kuan",
        material: "HSD",
        materialCode: "50700",
        stock: 6.05,
        ullage: 2.061,
        ullageTW: 0.041,
        dailyAvgSale: 0.672,
        previousDaySale: 0.660,
        daysCover: 9,
        extractedDate: "22-4-2025"
    }
];

// Initialize the application
document.addEventListener('DOMContentLoaded', function() {
    initializeApp();
    setupEventListeners();
});

function initializeApp() {
    console.log('Initializing IOCL Fuel Logistics Dashboard...');
    // Start with empty state
    showEmptyState();
    resetAllMetrics();
}

function showEmptyState() {
    const emptyState = document.getElementById('emptyState');
    const tableContainer = document.getElementById('analysisTableContainer');
    
    if (emptyState) emptyState.style.display = 'flex';
    if (tableContainer) tableContainer.style.display = 'none';
}

function hideEmptyState() {
    const emptyState = document.getElementById('emptyState');
    const tableContainer = document.getElementById('analysisTableContainer');
    
    if (emptyState) emptyState.style.display = 'none';
    if (tableContainer) tableContainer.style.display = 'block';
}

function resetAllMetrics() {
    // Reset all metrics to 0
    const metricElements = [
        'totalLocations', 'criticalLow', 'normalHigh', 'avgDaysCover',
        'totalStock', 'totalUllage', 'avgDailySales', 'criticalLocations',
        'stockTurnover', 'capacityUtil', 'avgLoadTime', 'efficiencyScore'
    ];
    
    metricElements.forEach(id => {
        const element = document.getElementById(id);
        if (element) element.textContent = '0';
    });
    
    // Clear historical data tables
    const historicalTableHead = document.getElementById('historicalTableHead');
    const historicalTableBody = document.getElementById('historicalTableBody');
    if (historicalTableHead) historicalTableHead.innerHTML = '';
    if (historicalTableBody) historicalTableBody.innerHTML = '';
    
    // Destroy existing charts
    destroyAllCharts();
}

function setupEventListeners() {
    // Hamburger menu toggle
    const hamburgerMenu = document.getElementById('hamburgerMenu');
    const sidebar = document.getElementById('sidebar');
    const mainContent = document.getElementById('mainContent');
    
    hamburgerMenu.addEventListener('click', function() {
        sidebar.classList.toggle('open');
        mainContent.classList.toggle('sidebar-open');
    });

    // Page navigation
    const navButtons = document.querySelectorAll('.nav-btn');
    navButtons.forEach(btn => {
        btn.addEventListener('click', function() {
            const targetPage = this.dataset.page;
            showPage(targetPage);
            
            // Update active nav button
            navButtons.forEach(b => b.classList.remove('active'));
            this.classList.add('active');
        });
    });

    // File upload handlers
    const mdpFiles = document.getElementById('mdpFiles');
    mdpFiles.addEventListener('change', handleMDPFileUpload);

    const indentFile = document.getElementById('indentFile');
    indentFile.addEventListener('change', handleIndentFileUpload);

    // Manual indent entry
    const addIndentBtn = document.getElementById('addIndent');
    addIndentBtn.addEventListener('click', handleManualIndentEntry);

    // Run Analysis button
    const runAnalysisBtn = document.getElementById('runAnalysis');
    runAnalysisBtn.addEventListener('click', runAnalysis);

    // Sorting controls
    const sortAscBtn = document.getElementById('sortAsc');
    const sortDescBtn = document.getElementById('sortDesc');
    sortAscBtn.addEventListener('click', () => sortTableByEarliestLoad('asc'));
    sortDescBtn.addEventListener('click', () => sortTableByEarliestLoad('desc'));

    // Historical data controls
    const sheetButtons = document.querySelectorAll('[data-sheet]');
    sheetButtons.forEach(btn => {
        btn.addEventListener('click', function() {
            sheetButtons.forEach(b => b.classList.remove('active'));
            this.classList.add('active');
            updateHistoricalData();
        });
    });

    const dayRadios = document.querySelectorAll('input[name="historicalDays"]');
    dayRadios.forEach(radio => {
        radio.addEventListener('change', updateHistoricalData);
    });

    // Trend analysis controls
    const trendButtons = document.querySelectorAll('[data-trend]');
    trendButtons.forEach(btn => {
        btn.addEventListener('click', function() {
            trendButtons.forEach(b => b.classList.remove('active'));
            this.classList.add('active');
            updateTrendAnalysis();
        });
    });
}

function showPage(pageId) {
    // Hide all pages
    const pages = document.querySelectorAll('.page');
    pages.forEach(page => page.classList.remove('active'));
    
    // Show target page
    const targetPage = document.getElementById(pageId);
    if (targetPage) {
        targetPage.classList.add('active');
    }
}

function handleMDPFileUpload(event) {
    const files = Array.from(event.target.files);
    const fileList = document.getElementById('mdpFileList');
    
    // Clear previous files
    uploadedFiles = [];
    fileList.innerHTML = '';
    
    files.forEach(file => {
        if (file.name.endsWith('.xlsx')) {
            // Extract number from filename for sorting
            const fileNumber = extractFileNumber(file.name);
            uploadedFiles.push({
                file: file,
                number: fileNumber,
                name: file.name
            });
            
            const fileItem = document.createElement('div');
            fileItem.className = 'file-item';
            fileItem.textContent = file.name;
            fileList.appendChild(fileItem);
        }
    });
    
    // Sort files by number
    uploadedFiles.sort((a, b) => a.number - b.number);
    console.log(`Uploaded ${uploadedFiles.length} files`);
}

function extractFileNumber(filename) {
    const match = filename.match(/(\d+)/);
    return match ? parseInt(match[1]) : 0;
}

function handleIndentFileUpload(event) {
    const file = event.target.files[0];
    if (file && file.name.endsWith('.xlsx')) {
        processIndentFile(file);
    }
}

function handleManualIndentEntry() {
    const location = document.getElementById('indentLocation').value;
    const material = document.getElementById('indentMaterial').value;
    const quantity = parseFloat(document.getElementById('indentQuantity').value);
    
    if (location && material && quantity) {
        indentData.push({
            location: location,
            material: material,
            quantity: quantity,
            date: new Date().toISOString().split('T')[0]
        });
        
        // Clear form
        document.getElementById('indentLocation').value = '';
        document.getElementById('indentMaterial').value = '';
        document.getElementById('indentQuantity').value = '';
        
        alert('Indent added successfully!');
    } else {
        alert('Please fill in all fields');
    }
}

async function runAnalysis() {
    console.log('Starting analysis...');
    const loadingOverlay = document.getElementById('loadingOverlay');
    loadingOverlay.classList.add('show');
    
    try {
        // Use sample data or process uploaded files
        if (uploadedFiles.length === 0) {
            console.log('No files uploaded, using sample data');
            processedData = enhanceDataWithCalculations([...sampleData]);
        } else {
            console.log('Processing uploaded files...');
            await processUploadedFiles();
        }
        
        console.log('Processed data:', processedData);
        
        // Generate historical data
        generateHistoricalData();
        
        // Update all pages with processed data
        updateAllPages();
        
        // Hide empty state and show data
        hideEmptyState();
        
        // Show success message
        alert('Analysis completed successfully!');
        
    } catch (error) {
        console.error('Error during analysis:', error);
        alert('Error during analysis: ' + error.message);
    } finally {
        loadingOverlay.classList.remove('show');
    }
}

async function processUploadedFiles() {
    const allData = [];
    historicalData = [];
    
    for (const fileObj of uploadedFiles) {
        try {
            const data = await processExcelFile(fileObj.file);
            const enhancedData = enhanceDataWithCalculations(data);
            
            allData.push(...enhancedData);
            
            // Add to historical data
            historicalData.push({
                day: fileObj.number,
                fileName: fileObj.name,
                data: enhancedData
            });
        } catch (error) {
            console.error(`Error processing file ${fileObj.name}:`, error);
        }
    }
    
    if (allData.length > 0) {
        // Use latest data for main analysis
        historicalData.sort((a, b) => a.day - b.day);
        processedData = historicalData[historicalData.length - 1].data;
    } else {
        // Fallback to sample data
        processedData = enhanceDataWithCalculations([...sampleData]);
    }
}

function processExcelFile(file) {
    return new Promise((resolve, reject) => {
        const reader = new FileReader();
        reader.onload = function(e) {
            try {
                const data = new Uint8Array(e.target.result);
                const workbook = XLSX.read(data, { type: 'array' });
                
                // Look for MDP&CLIPstockcard sheet
                const sheetName = workbook.SheetNames.find(name => 
                    name.toLowerCase().includes('mdp') || name.toLowerCase().includes('clip')
                ) || workbook.SheetNames[0];
                
                const worksheet = workbook.Sheets[sheetName];
                const jsonData = XLSX.utils.sheet_to_json(worksheet, { header: 1 });
                
                // Extract data starting from row 3 (index 2)
                const extractedData = extractDataFromSheet(jsonData, file.name);
                resolve(extractedData);
            } catch (error) {
                reject(error);
            }
        };
        reader.onerror = function() {
            reject(new Error('Failed to read file'));
        };
        reader.readAsArrayBuffer(file);
    });
}

/**
 * MODIFIED FUNCTION
 * Extracts data from the sheet, including the "Day Sale-IOC" column.
 */
function extractDataFromSheet(jsonData, fileName) {
    const extractedData = [];
    const targetLocations = ['Aonla', 'Banthra', 'Gonda', 'Jammu', 'Jhansi', 'JHANSI', 'Jodhpur', 'Lal Kuan'];
    const targetMaterials = ['16700', '50700'];
    
    // Headers are in row 2 (index 1), data starts from row 3 (index 2)
    if (jsonData.length < 3) return extractedData;
    
    const headers = jsonData[1] || [];
    const dataRows = jsonData.slice(2);
    
    // Extract date from headers
    const dateString = extractDateFromHeaders(headers);
    
    dataRows.forEach(row => {
        try {
            const materialCode = row[0] ? row[0].toString() : '';
            const plantName = row[3] ? row[3].toString() : '';
            const availableStock = parseFloat(row[9]) || 0;
            const ullage = parseFloat(row[11]) || 0;
            const ullageTW = parseFloat(row[12]) || 0;
            // MODIFICATION: Read "Day Sale-IOC" from Column Z (index 25) for previous day sale
            const previousDaySale = parseFloat(row[25]) || 0; 
            // Dly Avg Sale from Column AP (index 41) for main dashboard
            const dailyAvgSale = parseFloat(row[41]) || 0;
            const daysCover = parseFloat(row[42]) || 0;
            
            // Check if this row matches our criteria
            if (targetMaterials.includes(materialCode) && 
                targetLocations.some(loc => plantName.toLowerCase().includes(loc.toLowerCase()))) {
                
                const material = materialCode === '16700' ? 'MS' : 'HSD';
                const location = targetLocations.find(loc => 
                    plantName.toLowerCase().includes(loc.toLowerCase())
                ) || plantName;
                
                extractedData.push({
                    location: location,
                    material: material,
                    materialCode: materialCode,
                    stock: availableStock / 1000, // Convert to TKL
                    ullage: ullage / 1000, // Convert to TKL
                    ullageTW: ullageTW / 50, // Convert to TW
                    dailyAvgSale: dailyAvgSale / 1000, // Convert to TKL
                    previousDaySale: previousDaySale / 1000, // MODIFICATION: Add previous day sale data
                    daysCover: daysCover,
                    extractedDate: dateString,
                    fileName: fileName
                });
            }
        } catch (error) {
            console.error('Error processing row:', error);
        }
    });
    
    return extractedData;
}

function extractDateFromHeaders(headers) {
    // Look for date in headers like "Available Stock as on22-4-2025"
    const dateHeader = headers.find(h => 
        h && h.toString().toLowerCase().includes('available stock as on')
    );
    
    if (dateHeader) {
        const dateMatch = dateHeader.toString().match(/(\d{1,2})-(\d{1,2})-(\d{4})/);
        if (dateMatch) {
            return `${dateMatch[1]}-${dateMatch[2]}-${dateMatch[3]}`;
        }
    }
    
    // Fallback to current date
    const today = new Date();
    return `${today.getDate()}-${today.getMonth() + 1}-${today.getFullYear()}`;
}

function enhanceDataWithCalculations(data) {
    return data.map(item => {
        const daysCover = item.daysCover || 0;
        let status = 'Normal';
        
        if (daysCover < 3) {
            status = 'Critical';
        } else if (daysCover < 7) {
            status = 'Low';
        } else if (daysCover > 10) {
            status = 'High';
        }
        
        // Calculate earliest and latest load dates
        const extractedDate = item.extractedDate || '22-4-2025';
        const baseDate = parseDate(extractedDate);
        const earliestLoad = new Date(baseDate);
        earliestLoad.setDate(baseDate.getDate() + Math.floor(daysCover / 2));
        const latestLoad = new Date(baseDate);
        latestLoad.setDate(baseDate.getDate() + daysCover);
        
        return {
            ...item,
            status: status,
            stockInTransitNorthern: (Math.random() * 5).toFixed(2),
            stockInTransitEastern: (Math.random() * 5).toFixed(2),
            day0Ullage: item.ullage,
            day1Ullage: (item.ullage + Math.random() * 0.5).toFixed(2),
            day2Ullage: (item.ullage + Math.random() * 1).toFixed(2),
            day3Ullage: (item.ullage + Math.random() * 1.5).toFixed(2),
            earliestLoad: formatDate(earliestLoad),
            latestLoad: formatDate(latestLoad)
        };
    });
}

function parseDate(dateString) {
    const parts = dateString.split('-');
    return new Date(parts[2], parts[1] - 1, parts[0]);
}

function formatDate(date) {
    return `${date.getDate()}-${date.getMonth() + 1}-${date.getFullYear()}`;
}

/**
 * MODIFIED FUNCTION
 * Generates sample historical data including previous day sales.
 */
function generateHistoricalData() {
    if (historicalData.length > 0) {
        // Already have historical data from files
        return;
    }
    
    // Generate sample historical data for the last 30 days
    historicalData = [];
    const today = new Date();
    
    for (let i = 29; i >= 0; i--) {
        const date = new Date(today);
        date.setDate(today.getDate() - i);
        
        const dateString = formatDate(date);
        const dayData = {
            day: 30 - i,
            date: dateString,
            data: processedData.map(item => ({
                ...item,
                stock: Math.max(0, item.stock + (Math.random() - 0.5) * 2),
                ullage: Math.max(0, item.ullage + (Math.random() - 0.5) * 1),
                dailyAvgSale: Math.max(0, item.dailyAvgSale + (Math.random() - 0.5) * 0.1),
                // MODIFICATION: Generate historical previous day sales
                previousDaySale: Math.max(0, (item.previousDaySale !== undefined ? item.previousDaySale : item.dailyAvgSale) + (Math.random() - 0.5) * 0.1)
            }))
        };
        
        historicalData.push(dayData);
    }
}

function updateAllPages() {
    console.log('Updating all pages with data:', processedData.length, 'records');
    updatePage1();
    updatePage2();
    updatePage3();
    updatePage4();
}

function updatePage1() {
    const tableBody = document.getElementById('analysisTableBody');
    tableBody.innerHTML = '';
    
    processedData.forEach(item => {
        const row = document.createElement('tr');
        row.innerHTML = `
            <td>${item.location}</td>
            <td>${item.material}</td>
            <td>${item.stock.toFixed(2)}</td>
            <td>${item.ullage.toFixed(2)}</td>
            <td>${item.ullageTW.toFixed(3)}</td>
            <td>${item.dailyAvgSale.toFixed(2)}</td>
            <td>${item.daysCover}</td>
            <td>${item.stockInTransitNorthern}</td>
            <td>${item.stockInTransitEastern}</td>
            <td>${item.day0Ullage.toFixed(2)}</td>
            <td>${item.day1Ullage}</td>
            <td>${item.day2Ullage}</td>
            <td>${item.day3Ullage}</td>
            <td><span class="status-cell status-${item.status.toLowerCase()}">${item.status}</span></td>
            <td>${item.earliestLoad}</td>
            <td>${item.latestLoad}</td>
        `;
        tableBody.appendChild(row);
    });
    
    console.log('Page 1 updated with', processedData.length, 'rows');
}

function updatePage2() {
    updateHistoricalData();
}

/**
 * MODIFIED FUNCTION
 * Updates the historical data table to use "previousDaySale" for the sales sheet.
 */
function updateHistoricalData() {
    const selectedSheet = document.querySelector('[data-sheet].active').dataset.sheet;
    const selectedDays = parseInt(document.querySelector('input[name="historicalDays"]:checked').value);
    
    // Check if we have enough data
    if (historicalData.length < selectedDays) {
        showInsufficientDataWarning();
        return;
    }
    
    hideInsufficientDataWarning();
    
    const recentData = historicalData.slice(-selectedDays);
    
    const tableHead = document.getElementById('historicalTableHead');
    const tableBody = document.getElementById('historicalTableBody');
    
    // Clear existing content
    tableHead.innerHTML = '';
    tableBody.innerHTML = '';
    
    // Create header row
    const headerRow = document.createElement('tr');
    headerRow.innerHTML = '<th>Location</th><th>Material</th>';
    recentData.forEach(day => {
        const th = document.createElement('th');
        th.textContent = day.date || `Day ${day.day}`;
        headerRow.appendChild(th);
    });
    tableHead.appendChild(headerRow);
    
    // Create data rows
    const uniqueLocations = [...new Set(processedData.map(item => `${item.location}-${item.material}`))];
    
    uniqueLocations.forEach(locMat => {
        const [location, material] = locMat.split('-');
        const row = document.createElement('tr');
        
        const locationCell = document.createElement('td');
        locationCell.textContent = location;
        row.appendChild(locationCell);
        
        const materialCell = document.createElement('td');
        materialCell.textContent = material;
        row.appendChild(materialCell);
        
        recentData.forEach(day => {
            const cell = document.createElement('td');
            const dayItem = day.data.find(item => 
                item.location === location && item.material === material
            );
            
            if (dayItem) {
                switch (selectedSheet) {
                    case 'stock':
                        cell.textContent = dayItem.stock.toFixed(2);
                        break;
                    case 'ullage':
                        cell.textContent = dayItem.ullage.toFixed(2);
                        break;
                    case 'sales':
                        // MODIFICATION: Use previousDaySale for this sheet. Fallback to dailyAvgSale if not present.
                        const saleValue = dayItem.previousDaySale !== undefined ? dayItem.previousDaySale : dayItem.dailyAvgSale;
                        cell.textContent = saleValue.toFixed(2);
                        break;
                }
            } else {
                cell.textContent = 'N/A';
            }
            row.appendChild(cell);
        });
        
        tableBody.appendChild(row);
    });
}

function showInsufficientDataWarning() {
    const warning = document.getElementById('insufficientData');
    const tableContainer = document.getElementById('historicalTableContainer');
    
    if (warning) warning.style.display = 'block';
    if (tableContainer) tableContainer.style.display = 'none';
}

function hideInsufficientDataWarning() {
    const warning = document.getElementById('insufficientData');
    const tableContainer = document.getElementById('historicalTableContainer');
    
    if (warning) warning.style.display = 'none';
    if (tableContainer) tableContainer.style.display = 'block';
}

function updatePage3() {
    // Update summary cards
    const totalLocations = [...new Set(processedData.map(item => item.location))].length;
    const criticalLow = processedData.filter(item => item.status === 'Critical' || item.status === 'Low').length;
    const normalHigh = processedData.filter(item => item.status === 'Normal' || item.status === 'High').length;
    const avgDaysCover = processedData.length > 0 ? (processedData.reduce((sum, item) => sum + item.daysCover, 0) / processedData.length).toFixed(1) : '0';
    
    document.getElementById('totalLocations').textContent = totalLocations;
    document.getElementById('criticalLow').textContent = criticalLow;
    document.getElementById('normalHigh').textContent = normalHigh;
    document.getElementById('avgDaysCover').textContent = avgDaysCover;
    
    // Create charts
    createDaysCoverChart();
    createMaterialChart();
    createStockUllageChart();
    createTrendChart();
    
    console.log('Page 3 updated - Analytics');
}

function updatePage4() {
    // Update KPI cards
    const totalStock = processedData.reduce((sum, item) => sum + item.stock, 0).toFixed(2);
    const totalUllage = processedData.reduce((sum, item) => sum + item.ullage, 0).toFixed(2);
    const avgDailySales = processedData.length > 0 ? (processedData.reduce((sum, item) => sum + item.dailyAvgSale, 0) / processedData.length).toFixed(2) : '0';
    const criticalLocations = processedData.filter(item => item.status === 'Critical').length;
    
    document.getElementById('totalStock').textContent = totalStock;
    document.getElementById('totalUllage').textContent = totalUllage;
    document.getElementById('avgDailySales').textContent = avgDailySales;
    document.getElementById('criticalLocations').textContent = criticalLocations;
    
    // Update operational metrics
    const stockTurnover = processedData.length > 0 ? (parseFloat(totalStock) / parseFloat(avgDailySales) / 365).toFixed(2) : '0';
    const capacityUtil = processedData.length > 0 ? ((parseFloat(totalStock) / (parseFloat(totalStock) + parseFloat(totalUllage))) * 100).toFixed(1) : '0';
    
    document.getElementById('stockTurnover').textContent = stockTurnover;
    document.getElementById('capacityUtil').textContent = capacityUtil + '%';
    document.getElementById('avgLoadTime').textContent = '2.5 hrs';
    document.getElementById('efficiencyScore').textContent = '85%';
    
    // Create charts
    createStatusChart();
    createRegionalChart();
    createPriorityChart();
    
    console.log('Page 4 updated - System Overview');
}

function destroyAllCharts() {
    Object.keys(chartInstances).forEach(key => {
        if (chartInstances[key]) {
            chartInstances[key].destroy();
            delete chartInstances[key];
        }
    });
}

function createDaysCoverChart() {
    const ctx = document.getElementById('daysCoverChart');
    if (!ctx) return;
    
    // Destroy existing chart
    if (chartInstances.daysCover) {
        chartInstances.daysCover.destroy();
    }
    
    const daysCoverData = processedData.map(item => item.daysCover);
    
    chartInstances.daysCover = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: processedData.map(item => `${item.location}-${item.material}`),
            datasets: [{
                label: 'Days Cover',
                data: daysCoverData,
                backgroundColor: ['#1FB8CD', '#FFC185', '#B4413C', '#ECEBD5', '#5D878F', '#DB4545', '#D2BA4C', '#964325', '#944454', '#13343B'],
                borderColor: '#1FB8CD',
                borderWidth: 1
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                title: {
                    display: true,
                    text: 'Days Cover Distribution'
                }
            },
            scales: {
                y: {
                    beginAtZero: true
                }
            }
        }
    });
}

function createMaterialChart() {
    const ctx = document.getElementById('materialChart');
    if (!ctx) return;
    
    // Destroy existing chart
    if (chartInstances.material) {
        chartInstances.material.destroy();
    }
    
    const msData = processedData.filter(item => item.material === 'MS');
    const hsdData = processedData.filter(item => item.material === 'HSD');
    
    chartInstances.material = new Chart(ctx, {
        type: 'doughnut',
        data: {
            labels: ['MS', 'HSD'],
            datasets: [{
                data: [msData.length, hsdData.length],
                backgroundColor: ['#1FB8CD', '#FFC185']
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                title: {
                    display: true,
                    text: 'Material Distribution'
                }
            }
        }
    });
}

function createStockUllageChart() {
    const ctx = document.getElementById('stockUllageChart');
    if (!ctx) return;
    
    // Destroy existing chart
    if (chartInstances.stockUllage) {
        chartInstances.stockUllage.destroy();
    }
    
    chartInstances.stockUllage = new Chart(ctx, {
        type: 'scatter',
        data: {
            datasets: [{
                label: 'Stock vs Ullage',
                data: processedData.map(item => ({
                    x: item.stock,
                    y: item.ullage,
                    label: `${item.location}-${item.material}`
                })),
                backgroundColor: '#FFC185',
                borderColor: '#B4413C',
                borderWidth: 2
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                title: {
                    display: true,
                    text: 'Stock vs Ullage Comparison'
                }
            },
            scales: {
                x: {
                    title: {
                        display: true,
                        text: 'Stock (TKL)'
                    }
                },
                y: {
                    title: {
                        display: true,
                        text: 'Ullage (TKL)'
                    }
                }
            }
        }
    });
}

function createTrendChart() {
    const ctx = document.getElementById('trendChart');
    if (!ctx) return;
    
    updateTrendAnalysis();
}

function updateTrendAnalysis() {
    const selectedTrend = document.querySelector('[data-trend].active').dataset.trend;
    const ctx = document.getElementById('trendChart');
    if (!ctx) return;
    
    // Destroy existing chart
    if (chartInstances.trend) {
        chartInstances.trend.destroy();
    }
    
    if (selectedTrend === 'overview') {
        chartInstances.trend = new Chart(ctx, {
            type: 'line',
            data: {
                labels: processedData.map(item => `${item.location}-${item.material}`),
                datasets: [{
                    label: 'Days Cover',
                    data: processedData.map(item => item.daysCover),
                    borderColor: '#1FB8CD',
                    backgroundColor: 'rgba(31, 184, 205, 0.1)',
                    fill: true
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    title: {
                        display: true,
                        text: 'Days Cover Overview'
                    }
                }
            }
        });
    } else if (historicalData.length > 0) {
        const labels = historicalData.map(day => day.date || `Day ${day.day}`);
        let datasets = [];
        
        switch (selectedTrend) {
            case 'sales':
                datasets = [{
                    label: 'Total Sales',
                    data: historicalData.map(day => 
                        day.data.reduce((sum, item) => sum + (item.previousDaySale !== undefined ? item.previousDaySale : item.dailyAvgSale), 0)
                    ),
                    borderColor: '#DB4545',
                    backgroundColor: 'rgba(219, 69, 69, 0.1)',
                    fill: true
                }];
                break;
            case 'stock':
                datasets = [{
                    label: 'Total Stock',
                    data: historicalData.map(day => 
                        day.data.reduce((sum, item) => sum + item.stock, 0)
                    ),
                    borderColor: '#1FB8CD',
                    backgroundColor: 'rgba(31, 184, 205, 0.1)',
                    fill: true
                }];
                break;
            case 'ullage':
                datasets = [{
                    label: 'Total Ullage',
                    data: historicalData.map(day => 
                        day.data.reduce((sum, item) => sum + item.ullage, 0)
                    ),
                    borderColor: '#FFC185',
                    backgroundColor: 'rgba(255, 193, 133, 0.1)',
                    fill: true
                }];
                break;
        }
        
        chartInstances.trend = new Chart(ctx, {
            type: 'line',
            data: {
                labels: labels,
                datasets: datasets
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    title: {
                        display: true,
                        text: `${selectedTrend.charAt(0).toUpperCase() + selectedTrend.slice(1)} Trend Analysis`
                    }
                },
                scales: {
                    y: {
                        beginAtZero: true
                    }
                }
            }
        });
    }
}

function createStatusChart() {
    const ctx = document.getElementById('statusChart');
    if (!ctx) return;
    
    // Destroy existing chart
    if (chartInstances.status) {
        chartInstances.status.destroy();
    }
    
    const statusCounts = {
        'Critical': processedData.filter(item => item.status === 'Critical').length,
        'Low': processedData.filter(item => item.status === 'Low').length,
        'Normal': processedData.filter(item => item.status === 'Normal').length,
        'High': processedData.filter(item => item.status === 'High').length
    };
    
    chartInstances.status = new Chart(ctx, {
        type: 'doughnut',
        data: {
            labels: Object.keys(statusCounts),
            datasets: [{
                data: Object.values(statusCounts),
                backgroundColor: ['#DB4545', '#D2BA4C', '#1FB8CD', '#5D878F']
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                title: {
                    display: true,
                    text: 'Status Distribution'
                }
            }
        }
    });
}

function createRegionalChart() {
    const ctx = document.getElementById('regionalChart');
    if (!ctx) return;
    
    // Destroy existing chart
    if (chartInstances.regional) {
        chartInstances.regional.destroy();
    }
    
    // Regional classification
    const northern = processedData.filter(item => 
        ['Jammu', 'Jodhpur', 'Lal Kuan'].includes(item.location)
    );
    const eastern = processedData.filter(item => 
        ['Aonla', 'Banthra', 'Gonda', 'Jhansi'].includes(item.location)
    );
    
    const northernStock = northern.reduce((sum, item) => sum + item.stock, 0);
    const easternStock = eastern.reduce((sum, item) => sum + item.stock, 0);
    
    chartInstances.regional = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: ['Northern Region', 'Eastern Region'],
            datasets: [{
                label: 'Total Stock (TKL)',
                data: [northernStock, easternStock],
                backgroundColor: ['#964325', '#944454']
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                title: {
                    display: true,
                    text: 'Regional Stock Distribution'
                }
            },
            scales: {
                y: {
                    beginAtZero: true
                }
            }
        }
    });
}

function createPriorityChart() {
    const ctx = document.getElementById('priorityChart');
    if (!ctx) return;
    
    // Destroy existing chart
    if (chartInstances.priority) {
        chartInstances.priority.destroy();
    }
    
    chartInstances.priority = new Chart(ctx, {
        type: 'scatter',
        data: {
            datasets: [{
                label: 'Loading Priority',
                data: processedData.map(item => ({
                    x: item.daysCover,
                    y: item.dailyAvgSale,
                    backgroundColor: item.status === 'Critical' ? '#DB4545' : 
                                   item.status === 'Low' ? '#D2BA4C' : '#1FB8CD'
                })),
                backgroundColor: function(context) {
                    const item = processedData[context.dataIndex];
                    return item.status === 'Critical' ? '#DB4545' : 
                           item.status === 'Low' ? '#D2BA4C' : '#1FB8CD';
                },
                borderColor: '#5D878F',
                borderWidth: 1
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                title: {
                    display: true,
                    text: 'Loading Priority Matrix'
                }
            },
            scales: {
                x: {
                    title: {
                        display: true,
                        text: 'Days Cover'
                    }
                },
                y: {
                    title: {
                        display: true,
                        text: 'Daily Sales (TKL)'
                    }
                }
            }
        }
    });
}

function sortTableByEarliestLoad(direction) {
    const sortedData = [...processedData].sort((a, b) => {
        const dateA = parseDate(a.earliestLoad);
        const dateB = parseDate(b.earliestLoad);
        return direction === 'asc' ? dateA - dateB : dateB - dateA;
    });
    
    processedData = sortedData;
    updatePage1();
}

function processIndentFile(file) {
    const reader = new FileReader();
    reader.onload = function(e) {
        try {
            const data = new Uint8Array(e.target.result);
            const workbook = XLSX.read(data, { type: 'array' });
            const worksheet = workbook.Sheets[workbook.SheetNames[0]];
            const jsonData = XLSX.utils.sheet_to_json(worksheet);
            
            jsonData.forEach(row => {
                if (row.Location && row.Material && row.Quantity) {
                    indentData.push({
                        location: row.Location,
                        material: row.Material,
                        quantity: parseFloat(row.Quantity),
                        date: row.Date || new Date().toISOString().split('T')[0]
                    });
                }
            });
            
            alert('Indent file processed successfully!');
        } catch (error) {
            alert('Error processing indent file: ' + error.message);
        }
    };
    reader.readAsArrayBuffer(file);
}
