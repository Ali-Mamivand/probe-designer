library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
#library(httr)
library(jsonlite)

##########################################################
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "🧬 FISH Probe Designer Suite"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("HCR V3 Probe Design", tabName = "design", icon = icon("dna")),
      menuItem("smFISH Probe Design", tabName = 'smFISH', icon = icon('microscope')),
      menuItem("Genome Browser", tabName = "igv", icon = icon("eye")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$script(HTML("
            $(document).on('click', '.search-probe-hcr-btn', function() {
              var sequence = $(this).data('sequence');
              var probeName = $(this).data('probe');
              var leftSeq = $(this).data('left');
              var rightSeq = $(this).data('right');
              var spacerLen = $(this).data('spacer');
              
              // Send to Shiny
              Shiny.setInputValue('probe_search_hcr_clicked', {
                sequence: sequence,
                probe: probeName,
                left: leftSeq,
                right: rightSeq,
                spacer: spacerLen,
                timestamp: new Date().getTime()
              }, {priority: 'event'});
            });
          ")),
        tags$script(HTML("
        $(document).on('click', '.search-probe-btn', function() {
          var sequence = $(this).data('sequence');
          var probeName = $(this).data('probe');
          
          // Send to Shiny
          Shiny.setInputValue('probe_search_clicked', {
            sequence: sequence,
            probe: probeName,
            timestamp: new Date().getTime()
          }, {priority: 'event'});
        });
      ")),
      # IGV.js library
      tags$script(src = "https://cdn.jsdelivr.net/npm/igv@2.15.7/dist/igv.min.js"),
      
      # Google Fonts
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"
      ),
      
      # IGV JavaScript handlers
      tags$script(HTML("
        let igvBrowser = null;
        
        $(document).ready(function() {
          console.log('Document ready');
        });
        
        function initializeIGV(genome) {
          const igvDiv = document.getElementById('igvDiv');
          if (!igvDiv) {
            console.log('IGV div not found, will try again later');
            return;
          }
          
          const options = {
            genome: genome,
            locus: 'chr1:155,000,000-156,000,000',
            showRuler: true,
            showIdeogram: true,
            showCenterGuide: false,
            showCursorGuide: true
          };
          
          igv.createBrowser(igvDiv, options)
            .then(function(browser) {
              igvBrowser = browser;
              console.log('IGV browser initialized');
              Shiny.setInputValue('igv_initialized', true);
            })
            .catch(function(error) {
              console.error('Error initializing IGV:', error);
            });
        }
        
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function (e) {
          if ($(e.target).attr('href') === '#shiny-tab-igv' && !igvBrowser) {
            setTimeout(function() {
              initializeIGV('hg38');
            }, 500);
          }
        });
        
        Shiny.addCustomMessageHandler('initIGV', function(message) {
          if (igvBrowser) {
            const igvDiv = document.getElementById('igvDiv');
            igvDiv.innerHTML = '';
          }
          initializeIGV(message.genome);
        });
        
        Shiny.addCustomMessageHandler('searchLocus', function(message) {
          if (igvBrowser) {
            igvBrowser.search(message.locus);
          }
        });
        
        Shiny.addCustomMessageHandler('zoomIn', function(message) {
          if (igvBrowser) igvBrowser.zoomIn();
        });
        
        Shiny.addCustomMessageHandler('zoomOut', function(message) {
          if (igvBrowser) igvBrowser.zoomOut();
        });
        
        Shiny.addCustomMessageHandler('getSequence', function(message) {
          if (igvBrowser) {
            const currentLocus = igvBrowser.currentLoci();
            let locus = currentLocus;
            
            if (locus && typeof locus === 'string') {
              const parts = locus.split(':');
              if (parts.length === 2) {
                const chr = parts[0];
                const range = parts[1].split('-');
                if (range.length === 2) {
                  const start = Math.floor(parseFloat(range[0].replace(/,/g, '')));
                  const end = Math.floor(parseFloat(range[1].replace(/,/g, '')));
                  const length = end - start;
                  
                  Shiny.setInputValue('region_info', {
                    chr: chr, start: start, end: end,
                    locus: chr + ':' + start + '-' + end,
                    length: length
                  }, {priority: 'event'});
                  
                  if (length > 100000) return;
                  
                  const genome = message.genome;
                  const url = 'https://api.genome.ucsc.edu/getData/sequence?genome=' + genome + 
                             ';chrom=' + chr + ';start=' + start + ';end=' + end;
                  
                  const controller = new AbortController();
                  const timeoutId = setTimeout(function() {
                    controller.abort();
                  }, 10000);
                  
                  fetch(url, { signal: controller.signal, mode: 'cors' })
                    .then(function(response) {
                      clearTimeout(timeoutId);
                      if (!response.ok) throw new Error('HTTP error ' + response.status);
                      return response.json();
                    })
                    .then(function(data) {
                      if (data && data.dna) {
                        let sequence = data.dna.toUpperCase();
                        if (message.reverse_complement) {
                          sequence = reverseComplement(sequence);
                        }
                        Shiny.setInputValue('extracted_sequence', {
                          sequence: sequence, chr: chr, start: start, end: end,
                          locus: chr + ':' + start + '-' + end,
                          length: sequence.length,
                          reverse_complement: message.reverse_complement || false
                        }, {priority: 'event'});
                      } else {
                        Shiny.setInputValue('sequence_fetch_failed', 'No DNA in response', {priority: 'event'});
                      }
                    })
                    .catch(function(error) {
                      clearTimeout(timeoutId);
                      Shiny.setInputValue('sequence_fetch_failed', error.name + ': ' + error.message, {priority: 'event'});
                    });
                }
              }
            }
          }
        });
        
        function reverseComplement(seq) {
          const complement = {
            'A': 'T', 'T': 'A', 'G': 'C', 'C': 'G',
            'N': 'N', 'R': 'Y', 'Y': 'R', 'S': 'S',
            'W': 'W', 'K': 'M', 'M': 'K', 'B': 'V',
            'V': 'B', 'D': 'H', 'H': 'D'
          };
          return seq.split('').reverse().map(function(base) {
            return complement[base] || base;
          }).join('');
        }
      ")),
      
      # Enhanced  CSS
      tags$style(HTML("
        /* Global Improvements */
        body, .main-header, .sidebar, .content-wrapper {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
        }
        
        code, pre, .sequence-display, textarea {
          font-family: 'JetBrains Mono', 'Courier New', monospace !important;
        }
        
       *:not(.plotly):not(.hoverlayer):not(.hovertext)  {
          transition: all 0.2s ease-in-out;
        }
        
        /* Header with Gradient */
        .main-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
          box-shadow: 0 2px 15px rgba(0,0,0,0.1);
        }
        
        .main-header .logo {
          font-weight: 600;
          font-size: 20px;
        }
        
        /* Sidebar */
        .sidebar {
          background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%) !important;
        }
        
        .sidebar-menu > li.active > a {
          border-left: 4px solid #667eea !important;
          background: rgba(102, 126, 234, 0.15) !important;
        }
        
        .sidebar-menu > li > a:hover {
          background: rgba(255, 255, 255, 0.05) !important;
          border-left: 4px solid #667eea !important;
        }
        
        /* Enhanced Boxes */
        .box {
          border-radius: 12px !important;
          box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important;
          border: none !important;
          overflow: hidden;
          transition: transform 0.2s, box-shadow 0.2s;
          animation: fadeInUp 0.5s ease-out;
        }
        
        .box:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 20px rgba(0,0,0,0.12) !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(118, 75, 162, 0.1) 100%);
          border-bottom: 2px solid #f0f0f0 !important;
          padding: 15px 20px;
        }
        
        .box-title {
          font-weight: 600;
          font-size: 18px;
          color: #2c3e50;
        }
        
        /* Inputs */
        .form-control, .selectize-input {
          border: 2px solid #e8e8e8 !important;
          border-radius: 8px !important;
          padding: 10px 15px !important;
          font-size: 14px;
          transition: all 0.3s;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #667eea !important;
          box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1) !important;
          transform: translateY(-1px);
        }
        
        label {
          font-weight: 500;
          color: #2c3e50;
          margin-bottom: 8px;
          font-size: 13px;
          letter-spacing: 0.3px;
        }
        
        h4 {
          color: #667eea;
          font-weight: 600;
          margin-top: 20px;
          margin-bottom: 15px;
          padding-bottom: 8px;
          border-bottom: 2px solid #f0f0f0;
        }
        
        /* IGV */
        #igvDiv {
          padding: 10px;
          border: none !important;
          width: 100%;
          height: 650px;
          border-radius: 12px;
          background: rgba(255, 255, 255, 0.9);
          backdrop-filter: blur(10px);
          box-shadow: inset 0 2px 8px rgba(0,0,0,0.05);
        }
        
        /* Modern Buttons */
        .modern-btn {
          border: none !important;
          border-radius: 50px !important;
          padding: 12px 28px !important;
          font-size: 15px !important;
          font-weight: 500 !important;
          color: white !important;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
          letter-spacing: 0.3px;
        }
        
        .modern-btn:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 20px rgba(0, 0, 0, 0.2);
        }
        
        .modern-btn:active {
          transform: scale(0.98);
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        }
        
        .btn-initiator {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        }
        
        .btn-design {
          background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;
        }
        
        .btn-download {
          background: linear-gradient(135deg, #434343 0%, #000000 100%) !important;
        }
        
        /* DataTables */
        .dataTables_wrapper {
          padding: 15px;
        }
        
        table.dataTable thead th {
          background: #2c3e50 !important;
          color: white !important;
          font-weight: 600;
          padding: 12px 15px;
          border: none !important;
        }
        
        /* FixedColumns - Force header visibility */
        div.DTFC_LeftHeadWrapper table thead th,
        div.DTFC_RightHeadWrapper table thead th {
          background: #2c3e50 !important;
          color: white !important;
          font-weight: 600 !important;
          padding: 12px 15px !important;
          border: none !important;
        }
        
        div.DTFC_LeftBodyWrapper table tbody td,
        div.DTFC_RightBodyWrapper table tbody td {
          background: white !important;
        }
        
        /* Ensure proper layering */
        .dataTables_scrollHead {
          z-index: 2 !important;
        }
        
        .DTFC_LeftHeadWrapper,
        .DTFC_RightHeadWrapper {
          z-index: 3 !important;
          background: #2c3e50 !important;
        }
        
        table.dataTable tbody tr:hover {
          background: rgba(102, 126, 234, 0.05) !important;
        }
        
        table.dataTable tbody td {
          padding: 10px 15px;
          border-bottom: 1px solid #f0f0f0;
        }
        /* Alerts */
        .alert {
          border: none !important;
          border-radius: 10px !important;
          padding: 15px 20px !important;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08);
          color: #2c3e50 !important;
        }
        
        .alert-success {
          background: linear-gradient(135deg, #11998e15 0%, #38ef7d15 100%) !important;
          border-left: 4px solid #11998e !important;
          color: #0d5349 !important;
        }
        
        .alert-info {
          background: linear-gradient(135deg, #667eea15 0%, #764ba215 100%) !important;
          border-left: 4px solid #667eea !important;
          color: #4a5ac4 !important;
        }
        
        .alert-warning {
          background: linear-gradient(135deg, #f093fb15 0%, #f5576c15 100%) !important;
          border-left: 4px solid #f5576c !important;
          color: #c44569 !important;
        }
        
        .alert-danger {
          background: linear-gradient(135deg, #ff416c15 0%, #ff415615 100%) !important;
          border-left: 4px solid #ff416c !important;
          color: #d63447 !important;
        }
        
        /* Alert text elements */
        .alert strong {
          font-weight: 600;
          color: inherit;
        }
        
        .alert p {
          margin: 0;
          color: inherit;
        }
        
        .alert i, .alert .fa {
          margin-right: 8px;
          opacity: 0.8;
        }
                
        /* Progress */
        .progress {
          height: 8px;
          border-radius: 10px;
          background: #f0f0f0;
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .progress-bar {
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
          border-radius: 10px;
        }
        
        pre {
          background: #f8f9fa;
          border: none;
          border-radius: 10px;
          padding: 15px;
          font-size: 13px;
          line-height: 1.6;
          box-shadow: inset 0 2px 5px rgba(0,0,0,0.05);
        }
        
        /* Sliders */
        .js-irs-0 .irs-bar,
        .js-irs-1 .irs-bar,
        .js-irs-2 .irs-bar {
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%) !important;
        }
        
        .irs-handle {
          border: 3px solid #667eea !important;
          background: white !important;
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        }
        
        /* Modals */
        .modal-content {
          border-radius: 15px !important;
          border: none !important;
          box-shadow: 0 10px 40px rgba(0,0,0,0.2) !important;
        }
        
        .modal-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 15px 15px 0 0 !important;
          border: none !important;
          padding: 20px 25px;
        }
        
        .modal-title {
          font-weight: 600;
          font-size: 20px;
          color: white;
        }
        
        /* Scrollbar */
        ::-webkit-scrollbar {
          width: 10px;
          height: 10px;
        }
        
        ::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 10px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(180deg, #667eea 0%, #764ba2 100%);
          border-radius: 10px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(180deg, #764ba2 0%, #667eea 100%);
        }
        
        /* Stat Cards */
        .stat-card {
          padding: 20px;
          border-radius: 12px;
          color: white;
          text-align: center;
          box-shadow: 0 4px 15px rgba(0,0,0,0.15);
          transition: transform 0.3s;
        }
        
        .stat-card:hover {
          transform: translateY(-5px);
        }
        
        .stat-card h3 {
          margin: 0;
          font-weight: 600;
          font-size: 32px;
          color: white;
          border: none;
        }
        
        .stat-card p {
          margin: 5px 0 0 0;
          opacity: 0.9;
          font-size: 14px;
        }
        
        /* Status Badge */
        .status-badge {
          display: inline-block;
          padding: 5px 12px;
          border-radius: 20px;
          font-size: 12px;
          font-weight: 500;
          color: white;
        }
        
        /* Welcome Banner */
        .welcome-banner {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 30px;
          border-radius: 15px;
          margin-bottom: 20px;
          box-shadow: 0 4px 20px rgba(102, 126, 234, 0.3);
        }
        
        .welcome-banner h2 {
          margin: 0 0 10px 0;
          font-weight: 600;
          color: white;
          border: none;
        }
        
        .welcome-banner p {
          margin: 0;
          font-size: 16px;
          opacity: 0.95;
        }
        
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(20px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        @media (max-width: 768px) {
          .modern-btn {
            padding: 10px 20px !important;
            font-size: 14px !important;
          }
          
          .box {
            margin-bottom: 20px;
          }
        }
      "))
    ),
    
    tabItems(
      # HCR Tab
      tabItem(tabName = "design",
              fluidRow(
                column(12,
                       div(class = "welcome-banner",
                           h2(icon("dna"), " HCR v3 Probe Designer"),
                           p("Design hybridization chain reaction probe pairs for signal amplification in FISH experiments.",
                             tags$span(class = "status-badge", style = "background: #11998e; margin-left: 15px;",
                                       icon("check-circle"), " System Ready"))
                       )
                )
              ),
              
              fluidRow(
                box(
                  title = div(icon("cog"), " Input Parameters"), 
                  status = "primary", solidHeader = TRUE, width = 3,
                  h4(icon("globe"), " Genome Reference"),
                  selectInput("genome_hcr", "Genome for BLAT Search:",
                              choices = c("Human (hg38)" = "hg38",
                                          "Human (hg19)" = "hg19",
                                          "Mouse (mm10)" = "mm10",
                                          "Mouse (mm39)" = "mm39",
                                          "C. elegans (ce11)" = "ce11",
                                          "D. melanogaster (dm6)" = "dm6"),
                              selected = "mm10"),
                  br(),
                  h4(icon("dna"), "Sequence Input"),
                  textInput("sequence_name", "Sequence Name:", value = "Target_Gene"),
                  textAreaInput("target_sequence", "Target Sequence:", 
                                value = "GAAACACGTCTTCCCTCGAAGGTTCCCGTCGACCTAGGGAGGACCTTACCTGTTCGTGAAACACACCAGGCTGTGGGCCTCAAGGACTTGCAAGCATCCACATCTGGCCTCCAGTCCTCACCTCTTCCAGAGATGTAGCAAAAACAAAACAAAACAAAACAAAAAACCGCATGGAGTGTGTTGTTCCTAGTGACACCTGAGAGCTGGTAGTTAGTAGAGCATGTGAGTCAAGGCCTGGTCTGTGTCTCTTTTCTCTTTCTCCTTAGTTTTCTCATAGCACTAACTAATCTGTTGGGTTCATTATTGGAATTAACCTGGTGCTGGATTGTATCTAGTGCAGCTGATTTTAACAATACCTACTGTGTTCCTGGCAATAGCGTGTTCCAATTAGAAACGACCAATATTAAACTAAGAAAAGATAGGACTTTATTTTCCAGTAGATAGAAATCAATAGCTATATCCATGTACTGTAGTCCTTCAGCGTCAATGTTCATTGTCATGTTACTGATCATGCATTGTCGAGGTGGTCTGAATGTTCTGACATTAACAGTTTTCCATGAAAACGTTTTTATTGTGTTTTCAATTTATTTATTAAGATGGATTCTCAGATATTTATATTTTTATTTTATTTTTTTCTACCCTGAGGTCTTTCGACATGTGGAAAGTGAATTTGAATGAAAAATTTTAAGCATTGTTTGCTTATTGTTCCAAGACATTGTCAATAAAAGCATTTAAGTTGAATGCGAACCGCGACTGCAGCGAGCAACTGAGAAGACTGGATAGAGCCGGCGGTTCCGCGAACGAGCAGTGACCGCGCTCCCACCCAGCTCTGCTCTGCAGCTCCCACCAGTGTCTACCCCTGGACCCCTTGCCGGGCTTTCCCCAAACTTCGACC",
                                height = "100px"),
                  br(),
                  h4(icon("sliders-h"), " Probe Parameters"),
                  numericInput("tm_target", "Target Tm (°C):", value = 70, min = 50, max = 90),
                  br(),
                  sliderInput("gc_limits", "GC Content Range (%):", min = 20, max = 80, value = c(40, 60), step = 2),
                  br(),
                  sliderInput('length_interval', 'Probe Length', min = 13, max = 50, value = c(20,23)),
                  br(),
                  numericInput("na_conc", "Na+ Concentration (M):", value = 0.3, min = 0.1, max = 1.0, step = 0.1),
                  h4(icon("dna"), " HCR Specific Parameters"),
                  numericInput("spacer_length", "Distance between parts:", value = 2, min = 1, max = 5),
                  numericInput("min_gap", "Min Gap Between Probes:", value = 20, min = 2, max = 500000),
                  numericInput("num_probes", "Number of Probe Pairs:", value = 20, min = 1, max = 5000),
                  div(
                    style = "display: flex; justify-content: center; align-items: center; gap: 2px; margin-top: 25px;",
                    actionButton("Initiator", "Select Initiator", icon = icon("dna"), class = "btn-lg modern-btn btn-initiator",style = "width: 100%; padding: 11px 7px !important; font-size: 13px !important;"),
                    actionButton("design_probes", "Design Probes", icon = icon("cogs"), class = "btn-lg modern-btn btn-design",style = "width: 100%; padding: 11px 7px !important; font-size: 13px !important;"),
                    downloadButton("download_csv", "Download", class = "btn-lg modern-btn btn-download",style = "width: 100%; padding: 11px 7px !important; font-size: 13px !important;")
                  )
                ),
                
                # Right side: Visualization, Info, and Table
                column(9,
                       box(
                         title = div(icon("chart-line"), " Probe Visualization"), 
                         status = "success", solidHeader = TRUE, width = 12,
                         plotlyOutput("probe_plot", height = "400px")
                       ),
                       box(
                         title = div(icon("table"), " Designed HCR Probe Pairs"), 
                         status = "warning", solidHeader = TRUE, width = 12,
                         DT::dataTableOutput("probe_table")
                       ),
                       box(
                         title = div(icon("info-circle"), " Sequence Information"), 
                         status = "info", solidHeader = TRUE, width = 12,
                         verbatimTextOutput("sequence_info"),
                         br(),
                         h4(icon("tasks"), " Design Progress"),
                         verbatimTextOutput("design_progress"),
                         br(),
                         uiOutput("stats_dashboard")
                       )
                       
                )
              )
      ),
      
      # smFISH Tab
      tabItem(tabName = 'smFISH',
              fluidRow(
                column(12,
                       div(class = "welcome-banner",
                           h2(icon("microscope"), " smFISH Probe Designer"),
                           p("Design single molecule FISH probes.",
                             tags$span(class = "status-badge", style = "background: #11998e; margin-left: 15px;",
                                       icon("check-circle"), " System Ready"))
                       )
                )
              ),
              
              fluidRow(
                box(
                  title = div(icon("cog"), " Input Parameters"), 
                  status = "primary", solidHeader = TRUE, width = 2,
                  h4(icon("globe"), " Genome Reference"),
                  selectInput("genome_smFISH", "Genome for BLAT Search:",
                              choices = c("Human (hg38)" = "hg38",
                                          "Human (hg19)" = "hg19",
                                          "Mouse (mm10)" = "mm10",
                                          "Mouse (mm39)" = "mm39",
                                          "C. elegans (ce11)" = "ce11",
                                          "D. melanogaster (dm6)" = "dm6"),
                              selected = "mm10"),
                  h4(icon("dna"), "Sequence Input"),
                  textInput("sequence_name_smFISH", "Sequence Name:", value = "Target_Gene"),
                  textAreaInput("target_sequence_smFISH", "Target Sequence:", 
                                value = "GAAACACGTCTTCCCTCGAAGGTTCCCGTCGACCTAGGGAGGACCTTACCTGTTCGTGAAACACACCAGGCTGTGGGCCTCAAGGACTTGCAAGCATCCACATCTGGCCTCCAGTCCTCACCTCTTCCAGAGATGTAGCAAAAACAAAACAAAACAAAACAAAAAACCGCATGGAGTGTGTTGTTCCTAGTGACACCTGAGAGCTGGTAGTTAGTAGAGCATGTGAGTCAAGGCCTGGTCTGTGTCTCTTTTCTCTTTCTCCTTAGTTTTCTCATAGCACTAACTAATCTGTTGGGTTCATTATTGGAATTAACCTGGTGCTGGATTGTATCTAGTGCAGCTGATTTTAACAATACCTACTGTGTTCCTGGCAATAGCGTGTTCCAATTAGAAACGACCAATATTAAACTAAGAAAAGATAGGACTTTATTTTCCAGTAGATAGAAATCAATAGCTATATCCATGTACTGTAGTCCTTCAGCGTCAATGTTCATTGTCATGTTACTGATCATGCATTGTCGAGGTGGTCTGAATGTTCTGACATTAACAGTTTTCCATGAAAACGTTTTTATTGTGTTTTCAATTTATTTATTAAGATGGATTCTCAGATATTTATATTTTTATTTTATTTTTTTCTACCCTGAGGTCTTTCGACATGTGGAAAGTGAATTTGAATGAAAAATTTTAAGCATTGTTTGCTTATTGTTCCAAGACATTGTCAATAAAAGCATTTAAGTTGAATGCGAACCGCGACTGCAGCGAGCAACTGAGAAGACTGGATAGAGCCGGCGGTTCCGCGAACGAGCAGTGACCGCGCTCCCACCCAGCTCTGCTCTGCAGCTCCCACCAGTGTCTACCCCTGGACCCCTTGCCGGGCTTTCCCCAAACTTCGACC",
                                height = "100px"),
                  h4(icon("sliders-h"), " Probe Parameters"),
                  numericInput("tm_target_smFISH", "Target Tm (°C):", value = 70, min = 50, max = 90),
                  sliderInput("gc_limits_smFISH", "GC Content Range (%):", min = 20, max = 80, value = c(40, 60), step = 5),
                  numericInput("na_conc_smFISH", "Na+ Concentration (M):", value = 0.3, min = 0.1, max = 1.0, step = 0.1),
                  numericInput("gap_smFISH", "Min Gap Between Probes (nt):", value = 2, min = 1, max = 1000),
                  numericInput("num_probes_smFISH", "Number of Probes:", value = 40, min = 1, max = 1000),
                  checkboxInput("terminal_t", "look for probes with terminal U/T", value = TRUE),
                  
                  br(),
                  div(
                    style = "display: flex; justify-content: center; align-items: center; gap: 3px; margin-top: 25px;",
                    actionButton("design_probes_smFISH", "Design Probes", icon = icon("microscope"), class = "btn-lg modern-btn btn-design",style = "width: 100%; padding: 11px 11px !important; font-size: 13px !important;"),
                    downloadButton("download_csv_smFISH", "Download", class = "btn-lg modern-btn btn-download",style = "width: 100%; padding: 11px 11px !important; font-size: 13px !important;")
                  )
                ),
                
                # Right side: Visualization, Info, and Table
                column(10,
                       box(
                         title = div(icon("chart-line"), " Probe Visualization"), 
                         status = "success", solidHeader = TRUE, width = 12,
                         plotlyOutput("probe_plot_smFISH", height = "400px")
                       ),
                       box(
                         title = div(icon("table"), " Designed smFISH Probes"), 
                         status = "warning", solidHeader = TRUE, width = 12,
                         DT::dataTableOutput("probe_table_smFISH")
                       ),
                       box(
                         title = div(icon("info-circle"), " Sequence Information"), 
                         status = "info", solidHeader = TRUE, width = 12,
                         verbatimTextOutput("sequence_info_smFISH"),
                         br(),
                         h4(icon("tasks"), " Design Progress"),
                         verbatimTextOutput("design_progress_smFISH"),
                         br(),
                         #uiOutput("stats_dashboard_smFISH")
                       )
                )
              )
      ),
      
      tabItem(tabName = "igv",
              # Welcome Banner
              fluidRow(
                column(12,
                       div(class = "welcome-banner",
                           h2(icon("eye"), " IGV Genome Browser"),
                           p("Visualize genomic regions, extract sequences, and search with BLAT integration.",
                             tags$span(class = "status-badge", style = "background: #11998e; margin-left: 15px;",
                                       icon("check-circle"), " Browser Ready"))
                       )
                )
              ),
              
              # Navigation Controls Box
              fluidRow(
                box(
                  title = div(icon("compass"), " Navigation Controls"), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3,
                           selectInput("genome", "Select Genome:",
                                       choices = c("Human (hg38)" = "hg38",
                                                   "Human (hg19)" = "hg19",
                                                   "Mouse (mm10)" = "mm10",
                                                   "Mouse (mm39)" = "mm39",
                                                   "C. elegans (ce11)" = "ce11",
                                                   "D. melanogaster (dm6)" = "dm6"),
                                       selected = "mm10")
                    ),
                    column(3,
                           textInput("locus", "Jump to Locus:",
                                     value = "chr1:155,000,000-156,000,000",
                                     placeholder = "e.g., chr1:1000-2000 or BRCA1")
                    ),
                    column(6,
                           br(),
                           div(
                             style = "display: flex; gap: 10px; justify-content: flex-start;",
                             actionButton("go_locus", "Go", 
                                          class = "modern-btn btn-initiator", 
                                          icon = icon("search"),
                                          style = "padding: 10px 24px !important;"),
                             actionButton("zoom_in", "Zoom In", 
                                          class = "modern-btn btn-design", 
                                          icon = icon("search-plus"),
                                          style = "padding: 10px 24px !important;"),
                             actionButton("zoom_out", "Zoom Out", 
                                          class = "modern-btn btn-design", 
                                          icon = icon("search-minus"),
                                          style = "padding: 10px 24px !important;")
                           )
                    )
                  ),
                  hr(),
                  
                  # Sequence Search Section
                  fluidRow(
                    column(12,
                           div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;",
                               h5(icon("search"), " Search Sequence in Genome", style = "margin-top: 0; color: #495057;"),
                               textAreaInput("search_sequence", NULL,
                                             placeholder = "Paste DNA sequence here (20-50 bp works best)...",
                                             height = "80px",
                                             width = "100%"),
                               div(
                                 style = "display: flex; gap: 15px; justify-content: center; margin-top: 10px;",
                                 actionButton("search_seq_btn", "Find in Genome", 
                                              class = "modern-btn btn-design", 
                                              icon = icon("dna"),
                                              style = "flex: 1; max-width: 300px;"),
                                 actionButton("clear_search", "Clear Results", 
                                              class = "modern-btn btn-download", 
                                              icon = icon("times"),
                                              style = "flex: 1; max-width: 300px;")
                               ),
                               br(),
                               uiOutput("search_results_ui")
                           )
                    )
                  ),
                  hr(),
                  
                  # Extract Sequence Section
                  fluidRow(
                    column(12,
                           div(style = "text-align: center;",
                               h5(icon("dna"), " Extract Sequence from Current Region", 
                                  style = "margin-bottom: 15px; color: #3c8dbc;"),
                               div(
                                 style = "display: flex; gap: 20px; justify-content: center; align-items: center;",
                                 actionButton("copy_sequence", "Forward Strand (+)", 
                                              class = "modern-btn btn-design", 
                                              icon = icon("arrow-right"),
                                              style = "min-width: 220px;"),
                                 actionButton("copy_reverse_complement", "Reverse Complement (-)", 
                                              class = "modern-btn btn-initiator", 
                                              icon = icon("exchange-alt"),
                                              style = "min-width: 220px;")
                               )
                           )
                    )
                  )
                )
              ),
              
              # IGV Browser Display
              fluidRow(
                box(
                  title = div(icon("chart-area"), " IGV Genome Browser"), 
                  status = "success", solidHeader = TRUE, width = 12,
                  div(id = "igvDiv", style = "width: 100%; height: 650px;")
                )
              ),
              
              # Current View Information
              fluidRow(
                box(
                  title = div(icon("info-circle"), " Current View Information"), 
                  status = "info", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("view_info")
                )
              )
      ),
      
      # Help Tab

      tabItem(tabName = "help",
              # Welcome Banner
              fluidRow(
                column(12,
                       div(class = "welcome-banner",
                           h2(icon("question-circle"), " Help & Documentation"),
                           p("Learn how to use the Probe Designer Suite for your FISH experiments.",
                             tags$span(class = "status-badge", style = "background: #11998e; margin-left: 15px;",
                                       icon("book"), " User Guide"))
                       )
                )
              ),
              
              # Main Help Content
              fluidRow(
                box(
                  title = div(icon("book-open"), " User Guide"), 
                  status = "info", solidHeader = TRUE, width = 12,
                  
                  # HCR Section
                  div(
                    style = "background: linear-gradient(135deg, #667eea10 0%, #764ba210 100%); 
                       padding: 20px; border-radius: 10px; margin-bottom: 20px; 
                       border-left: 4px solid #667eea;",
                    h3(icon("dna"), " HCR v3 Probe Design", style = "color: #667eea; margin-top: 0;"),
                    p("Design hybridization chain reaction probe pairs for signal amplification in FISH experiments."),
                    tags$ul(
                      tags$li(strong("Step 1:"), " Enter your target sequence in the text box. You can also select your sequence using the Genome Browser tab"),
                      tags$li(strong("Step 2:"), " Adjust probe parameters (Tm, GC content, probe length)"),
                      tags$li(strong("Step 3:"), " Configure HCR-specific parameters (spacer length, gap between probes)"),
                      tags$li(strong("Step 4:"), " Click 'Select Initiator' to choose an HCR amplifier"),
                      tags$li(strong("Step 5:"), " Click 'Design Probes' to generate probe pairs"),
                      tags$li(strong("Step 6:"), " Review results in the table and visualization"),
                      tags$li(strong("Step 7:"), " Download results as CSV")
                    )
                  ),
                  
                  # smFISH Section
                  div(
                    style = "background: linear-gradient(135deg, #11998e10 0%, #38ef7d10 100%); 
                       padding: 20px; border-radius: 10px; margin-bottom: 20px; 
                       border-left: 4px solid #11998e;",
                    h3(icon("microscope"), " smFISH Probe Design", style = "color: #11998e; margin-top: 0;"),
                    p("Design single molecule FISH probes with optional terminal T/U requirements for TdT labeling."),
                    tags$ul(
                      tags$li(strong("Step 1:"), " Enter your target sequence in the text box. You can also select your sequence using the Genome Browser tab"),
                      tags$li(strong("Step 2:"), " Configure probe parameters (Tm, GC content range)"),
                      tags$li(strong("Step 3:"), " Set minimum gap between probes"),
                      tags$li(strong("Step 4:"), " Enable/disable terminal T requirement for TdT labeling"),
                      tags$li(strong("Step 5:"), " Click 'Design Probes' to generate probes"),
                      tags$li(strong("Step 6:"), " Download probes with sequences")
                    )
                  ),
                  
                  # IGV Section
                  div(
                    style = "background: linear-gradient(135deg, #4facfe10 0%, #00f2fe10 100%); 
                       padding: 20px; border-radius: 10px; margin-bottom: 20px; 
                       border-left: 4px solid #4facfe;",
                    h3(icon("eye"), " IGV Genome Browser", style = "color: #4facfe; margin-top: 0;"),
                    p("Interactive genome browser for visualizing genomic regions and extracting sequences."),
                    tags$ul(
                      tags$li(strong("Navigate:"), " Enter coordinates (e.g., chr1:1000-2000) or gene names to jump to regions"),
                      tags$li(strong("Zoom:"), " Use zoom buttons to adjust the view resolution"),
                      tags$li(strong("Search Sequences:"), " Paste a DNA sequence (20-100 bp) and click 'Find in Genome' to search using BLAT"),
                      tags$li(strong("Extract Sequences:"), " Click 'Forward Strand' or 'Reverse Complement' to extract sequences from the current view"),
                      tags$li(strong("Download:"), " Save extracted sequences in FASTA format for downstream analysis")
                    )
                  ),
                  
                  hr(),
                  
                  # Tips Section
                  div(
                    style = "background: linear-gradient(135deg, #f093fb10 0%, #f5576c10 100%); 
                       padding: 20px; border-radius: 10px; border-left: 4px solid #f093fb;",
                    h4(icon("lightbulb"), " Tips for Better Results", style = "color: #f093fb; margin-top: 0;"),
                    tags$ul(
                      tags$li(strong("Sequence :"), " Use clean sequences without ambiguous nucleotides (N's). It is recommended to perform a BLAST search for your gene of interest and select a unique sequence as input to increase specificity"),
                      tags$li(strong("Tm Range:"), " Adjust Tm based on your hybridization conditions (typically 65-75°C)"),
                      tags$li(strong("GC Content:"), " Aim for 40-60% GC content for optimal probe stability"),
                      tags$li(strong("Probe Length:"), " For better specificity, keep your probe sequences longer than 20 nucleotides "),
                      tags$li(strong("Spacing:"), " You can modify the distance between probe parts in HCR, but a value of 2 is advised."),
                      tags$li(strong("Verification:"), " Always verify probe specificity using BLAST before ordering"),
                      tags$li(strong("Avoid Repeats:"), " Screen sequences for repetitive elements that may lead to self-complementarity")
                    )
                  ),
                  
                  hr(),
                  
                  # Technical Notes
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                    h4(icon("info-circle"), " Technical Notes", style = "margin-top: 0;"),
                    tags$p(strong("HCR Probes:"), " Each probe pair consists of two parts separated by a spacer, with initiator sequences attached.The default spacer ‘at’ shown in the result table can be changed before ordering."),
                    tags$p(strong("smFISH Probes:"), " The terminal T requirement enables enzymatic labeling with TdT (Terminal deoxynucleotidyl Transferase). If this option is selected, the final probe will be one nucleotide longer due to the addition of a labeled T."),
                    tags$p(strong("BLAT Search:"), " Sequence searches are performed using UCSC’s BLAT tool, which provides high-sensitivity alignments. Please note that BLAT does not support sequences shorter than 20 bases."),
                    tags$p(strong("Speed:"), " Use target sequences shorter than 1000 bases for faster results. Typically, processing takes less than one minute, but designing HCR probes for longer sequences may take more time."),
                    
                  )
                )
              ),
              
              # Quick Reference Card
              fluidRow(
                column(4,
                       box(
                         title = div(icon("dna"), "HCR Default Parameters"),
                         status = "primary", solidHeader = TRUE, width = 12,
                         tags$p(strong("Target Tm:"), " 70°C"),
                         tags$p(strong("GC Range:"), " 40-60%"),
                         tags$p(strong("Probe Length:"), " 20-23 nt"),
                         tags$p(strong("Distance Between Parts:"), " 2 nt"),
                         tags$p(strong("Min Gap Between Probes:"), " 20 nt")
                       )
                ),
                column(4,
                       box(
                         title = div(icon("microscope"), "smFISH Default Parameters"),
                         status = "success", solidHeader = TRUE, width = 12,
                         tags$p(strong("Target Tm:"), " 70°C"),
                         tags$p(strong("GC Range:"), " 40-60%"),
                         tags$p(strong("Min Gap Between Probes:"), " 2 nt"),
                         tags$p(strong("Terminal T:"), " Yes"),
                         tags$p(strong("Number of Probes:"), " 40")
                       )
                ),
                column(4,
                       box(
                         title = div(icon("eye"), " IGV Default Parameters"),
                         status = "warning", solidHeader = TRUE, width = 12,
                         tags$p(strong("Genomes:"), " hg38, hg19, mm10, mm39, ce11, dm6"),
                         tags$p(strong("Search:"), " 20-5000 bp"),
                         tags$p(strong("Extract:"), " <100 kb"),
                         tags$p(strong("Format:"), " FASTA"),
                         tags$p(strong("Tool:"), " BLAT")
                       )
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  odd = toupper(c('GTCCCTgCCTCTATATCT','CCTCGTAAATCCTCATCA','CCTCAACCTACCTCCAAC','gAggAgggCAgCAAACgg','CTCACTCCCAATCTCTAT'))
  even = toupper(c('CCACTCAACTTTAACCCG','ATCATCCAGTAAACCGCC','TCTCACCATATTCgCTTC','gAAgAgTCTTCCTTTACg','CTACCCTACAAATCCAAT'))
  HCR_Amplifier = c('B3','B2','B4','B1','B5')
  initiator_df = data.frame( odd = odd, even = even, `HCR Amplifier` = HCR_Amplifier)
  initiator_df = sort_by(initiator_df,HCR_Amplifier)
  selected_init = 1
  
  # Reactive values to store results
  values <- reactiveValues(
    probe_results = NULL,
    sequence_length = 0,
    design_completed = FALSE
  )
  
  # Reactive values for smFISH
  values_smFISH <- reactiveValues(
    probe_results = NULL,
    sequence_length = 0,
    design_completed = FALSE
  )
  
  #######  functions  #############
  testSequence <- function(seq, tm_target, gc_limits, na_adj) {
    l <- nchar(seq)
    gc <- length(unlist(gregexpr('G', seq)[[1]])) + length(unlist(gregexpr('C', seq)[[1]]))
    Tm <- 100.5 + 41 * (gc) / l - 820 / l + na_adj
    gc <- (gc) / l
    valid <- gc >= gc_limits[1] & gc <= gc_limits[2] & Tm >= tm_target
    return(data.frame(seq = seq, l = l, gc = gc, Tm = Tm, valid = valid))
  }
  
  testHCRProbePair <- function(seq1, seq2, start_pos, tm_target, gc_limits, na_adj) {
    part1 <- testSequence(seq1, tm_target, gc_limits, na_adj)
    part2 <- testSequence(seq2, tm_target, gc_limits, na_adj)
    
    pair_valid <- part1$valid && part2$valid
    avg_Tm <- (part1$Tm + part2$Tm) / 2
    avg_gc <- (part1$gc * part1$l + part2$gc * part2$l) / (part1$l + part2$l)
    return(data.frame(
      part1_seq = seq1,
      part2_seq = seq2,
      part1_l = part1$l,
      part2_l = part2$l,
      part1_gc = part1$gc,
      part2_gc = part2$gc,
      part1_Tm = part1$Tm,
      part2_Tm = part2$Tm,
      avg_Tm = avg_Tm,
      avg_gc = avg_gc,
      start = start_pos,
      valid = pair_valid,
      right_initiator = initiator_df[selected_init,'odd'],
      left_initiator = initiator_df[selected_init,'even']
    ))
  }
  
  getRevCompl <- function(x) {
    t <- chartr("ACGTU", "TGCAA", gsub("[^AGCTU]", "", toupper(x)))
    return(paste(rev(unlist(strsplit(t, "")[[1]])), collapse = ""))
  }
  
  removeOverlaps <- function(solutions, min_gap) {
    if (nrow(solutions) == 0) return(solutions)
    
    solutions <- solutions[order(abs(solutions$avg_Tm - input$tm_target)), ]
    selected <- solutions[1, ]
    
    for (i in 2:nrow(solutions)) {
      current_probe <- solutions[i, ]
      current_end <- current_probe$start + current_probe$part1_l + input$spacer_length + current_probe$part2_l - 1
      
      overlaps <- FALSE
      for (j in 1:nrow(selected)) {
        selected_end <- selected[j, ]$start + selected[j, ]$part1_l + input$spacer_length + selected[j, ]$part2_l - 1
        
        if (!(current_probe$start > selected_end + min_gap || selected[j, ]$start > current_end + min_gap)) {
          overlaps <- TRUE
          break
        }
      }
      
      if (!overlaps) {
        selected <- rbind(selected, current_probe)
      }
    }
    
    return(selected)
  }
  
  ########## Display sequence information ###########
  output$sequence_info <- renderText({
    if (input$target_sequence != "") {
      clean_seq <- gsub("[^AGCTU]", "", toupper(input$target_sequence))
      values$sequence_length <- nchar(clean_seq)
      
      paste(
        "Sequence Name:", input$sequence_name, "\n",
        "Cleaned Sequence Length:", values$sequence_length, "nucleotides\n",
        "GC Content:", round(100 * (length(unlist(gregexpr('G', clean_seq)[[1]])) + 
                                      length(unlist(gregexpr('C', clean_seq)[[1]]))) / nchar(clean_seq), 1), "%\n",
        "First 50 nt:", substr(clean_seq, 1, 50), if(nchar(clean_seq) > 50) "..." else ""
      )
    } else {
      "Please enter a target sequence"
    }
  })
  
  ########## Main HCR probe design function ##########
  # Handle HCR probe search button clicks
  observeEvent(input$probe_search_hcr_clicked, {
    req(input$probe_search_hcr_clicked)
    
    probe_data <- input$probe_search_hcr_clicked
    search_seq <- probe_data$sequence
    probe_name <- probe_data$probe
    left_seq <- probe_data$left
    right_seq <- probe_data$right
    spacer_len <- probe_data$spacer
    
    # Clean the sequence
    search_seq <- gsub("[^ATCGN]", "", toupper(search_seq))
    
    if (nchar(search_seq) < 20) {
      showNotification("Sequence too short for BLAT search.", type = "warning")
      return()
    }
    
    # Use the genome from HCR tab
    genome <- input$genome_hcr
    
    # Create UCSC BLAT URL
    blat_url <- sprintf(
      "https://genome.ucsc.edu/cgi-bin/hgBlat?db=%s&type=DNA&userSeq=%s",
      genome,
      URLencode(search_seq, reserved = TRUE)
    )
    
    # Show modal with iframe
    showModal(modalDialog(
      title = div(
        icon("dna"), 
        strong(paste(" BLAT Search: ", probe_name)),
        style = "color: #2c3e50;"
      ),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("open_external_probe_hcr", "Open in New Tab", 
                     icon = icon("external-link-alt"),
                     onclick = sprintf("window.open('%s', '_blank');", blat_url)),
        modalButton("Close")
      ),
      
      # Instructions
      div(
        style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        tags$p(
          style = "margin: 0; color: #0c5460;",
          icon("info-circle"), 
          strong(" Instructions:"),
          " This HCR probe pair is searched as a combined sequence with N's representing the spacer region. ",
          "Click on any match to view it in the Genome Browser."
        )
      ),
      
      # Sequence info
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;color: 'white;",
        tags$p(style = "margin: 5px 0;", 
               strong("Probe:"), probe_name),
        tags$p(style = "margin: 5px 0;", 
               strong("Left part:"), tags$code(left_seq), " (", nchar(left_seq), " bp)"),
        tags$p(style = "margin: 5px 0;", 
               strong("Spacer:"), paste(rep("N", spacer_len), collapse = ""), " (", spacer_len, " bp)"),
        tags$p(style = "margin: 5px 0;", 
               strong("Right part:"), tags$code(right_seq), " (", nchar(right_seq), " bp)"),
        tags$p(style = "margin: 5px 0;", 
               strong("Combined:"), tags$code(substr(search_seq, 1, 50)), if(nchar(search_seq) > 50) "..." else ""),
        tags$p(style = "margin: 5px 0;", 
               strong("Total length:"), nchar(search_seq), "bp  |  ",
               strong("Genome:"), genome)
      ),
      
      # iframe for UCSC BLAT
      tags$iframe(
        src = blat_url,
        width = "100%",
        height = "600px",
        style = "border: 2px solid #dee2e6; border-radius: 5px;",
        sandbox = "allow-same-origin allow-scripts allow-forms allow-popups allow-popups-to-escape-sandbox"
      ),
      
      # Additional help
      div(
        style = "margin-top: 10px; font-size: 12px; color: #666;",
        tags$p(
          icon("lightbulb"), 
          strong(" Tip:"), 
          " The spacer region (N's) helps maintain proper spacing between probe parts during alignment. ",
          "If BLAT doesn't load, click 'Open in New Tab' button."
        )
      )
    ))
  })
  
  observeEvent(input$design_probes, {
    # Validate inputs
    if (input$target_sequence == "") {
      showNotification("Please enter a target sequence", type = "error")
      return()
    }
    
    if (values$sequence_length < input$length_interval[1] + input$spacer_length + input$length_interval[2]) {
      showNotification("Sequence too short for probe design", type = "error")
      return()
    }
    
    # Show progress
    withProgress(message = 'Designing HCR probes...', value = 0, {
      
      # Clean and prepare sequence
      clean_seq <- gsub("[^AGCTU]", "", toupper(input$target_sequence))
      input_seq <- getRevCompl(clean_seq)
      lInput <- nchar(input_seq)
      
      incProgress(0.2, detail = "Processing sequence...")
      
      # Parameters
      gc_limits <- input$gc_limits / 100
      na_adj <- 16.6 * log10(input$na_conc)
      
      # Define flexible length ranges
      min_part_length <- input$length_interval[1]
      max_part_length <- input$length_interval[2]
      
      # Pre-calculate all valid length combinations
      valid_combinations <- expand.grid(
        part1_len = min_part_length:max_part_length,
        part2_len = min_part_length:max_part_length
      )
      
      # Calculate total probe lengths for each combination
      valid_combinations$total_length <- valid_combinations$part1_len + 
        valid_combinations$part2_len + 
        input$spacer_length
      
      # Pre-allocate results list for better performance
      solutions_list <- vector("list", length = 1000)
      solution_count <- 0
      
      # Progress tracking
      total_positions <- lInput - min(valid_combinations$total_length) + 1
      positions_processed <- 0
      
      # Main optimization: iterate through positions, then test all valid combinations
      for (i in 1:lInput) {
        # Filter combinations that fit within remaining sequence length
        remaining_length <- lInput - i + 1
        applicable_combinations <- valid_combinations[
          valid_combinations$total_length <= remaining_length, 
        ]
        
        if (nrow(applicable_combinations) == 0) break
        
        # Test each applicable combination at this position
        for (combo_idx in 1:nrow(applicable_combinations)) {
          part1_len <- applicable_combinations$part1_len[combo_idx]
          part2_len <- applicable_combinations$part2_len[combo_idx]
          
          # Extract sequences
          part1_seq <- substring(input_seq, i, i + part1_len - 1)
          part2_start <- i + part1_len + input$spacer_length
          part2_seq <- substring(input_seq, part2_start, part2_start + part2_len - 1)
          
          # Quick GC content pre-screening to avoid expensive calculations
          part1_gc_count <- lengths(gregexpr('[GC]', part1_seq))
          part2_gc_count <- lengths(gregexpr('[GC]', part2_seq))
          part1_gc_frac <- part1_gc_count / part1_len
          part2_gc_frac <- part2_gc_count / part2_len
          
          # Skip if GC content is outside acceptable range
          if (part1_gc_frac >= gc_limits[1] && part1_gc_frac <= gc_limits[2] &&
              part2_gc_frac >= gc_limits[1] && part2_gc_frac <= gc_limits[2]) {
            
            # Full validation for promising candidates
            probe_pair <- testHCRProbePair(part1_seq, part2_seq, i, input$tm_target, gc_limits, na_adj)
            if (probe_pair$valid) {
              solution_count <- solution_count + 1
              
              # Expand list if needed
              if (solution_count > length(solutions_list)) {
                solutions_list <- c(solutions_list, vector("list", length = 1000))
              }
              
              solutions_list[[solution_count]] <- probe_pair
            }
          }
        }
        
        positions_processed <- positions_processed + 1
        
        # Progress update every 50 positions
        if (i %% 50 == 0) {
          incProgress(0.4 * positions_processed / total_positions,
                      detail = paste("Position", i, "of", lInput, "-", solution_count, " probes found"))
        }
      }
      
      # Combine results 
      if (solution_count > 0) {
        solutions <- do.call(rbind, solutions_list[1:solution_count])
      } else {
        solutions <- NULL
      }
      
      
      incProgress(0.8, detail = "Removing overlaps...")
      
      if (!is.null(solutions) && nrow(solutions) > 0) {
        solutions <- removeOverlaps(solutions, input$min_gap)
        
        # Select best probes
        solutions <- solutions[order(abs(solutions$avg_Tm - input$tm_target)), ]
        if (nrow(solutions) > input$num_probes) {
          solutions <- solutions[1:input$num_probes, ]
        }
        
        # Create final output
        probe_output <- data.frame(
          Name = paste(input$sequence_name, sprintf("%02d", 1:nrow(solutions)), sep = "_HCR_"),
          Odd_initiator = solutions$left_initiator,
          Left_Sequence = solutions$part2_seq,
          Left_Length = solutions$part2_l,
          Left_Tm = round(solutions$part2_Tm, 1),
          Left_GC = round(solutions$part2_gc, 3),
          Left_full_sequence = paste0(solutions$left_initiator, 'at',solutions$part2_seq),
          Right_Sequence = solutions$part1_seq,
          Even_initiator = solutions$right_initiator,
          Right_Length = solutions$part1_l,
          Right_Tm = round(solutions$part1_Tm, 1),
          Right_GC = round(solutions$part1_gc, 3),
          Right_full_sequence = paste0(solutions$part2_seq,'at',solutions$right_initiator),
          Avg_Tm = round(solutions$avg_Tm, 1),
          Avg_GC = round(solutions$avg_gc, 3),
          Start_Pos = lInput - solutions$start + 1,
          stringsAsFactors = FALSE
        )
        
        values$probe_results <- probe_output
        values$design_completed <- TRUE
        
        incProgress(1.0, detail = "Complete!")
        
        showNotification(paste("Successfully designed", nrow(probe_output), "HCR probe pairs!"), type = "message")
        
      } else {
        values$probe_results <- NULL
        values$design_completed <- FALSE
        showNotification("No valid probe pairs found. Try adjusting parameters.", type = "warning")
      }
    })
  })
  
  # Display design progress
  output$design_progress <- renderText({
    if (values$design_completed && !is.null(values$probe_results)) {
      paste(
        "Design Status: COMPLETED\n",
        "Probe pairs designed:", nrow(values$probe_results), "\n",
        "Right Tm range:", min(values$probe_results$Right_Tm), "-", max(values$probe_results$Right_Tm), "°C\n",
        "Left Tm range:", min(values$probe_results$Left_Tm), "-", max(values$probe_results$Left_Tm), "°C\n",
        "Right GC range:", round(min(values$probe_results$Right_GC), 3), "-", round(max(values$probe_results$Right_GC), 3), "\n",
        "Left GC range:", round(min(values$probe_results$Left_GC), 3), "-", round(max(values$probe_results$Left_GC), 3)
      )
    } else if (input$design_probes > 0) {
      "Design Status: No valid probes found"
    } else {
      "Design Status: Ready to design probes"
    }
  })
  
  # Probe table output
  output$probe_table <- DT::renderDataTable({
    if (!is.null(values$probe_results)) {
      # Add a column with action buttons
      table_data <- values$probe_results
      
      # Create buttons for each row
      table_data$Blat <- sapply(1:nrow(table_data), function(i) {
        spacer_ns <- paste(rep("N", input$spacer_length), collapse = "")
        combined_seq <- paste0(table_data$Left_Sequence[i], spacer_ns, table_data$Right_Sequence[i])
        
        paste0(
          '<button class="btn btn-primary btn-sm search-probe-hcr-btn" ',
          'data-sequence="', combined_seq, '" ',
          'data-probe="', table_data$Name[i], '" ',
          'data-left="', table_data$Left_Sequence[i], '" ',
          'data-right="', table_data$Right_Sequence[i], '" ',
          'data-spacer="', input$spacer_length, '" ',
          'style="background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white; padding: 5px 10px; border-radius: 4px; cursor: pointer;">',
          '<i class="fa fa-search"></i>',
          '</button>'
        )
      })
      
      DT::datatable(table_data, 
                    extensions = 'FixedColumns',
                    options = list(
                      scrollX = TRUE,
                      scrollCollapse = TRUE,
                      pageLength = 10,
                      fixedColumns = list(leftColumns = 1, rightColumns = 1),
                      columnDefs = list(
                        list(targets = 0, orderable = TRUE),
                        list(targets = ncol(table_data) - 1, orderable = FALSE)
                      )
                    ),
                    rownames = FALSE,
                    escape = FALSE) %>%
        DT::formatStyle(columns = c("Name"), backgroundColor = "#e3f2fd", fontWeight = "bold") %>%
        DT::formatStyle(columns = c(2:7), backgroundColor = "lightblue") %>%
        DT::formatStyle(columns = c(8:13), backgroundColor = "lightgreen") %>%
        DT::formatStyle(columns = c("Avg_Tm")) %>% #backgroundColor = "lightyellow") %>%
        DT::formatStyle(columns = c("Avg_GC")) %>% #backgroundColor = "lightcoral") %>%
        DT::formatStyle(columns = c("Blat"), backgroundColor = '#f5f5f5', textAlign = "center") 
    }
  })
  
  # Probe visualization
  # ==================== HCR PLOT  ====================
  # Probe visualization
  output$probe_plot <- renderPlotly({
    if (is.null(values$probe_results)) {
      return(NULL)
    }
    
    tryCatch({
      # Start with empty plot
      fig <- plot_ly()
      
      # Add baseline
      fig <- fig %>% 
        add_segments(
          x = 1, 
          xend = values$sequence_length, 
          y = 0, 
          yend = 0,
          line = list(color = "black", width = 4),
          showlegend = FALSE,
          hoverinfo = "none",
          inherit = FALSE
        )
      
      # Add each probe pair (left and right)
      for (i in 1:nrow(values$probe_results)) {
        start_pos <- values$probe_results$Start_Pos[i]
        part1_len <- values$probe_results$Right_Length[i]
        part2_len <- values$probe_results$Left_Length[i]
        
        # Right probe (part1) - Green color
        fig <- fig %>%
          add_segments(
            x = start_pos,
            xend = start_pos - part1_len + 1,
            y = i, 
            yend = i,
            line = list(color = "#11998e", width = 6),
            showlegend = FALSE,
            text = paste0(
              "<b>Probe ", i, " Right</b><br>",
              "<b>Tm:</b> ", values$probe_results$Right_Tm[i], "°C<br>",
              "<b>GC:</b> ", round(values$probe_results$Right_GC[i] * 100, 1), "%<br>",
              "<b>Length:</b> ", part1_len, " nt<br>",
              "<b>Seq:</b> ", values$probe_results$Right_Sequence[i]
            ),
            hoverinfo = "text",
            inherit = FALSE
          )
        
        # Left probe (part2) - Purple color
        part2_start <- start_pos - part1_len - input$spacer_length
        fig <- fig %>%
          add_segments(
            x = part2_start,
            xend = part2_start - part2_len + 1,
            y = i, 
            yend = i,
            line = list(color = "#667eea", width = 6),
            showlegend = FALSE,
            text = paste0(
              "<b>Probe ", i, " Left</b><br>",
              "<b>Tm:</b> ", values$probe_results$Left_Tm[i], "°C<br>",
              "<b>GC:</b> ", round(values$probe_results$Left_GC[i] * 100, 1), "%<br>",
              "<b>Length:</b> ", part2_len, " nt<br>",
              "<b>Seq:</b> ", values$probe_results$Left_Sequence[i]
            ),
            hoverinfo = "text",
            inherit = FALSE
          )
      }
      
      # Configure layout with matching hover style
      fig <- fig %>%
        layout(
          title = list(
            text = paste("HCR v3 Probe Pairs for", input$sequence_name),
            font = list(size = 16)
          ),
          xaxis = list(
            title = "Position along target (nucleotides)",
            zeroline = FALSE
          ),
          yaxis = list(
            title = "HCR Probe Pairs",
            tickmode = "array",
            tickvals = 1:nrow(values$probe_results),
            ticktext = paste("Pair", 1:nrow(values$probe_results))
          ),
          hovermode = "closest",
          hoverlabel = list(
            bgcolor = "#2c3e50",
            font = list(size = 13, color = "white", family = "Inter"),
            bordercolor = "#667eea",
            align = "left"
          )
        )
      
      return(fig)
      
    }, error = function(e) {
      message("Error in probe plot: ", e$message)
      return(NULL)
    })
  })
  
  
  # Initiator handler
  output$initiator_df = renderDT({
    datatable(initiator_df,selection = 'single')
  })
  
  observeEvent(input$Initiator, {
    showModal(modalDialog(
      fluidPage(DTOutput('initiator_df'),
                actionButton('select_seq', 'select')),
      title = NULL,
      footer = modalButton("Dismiss"),
      size = c("m", "s", "l", "xl"),
      easyClose = TRUE,
      fade = TRUE)
    )
  })
  
  observeEvent(input$select_seq,{
    selected_row <- input$initiator_df_rows_selected
    selected_init <<- selected_row
    showModal(modalDialog('Done!',easyClose = T))
  })
  
  # Download handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(input$sequence_name, "_HCR_v3_probes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$probe_results)) {
        write.csv(values$probe_results, file, row.names = FALSE)
      }
    }
  )
  
  ########################## smFISH SECTION ##########################
  # Add this in your server function
  observeEvent(input$probe_search_clicked, {
    req(input$probe_search_clicked)
    
    probe_data <- input$probe_search_clicked
    search_seq <- probe_data$sequence
    probe_name <- probe_data$probe
    
    # Clean the sequence
    search_seq <- gsub("[^ATCG]", "", toupper(search_seq))
    
    if (nchar(search_seq) < 20) {
      showNotification("Sequence too short for BLAT search.", type = "warning")
      return()
    }
    
    # Get reverse complement
    rev_comp_seq <- getRevCompl(search_seq)
    
    # Use the genome from smFISH tab
    genome <- input$genome_smFISH
    
    # Create UCSC BLAT URL
    blat_url <- sprintf(
      "https://genome.ucsc.edu/cgi-bin/hgBlat?db=%s&type=DNA&userSeq=%s",
      genome,
      URLencode(search_seq, reserved = TRUE)
    )
    
    # Show modal with iframe
    showModal(modalDialog(
      title = div(
        icon("dna"), 
        strong(paste(" BLAT Search: ", probe_name)),
        style = "color: #2c3e50;"
      ),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("open_external_probe", "Open in New Tab", 
                     icon = icon("external-link-alt"),
                     onclick = sprintf("window.open('%s', '_blank');", blat_url)),
        modalButton("Close")
      ),
      
      # Instructions
      div(
        style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        tags$p(
          style = "margin: 0; color: #0c5460;",
          icon("info-circle"), 
          strong(" Instructions:"),
          " Click on any match in the results below to view it in the Genome Browser. ",
          "You can then navigate to the IGV tab to visualize the genomic context."
        )
      ),
      
      # Sequence info
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
        tags$p(style = "margin: 5px 0;", 
               strong("Probe:"), probe_name),
        tags$p(style = "margin: 5px 0;", 
               strong("Sequence:"), tags$code(search_seq)),
        tags$p(style = "margin: 5px 0;", 
               strong("Length:"), nchar(search_seq), "bp  |  ",
               strong("Genome:"), genome)
      ),
      
      # iframe for UCSC BLAT
      tags$iframe(
        src = blat_url,
        width = "100%",
        height = "600px",
        style = "border: 2px solid #dee2e6; border-radius: 5px;",
        sandbox = "allow-same-origin allow-scripts allow-forms allow-popups allow-popups-to-escape-sandbox"
      ),
      
      # Additional help
      div(
        style = "margin-top: 10px; font-size: 12px; color: #666;",
        tags$p(
          icon("lightbulb"), 
          strong(" Tip:"), 
          " If BLAT doesn't load, click 'Open in New Tab' to view results in a separate browser window."
        )
      )
    ))
  })
  
  # Display sequence information for smFISH
  output$sequence_info_smFISH <- renderText({
    if (input$target_sequence_smFISH != "") {
      clean_seq <- gsub("[^AGCTU]", "", toupper(input$target_sequence_smFISH))
      values_smFISH$sequence_length <- nchar(clean_seq)
      
      paste(
        "Sequence Name:", input$sequence_name_smFISH, "\n",
        "Cleaned Sequence Length:", values_smFISH$sequence_length, "nucleotides\n",
        "GC Content:", round(100 * (length(unlist(gregexpr('G', clean_seq)[[1]])) + 
                                      length(unlist(gregexpr('C', clean_seq)[[1]]))) / nchar(clean_seq), 1), "%\n",
        "First 50 nt:", substr(clean_seq, 1, 50), if(nchar(clean_seq) > 50) "..." else ""
      )
    } else {
      "Please enter a target sequence"
    }
  })
  
  # Main smFISH probe design function
  observeEvent(input$design_probes_smFISH, {
    
    if (input$target_sequence_smFISH == "") {
      showNotification("Please enter a target sequence", type = "error")
      return()
    }
    
    withProgress(message = 'Designing smFISH probes...', value = 0, {
      
      # Parameters
      tm_target <- input$tm_target_smFISH
      gc_limits <- input$gc_limits_smFISH / 100
      na_adj <- 16.6 * log10(input$na_conc_smFISH)
      gap <- input$gap_smFISH
      min_length <- ceiling((-820) / (tm_target - na_adj - 100.5 - 41 * gc_limits[2]))
      
      incProgress(0.1, detail = "Processing sequence...")
      
      # Get reverse complement
      clean_seq <- gsub("[^AGCTU]", "", toupper(input$target_sequence_smFISH))
      input_seq <- getRevCompl(clean_seq)
      l_input <- nchar(input_seq)
      
      # Helper function for scanning positions
      scanPositions <- function(x) {
        n <- length(x)
        cnt <- 1
        prev <- x[1]
        local_solutions <- data.frame()
        
        while (cnt <= n) {
          while (x[cnt] - prev + 1 < min_length) {
            cnt <- cnt + 1
            if (cnt > n) break
          }
          if (cnt > n) break
          
          pointer <- x[cnt] - min_length + 1
          pprobe <- NULL
          
          repeat {
            pprobe <- testSequence(substring(input_seq, pointer, x[cnt]), tm_target, gc_limits, na_adj)
            pointer <- pointer - 1
            if (pprobe$valid == TRUE | pointer < prev | x[cnt] - pointer > 2 * min_length) break
          }
          
          if (pprobe$valid) {
            pprobe$start <- l_input - x[cnt] + pprobe$l
            local_solutions <- rbind(local_solutions, pprobe)
            prev <- x[cnt] + gap + 1
          }
          cnt <- cnt + 1
        }
        return(local_solutions)
      }
      
      incProgress(0.2, detail = "Scanning for probes...")
      
      solutions <- data.frame()
      if (input$terminal_t) {
        # With terminal T rule
        terminator <- 'T'
        posT <- unlist(gregexpr(terminator, input_seq))
        if (posT[1] != -1) {
          solutions <- scanPositions(posT)
          incProgress(0.3, detail = paste(nrow(solutions), "probes with terminal T found..."))
        }
        
        # Scan remaining sequence
        if (nrow(solutions) > 0) {
          bgn <- c(1, l_input - solutions$start + solutions$l + gap)
          edn <- c(l_input - solutions$start - gap, l_input)
          for (i in 1:length(bgn)) {
            if (edn[i] - bgn[i] > min_length) {
              posT <- bgn[i]:edn[i]
              solutions <- rbind(solutions, scanPositions(posT))
            }
          }
        }
      } else {
        # Without terminal constraint
        posT <- 1:l_input
        solutions <- scanPositions(posT)
      }
      
      incProgress(0.7, detail = "Selecting best probes...")
      
      if (nrow(solutions) > 0) {
        # Pick best solutions
        solutions <- solutions[order(abs(solutions$Tm - tm_target) + abs(nchar(as.character(solutions$seq)) - min_length)), ]
        if (nrow(solutions) > input$num_probes_smFISH) {
          solutions <- solutions[1:input$num_probes_smFISH, ]
        }
        
        # Create final output
        rownames(solutions) <- NULL
        probe_output <- data.frame(
          Name = paste(input$sequence_name_smFISH, "_", sprintf("%02d", 1:nrow(solutions)), sep = ""),
          Sequence = as.character(solutions$seq),
          Length = solutions$l,
          Tm = round(solutions$Tm, 1),
          GC = round(solutions$gc, 3),
          Start_Pos = solutions$start,
          stringsAsFactors = FALSE
        )
        #probe_output$Length = probe_output$Length - pos_adjustment
        # Create SeqToOrder (removes terminal T for TdT labeling)
        probe_output$SeqToOrder <- substr(probe_output$Sequence, 1, probe_output$Length - 1)
        probe_output$SeqToOrder[substr(probe_output$Sequence, probe_output$Length, probe_output$Length) != "T"] <- 
          as.character(probe_output$Sequence[substr(probe_output$Sequence, probe_output$Length, probe_output$Length) != "T"])
        probe_output$Length <- nchar(probe_output$SeqToOrder)
        
        # Reorder columns
        probe_output <- probe_output[, c("Name", "SeqToOrder", "Tm", "Length", "GC", "Start_Pos")]
        
        values_smFISH$probe_results <- probe_output
        values_smFISH$design_completed <- TRUE
        
        incProgress(1.0, detail = "Complete!")
        
        showNotification(paste("Successfully designed", nrow(probe_output), "smFISH probes!"), type = "message")
        
      } else {
        values_smFISH$probe_results <- NULL
        values_smFISH$design_completed <- FALSE
        showNotification("No valid probes found. Try adjusting parameters.", type = "warning")
      }
    })
  })
  
  # Display design progress for smFISH
  output$design_progress_smFISH <- renderText({
    if (values_smFISH$design_completed && !is.null(values_smFISH$probe_results)) {
      paste(
        "Design Status: COMPLETED\n",
        "Probes designed:", nrow(values_smFISH$probe_results), "\n",
        "Tm range:", min(values_smFISH$probe_results$Tm), "-", max(values_smFISH$probe_results$Tm), "°C\n",
        "GC range:", round(min(values_smFISH$probe_results$GC), 3), "-", round(max(values_smFISH$probe_results$GC), 3), "\n",
        "Length range:", min(values_smFISH$probe_results$Length), "-", max(values_smFISH$probe_results$Length), "nt"
      )
    } else if (input$design_probes_smFISH > 0) {
      "Design Status: No valid probes found"
    } else {
      "Design Status: Ready to design probes"
    }
  })
  
  # Probe table output for smFISH
  output$probe_table_smFISH <- DT::renderDataTable({
    if (!is.null(values_smFISH$probe_results)) {
      # Add a column with action buttons
      table_data <- values_smFISH$probe_results
      
      # Create buttons for each row
      table_data$Blat <- sapply(1:nrow(table_data), function(i) {
        paste0(
          '<button class="btn btn-primary btn-sm search-probe-btn" ',
          'data-sequence="', table_data$SeqToOrder[i], '" ',
          'data-probe="', table_data$Name[i], '" ',
          'style="background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white; padding: 5px 10px; border-radius: 4px; cursor: pointer;">',
          '<i class="fa fa-search"></i>',
          '</button>'
        )
      })
      
      DT::datatable(table_data, 
                    extensions = 'FixedColumns',
                    options = list(
                      scrollX = TRUE,
                      scrollCollapse = TRUE,
                      pageLength = 10,
                      fixedColumns = list(leftColumns = 1, rightColumns = 1),
                      columnDefs = list(
                        list(targets = 0, orderable = TRUE),
                        list(targets = ncol(table_data) - 1, orderable = FALSE)
                      )
                    ),
                    rownames = FALSE,
                    escape = FALSE) %>%  
        DT::formatStyle(columns = c("Name"), backgroundColor = "#e3f2fd", fontWeight = "bold") %>%
        DT::formatStyle(columns = c("Tm"), ) %>%
        DT::formatStyle(columns = c("GC"), ) %>%
        DT::formatStyle(columns = c("Length")) %>%
        DT::formatStyle(columns = c("Blat"), backgroundColor = "#f5f5f5", textAlign = "center")
    }
  })
  
  # Probe visualization for smFISH

  output$probe_plot_smFISH <- renderPlotly({
  if (is.null(values_smFISH$probe_results)) {
    return(NULL)
  }
  
  tryCatch({
    boundaries <- data.frame(
      x0 = values_smFISH$probe_results$Start_Pos,
      x1 = values_smFISH$probe_results$Start_Pos + values_smFISH$probe_results$Length - 1,
      y = 1:nrow(values_smFISH$probe_results),
      probe_info = paste0("<b>Probe ", 1:nrow(values_smFISH$probe_results), "</b><br>",
                          "<b>Tm:</b> ", round(values_smFISH$probe_results$Tm, 1), "°C<br>",
                          "<b>GC:</b> ", round(values_smFISH$probe_results$GC*100, 1), "%<br>",
                          "<b>Length:</b> ", values_smFISH$probe_results$Length, " nt<br>",
                          "<b>Start:</b> ", values_smFISH$probe_results$Start_Pos),
      stringsAsFactors = FALSE
    )
    
    p <- plot_ly() %>%
      add_segments(
        x = 1, 
        xend = values_smFISH$sequence_length,
        y = 0, 
        yend = 0,
        line = list(color = "black", width = 4),
        showlegend = FALSE,
        hoverinfo = "none",
        inherit = FALSE
      ) %>%
      add_segments(
        data = boundaries,
        x = ~x0,
        xend = ~x1,
        y = ~y,
        yend = ~y,
        line = list(color = "#1f77b4", width = 6),
        text = ~probe_info,
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(
          text = paste("smFISH Probes for", input$sequence_name_smFISH),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Position along target (nucleotides)",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "smFISH Probes",
          tickmode = "array",
          tickvals = 1:nrow(values_smFISH$probe_results),
          ticktext = paste("Probe", 1:nrow(values_smFISH$probe_results))
        ),
        hovermode = "closest",
        hoverlabel = list(
          bgcolor = "#2c3e50",
          font = list(size = 13, color = "white", family = "Inter"),
          bordercolor = "#667eea",
          align = "left"
        )
      )
    
    return(p)
    
  }, error = function(e) {
    message("Error in probe plot: ", e$message)
    return(NULL)
  })
})
  # Download handler for smFISH
  output$download_csv_smFISH <- downloadHandler(
    filename = function() {
      paste(input$sequence_name_smFISH, "_smFISH_probes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values_smFISH$probe_results)) {
        write.csv(values_smFISH$probe_results, file, row.names = FALSE)
      }
    }
  )
  
  # searchSequenceBlat
  
  searchSequenceBlat <- function(sequence, genome) {
    tryCatch({
      # Map genome names
      species_map <- c(
        "hg38" = "human",
        "hg19" = "human",
        "mm10" = "mouse",
        "mm39" = "mouse"
      )
      
      species <- species_map[genome]
      if (is.na(species)) species <- "human"
      
      # Use Ensembl REST API
      url <- paste0("https://rest.ensembl.org/sequence/region/", species, "/", sequence, "?content-type=application/json")
      
      # This is a simple approach - for exact matching, we'd need BLAT equivalent
      # For now, let's tell the user to use UCSC manually
      showNotification(
        "Due to UCSC bot protection, please use the 'Open in BLAT' link below to search manually.",
        type = "warning",
        duration = 10
      )
      
      return(list())
      
    }, error = function(e) {
      return(list())
    })
  }
  
  # search_seq_btn observeEvent :
  
  observeEvent(input$search_seq_btn, {
    req(input$search_sequence)
    
    # Clean the sequence
    search_seq <- gsub("[^ATCG]", "", toupper(input$search_sequence))
    
    if (nchar(search_seq) < 20) {
      showNotification("Sequence too short. Please enter at least 20 nucleotides.", type = "warning")
      return()
    }
    
    if (nchar(search_seq) > 5000) {
      showNotification("Sequence too long. Please use 20-5000 nucleotides.", type = "warning")
      return()
    }
    
    # Get reverse complement
    rev_comp_seq <- getRevCompl(search_seq)
    
    # Store search info
    search_results(list(
      forward = search_seq,
      reverse = rev_comp_seq,
      length = nchar(search_seq),
      matches = list()
    ))
    
    # Create UCSC BLAT URL
    blat_url <- sprintf(
      "https://genome.ucsc.edu/cgi-bin/hgBlat?db=%s&type=DNA&userSeq=%s",
      input$genome,
      URLencode(search_seq, reserved = TRUE)
    )
    
    # Show modal with iframe
    showModal(modalDialog(
      title = div(
        icon("dna"), 
        strong(" UCSC BLAT Search Results"),
        style = "color: #2c3e50;"
      ),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("open_external", "Open in New Tab", 
                     icon = icon("external-link-alt"),
                     onclick = sprintf("window.open('%s', '_blank');", blat_url)),
        modalButton("Close")
      ),
      
      # Instructions
      div(
        style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        tags$p(
          style = "margin: 0; color: #0c5460;",
          icon("info-circle"), 
          strong(" Instructions:"),
          " Click on any match in the results below to view it in the Genome Browser. ",
          "Copy the coordinates (e.g., chr1:12345-67890) from the browser location and paste them into the IGV navigation box above."
        )
      ),
      
      # Sequence info
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
        tags$p(style = "margin: 5px 0;", 
               strong("Searching:"), tags$code(substr(search_seq, 1, 50)), 
               if(nchar(search_seq) > 50) "..." else ""),
        tags$p(style = "margin: 5px 0;", 
               strong("Length:"), nchar(search_seq), "bp  |  ",
               strong("Genome:"), input$genome)
      ),
      
      # iframe for UCSC BLAT
      tags$iframe(
        src = blat_url,
        width = "100%",
        height = "600px",
        style = "border: 2px solid #dee2e6; border-radius: 5px;",
        sandbox = "allow-same-origin allow-scripts allow-forms allow-popups allow-popups-to-escape-sandbox"
      ),
      
      # Additional help
      div(
        style = "margin-top: 10px; font-size: 12px; color: #666;",
        tags$p(
          icon("lightbulb"), 
          strong(" Tip:"), 
          " If BLAT doesn't load, click 'Open in New Tab' to view results in a separate browser window."
        )
      )
    ))
  })
  
  # update the search_results_ui to show that BLAT search is available:
  output$search_results_ui <- renderUI({
    results <- search_results()
    
    if (is.null(results)) {
      return(
        tags$div(
          style = "margin-top: 10px; padding: 10px; background-color: #e9ecef; border-radius: 4px;",
          tags$small(icon("info-circle"), " Enter a DNA sequence (20-5000 bp) to search in the genome.")
        )
      )
    }
    
    # Show that search was initiated
    tagList(
      tags$div(
        style = "margin-top: 10px; padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;",
        tags$h6(
          icon("check-circle"), 
          strong(" BLAT Search Opened"),
          style = "margin-top: 0; color: #155724;"
        ),
        tags$p(strong("Sequence length:"), results$length, "bp"),
        tags$p(
          icon("info-circle"),
          " The BLAT search results are displayed in the popup window. ",
          "Click on any match to view it in the Genome Browser, then copy the coordinates to navigate in IGV."
        ),
        hr(),
        tags$p(
          "If the popup was blocked or closed, click below to search again:"
        ),
        tags$button(
          "Reopen BLAT Search",
          class = "btn btn-primary btn-sm",
          onclick = "document.getElementById('search_seq_btn').click();"
        )
      )
    )
  })
  # Update the search_results_ui output:
  output$search_results_ui <- renderUI({
    results <- search_results()
    
    if (is.null(results)) {
      return(
        tags$div(
          style = "margin-top: 10px; padding: 10px; background-color: #e9ecef; border-radius: 4px;",
          tags$small(icon("info-circle"), " Enter a DNA sequence (20-5000 bp) to search in the genome.")
        )
      )
    }
    
    matches <- results$matches
    
    if (length(matches) == 0) {
      return(
        tagList(
          tags$div(
            style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;",
            tags$h6(icon("search"), strong(" Search Results"), style = "margin-top: 0; color: #856404;"),
            tags$p(strong("Sequence length:"), results$length, "bp"),
            #tags$p(icon("times-circle"), " No exact matches found."),
            hr(),
            #tags$p("Try searching manually with UCSC BLAT:"),
            tags$a("Open in BLAT", 
                   href = paste0("https://genome.ucsc.edu/cgi-bin/hgBlat?db=", input$genome, 
                                 "&type=DNA&userSeq=", results$forward),
                   target = "_blank",
                   class = "btn btn-primary btn-sm")
          )
        )
      )
    }
    
    # Create list of matches with jump buttons
    match_items <- lapply(seq_along(matches), function(i) {
      m <- matches[[i]]
      locus_str <- sprintf("%s:%s-%s", 
                           m$chr,
                           format(m$start, big.mark = ","),
                           format(m$end, big.mark = ","))
      
      tags$div(
        style = "margin-bottom: 8px; padding: 8px; background-color: white; border-radius: 3px;",
        tags$span(
          style = "font-weight: bold; color: #2c3e50;",
          sprintf("Match %d: ", i)
        ),
        tags$code(locus_str),
        tags$button(
          "View in IGV",
          class = "btn btn-sm btn-primary",
          style = "margin-left: 10px; padding: 2px 8px;",
          onclick = sprintf(
            "Shiny.setInputValue('jump_to_match', {index: %d}, {priority: 'event'});",
            i
          )
        )
      )
    })
    
    tagList(
      tags$div(
        style = "margin-top: 10px; padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;",
        tags$h6(
          icon("check-circle"), 
          strong(sprintf(" Found %d match(es)", length(matches))),
          style = "margin-top: 0; color: #155724;"
        ),
        tags$p(strong("Sequence length:"), results$length, "bp"),
        tags$div(
          style = "max-height: 200px; overflow-y: auto; margin-top: 10px;",
          match_items
        )
      )
    )
  })
  

  getRevCompl <- function(seq) {
    complement_map <- c(
      'A' = 'T', 'T' = 'A', 
      'G' = 'C', 'C' = 'G',
      'N' = 'N', 'R' = 'Y', 'Y' = 'R',
      'S' = 'S', 'W' = 'W', 'K' = 'M', 
      'M' = 'K', 'B' = 'V', 'V' = 'B',
      'D' = 'H', 'H' = 'D'
    )
    seq <- toupper(seq)
    seq_chars <- strsplit(seq, "")[[1]]
    complement_chars <- sapply(seq_chars, function(base) {
      if (base %in% names(complement_map)) complement_map[base] else base
    })
    paste(rev(complement_chars), collapse = "")
  }
  igv_ready <- reactiveVal(FALSE)
  
  observeEvent(input$igv_initialized, {
    igv_ready(TRUE)
  })
  
  observeEvent(input$genome, {
    session$sendCustomMessage(type = "initIGV", message = list(genome = input$genome))
  }, ignoreInit = TRUE)
  
  observeEvent(input$go_locus, {
    req(input$locus)
    if (igv_ready()) {
      session$sendCustomMessage(type = "searchLocus", message = list(locus = input$locus))
    }
  })
  
  observeEvent(input$zoom_in, {
    if (igv_ready()) session$sendCustomMessage(type = "zoomIn", message = list())
  })
  
  observeEvent(input$zoom_out, {
    if (igv_ready()) session$sendCustomMessage(type = "zoomOut", message = list())
  })
  
  observeEvent(input$copy_sequence, {
    if (igv_ready()) {
      showNotification("Extracting sequence...", id = "extract_msg", duration = NULL)
      session$sendCustomMessage(type = "getSequence", message = list(genome = input$genome, reverse_complement = FALSE))
    }
  })
  
  observeEvent(input$copy_reverse_complement, {
    if (igv_ready()) {
      showNotification("Extracting reverse complement...", id = "extract_msg", duration = NULL)
      session$sendCustomMessage(type = "getSequence", message = list(genome = input$genome, reverse_complement = TRUE))
    }
  })
  
  observeEvent(input$region_info, {
    region <- input$region_info
    if (region$length > 100000) {
      removeNotification(id = "extract_msg")
      showNotification("Region too large. Please zoom in to <100kb.", type = "warning")
    } else {
      removeNotification(id = "extract_msg")
      showNotification(paste("Fetching", format(region$length, big.mark = ","), "bp..."), 
                       id = "extract_msg", duration = NULL)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$extracted_sequence, {
    removeNotification(id = "extract_msg")
    seq_data <- input$extracted_sequence
    if (is.null(seq_data)) return()
    
    seq <- seq_data$sequence
    formatted_seq <- paste(sapply(seq(1, nchar(seq), 60), function(i) {
      substr(seq, i, min(i+59, nchar(seq)))
    }), collapse = "\n")
    
    strand_info <- if (seq_data$reverse_complement) " (- strand)" else " (+ strand)"
    fasta_text <- paste0(">", seq_data$locus, strand_info, " length=", seq_data$length, "bp\n", formatted_seq)
    
    showModal(modalDialog(
      title = paste("Sequence from", seq_data$locus, strand_info),
      size = "l",
      tags$div(class = "alert alert-success", icon("check-circle"), strong(" Sequence extracted!")),
      tags$p(strong("Length:"), format(seq_data$length, big.mark = ","), "bp"),
      tags$p(strong("Strand:"), if (seq_data$reverse_complement) "- (reverse complement)" else "+ (forward)"),
      hr(),
      tags$textarea(id = "seq_modal_text",
                    style = "width: 100%; height: 300px; font-family: monospace; font-size: 12px;",
                    fasta_text),
      tags$script(HTML("
        setTimeout(function() {
          var textarea = document.getElementById('seq_modal_text');
          if (textarea) textarea.select();
        }, 100);
      ")),
      easyClose = TRUE,
      footer = tagList(
        downloadButton("download_seq", "Download FASTA"),
        modalButton("Close")
      )
    ))
  }, ignoreInit = TRUE)
  
  output$download_seq <- downloadHandler(
    filename = function() {
      if (!is.null(input$extracted_sequence)) {
        seq_data <- input$extracted_sequence
        strand <- if (seq_data$reverse_complement) "_minus" else "_plus"
        paste0(seq_data$chr, "_", seq_data$start, "-", seq_data$end, strand, ".fasta")
      } else "sequence.fasta"
    },
    content = function(file) {
      if (!is.null(input$extracted_sequence)) {
        seq_data <- input$extracted_sequence
        seq <- seq_data$sequence
        formatted_seq <- paste(sapply(seq(1, nchar(seq), 60), function(i) {
          substr(seq, i, min(i+59, nchar(seq)))
        }), collapse = "\n")
        strand_info <- if (seq_data$reverse_complement) " (- strand)" else " (+ strand)"
        fasta_content <- paste0(">", seq_data$locus, strand_info, " length=", seq_data$length, "bp\n", formatted_seq, "\n")
        writeLines(fasta_content, file)
      }
    }
  )
  
  output$view_info <- renderText({
    status <- if (igv_ready()) "Ready" else "Initializing..."
    paste("IGV Status:", status, "\n",
          "Genome:", input$genome, "\n",
          "Current Locus:", input$locus)
  })
  
  # Sequence search functionality
  search_results <- reactiveVal(NULL)
  
 
  observeEvent(input$clear_search, {
    search_results(NULL)
    updateTextAreaInput(session, "search_sequence", value = "")
  })
  
  # Add to server function - Stats Dashboard for HCR
  output$stats_dashboard <- renderUI({
    if (values$design_completed && !is.null(values$probe_results)) {
      fluidRow(
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);",
                 h3(nrow(values$probe_results)),
                 p("Probe Pairs")
               )
        ),
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);",
                 h3(paste0(round(mean(values$probe_results$Avg_Tm), 1), "°C")),
                 p("Avg Tm")
               )
        ),
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);",
                 h3(paste0(round(mean(values$probe_results$Avg_GC) * 100, 1), "%")),
                 p("Avg GC Content")
               )
        ),
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);",
                 h3(paste0(round(mean(c(values$probe_results$Left_Length, values$probe_results$Right_Length))), " nt")),
                 p("Avg Length")
               )
        )
      )
    }
  })
  
  # Stats Dashboard for smFISH
  output$stats_dashboard_smFISH <- renderUI({
    if (values_smFISH$design_completed && !is.null(values_smFISH$probe_results)) {
      fluidRow(
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);",
                 h3(nrow(values_smFISH$probe_results)),
                 p("Probes")
               )
        ),
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);",
                 h3(paste0(round(mean(values_smFISH$probe_results$Tm), 1), "°C")),
                 p("Avg Tm")
               )
        ),
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);",
                 h3(paste0(round(mean(values_smFISH$probe_results$GC) * 100, 1), "%")),
                 p("Avg GC")
               )
        ),
        column(3,
               div(
                 class = "stat-card",
                 style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);",
                 h3(paste0(round(mean(values_smFISH$probe_results$Length)), " nt")),
                 p("Avg Length")
               )
        )
      )
    }
  })
  
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))