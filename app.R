library(shiny)
library(shinyjs)
library(igraph)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(readxl)
library(colourpicker)
library(bslib)
library(stringr)
library(purrr)
library(openxlsx)
library(reactable)
library(shinyWidgets)
library(writexl)

# Workaround for Chromium Issue 468227 (Fixes Shinylive .htm download bug)
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define UI for the application
ui <- page_navbar(
  title = div(
    class = "brand-title",
    "Open",
    span("Linkography")
  ),
  
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#2d3436",
    primary = "#0A4F57",
    secondary = "#06AED5",
    base_font = "Inter",
    heading_font = "Outfit"
  ),
  
  header = tagList(
    useShinyjs(),
    
    tags$head(
      
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;700&family=Outfit:wght@400;700;800;850&display=swap", rel = "stylesheet"),
      
      tags$style(HTML("
      :root {
        --cater-midnight: #0A4F57;
        --cater-latte: #FFFAEA;
        --cater-salmon: #FA8072;
        --cater-electric: #06AED5;
        --cater-white: #FFFFFF;
        --cater-text: #2d3436;
        --cater-page: #F6F7F9;
        --cater-border: rgba(10, 79, 87, 0.14);
      }

      html,
      body {
        height: 100%;
        margin: 0;
        overflow: hidden;
        background: var(--cater-white);
        color: var(--cater-text);
        overscroll-behavior: none;
      }

      .navbar {
        min-height: 54px;
        padding-top: 3px;
        padding-bottom: 3px;
        background: var(--cater-white) !important;
        box-shadow: 0 2px 14px rgba(10, 79, 87, 0.10);
        z-index: 1000;
      }

      .navbar .container-fluid {
        min-height: 48px;
      }
      
      /* Main Navbar Tab Styling */
      .navbar-nav .nav-link {
        font-weight: 700 !important;
        font-size: 0.92rem !important;
        color: #5B6770 !important;
        padding: 6px 10px !important; /* <--- NEW: Tighter internal padding */
        margin: 0 2px !important;     /* <--- NEW: Brings the tabs physically closer together */
        border-radius: 8px !important;
        transition: all 0.2s ease !important;
        min-width: 175px !important;  /* <--- NEW: Slightly smaller width so short words don't float in empty space */
        text-align: center !important;
      }

      .navbar-nav .nav-link:hover {
        color: var(--cater-midnight) !important;
        background: rgba(250, 128, 114, 0.1) !important;
      }

      .navbar-nav .nav-link.active {
        color: var(--cater-white) !important;
        background: var(--cater-salmon) !important; 
        box-shadow: 0 4px 12px rgba(250, 128, 114, 0.35) !important;
      }

      .brand-title {
        font-weight: 800;
        letter-spacing: 0.02em;
        color: var(--cater-midnight);
        margin-right: 40px;
      }

      .brand-title span {
        font-weight: 500;
        color: var(--cater-salmon);
        margin-left: 0px;
      }

      .bslib-page-navbar {
        height: 100vh;
        overflow: hidden;
      }

      .tab-content {
        height: calc(100vh - 54px);
        overflow: hidden;
      }

      .tab-pane {
        height: 100%;
        overflow: hidden;
      }

      .plot-shell,
      .table-page {
        height: 100%;
        min-height: 0;
        overflow: hidden;
        padding: 10px 12px 12px 12px;
        box-sizing: border-box;
        background: var(--cater-page);
      }

      .plot-stage {
        height: 100%;
        min-height: 0;
        display: grid;
        grid-template-columns: 60px 320px minmax(0, 1fr);
        gap: 12px;
      }

      .tool-rail {
        height: 100%;
        min-height: 0;
        background: var(--cater-midnight);
        border-radius: 18px;
        padding: 8px 7px;
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 7px;
        box-shadow: 0 10px 24px rgba(10, 79, 87, 0.16);
      }

      .tool-button {
        width: 44px !important;
        height: 44px !important;
        min-width: 44px !important;
        padding: 0 !important;
        border-radius: 13px !important;
        border: 0 !important;
        background: rgba(255, 255, 255, 0.12) !important;
        color: rgba(255, 255, 255, 0.86) !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        box-shadow: none !important;
      }

      .tool-button:hover {
        background: rgba(255, 255, 255, 0.22) !important;
        color: white !important;
      }

      .tool-button.active {
        background: var(--cater-white) !important;
        color: var(--cater-midnight) !important;
      }

      .tool-spacer {
        flex: 1 1 auto;
      }

      .settings-pane,
      .plot-holder,
      .plot-card,
      .control-card {
        background: var(--cater-white);
        border: 0;
        border-radius: 18px;
        box-shadow: 0 8px 22px rgba(31, 41, 51, 0.08);
        overflow: hidden;
      }

      .settings-pane,
      .plot-holder {
        height: 100%;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      .settings-header,
      .settings-note,
      .plot-subtitle {
        display: none;
      }

      .settings-body {
        flex: 1 1 auto;
        min-height: 0;
        overflow-y: auto;
        overflow-x: hidden;
        padding: 14px 16px 16px 16px;
      }

      .settings-body::-webkit-scrollbar {
        width: 8px;
      }

      .settings-body::-webkit-scrollbar-track {
        background: #EEF1F4;
        border-radius: 999px;
      }

      .settings-body::-webkit-scrollbar-thumb {
        background: #B8C4CA;
        border-radius: 999px;
      }

      .settings-body .tab-content,
      .settings-body .tab-pane {
        height: auto;
        overflow: visible;
      }

      .settings-body .shiny-input-container,
      .settings-body .form-control {
        width: 100% !important;
        max-width: 100% !important;
      }

      .settings-body .shiny-input-container {
        margin-bottom: 12px;
      }

      .settings-section-title {
        font-size: 0.74rem;
        font-weight: 850;
        color: var(--cater-midnight);
        text-transform: uppercase;
        letter-spacing: 0.08em;
        margin: 4px 0 10px 0;
      }

      .metric-box {
        background: #EEF7F8;
        border: 1px solid #D5EAED;
        border-radius: 13px;
        padding: 10px 12px;
        margin-top: 10px;
        margin-bottom: 12px;
      }

      .metric-label {
        display: block;
        text-transform: uppercase;
        font-size: 0.7rem;
        letter-spacing: 0.08em;
        color: #567178;
        font-weight: 800;
        margin-bottom: 4px;
      }

      .metric-box pre {
        margin: 0;
        padding: 0;
        background: transparent;
        border: 0;
        color: var(--cater-midnight);
        font-weight: 700;
        white-space: normal;
      }

      .plot-holder-header {
        flex: 0 0 42px;
        height: 42px;
        padding: 8px 14px;
        border-bottom: 1px solid #E7EAEE;
        display: flex;
        align-items: center;
      }

      .plot-title {
        font-weight: 850;
        color: var(--cater-midnight);
      }

      .plot-holder-body {
        flex: 1 1 auto;
        min-height: 0;
        overflow: hidden;
        padding: 2px;
      }

      .plot-holder-body .shiny-plot-output {
        height: 100% !important;
        width: 100% !important;
      }

      .control-pair {
        display: grid;
        grid-template-columns: 1fr;
        gap: 0;
      }

      .btn-default,
      .btn-primary {
        border-radius: 999px;
        font-weight: 700;
      }

      .plot-card .card-header,
      .control-card .card-header {
        background: var(--cater-midnight);
        border-bottom: 4px solid var(--cater-salmon);
        padding: 13px 17px;
        font-weight: 850;
        color: var(--cater-white);
        letter-spacing: 0.01em;
      }
      
      /* Custom Modern File Input */
      .settings-body .input-group {
        display: flex !important;
        flex-wrap: nowrap !important;
        align-items: stretch;
        border-radius: 999px !important;
        border: 1px solid rgba(10, 79, 87, 0.2) !important;
        overflow: hidden; /* Clips the square buttons to fit the pill shape */
        background: var(--cater-white);
        box-shadow: 0 2px 6px rgba(10, 79, 87, 0.04);
      }

      .settings-body .input-group-prepend,
      .settings-body .input-group-btn {
        display: flex;
        margin: 0 !important;
      }

      .settings-body .input-group-btn .btn-file {
        border-radius: 0 !important; 
        border: none !important;
        background: var(--cater-midnight) !important;
        color: var(--cater-white) !important;
        font-weight: 700 !important;
        padding: 8px 16px !important;
        margin: 0 !important;
        display: flex;
        align-items: center;
        white-space: nowrap;
        transition: all 0.2s ease;
      }

      .settings-body .input-group-btn .btn-file:hover {
        background: #0d606a !important; /* Slightly lighter on hover */
      }

      .settings-body .input-group .form-control {
        border: none !important;
        background: transparent !important;
        box-shadow: none !important;
        padding: 8px 12px !important;
        color: var(--cater-text) !important;
        font-size: 0.82rem !important;
        height: auto !important;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis; /* Adds '...' if the filename is super long */
      }

      /* Table page layout */
      .table-page .plot-card,
      .table-page > .card {
        height: 100%;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      .table-page .card-body {
        flex: 1 1 auto;
        min-height: 0;
        overflow: hidden;
        display: flex;
        flex-direction: column;
        padding: 12px;
      }

      .table-page .tabbable {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      .table-page .tab-content {
        flex: 1 1 auto;
        height: auto;
        min-height: 0;
        overflow: hidden;
      }

      .table-page .tab-pane {
        height: 100%;
        min-height: 0;
        overflow: hidden;
        display: flex;
        flex-direction: column;
      }

      /* Reactable specific sizing to fill container */
      .reactable {
        flex: 1 1 auto;
        height: 100% !important;
      }

      /* CATER tabs */
      .table-page .nav-tabs {
        flex: 0 0 auto;
        border-bottom: 0 !important;
        gap: 6px;
        margin-bottom: 14px;
        padding: 8px;
        background: var(--cater-white);
        border: 1px solid rgba(10, 79, 87, 0.12);
        border-radius: 999px;
        display: inline-flex;
        flex-wrap: wrap;
        box-shadow: 0 5px 14px rgba(10, 79, 87, 0.06);
      }

      .table-page .nav-tabs .nav-link {
        border: 0 !important;
        border-radius: 999px !important;
        color: var(--cater-midnight) !important;
        font-weight: 750;
        font-size: 0.84rem;
        padding: 7px 13px;
        background: transparent !important;
      }

      .table-page .nav-tabs .nav-link:hover {
        background: rgba(6, 174, 213, 0.10) !important;
        color: var(--cater-midnight) !important;
      }

      .table-page .nav-tabs .nav-link.active,
      .table-page .nav-tabs .nav-item.show .nav-link {
        background: var(--cater-midnight) !important;
        color: var(--cater-white) !important;
        box-shadow: 0 5px 12px rgba(10, 79, 87, 0.18);
      }

      /* Critical Moves sidebar page */
      .table-page .bslib-sidebar-layout {
        height: 100%;
        min-height: 0;
      }

      .table-page .bslib-sidebar-layout > .main {
        height: 100%;
        min-height: 0;
        overflow: hidden;
      }

      .table-page .bslib-sidebar-layout > .main > .card {
        height: 100%;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      @media (max-width: 1150px) {
        html,
        body {
          overflow: auto;
        }

        .bslib-page-navbar,
        .tab-content,
        .tab-pane {
          height: auto;
          overflow: visible;
        }

        .plot-shell,
        .table-page {
          height: auto;
          overflow: visible;
        }

        .plot-stage {
          height: auto;
          grid-template-columns: 1fr;
        }

        .tool-rail {
          height: auto;
          flex-direction: row;
          justify-content: flex-start;
          overflow-x: auto;
        }

        .settings-pane {
          height: auto;
        }

        .settings-body {
          overflow-y: visible;
        }

        .plot-holder {
          height: 720px;
        }

        .table-page .plot-card,
        .table-page > .card,
        .table-page .card-body,
        .table-page .tabbable,
        .table-page .tab-content,
        .table-page .tab-pane {
          height: auto;
          overflow: visible;
        }
      }
      "))
    )
  ),
  
  nav_panel(
    "Plot",
    
    div(
      class = "plot-shell",
      
      div(
        class = "plot-stage",
        
        div(
          class = "tool-rail",
          
          actionButton("tool_data", label = NULL, icon = icon("file-upload"), class = "tool-button", title = "Data"),
          actionButton("tool_base", label = NULL, icon = icon("sliders-h"), class = "tool-button", title = "Base plot"),
          actionButton("tool_labels", label = NULL, icon = icon("font"), class = "tool-button", title = "Move labels"),
          actionButton("tool_overlays", label = NULL, icon = icon("layer-group"), class = "tool-button", title = "Overlays"),
          actionButton("tool_sawtooth", label = NULL, icon = icon("wave-square"), class = "tool-button", title = "Sawtooth"),
          actionButton("tool_webchunk", label = NULL, icon = icon("sitemap"), class = "tool-button", title = "Web and chunk"),
          actionButton("tool_archio", label = NULL, icon = icon("rainbow"), class = "tool-button", title = "Archiograph"),
          actionButton("tool_trace", label = NULL, icon = icon("route"), class = "tool-button", title = "Trace pathways"),
          actionButton("tool_custom", label = NULL, icon = icon("draw-polygon"), class = "tool-button", title = "Custom patterns"),
          
          div(class = "tool-spacer"),
          
          actionButton("tool_export", label = NULL, icon = icon("file-export"), class = "tool-button", title = "Export")
        ),
        
        div(
          class = "settings-pane",
          
          div(
            class = "settings-body",
            
            tabsetPanel(
              id = "settingsPanel",
              type = "hidden",
              selected = "data",
              
              tabPanel(
                title = "Data",
                value = "data",
                
                div(class = "settings-section-title", "Data"),
                
                fileInput(
                  "file",
                  "Upload Excel file",
                  accept = c(".xlsx"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                ),
                
                div(
                  class = "metric-box",
                  span(class = "metric-label", "Link index"),
                  verbatimTextOutput("linkIndexOutput")
                )
              ),
              
              tabPanel(
                title = "Base plot",
                value = "base",
                
                div(class = "settings-section-title", "Base links"),
                
                colourpicker::colourInput("segmentColor", "Base link colour", value = "black"),
                sliderInput("segmentSize", "Base link line weight", min = 0.5, max = 5, value = 1, step = 0.1),
                
                div(class = "settings-section-title", "Move points"),
                
                colourpicker::colourInput("pointColor", "Move point colour", value = "black"),
                sliderInput("pointSize", "Move point size", min = 1, max = 10, value = 3, step = 0.5),
                
                div(class = "settings-section-title", "Links"),
                
                checkboxInput("showMidpoints", "Show links", value = TRUE),
                
                conditionalPanel(
                  condition = "input.showMidpoints",
                  colourpicker::colourInput("midPointColor", "Link colour", value = "black"),
                  sliderInput("midPointSize", "Link size", min = 1, max = 10, value = 3, step = 0.5)
                ),
                
                div(class = "settings-section-title", "Vertical range"),
                
                sliderInput("yendScale", "Vertical scaling", min = 0, max = 2, value = 0.5, step = 0.01),
                checkboxInput("autoYAxis", "Auto-fit vertical range", value = TRUE),
                
                conditionalPanel(
                  condition = "!input.autoYAxis",
                  sliderInput(
                    "yAxisRange",
                    "Manual y-axis range",
                    min = -100,
                    max = 50,
                    value = c(-30, 5)
                  )
                ),
                
                div(class = "settings-section-title", "Crop linkograph"),
                sliderInput("plotCropRange", "Crop visible moves", min = 1, max = 100, value = c(1, 100), step = 1),
              ),
              
              tabPanel(
                title = "Labels",
                value = "labels",
                
                div(class = "settings-section-title", "Move labels"),
                
                sliderInput("textSize", "Label size", min = 1, max = 10, value = 3, step = 0.5),
                sliderInput("textOffsetX", "Label horizontal position", min = -2, max = 2, value = 0, step = 0.01),
                sliderInput("textOffsetY", "Label vertical position", min = -2, max = 2, value = 0, step = 0.01),
                sliderInput("moveDisplayFrequency", "Display every nth move label", min = 1, max = 10, value = 1, step = 1),
                
                div(class = "settings-section-title", "Critical Move Notation"),
                
                checkboxInput("showCmNotation", "Show CM notation on plot", value = FALSE),
                conditionalPanel(
                  condition = "input.showCmNotation",
                  sliderInput("cmNotationY", "Notation Height", min = 0, max = 3, value = 0.6, step = 0.05),
                  sliderInput("cmLabelXOffset", "Label Left Offset", min = -5, max = 0, value = -0.75, step = 0.25),
                  sliderInput("cmNotationSize", "Notation Font Size", min = 2, max = 10, value = 4.5, step = 0.1)
                ),
              ),
              
              tabPanel(
                title = "Overlays",
                value = "overlays",
                
                div(class = "settings-section-title", "Unidirectional forward"),
                
                checkboxInput("showUniForward", "Show unidirectional forward lines", value = FALSE),
                
                conditionalPanel(
                  condition = "input.showUniForward",
                  colourpicker::colourInput("uniForwardColor", "Forward line colour", value = "deeppink"),
                  sliderInput("uniForwardWeight", "Forward line weight", min = 0.5, max = 3, value = 2, step = 0.1)
                ),
                
                div(class = "settings-section-title", "Unidirectional backward"),
                
                checkboxInput("showUniBackward", "Show unidirectional backward lines", value = FALSE),
                
                conditionalPanel(
                  condition = "input.showUniBackward",
                  colourpicker::colourInput("uniBackwardColor", "Backward line colour", value = "darkgoldenrod2"),
                  sliderInput("uniBackwardWeight", "Backward line weight", min = 0.5, max = 3, value = 2, step = 0.1)
                ),
                
                div(class = "settings-section-title", "Bidirectional"),
                
                checkboxInput("showBidirectional", "Show bidirectional lines", value = FALSE),
                
                conditionalPanel(
                  condition = "input.showBidirectional",
                  colourpicker::colourInput("bidirectionalColor", "Bidirectional line colour", value = "green"),
                  sliderInput("bidirectionalWeight", "Bidirectional line weight", min = 0.5, max = 3, value = 2, step = 0.1)
                )
              ),
              
              # Paste this right after the closing parenthesis of the "webchunk" tabPanel
              
              tabPanel(
                title = "Sawtooth",
                value = "sawtooth",
                
                div(class = "settings-section-title", "Sawtooth analysis"),
                
                checkboxInput("showSawtooth", "Show sawtooth patterns", value = FALSE),
                
                conditionalPanel(
                  condition = "input.showSawtooth",
                  div(class = "settings-section-title", "Master Styling"),
                  checkboxInput("globalSawtoothStyle", "Apply master style to all sawtooths", value = TRUE),
                  colourpicker::colourInput("sawtoothGlobalColor", "Master colour", value = "red"),
                  sliderInput("sawtoothGlobalWeight", "Master line weight", min = 0.5, max = 5, value = 2, step = 0.1),
                  
                  hr(),
                  div(class = "settings-section-title", "Identified Sawtooths"),
                  uiOutput("sawtoothResultsUI")
                )
              ),
              
              tabPanel(
                title = "Web and chunk",
                value = "webchunk",
                
                div(class = "settings-section-title", "Web and chunk analysis"),
                
                checkboxInput("showWebChunk", "Show web and chunk patterns", value = FALSE),
                numericInput("minMoves", "Minimum moves", value = 3, min = 2),
                numericInput("maxMoves", "Maximum moves", value = 8, min = 3),
                numericInput("minConnectionPercentage", "Minimum connection %", value = 50, min = 0, max = 100),
                numericInput("maxConnectionPercentage", "Maximum connection %", value = 100, min = 0, max = 100),
                
                actionButton("runWebChunk", "Run analysis", class = "btn-primary", style = "width: 100%; margin-bottom: 15px;"),
                
                conditionalPanel(
                  condition = "input.showWebChunk && input.runWebChunk > 0",
                  
                  div(class = "settings-section-title", "Master Styling"),
                  checkboxInput("globalChunkStyle", "Apply master style to all chunks", value = TRUE),
                  colourpicker::colourInput("webChunkGlobalColor", "Master colour", value = "green"),
                  sliderInput("webChunkGlobalWeight", "Master line weight", min = 0.5, max = 5, value = 2, step = 0.1),
                  
                  hr(),
                  div(class = "settings-section-title", "Identified Chunks"),
                  uiOutput("webChunkResultsUI")
                )
              ),
              
              tabPanel(
                title = "Archiograph",
                value = "archio",
                
                div(class = "settings-section-title", "Archiograph"),
                
                checkboxInput("showArchiograph", "Show archiograph", value = FALSE),
                
                conditionalPanel(
                  condition = "input.showArchiograph",
                  selectInput("archiographColumn", "Categorical column", choices = NULL),
                  verbatimTextOutput("numFactorsOutput"),
                  uiOutput("colorPickersUI"),
                  sliderInput("archiographWeight", "Archiograph line weight", min = 0.5, max = 5, value = 1, step = 0.1),
                  sliderInput("yarcScale", "Archiograph vertical scaling", min = 0, max = 3, value = 1, step = 0.01)
                )
              ),
              
              tabPanel(
                title = "Trace",
                value = "trace",
                
                # ---> NEW: Quick Import Action Button <---
                div(class = "settings-section-title", "Quick Action"),
                actionButton(
                  "importCmToTrace", 
                  "Import Active CM Moves", 
                  class = "btn-primary", 
                  style = "width: 100%; margin-bottom: 15px; font-size: 0.82rem;"
                ),
                
                div(class = "settings-section-title", "Trace Forward"),
                checkboxInput("showTraceForward", "Trace forward links", value = FALSE),
                conditionalPanel(
                  condition = "input.showTraceForward",
                  selectizeInput("traceForwardMoves", "Select origin moves", choices = NULL, multiple = TRUE),
                  colourpicker::colourInput("traceForwardColor", "Trace colour", value = "darkorange"),
                  sliderInput("traceForwardWeight", "Line weight", min = 0.5, max = 5, value = 2, step = 0.1)
                ),
                
                div(class = "settings-section-title", "Trace Backward"),
                checkboxInput("showTraceBackward", "Trace backward links", value = FALSE),
                conditionalPanel(
                  condition = "input.showTraceBackward",
                  selectizeInput("traceBackwardMoves", "Select origin moves", choices = NULL, multiple = TRUE),
                  colourpicker::colourInput("traceBackwardColor", "Trace colour", value = "purple"),
                  sliderInput("traceBackwardWeight", "Line weight", min = 0.5, max = 5, value = 2, step = 0.1)
                )
              ),
              
              tabPanel(
                title = "Custom patterns",
                value = "custom",
                
                div(class = "settings-section-title", "Custom patterns"),
                
                actionButton("addPattern", "Add new pattern", class = "btn-primary"),
                br(),
                br(),
                uiOutput("customPatternsUI")
              ),
              
              tabPanel(
                title = "Export",
                value = "export",
                
                div(class = "settings-section-title", "Export"),
                
                div(
                  class = "control-pair",
                  numericInput("pdfWidth", "Image width (mm)", value = 297, min = 1),
                  numericInput("pdfHeight", "Image height (mm)", value = 210, min = 1)
                ),
                
                # ---> NEW: Export Scale adjustment <---
                sliderInput("pdfScale", "Element scaling (increase to make lines thinner in pdf)", min = 0.5, max = 4, value = 1.5, step = 0.1),
                
                numericInput("imageDpi", "Raster resolution (DPI)", value = 300, min = 72, max = 1200, step = 10),
                
                div(
                  style = "display: flex; gap: 10px; margin-top: 15px;",
                  downloadButton(
                    "downloadPlot", 
                    "Download PDF", 
                    class = "btn-primary", 
                    style = "flex: 1; text-align: center; padding: 6px 12px; font-size: 0.8rem; display: flex; align-items: center; justify-content: center; gap: 8px; height: 38px;"
                  ),
                  downloadButton(
                    "downloadPlotJPG", 
                    "Download JPG", 
                    class = "btn-primary", 
                    style = "flex: 1; text-align: center; padding: 6px 12px; font-size: 0.8rem; display: flex; align-items: center; justify-content: center; gap: 8px; height: 38px;"
                  )
                )
              )
            )
          )
        ),
        
        div(
          class = "plot-holder",
          
          div(
            class = "plot-holder-header",
            div(class = "plot-title", "Linkograph")
          ),
          
          div(
            class = "plot-holder-body",
            style = "position: relative;", # <--- NEW: Tells the button to float relative to this container
            
            plotOutput(
              "interactivePlot", 
              height = "100%",
              dblclick = "plot_dblclick",
              brush = brushOpts(
                id = "plot_brush",
                resetOnNew = TRUE
              )
            ),
            
            # ---> NEW: Floating Fit to Viewer Button <---
            actionButton(
              "resetView", 
              label = NULL, # Or "Fit to Viewer" if you prefer text over just the icon
              icon = icon("expand"),
              title = "Fit to Viewer",
              style = "position: absolute; bottom: 15px; right: 15px; opacity: 0.8; background-color: white; box-shadow: 0px 2px 5px rgba(0,0,0,0.2); border: none; z-index: 10;"
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Link Span",
    div(
      class = "table-page",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          card(
            class = "control-card",
            card_header("Link span settings"),
            card_body(
              
              # ---> NEW: Modern segmented toggle / slider <---
              radioGroupButtons(
                inputId = "spanThresholdMode",
                label = "Input Mode:",
                choices = c("Percentile" = "percentile", "Absolute" = "absolute"),
                selected = "percentile",
                justified = TRUE,
                status = "primary",
                size = "sm"
              ),
              
              div(class = "settings-section-title", "Percentile Thresholds"),
              numericInput("spanCutoff1_perc", "Short to Medium (%)", value = 25, min = 1, max = 99),
              numericInput("spanCutoff2_perc", "Medium to Long (%)", value = 75, min = 1, max = 99),
              
              div(class = "settings-section-title", "Absolute Thresholds (Moves)"),
              numericInput("spanCutoff1_abs", "Short to Medium Cutoff", value = 1, min = 1),
              numericInput("spanCutoff2_abs", "Medium to Long Cutoff", value = 2, min = 1),
              
              downloadButton(
                "downloadSpanOutputs",
                "Download all outputs as Excel",
                class = "btn-primary"
              )
            )
          )
        ),
        card(
          class = "plot-card",
          card_header("Link span outputs"),
          card_body(
            tabsetPanel(
              tabPanel("Overall Statistics", reactableOutput("spanOverallStatsTable")),
              tabPanel("Span Categories", reactableOutput("spanCategoriesTable")),
              tabPanel("Move-Level Summary", reactableOutput("spanMoveLevelTable")),
              tabPanel("Distance Matrix", reactableOutput("linkSpanTable"))
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Move Classification",
    div(
      class = "table-page",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          card(
            class = "control-card",
            card_header("Classification settings"),
            card_body(
              # ---> NEW: Multi-select dropdown for variables <---
              pickerInput(
                inputId = "quantGroupVars",
                label = "Group data by (select one or more):",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,          # Adds 'Select All' / 'Deselect All' buttons
                  `selected-text-format` = "count > 2", # Cleans up the display if they select lots of variables
                  `live-search` = TRUE,           # Keeps the search functionality!
                  container = "body"
                )
              ),
              # ---> NEW: Dedicated download button <---
              downloadButton(
                "downloadClassOutputs",
                "Download all outputs as Excel",
                class = "btn-primary"
              )
            )
          )
        ),
        card(
          class = "plot-card",
          card_header("Move classification outputs"),
          card_body(
            tabsetPanel(
              tabPanel("Descriptive Statistics", reactableOutput("descriptiveStatisticsTable")),
              tabPanel("Inter/Intra Links", reactableOutput("interIntraLinksTable")),
              tabPanel("Direction Counts", reactableOutput("moveDirectionCountsTable")),
              tabPanel("Move Classification", reactableOutput("moveDirectionClassificationTable")),
              tabPanel("Direction by Variable", reactableOutput("moveDirectionByVariableTable")),
              tabPanel("Directed Matrices", reactableOutput("directedLinkCountsTable")),
              tabPanel("Undirected Matrices", reactableOutput("undirectedLinkCountsTable"))
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Critical Moves",
    div(
      class = "table-page",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          card(
            class = "control-card",
            card_header("Critical move settings"),
            card_body(
              pickerInput(
                inputId = "cmGroupVars",
                label = "Group data by (select one or more):",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = "count > 2",
                  `live-search` = TRUE,
                  container = "body"
                )
              ),
              numericInput(
                "selectedCmThreshold",
                "Selected directional CM threshold",
                value = 2,
                min = 1,
                step = 1
              ),
              downloadButton(
                "downloadQuantOutputs",
                "Download all outputs as Excel",
                class = "btn-primary"
              )
            )
          )
        ),
        card(
          class = "plot-card",
          card_header("Directional critical move outputs"),
          card_body(
            tabsetPanel(
              tabPanel("Threshold List", reactableOutput("cmThresholdsTable")),
              tabPanel("Ranking List", reactableOutput("cmRankingTable")),
              tabPanel("CM Move Counts", reactableOutput("cmMoveCountsTable")),
              tabPanel("CM Counts by Variable", reactableOutput("cmCountsByVariableTable"))
            )
          )
        )
      )
    )
  ),
  
  nav_spacer(),
  
  nav_item(
    tags$a(
      href = "https://www.cater-network.com", 
      target = "_blank", 
      # We add 'display: block;' and 'text-decoration: none;' so it behaves like the old div
      style = "display: block; font-size: 0.75rem; color: #5B6770; line-height: 1.3; text-align: right; margin-top: 4px; text-decoration: none;",
      
      tags$strong("CATER: Centre for the Advancement of Technology Education Research"), tags$br(),
      "Developed by ",
      
      # The names are now just a styled span since the whole block is already a link
      tags$span(
        style = "color: var(--cater-midnight); font-weight: 700;",
        "Nicolaas Blom & Jeffrey Buckley"
      ), 
      tags$br(),
      tags$em("Based on the work of Gabriella Goldschmidt (2014)")
    )
  )
)


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

empty_table <- function(message = "No eligible variables were found for this output.") {
  tibble::tibble(Note = message)
}

##### Function: import and structure linkography data #####

# Helper function to beautifully format column names for UI and Excel
clean_df_names <- function(df) {
  if(is.data.frame(df)) {
    new_names <- names(df)
    # Expand shorthand prefixes
    new_names <- stringr::str_replace_all(new_names, "^n_", "Number of ")
    new_names <- stringr::str_replace_all(new_names, "^percent_", "Percentage of ")
    new_names <- stringr::str_replace_all(new_names, "^cm_", "CM ")
    # Replace underscores with spaces and Title Case the words
    new_names <- stringr::str_replace_all(new_names, "_", " ")
    new_names <- stringr::str_to_title(new_names)
    # Fix specific acronyms/grammar altered by Title Case
    new_names <- stringr::str_replace_all(new_names, "\\bCm\\b", "CM")
    new_names <- stringr::str_replace_all(new_names, "\\bOf\\b", "of")
    names(df) <- new_names
  }
  return(df)
}

# Helper function to stack multiple tables into a single data frame for writexl
stack_tables_for_writexl <- function(table_list) {
  if (length(table_list) == 0) return(data.frame(Note = "No data available"))
  
  # 1. Find the maximum number of columns needed across all tables
  max_cols <- max(sapply(table_list, ncol))
  generic_col_names <- paste0("Col_", 1:max_cols)
  
  stacked_list <- list()
  
  for (table_name in names(table_list)) {
    current_table <- table_list[[table_name]]
    
    # Convert everything to character to avoid class conflicts (e.g., numeric vs character)
    current_table[] <- lapply(current_table, as.character)
    original_headers <- names(current_table)
    
    # Pad with empty columns if this table is narrower than max_cols
    if (ncol(current_table) < max_cols) {
      padding <- as.data.frame(matrix(NA_character_, nrow = nrow(current_table), ncol = max_cols - ncol(current_table)))
      current_table <- cbind(current_table, padding)
    }
    names(current_table) <- generic_col_names
    
    # Create Title Row (Table name in first column)
    title_row <- as.data.frame(matrix(NA_character_, nrow = 1, ncol = max_cols))
    names(title_row) <- generic_col_names
    title_row[1, 1] <- paste("---", toupper(table_name), "---") 
    
    # Create Header Row (Original column names)
    header_row <- as.data.frame(matrix(NA_character_, nrow = 1, ncol = max_cols))
    names(header_row) <- generic_col_names
    header_row[1, 1:length(original_headers)] <- original_headers
    
    # Create Spacer Row
    spacer_row <- as.data.frame(matrix(NA_character_, nrow = 1, ncol = max_cols))
    names(spacer_row) <- generic_col_names
    
    # Bind them together
    stacked_list[[length(stacked_list) + 1]] <- dplyr::bind_rows(title_row, header_row, current_table, spacer_row)
  }
  
  # Combine the full list into one massive data frame
  final_df <- dplyr::bind_rows(stacked_list)
  
  # Give the final data frame "invisible" unique column names (writexl requires unique names)
  # Using different amounts of blank spaces to keep them unique but invisible in Excel
  names(final_df) <- strrep(" ", 1:max_cols)
  
  return(final_df)
}

read_linkography <- function(file, sheet = 1) {
  
  # Import raw Excel file
  raw_data <- suppressMessages(readxl::read_excel(file, sheet = sheet))
  
  # ---> NEW: Robust Move Column Finder <---
  # Uses regex to find "move", "Move", "moves", or "Moves" (case-insensitive)
  move_col_idx <- grep("(?i)^moves?$", names(raw_data))
  
  if (length(move_col_idx) > 0) {
    names(raw_data)[move_col_idx[1]] <- "move" # Standardize the matched column
  } else {
    names(raw_data)[1] <- "move" # Fallback: assume the first column is the move column
  }
  
  # Identify unnamed columns created by readxl, e.g., ...2, ...3, etc.
  link_cols <- names(raw_data)[stringr::str_detect(names(raw_data), "^\\.\\.\\.[0-9]+$")]
  
  if (length(link_cols) == 0) {
    stop("No unnamed link columns were detected. Check that the link columns in Excel have no column names.")
  }
  
  # One row per move, keeping ALL other named categorical variables automatically
  moves <- raw_data %>%
    dplyr::select(-dplyr::all_of(link_cols)) %>%
    dplyr::mutate(
      move = as.integer(move)
      # ---> REMOVED: Hardcoded participant mutations are completely gone
    )
  
  # One row per link
  links <- raw_data %>%
    dplyr::select(move, dplyr::all_of(link_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(link_cols),
      names_to = "link_column",
      values_to = "target_move",
      values_drop_na = TRUE
    ) %>%
    dplyr::transmute(
      source_move = as.integer(move),
      target_move = as.integer(target_move)
      # ---> REMOVED: Hardcoded source_participant is completely gone
    )
  
  # Basic checks
  missing_targets <- setdiff(unique(links$target_move), moves$move)
  
  if (length(missing_targets) > 0) {
    warning(
      "Some target moves in the link columns are not present in the move column: ",
      paste(missing_targets, collapse = ", ")
    )
  }
  
  forward_or_self_links <- links %>%
    dplyr::filter(target_move >= source_move)
  
  if (nrow(forward_or_self_links) > 0) {
    warning("Some links point to the same move or to a later move. Check `forward_or_self_links`.")
  }
  
  duplicate_links <- links %>%
    dplyr::count(source_move, target_move) %>%
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_links) > 0) {
    warning("Some source-target links appear more than once. Check `duplicate_links`.")
  }
  
  return(
    list(
      raw_data = raw_data,
      moves = moves,
      links = links,
      link_cols = link_cols,
      missing_targets = missing_targets,
      forward_or_self_links = forward_or_self_links,
      duplicate_links = duplicate_links
    )
  )
}

##### Function: inter/intra links for selected move-level variables #####

summarise_inter_intra_by_variables <- function(moves, links, variables) {
  
  # Check that all requested variables exist in moves
  missing_variables <- setdiff(variables, names(moves))
  
  if (length(missing_variables) > 0) {
    stop(
      "The following variables are not in the moves dataset: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  # Function to process one variable
  process_variable <- function(variable_name) {
    
    # Count number of moves in each category of the selected variable
    category_move_counts <- moves %>%
      dplyr::transmute(
        source_category = as.character(.data[[variable_name]])
      ) %>%
      dplyr::filter(!is.na(source_category)) %>%
      dplyr::count(
        source_category,
        name = "n_moves"
      )
    
    source_values <- moves %>%
      dplyr::select(
        source_move = move,
        source_category = dplyr::all_of(variable_name)
      ) %>%
      dplyr::mutate(
        source_category = as.character(source_category)
      )
    
    target_values <- moves %>%
      dplyr::select(
        target_move = move,
        target_category = dplyr::all_of(variable_name)
      ) %>%
      dplyr::mutate(
        target_category = as.character(target_category)
      )
    
    links %>%
      dplyr::select(source_move, target_move) %>%
      dplyr::left_join(source_values, by = "source_move") %>%
      dplyr::left_join(target_values, by = "target_move") %>%
      dplyr::mutate(
        variable = variable_name,
        link_type = dplyr::case_when(
          is.na(source_category) | is.na(target_category) ~ NA_character_,
          source_category == target_category ~ "Intra",
          source_category != target_category ~ "Inter"
        )
      ) %>%
      dplyr::filter(!is.na(link_type)) %>%
      dplyr::count(
        variable,
        source_category,
        link_type,
        name = "n_links"
      ) %>%
      tidyr::complete(
        variable,
        source_category = category_move_counts$source_category,
        link_type = c("Intra", "Inter"),
        fill = list(n_links = 0)
      ) %>%
      tidyr::pivot_wider(
        names_from = link_type,
        values_from = n_links,
        values_fill = 0
      ) %>%
      dplyr::left_join(
        category_move_counts,
        by = "source_category"
      )
  }
  
  # Run over all selected variables and combine
  output <- lapply(variables, process_variable) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      total_links = Intra + Inter,
      
      # Link index for each source category
      link_index = dplyr::case_when(
        n_moves > 0 ~ total_links / n_moves,
        TRUE ~ NA_real_
      ),
      
      # Within-category percentages
      intra_percentage = dplyr::case_when(
        total_links > 0 ~ Intra / total_links * 100,
        TRUE ~ NA_real_
      ),
      inter_percentage = dplyr::case_when(
        total_links > 0 ~ Inter / total_links * 100,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(
      total_intra_links_for_variable = sum(Intra, na.rm = TRUE),
      total_inter_links_for_variable = sum(Inter, na.rm = TRUE),
      
      # Contribution to all intra/inter links within each variable
      percentage_of_all_intra_links = dplyr::case_when(
        total_intra_links_for_variable > 0 ~ Intra / total_intra_links_for_variable * 100,
        TRUE ~ NA_real_
      ),
      percentage_of_all_inter_links = dplyr::case_when(
        total_inter_links_for_variable > 0 ~ Inter / total_inter_links_for_variable * 100,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      category = source_category,
      intra_links = Intra,
      inter_links = Inter
    ) %>%
    dplyr::select(
      variable,
      category,
      n_moves,
      link_index,
      intra_links,
      inter_links,
      total_links,
      intra_percentage,
      inter_percentage,
      percentage_of_all_intra_links,
      percentage_of_all_inter_links
    )
  
  return(output)
}

##### Function: classify moves by link direction #####

classify_move_direction <- function(moves, links, raw_data = NULL) {
  
  move_type_levels <- c(
    "Unidirectional forward move",
    "Unidirectional backward move",
    "Bidirectional move",
    "Orphan move"
  )
  
  move_direction_classification <- moves %>%
    dplyr::select(move) %>%
    
    # Links made by this move to previous/lower-numbered moves
    dplyr::left_join(
      links %>%
        dplyr::filter(target_move < source_move) %>%
        dplyr::count(source_move, name = "backward_links") %>%
        dplyr::rename(move = source_move),
      by = "move"
    ) %>%
    
    # Links received from later/higher-numbered moves
    dplyr::left_join(
      links %>%
        dplyr::filter(source_move > target_move) %>%
        dplyr::count(target_move, name = "forward_links") %>%
        dplyr::rename(move = target_move),
      by = "move"
    ) %>%
    
    dplyr::mutate(
      backward_links = tidyr::replace_na(backward_links, 0L),
      forward_links = tidyr::replace_na(forward_links, 0L),
      
      move_type = dplyr::case_when(
        forward_links > 0 & backward_links == 0 ~ "Unidirectional forward move",
        backward_links > 0 & forward_links == 0 ~ "Unidirectional backward move",
        forward_links > 0 & backward_links > 0 ~ "Bidirectional move",
        forward_links == 0 & backward_links == 0 ~ "Orphan move"
      ),
      
      move_type = factor(
        move_type,
        levels = move_type_levels
      )
    )
  
  move_direction_counts <- move_direction_classification %>%
    dplyr::count(
      move_type,
      name = "n_moves",
      .drop = FALSE
    ) %>%
    dplyr::mutate(
      percentage = n_moves / sum(n_moves) * 100
    )
  
  moves_with_direction_classification <- moves %>%
    dplyr::left_join(
      move_direction_classification %>%
        dplyr::select(
          move,
          backward_links,
          forward_links,
          move_type
        ),
      by = "move"
    )
  
  if (!is.null(raw_data)) {
    raw_data_with_direction_classification <- raw_data %>%
      dplyr::left_join(
        move_direction_classification %>%
          dplyr::select(
            move,
            backward_links,
            forward_links,
            move_type
          ),
        by = "move"
      )
  } else {
    raw_data_with_direction_classification <- NULL
  }
  
  return(
    list(
      move_direction_classification = move_direction_classification,
      move_direction_counts = move_direction_counts,
      moves_with_direction_classification = moves_with_direction_classification,
      raw_data_with_direction_classification = raw_data_with_direction_classification
    )
  )
}

##### Function: move direction counts for selected move-level variables #####

summarise_move_direction_by_variables <- function(moves_with_direction_classification,
                                                  variables,
                                                  missing_label = "Missing") {
  
  # Check that all requested variables exist
  missing_variables <- setdiff(
    variables,
    names(moves_with_direction_classification)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      "The following variables are not in the moves dataset: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  move_type_levels <- c(
    "Unidirectional forward move",
    "Unidirectional backward move",
    "Bidirectional move",
    "Orphan move"
  )
  
  # Function to process one variable
  process_variable <- function(variable_name) {
    
    moves_with_direction_classification %>%
      dplyr::select(
        move,
        category = dplyr::all_of(variable_name),
        move_type
      ) %>%
      dplyr::mutate(
        variable = variable_name,
        category = as.character(category),
        category = tidyr::replace_na(category, missing_label),
        move_type = as.character(move_type),
        move_type = factor(
          move_type,
          levels = move_type_levels
        )
      ) %>%
      dplyr::count(
        variable,
        category,
        move_type,
        name = "n_moves",
        .drop = FALSE
      ) %>%
      tidyr::complete(
        variable,
        category,
        move_type = factor(move_type_levels, levels = move_type_levels),
        fill = list(n_moves = 0)
      )
  }
  
  output_long <- lapply(variables, process_variable) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(variable, category) %>%
    dplyr::mutate(
      total_moves = sum(n_moves),
      percentage = dplyr::case_when(
        total_moves > 0 ~ n_moves / total_moves * 100,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  output_wide <- output_long %>%
    dplyr::mutate(
      move_type_clean = dplyr::case_when(
        move_type == "Unidirectional forward move" ~ "unidirectional_forward",
        move_type == "Unidirectional backward move" ~ "unidirectional_backward",
        move_type == "Bidirectional move" ~ "bidirectional",
        move_type == "Orphan move" ~ "orphan"
      )
    ) %>%
    dplyr::select(
      variable,
      category,
      move_type_clean,
      n_moves,
      percentage
    ) %>%
    tidyr::pivot_wider(
      names_from = move_type_clean,
      values_from = c(n_moves, percentage),
      values_fill = list(
        n_moves = 0,
        percentage = 0
      )
    ) %>%
    dplyr::mutate(
      total_moves =
        n_moves_unidirectional_forward +
        n_moves_unidirectional_backward +
        n_moves_bidirectional +
        n_moves_orphan
    ) %>%
    dplyr::rename(
      percent_moves_unidirectional_forward = percentage_unidirectional_forward,
      percent_moves_unidirectional_backward = percentage_unidirectional_backward,
      percent_moves_bidirectional = percentage_bidirectional,
      percent_moves_orphan = percentage_orphan
    ) %>%
    dplyr::select(
      variable,
      category,
      n_moves_unidirectional_forward,
      percent_moves_unidirectional_forward,
      n_moves_unidirectional_backward,
      percent_moves_unidirectional_backward,
      n_moves_bidirectional,
      percent_moves_bidirectional,
      n_moves_orphan,
      percent_moves_orphan,
      total_moves
    )
  
  return(
    list(
      move_direction_by_variable_long = output_long,
      move_direction_by_variable_wide = output_wide
    )
  )
}

##### Function: explore directional critical move thresholds at original move level #####

explore_directional_cm_thresholds <- function(moves_with_direction_classification,
                                              percentages = 10:15,
                                              round_method = "round_half_up") {
  
  required_columns <- c("move", "forward_links", "backward_links")
  
  missing_required_columns <- setdiff(
    required_columns,
    names(moves_with_direction_classification)
  )
  
  if (length(missing_required_columns) > 0) {
    stop(
      "moves_with_direction_classification is missing these required columns: ",
      paste(missing_required_columns, collapse = ", ")
    )
  }
  
  # One row per original move
  move_level_data <- moves_with_direction_classification %>%
    dplyr::select(
      move,
      forward_links,
      backward_links
    )
  
  n_original_moves <- nrow(move_level_data)
  
  # Two directional scores per original move:
  # one forward score and one backward score
  directional_scores <- move_level_data %>%
    tidyr::pivot_longer(
      cols = c(forward_links, backward_links),
      names_to = "direction",
      values_to = "directional_links"
    ) %>%
    dplyr::mutate(
      direction = dplyr::case_when(
        direction == "forward_links" ~ "Forward",
        direction == "backward_links" ~ "Backward"
      )
    ) %>%
    dplyr::arrange(
      dplyr::desc(directional_links),
      move,
      direction
    ) %>%
    dplyr::mutate(
      directional_rank = dplyr::row_number()
    )
  
  get_n_selected <- function(percentage) {
    
    # Important: percentage is based on the number of original moves,
    # not the number of directional scores.
    raw_n <- n_original_moves * percentage / 100
    
    n_selected <- dplyr::case_when(
      round_method == "ceiling" ~ ceiling(raw_n),
      round_method == "floor" ~ floor(raw_n),
      round_method == "round_half_up" ~ floor(raw_n + 0.5),
      TRUE ~ floor(raw_n + 0.5)
    )
    
    n_selected <- max(n_selected, 1)
    n_selected <- min(n_selected, nrow(directional_scores))
    
    return(n_selected)
  }
  
  process_percentage <- function(percentage) {
    
    n_selected <- get_n_selected(percentage)
    
    threshold <- directional_scores$directional_links[n_selected]
    
    move_classification <- move_level_data %>%
      dplyr::mutate(
        is_cm_forward = forward_links >= threshold,
        is_cm_backward = backward_links >= threshold,
        is_cm_bidirectional = is_cm_forward & is_cm_backward,
        is_cm_any_direction = is_cm_forward | is_cm_backward
      )
    
    tibble::tibble(
      total_number_of_moves = n_original_moves,
      target_percentage = percentage,
      target_number_of_critical_moves = n_selected,
      critical_move_threshold = threshold,
      
      actual_qualifying_scores = 
        sum(directional_scores$directional_links >= threshold),
      
      n_cm_forward_moves =
        sum(move_classification$is_cm_forward),
      
      n_cm_backward_moves =
        sum(move_classification$is_cm_backward),
      
      n_cm_bidirectional_moves =
        sum(move_classification$is_cm_bidirectional),
      
      n_unique_cm_moves =
        sum(move_classification$is_cm_any_direction),
      
      percent_unique_cm_moves =
        n_unique_cm_moves / n_original_moves * 100
    )
  }
  
  directional_cm_thresholds <- lapply(
    percentages,
    process_percentage
  ) %>%
    dplyr::bind_rows()
  
  return(
    list(
      directional_scores = directional_scores,
      directional_cm_thresholds = directional_cm_thresholds
    )
  )
}

##### Function: summarise directional critical moves overall #####

summarise_directional_critical_moves_overall <- function(moves_with_direction_classification,
                                                         critical_move_thresholds,
                                                         empty_move_label = "") {
  
  required_columns <- c("move", "forward_links", "backward_links")
  
  missing_required_columns <- setdiff(
    required_columns,
    names(moves_with_direction_classification)
  )
  
  if (length(missing_required_columns) > 0) {
    stop(
      "moves_with_direction_classification is missing these required columns: ",
      paste(missing_required_columns, collapse = ", ")
    )
  }
  
  cm_type_levels <- c(
    "Exclusive: CM Forward Only",
    "Exclusive: CM Backward Only",
    "Exclusive: CM Bidirectional",
    "Inclusive: CM Forward Total",
    "Inclusive: CM Backward Total"
  )
  
  format_move_numbers <- function(x) {
    x <- sort(unique(x))
    
    if (length(x) == 0) {
      return(empty_move_label)
    } else {
      return(paste(x, collapse = ", "))
    }
  }
  
  directional_critical_moves <- moves_with_direction_classification %>%
    tidyr::crossing(
      critical_move_threshold = critical_move_thresholds
    ) %>%
    dplyr::mutate(
      is_cm_forward = forward_links >= critical_move_threshold,
      is_cm_backward = backward_links >= critical_move_threshold,
      is_cm_bidirectional = is_cm_forward & is_cm_backward,
      is_cm_any_direction = is_cm_forward | is_cm_backward,
      
      cm_type = dplyr::case_when(
        is_cm_forward & is_cm_backward ~ "CM bidirectional",
        is_cm_forward & !is_cm_backward ~ "CM forward only",
        !is_cm_forward & is_cm_backward ~ "CM backward only",
        TRUE ~ "Not CM"
      )
    )
  
  directional_critical_move_counts <- directional_critical_moves %>%
    dplyr::group_by(critical_move_threshold) %>%
    dplyr::summarise(
      n_total_moves = dplyr::n(),
      total_critical_moves = sum(is_cm_any_direction, na.rm = TRUE),
      
      n_cm_forward_only = sum(is_cm_forward & !is_cm_backward, na.rm = TRUE),
      n_cm_backward_only = sum(!is_cm_forward & is_cm_backward, na.rm = TRUE),
      n_cm_bidirectional = sum(is_cm_bidirectional, na.rm = TRUE),
      n_cm_forward_total = sum(is_cm_forward, na.rm = TRUE),
      n_cm_backward_total = sum(is_cm_backward, na.rm = TRUE),
      
      moves_cm_forward_only = format_move_numbers(
        move[is_cm_forward & !is_cm_backward]
      ),
      moves_cm_backward_only = format_move_numbers(
        move[!is_cm_forward & is_cm_backward]
      ),
      moves_cm_bidirectional = format_move_numbers(
        move[is_cm_bidirectional]
      ),
      moves_cm_forward_total = format_move_numbers(
        move[is_cm_forward]
      ),
      moves_cm_backward_total = format_move_numbers(
        move[is_cm_backward]
      ),
      
      .groups = "drop"
    )
  
  counts_long <- directional_critical_move_counts %>%
    dplyr::select(
      critical_move_threshold,
      n_total_moves,
      total_critical_moves,
      n_cm_forward_only,
      n_cm_backward_only,
      n_cm_bidirectional,
      n_cm_forward_total,
      n_cm_backward_total
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("n_cm_"),
      names_to = "cm_type_clean",
      values_to = "n_critical_moves"
    )
  
  moves_long <- directional_critical_move_counts %>%
    dplyr::select(
      critical_move_threshold,
      moves_cm_forward_only,
      moves_cm_backward_only,
      moves_cm_bidirectional,
      moves_cm_forward_total,
      moves_cm_backward_total
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("moves_cm_"),
      names_to = "cm_type_clean",
      values_to = "move_numbers"
    ) %>%
    dplyr::mutate(
      cm_type_clean = stringr::str_replace(
        cm_type_clean,
        "^moves_",
        "n_"
      )
    )
  
  directional_critical_move_counts <- counts_long %>%
    dplyr::left_join(
      moves_long,
      by = c("critical_move_threshold", "cm_type_clean")
    ) %>%
    dplyr::mutate(
      cm_type = dplyr::case_when(
        cm_type_clean == "n_cm_forward_only" ~ "Exclusive: CM Forward Only",
        cm_type_clean == "n_cm_backward_only" ~ "Exclusive: CM Backward Only",
        cm_type_clean == "n_cm_bidirectional" ~ "Exclusive: CM Bidirectional",
        cm_type_clean == "n_cm_forward_total" ~ "Inclusive: CM Forward Total",
        cm_type_clean == "n_cm_backward_total" ~ "Inclusive: CM Backward Total"
      ),
      cm_type = factor(cm_type, levels = cm_type_levels),
      percent_critical_moves = dplyr::case_when(
        total_critical_moves > 0 ~ n_critical_moves / total_critical_moves * 100,
        TRUE ~ 0
      ),
      percent_total_moves = dplyr::case_when( # <--- NEW: Calculate new column
        n_total_moves > 0 ~ n_critical_moves / n_total_moves * 100,
        TRUE ~ 0
      )
    ) %>%
    dplyr::arrange(
      critical_move_threshold,
      n_total_moves,
      cm_type
    ) %>%
    dplyr::select(
      critical_move_threshold,
      total_critical_moves,
      cm_type,
      n_critical_moves,
      percent_critical_moves,
      percent_total_moves,
      move_numbers
    )
  
  return(
    list(
      directional_critical_moves = directional_critical_moves,
      directional_critical_move_counts = directional_critical_move_counts
    )
  )
}
##### Function: summarise directional critical moves by selected variables #####

summarise_directional_critical_moves_by_variables <- function(moves_with_direction_classification,
                                                              variables,
                                                              critical_move_thresholds,
                                                              missing_label = "Missing",
                                                              empty_move_label = "") {
  
  required_columns <- c("move", "forward_links", "backward_links")
  
  missing_required_columns <- setdiff(
    required_columns,
    names(moves_with_direction_classification)
  )
  
  if (length(missing_required_columns) > 0) {
    stop(
      "moves_with_direction_classification is missing these required columns: ",
      paste(missing_required_columns, collapse = ", ")
    )
  }
  
  missing_variables <- setdiff(
    variables,
    names(moves_with_direction_classification)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      "The following variables are not in the moves dataset: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  cm_type_levels <- c(
    "Exclusive: CM Forward Only",
    "Exclusive: CM Backward Only",
    "Exclusive: CM Bidirectional",
    "Inclusive: CM Forward Total",
    "Inclusive: CM Backward Total"
  )
  
  format_move_numbers <- function(x) {
    x <- sort(unique(x))
    
    if (length(x) == 0) {
      return(empty_move_label)
    } else {
      return(paste(x, collapse = ", "))
    }
  }
  
  process_variable <- function(variable_name) {
    
    classified_moves <- moves_with_direction_classification %>%
      dplyr::select(
        move,
        category = dplyr::all_of(variable_name),
        forward_links,
        backward_links
      ) %>%
      dplyr::mutate(
        variable = variable_name,
        category = as.character(category),
        category = tidyr::replace_na(category, missing_label)
      ) %>%
      tidyr::crossing(
        critical_move_threshold = critical_move_thresholds
      ) %>%
      dplyr::mutate(
        is_cm_forward = forward_links >= critical_move_threshold,
        is_cm_backward = backward_links >= critical_move_threshold,
        is_cm_bidirectional = is_cm_forward & is_cm_backward,
        is_cm_any_direction = is_cm_forward | is_cm_backward,
        
        cm_type = dplyr::case_when(
          is_cm_forward & is_cm_backward ~ "CM bidirectional",
          is_cm_forward & !is_cm_backward ~ "CM forward only",
          !is_cm_forward & is_cm_backward ~ "CM backward only",
          TRUE ~ "Not CM"
        )
      )
    
    summary_wide <- classified_moves %>%
      dplyr::group_by(
        critical_move_threshold,
        variable,
        category
      ) %>%
      dplyr::summarise(
        n_total_moves_in_category = dplyr::n(),
        total_critical_moves = sum(is_cm_any_direction, na.rm = TRUE),
        
        n_cm_forward_only = sum(is_cm_forward & !is_cm_backward, na.rm = TRUE),
        n_cm_backward_only = sum(!is_cm_forward & is_cm_backward, na.rm = TRUE),
        n_cm_bidirectional = sum(is_cm_bidirectional, na.rm = TRUE),
        n_cm_forward_total = sum(is_cm_forward, na.rm = TRUE),
        n_cm_backward_total = sum(is_cm_backward, na.rm = TRUE),
        
        moves_cm_forward_only = format_move_numbers(
          move[is_cm_forward & !is_cm_backward]
        ),
        moves_cm_backward_only = format_move_numbers(
          move[!is_cm_forward & is_cm_backward]
        ),
        moves_cm_bidirectional = format_move_numbers(
          move[is_cm_bidirectional]
        ),
        moves_cm_forward_total = format_move_numbers(
          move[is_cm_forward]
        ),
        moves_cm_backward_total = format_move_numbers(
          move[is_cm_backward]
        ),
        
        .groups = "drop"
      )
    
    counts_long <- summary_wide %>%
      dplyr::select(
        critical_move_threshold,
        variable,
        category,
        n_total_moves_in_category,
        total_critical_moves,
        n_cm_forward_only,
        n_cm_backward_only,
        n_cm_bidirectional,
        n_cm_forward_total,
        n_cm_backward_total
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("n_cm_"),
        names_to = "cm_type_clean",
        values_to = "n_critical_moves"
      )
    
    moves_long <- summary_wide %>%
      dplyr::select(
        critical_move_threshold,
        variable,
        category,
        moves_cm_forward_only,
        moves_cm_backward_only,
        moves_cm_bidirectional,
        moves_cm_forward_total,
        moves_cm_backward_total
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("moves_cm_"),
        names_to = "cm_type_clean",
        values_to = "move_numbers"
      ) %>%
      dplyr::mutate(
        cm_type_clean = stringr::str_replace(
          cm_type_clean,
          "^moves_",
          "n_"
        )
      )
    
    counts_long %>%
      dplyr::left_join(
        moves_long,
        by = c(
          "critical_move_threshold",
          "variable",
          "category",
          "cm_type_clean"
        )
      ) %>%
      dplyr::mutate(
        cm_type = dplyr::case_when(
          cm_type_clean == "n_cm_forward_only" ~ "Exclusive: CM Forward Only",
          cm_type_clean == "n_cm_backward_only" ~ "Exclusive: CM Backward Only",
          cm_type_clean == "n_cm_bidirectional" ~ "Exclusive: CM Bidirectional",
          cm_type_clean == "n_cm_forward_total" ~ "Inclusive: CM Forward Total",
          cm_type_clean == "n_cm_backward_total" ~ "Inclusive: CM Backward Total"
        ),
        cm_type = factor(cm_type, levels = cm_type_levels),
        percent_critical_moves = dplyr::case_when(
          total_critical_moves > 0 ~ n_critical_moves / total_critical_moves * 100,
          TRUE ~ 0
        ),
        percent_total_moves = dplyr::case_when( # <--- NEW: Calculate new column
          n_total_moves_in_category > 0 ~ n_critical_moves / n_total_moves_in_category * 100,
          TRUE ~ 0
        )
      ) %>%
      dplyr::arrange(
        critical_move_threshold,
        variable,
        category,
        cm_type
      ) %>%
      dplyr::select(
        critical_move_threshold,
        variable,
        category,
        n_total_moves_in_category,
        total_critical_moves,
        cm_type,
        n_critical_moves,
        percent_critical_moves,
        percent_total_moves,
        move_numbers
      )
  }
  
  directional_critical_moves_by_variable <- lapply(
    variables,
    process_variable
  ) %>%
    dplyr::bind_rows()
  
  return(directional_critical_moves_by_variable)
}
##### Function: link matrices for selected move-level variables #####

summarise_links_between_categories <- function(moves,
                                               links,
                                               variables,
                                               missing_label = "Missing") {
  
  # Check selected variables exist
  missing_variables <- setdiff(variables, names(moves))
  
  if (length(missing_variables) > 0) {
    stop(
      "The following variables are not in the moves dataset: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  process_variable <- function(variable_name) {
    
    source_values <- moves %>%
      dplyr::select(
        source_move = move,
        source_category = dplyr::all_of(variable_name)
      ) %>%
      dplyr::mutate(
        source_category = as.character(source_category),
        source_category = tidyr::replace_na(source_category, missing_label)
      )
    
    target_values <- moves %>%
      dplyr::select(
        target_move = move,
        target_category = dplyr::all_of(variable_name)
      ) %>%
      dplyr::mutate(
        target_category = as.character(target_category),
        target_category = tidyr::replace_na(target_category, missing_label)
      )
    
    category_levels <- moves %>%
      dplyr::pull(dplyr::all_of(variable_name)) %>%
      as.character() %>%
      tidyr::replace_na(missing_label) %>%
      unique() %>%
      sort()
    
    observed_counts <- links %>%
      dplyr::select(source_move, target_move) %>%
      dplyr::left_join(source_values, by = "source_move") %>%
      dplyr::left_join(target_values, by = "target_move") %>%
      dplyr::count(
        source_category,
        target_category,
        name = "n_links"
      )
    
    full_counts <- tidyr::expand_grid(
      variable = variable_name,
      source_category = category_levels,
      target_category = category_levels
    ) %>%
      dplyr::left_join(
        observed_counts,
        by = c("source_category", "target_category")
      ) %>%
      dplyr::mutate(
        n_links = tidyr::replace_na(n_links, 0L)
      )
    
    matrix_wide <- full_counts %>%
      dplyr::select(
        variable,
        source_category,
        target_category,
        n_links
      ) %>%
      dplyr::mutate(
        target_category = paste0("target_", target_category)
      ) %>%
      tidyr::pivot_wider(
        names_from = target_category,
        values_from = n_links,
        values_fill = 0
      ) %>%
      dplyr::arrange(source_category)
    
    return(
      list(
        long = full_counts,
        wide = matrix_wide
      )
    )
  }
  
  outputs <- lapply(variables, process_variable)
  names(outputs) <- variables
  
  link_counts_between_categories_long <- lapply(outputs, `[[`, "long") %>%
    dplyr::bind_rows()
  
  link_matrices_by_variable <- lapply(outputs, `[[`, "wide")
  
  return(
    list(
      link_counts_between_categories_long = link_counts_between_categories_long,
      link_matrices_by_variable = link_matrices_by_variable
    )
  )
}

##### Function: undirected link matrices for selected move-level variables #####

summarise_undirected_links_between_categories <- function(moves,
                                                          links,
                                                          variables,
                                                          missing_label = "Missing") {
  
  # Check selected variables exist
  missing_variables <- setdiff(variables, names(moves))
  
  if (length(missing_variables) > 0) {
    stop(
      "The following variables are not in the moves dataset: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  process_variable <- function(variable_name) {
    
    category_values <- moves %>%
      dplyr::select(
        move,
        category = dplyr::all_of(variable_name)
      ) %>%
      dplyr::mutate(
        category = as.character(category),
        category = tidyr::replace_na(category, missing_label)
      )
    
    category_levels <- category_values %>%
      dplyr::pull(category) %>%
      unique() %>%
      sort()
    
    links_with_categories <- links %>%
      dplyr::select(source_move, target_move) %>%
      dplyr::left_join(
        category_values %>%
          dplyr::rename(
            source_move = move,
            source_category = category
          ),
        by = "source_move"
      ) %>%
      dplyr::left_join(
        category_values %>%
          dplyr::rename(
            target_move = move,
            target_category = category
          ),
        by = "target_move"
      ) %>%
      dplyr::mutate(
        source_index = match(source_category, category_levels),
        target_index = match(target_category, category_levels),
        
        row_index = pmin(source_index, target_index),
        column_index = pmax(source_index, target_index),
        
        row_category = category_levels[row_index],
        column_category = category_levels[column_index]
      )
    
    observed_counts <- links_with_categories %>%
      dplyr::count(
        row_category,
        column_category,
        name = "n_links"
      )
    
    full_matrix_long <- tidyr::expand_grid(
      row_index = seq_along(category_levels),
      column_index = seq_along(category_levels)
    ) %>%
      dplyr::mutate(
        variable = variable_name,
        row_category = category_levels[row_index],
        column_category = category_levels[column_index],
        valid_upper_triangle = row_index <= column_index
      ) %>%
      dplyr::left_join(
        observed_counts,
        by = c("row_category", "column_category")
      ) %>%
      dplyr::mutate(
        n_links = dplyr::case_when(
          valid_upper_triangle & is.na(n_links) ~ 0L,
          !valid_upper_triangle ~ NA_integer_,
          TRUE ~ n_links
        )
      ) %>%
      dplyr::select(
        variable,
        row_category,
        column_category,
        n_links
      )
    
    matrix_wide <- full_matrix_long %>%
      tidyr::pivot_wider(
        names_from = column_category,
        values_from = n_links
      ) %>%
      dplyr::arrange(row_category)
    
    return(
      list(
        long = full_matrix_long,
        wide = matrix_wide
      )
    )
  }
  
  outputs <- lapply(variables, process_variable)
  names(outputs) <- variables
  
  undirected_link_counts_long <- lapply(outputs, `[[`, "long") %>%
    dplyr::bind_rows()
  
  undirected_link_matrices_by_variable <- lapply(outputs, `[[`, "wide")
  
  return(
    list(
      undirected_link_counts_long = undirected_link_counts_long,
      undirected_link_matrices_by_variable = undirected_link_matrices_by_variable
    )
  )
}
##### Function: write a named list of tibbles to one Excel sheet #####

write_list_of_tables_to_sheet <- function(wb,
                                          sheet_name,
                                          table_list,
                                          start_row = 1,
                                          start_col = 1) {
  
  # Add sheet if it does not already exist
  if (!sheet_name %in% names(wb)) {
    addWorksheet(wb, sheet_name)
  }
  
  title_style <- createStyle(
    textDecoration = "bold",
    fontSize = 12
  )
  
  current_row <- start_row
  
  for (table_name in names(table_list)) {
    
    current_table <- table_list[[table_name]]
    
    # Write table title
    writeData(
      wb,
      sheet = sheet_name,
      x = table_name,
      startRow = current_row,
      startCol = start_col
    )
    
    addStyle(
      wb,
      sheet = sheet_name,
      style = title_style,
      rows = current_row,
      cols = start_col,
      gridExpand = TRUE
    )
    
    # Write table
    writeData(
      wb,
      sheet = sheet_name,
      x = current_table,
      startRow = current_row + 1,
      startCol = start_col
    )
    
    # Move down: title row + header row + data rows + gap
    current_row <- current_row + nrow(current_table) + 4
  }
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = 1:50,
    widths = "auto"
  )
}


# Define server logic
server <- function(input, output, session) {
  
  switch_settings_panel <- function(panel, button_id) {
    updateTabsetPanel(session, "settingsPanel", selected = panel)
    
    shinyjs::removeClass(selector = ".tool-button", class = "active")
    shinyjs::addClass(id = button_id, class = "active")
  }
  
  session$onFlushed(function() {
    shinyjs::addClass(id = "tool_data", class = "active")
  }, once = TRUE)
  
  observeEvent(input$tool_data, {
    switch_settings_panel("data", "tool_data")
  })
  
  observeEvent(input$tool_base, {
    switch_settings_panel("base", "tool_base")
  })
  
  observeEvent(input$tool_labels, {
    switch_settings_panel("labels", "tool_labels")
  })
  
  observeEvent(input$tool_overlays, {
    switch_settings_panel("overlays", "tool_overlays")
  })
  
  observeEvent(input$tool_sawtooth, {
    switch_settings_panel("sawtooth", "tool_sawtooth")
  })
  
  observeEvent(input$tool_webchunk, {
    switch_settings_panel("webchunk", "tool_webchunk")
  })
  
  observeEvent(input$tool_archio, {
    switch_settings_panel("archio", "tool_archio")
  })
  
  observeEvent(input$tool_custom, {
    switch_settings_panel("custom", "tool_custom")
  })
  
  observeEvent(input$tool_export, {
    switch_settings_panel("export", "tool_export")
  })
  
  observeEvent(input$tool_trace, {
    switch_settings_panel("trace", "tool_trace")
  })
  
  # Reset the plot view to fit everything
  # Toggle Auto-Scaling and Fit to Viewer
  observeEvent(input$resetView, {
    # Check if autoscaling is currently on or off
    current_state <- input$autoYAxis
    
    # Flip the checkbox to the opposite state
    updateCheckboxInput(session, "autoYAxis", value = !current_state)
    
    # If we are turning it ON, also snap the X-axis zoom to full width 
    # so the entire plot perfectly fits the viewer
    if (!current_state) {
      req(processed_data())
      max_move <- max(processed_data()$moves_df$move, na.rm = TRUE)
      updateSliderInput(session, "plotCropRange", value = c(1, max_move))
    }
  })
  
  # Handle Mouse-Driven Zooming (Brush + Double Click)
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    
    if (!is.null(brush)) {
      # 1. User highlighted a box and double-clicked. Let's zoom!
      
      # Turn OFF Auto Y-Axis so our manual zoom takes priority
      updateCheckboxInput(session, "autoYAxis", value = FALSE)
      
      # Update the X-axis (Crop Range slider) based on the brush box
      # We round to integers because moves are discrete numbers
      updateSliderInput(session, "plotCropRange", 
                        value = c(round(brush$xmin), round(brush$xmax)))
      
      # Update the Y-axis (Vertical range slider) based on the brush box
      updateSliderInput(session, "yAxisRange", 
                        value = c(brush$ymin, brush$ymax))
      
    } else {
      # 2. User double-clicked WITHOUT drawing a box. Reset the view!
      
      # Turn Auto Y-Axis back ON
      updateCheckboxInput(session, "autoYAxis", value = TRUE)
      
      # Reset the X-axis to the full span of the data
      req(processed_data())
      max_move <- max(processed_data()$moves_df$move, na.rm = TRUE)
      updateSliderInput(session, "plotCropRange", value = c(1, max_move))
    }
  })
  
  # Automatically import current Critical Move selections into Trace inputs
  observeEvent(input$importCmToTrace, {
    req(processed_data())
    pd <- processed_data()
    
    # Grab the cleaned critical move count table
    cm_counts <- pd$directional_critical_move_counts
    all_moves <- pd$moves_df$move
    
    # Helper to pull the comma-separated string, split it, and clean it up
    extract_move_vector <- function(type_label) {
      move_str <- cm_counts %>%
        dplyr::filter(`CM Type` == type_label) %>%
        dplyr::pull(`Move Numbers`)
      
      if (length(move_str) == 0 || is.na(move_str) || move_str == "") {
        return(character(0))
      }
      
      # Split by comma and trim any whitespace
      parsed <- unlist(stringr::str_split(move_str, ",\\s*"))
      return(parsed)
    }
    
    # Extract clean vectors for both directions
    fwd_cm_moves <- extract_move_vector("Inclusive: CM Forward Total")
    bwd_cm_moves <- extract_move_vector("Inclusive: CM Backward Total")
    
    # Overwrite the dropdowns with the imported vectors
    updateSelectizeInput(session, "traceForwardMoves", choices = all_moves, selected = fwd_cm_moves, server = TRUE)
    updateSelectizeInput(session, "traceBackwardMoves", choices = all_moves, selected = bwd_cm_moves, server = TRUE)
    
    # Optional Quality of Life: Auto-turn on the checkboxes if they are unchecked
    if (!input$showTraceForward && length(fwd_cm_moves) > 0) updateCheckboxInput(session, "showTraceForward", value = TRUE)
    if (!input$showTraceBackward && length(bwd_cm_moves) > 0) updateCheckboxInput(session, "showTraceBackward", value = TRUE)
  })
  
  # Reactive expression to read and process the uploaded data
  processed_data <- reactive({
    req(input$file)
    
    linkography_data <- read_linkography(input$file$datapath)
    
    moves <- linkography_data$moves
    links <- linkography_data$links
    raw_data <- linkography_data$raw_data
    
    ##### Basic summary #####
    
    linkography_summary <- tibble::tibble(
      n_moves = nrow(moves),
      n_links = nrow(links),
      first_move = min(moves$move, na.rm = TRUE),
      last_move = max(moves$move, na.rm = TRUE),
      n_link_columns = length(linkography_data$link_cols)
    )
    
    descriptive_statistics <- tibble::tibble(
      Statistic = c(
        "Number of moves",
        "Number of links",
        "Link index"
      ),
      Value = c(
        nrow(moves),
        nrow(links),
        nrow(links) / nrow(moves)
      )
    ) %>%
      dplyr::mutate(
        Value = dplyr::case_when(
          Statistic == "Link index" ~ sprintf("%.2f", Value),
          TRUE ~ sprintf("%.0f", Value)
        )
      )
    
    ##### Plot-compatible objects #####
    
    edge_list <- links %>%
      dplyr::transmute(
        from = source_move,
        to = target_move
      )
    
    link_index <- nrow(edge_list) / nrow(moves)
    total_moves <- nrow(moves)
    
    moves_df <- moves %>%
      dplyr::distinct(move) %>%
      dplyr::arrange(move) %>%
      dplyr::mutate(
        x = dplyr::row_number(),
        y = 0
      )
    
    connections_df <- edge_list %>%
      dplyr::left_join(
        moves_df %>% dplyr::select(move, x),
        by = c("from" = "move")
      ) %>%
      dplyr::rename(from_x = x) %>%
      dplyr::left_join(
        moves_df %>% dplyr::select(move, x),
        by = c("to" = "move")
      ) %>%
      dplyr::rename(to_x = x) %>%
      dplyr::mutate(
        distance = abs(from_x - to_x),
        mid_x = (from_x + to_x) / 2,
        mid_y = -distance
      )
    
    ##### Move direction classification #####
    
    move_direction_output <- classify_move_direction(
      moves = moves,
      links = links,
      raw_data = raw_data
    )
    
    move_direction_classification <- move_direction_output$move_direction_classification
    move_direction_counts <- move_direction_output$move_direction_counts
    moves_with_direction_classification <- move_direction_output$moves_with_direction_classification
    raw_data_with_direction_classification <- move_direction_output$raw_data_with_direction_classification
    
    uni_forward_moves <- move_direction_classification %>%
      dplyr::filter(move_type == "Unidirectional forward move") %>%
      dplyr::pull(move)
    
    uni_backward_moves <- move_direction_classification %>%
      dplyr::filter(move_type == "Unidirectional backward move") %>%
      dplyr::pull(move)
    
    bidirectional_moves <- move_direction_classification %>%
      dplyr::filter(move_type == "Bidirectional move") %>%
      dplyr::pull(move)
    
    ##### Sawtooth identification for plot overlay #####
    
    move_connections_df <- lapply(unique(c(moves_df$move)), function(move) {
      connected_moves <- unique(c(
        connections_df$from[connections_df$to == move],
        connections_df$to[connections_df$from == move]
      ))
      connected_moves <- setdiff(connected_moves, move)
      data.frame(move = move, connected_moves = I(list(connected_moves)))
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(connected_values = list(unlist(connected_moves))) %>%
      dplyr::ungroup()
    
    sawtooth_patterns <- identify_sawtooth_patterns(move_connections_df)
    sawtooth_sequence <- unlist(sawtooth_patterns)
    
    if (length(sawtooth_sequence) == 0) {
      connections_df <- connections_df %>%
        dplyr::mutate(sawtooth_role = "Not in Sawtooth")
    } else {
      connections_df <- connections_df %>%
        dplyr::mutate(
          sawtooth_role = dplyr::case_when(
            from %in% sawtooth_sequence & to %in% sawtooth_sequence & from == min(sawtooth_sequence) ~ "Forward from First",
            from %in% sawtooth_sequence & to %in% sawtooth_sequence & to == max(sawtooth_sequence) ~ "Backward to Last",
            from %in% sawtooth_sequence & to %in% sawtooth_sequence ~ "Middle Moves",
            TRUE ~ "Not in Sawtooth"
          )
        )
    }
    
    ##### Link span table, retained from the original app #####
    
    link_span_moves_df <- connections_df %>%
      dplyr::select(from, to, distance)
    
    unique_moves <- unique(c(moves$move))
    
    distance_df <- expand.grid(
      from = unique_moves,
      to = unique_moves
    ) %>%
      dplyr::left_join(
        link_span_moves_df,
        by = c("from", "to")
      )
    
    distance_table <- distance_df %>%
      tidyr::spread(key = to, value = distance)
    
    distance_table[is.na(distance_table)] <- ""
    colnames(distance_table)[which(names(distance_table) == "from")] <- "Move"
    
    ##### Variable-based outputs from the analysis script #####
    
    exclude_cols <- c("move", "from", "to", "x", "y")
    
    # ---> FIXED: We calculate everything behind the scenes instantly. 
    # This completely severs the infinite loop!
    analysis_variables <- setdiff(names(moves), exclude_cols)
    
    if (length(analysis_variables) > 0) {
      inter_intra_links_by_variable <- summarise_inter_intra_by_variables(
        moves = moves,
        links = links,
        variables = analysis_variables
      )
      
      move_direction_by_variable_output <- summarise_move_direction_by_variables(
        moves_with_direction_classification = moves_with_direction_classification,
        variables = analysis_variables
      )
      
      move_direction_by_variable_long <- move_direction_by_variable_output$move_direction_by_variable_long
      move_direction_by_variable_wide <- move_direction_by_variable_output$move_direction_by_variable_wide
      
      link_matrix_output <- summarise_links_between_categories(
        moves = moves,
        links = links,
        variables = analysis_variables
      )
      
      link_counts_between_categories_long <- link_matrix_output$link_counts_between_categories_long
      link_matrices_by_variable <- link_matrix_output$link_matrices_by_variable
      
      undirected_link_matrix_output <- summarise_undirected_links_between_categories(
        moves = moves,
        links = links,
        variables = analysis_variables
      )
      
      undirected_link_counts_long <- undirected_link_matrix_output$undirected_link_counts_long
      undirected_link_matrices_by_variable <- undirected_link_matrix_output$undirected_link_matrices_by_variable
    } else {
      inter_intra_links_by_variable <- empty_table()
      move_direction_by_variable_long <- empty_table()
      move_direction_by_variable_wide <- empty_table()
      link_counts_between_categories_long <- empty_table()
      link_matrices_by_variable <- list()
      undirected_link_counts_long <- empty_table()
      undirected_link_matrices_by_variable <- list()
    }
    
    ##### Directional critical move outputs #####
    
    directional_cm_threshold_output <- explore_directional_cm_thresholds(
      moves_with_direction_classification = moves_with_direction_classification,
      percentages = 5:15,
      round_method = "round_half_up"
    )
    
    directional_scores <- directional_cm_threshold_output$directional_scores
    directional_cm_thresholds <- directional_cm_threshold_output$directional_cm_thresholds
    
    selected_directional_cm_threshold <- input$selectedCmThreshold %||% 2
    selected_directional_cm_threshold <- as.numeric(selected_directional_cm_threshold)
    
    if (is.na(selected_directional_cm_threshold) || selected_directional_cm_threshold < 1) {
      selected_directional_cm_threshold <- 2
    }
    
    directional_critical_move_output <- summarise_directional_critical_moves_overall(
      moves_with_direction_classification = moves_with_direction_classification,
      critical_move_thresholds = selected_directional_cm_threshold
    )
    
    directional_critical_moves <- directional_critical_move_output$directional_critical_moves
    directional_critical_move_counts <- directional_critical_move_output$directional_critical_move_counts
    
    if (length(analysis_variables) > 0) {
      directional_critical_moves_by_variable <- summarise_directional_critical_moves_by_variables(
        moves_with_direction_classification = moves_with_direction_classification,
        variables = analysis_variables,
        critical_move_thresholds = selected_directional_cm_threshold
      )
    } else {
      directional_critical_moves_by_variable <- empty_table()
    }
    
    # Compatibility values for older text outputs, if still present anywhere in the UI
    critical_move_a <- floor(nrow(moves) / 10)
    critical_move_b <- critical_move_a + 1
    critical_move_c <- critical_move_a + 2
    
    ##### Final Cleanup for Presentation and Export #####
    descriptive_statistics <- clean_df_names(descriptive_statistics)
    inter_intra_links_by_variable <- clean_df_names(inter_intra_links_by_variable)
    move_direction_counts <- clean_df_names(move_direction_counts)
    moves_with_direction_classification <- clean_df_names(moves_with_direction_classification)
    move_direction_by_variable_wide <- clean_df_names(move_direction_by_variable_wide)
    link_counts_between_categories_long <- clean_df_names(link_counts_between_categories_long)
    undirected_link_counts_long <- clean_df_names(undirected_link_counts_long)
    directional_cm_thresholds <- clean_df_names(directional_cm_thresholds)
    directional_scores <- clean_df_names(directional_scores)
    directional_critical_move_counts <- clean_df_names(directional_critical_move_counts)
    directional_critical_moves_by_variable <- clean_df_names(directional_critical_moves_by_variable)
    
    list(
      raw_data = raw_data,
      moves = moves,
      links = links,
      linkography_summary = linkography_summary,
      descriptive_statistics = descriptive_statistics,
      analysis_variables = analysis_variables,
      moves_df = moves_df,
      connections_df = connections_df,
      uni_forward_moves = uni_forward_moves,
      uni_backward_moves = uni_backward_moves,
      bidirectional_moves = bidirectional_moves,
      sawtooth_sequence = sawtooth_sequence,
      sawtooth_patterns_list = sawtooth_patterns,
      link_index = link_index,
      total_moves = total_moves,
      critical_move_a = critical_move_a,
      critical_move_b = critical_move_b,
      critical_move_c = critical_move_c,
      distance_table = distance_table,
      edge_list = edge_list,
      archiograph_moves_df = raw_data,
      inter_intra_links_by_variable = inter_intra_links_by_variable,
      move_direction_classification = move_direction_classification,
      move_direction_counts = move_direction_counts,
      moves_with_direction_classification = moves_with_direction_classification,
      raw_data_with_direction_classification = raw_data_with_direction_classification,
      move_direction_by_variable_long = move_direction_by_variable_long,
      move_direction_by_variable_wide = move_direction_by_variable_wide,
      directional_scores = directional_scores,
      directional_cm_thresholds = directional_cm_thresholds,
      selected_directional_cm_threshold = selected_directional_cm_threshold,
      directional_critical_moves = directional_critical_moves,
      directional_critical_move_counts = directional_critical_move_counts,
      directional_critical_moves_by_variable = directional_critical_moves_by_variable,
      link_counts_between_categories_long = link_counts_between_categories_long,
      link_matrices_by_variable = link_matrices_by_variable,
      undirected_link_counts_long = undirected_link_counts_long,
      undirected_link_matrices_by_variable = undirected_link_matrices_by_variable,
      move_classification = moves_with_direction_classification
    )
  })
  
  # --- UI Sync for Link Span Thresholds ---
  # Helper to get current distance stats cleanly
  span_stats <- reactive({
    req(processed_data())
    conn <- processed_data()$connections_df
    if(nrow(conn) == 0) return(NULL)
    dists <- conn$distance
    list(
      max_dist = max(dists, na.rm = TRUE),
      dists = dists,
      ecdf_fun = ecdf(dists)
    )
  })
  
  # Visually disable the inactive inputs so the user knows which ones to type in
  observeEvent(input$spanThresholdMode, {
    if (input$spanThresholdMode == "percentile") {
      shinyjs::enable("spanCutoff1_perc")
      shinyjs::enable("spanCutoff2_perc")
      shinyjs::disable("spanCutoff1_abs")
      shinyjs::disable("spanCutoff2_abs")
    } else {
      shinyjs::disable("spanCutoff1_perc")
      shinyjs::disable("spanCutoff2_perc")
      shinyjs::enable("spanCutoff1_abs")
      shinyjs::enable("spanCutoff2_abs")
    }
  })
  
  # When new data loads, initialize the max cap and sync to defaults
  observeEvent(span_stats(), {
    stats <- span_stats()
    req(stats)
    
    cap <- max(1, stats$max_dist - 1)
    updateNumericInput(session, "spanCutoff2_abs", max = cap)
    
    # Sync from default percentiles to absolute on first load
    p1 <- if (is.null(input$spanCutoff1_perc) || is.na(input$spanCutoff1_perc)) 25 else input$spanCutoff1_perc
    p2 <- if (is.null(input$spanCutoff2_perc) || is.na(input$spanCutoff2_perc)) 75 else input$spanCutoff2_perc
    
    new_abs1 <- round(quantile(stats$dists, p1 / 100, na.rm = TRUE, names = FALSE))
    new_abs2 <- round(quantile(stats$dists, p2 / 100, na.rm = TRUE, names = FALSE))
    
    if (new_abs2 > cap) new_abs2 <- cap
    if (new_abs1 > new_abs2) new_abs1 <- new_abs2
    
    updateNumericInput(session, "spanCutoff1_abs", value = new_abs1)
    updateNumericInput(session, "spanCutoff2_abs", value = new_abs2)
  })
  
  # Sync: Percentile -> Absolute
  observeEvent(c(input$spanCutoff1_perc, input$spanCutoff2_perc), {
    req(input$spanThresholdMode == "percentile")
    
    stats <- span_stats()
    req(stats)
    
    p1 <- if (is.null(input$spanCutoff1_perc) || is.na(input$spanCutoff1_perc)) 25 else input$spanCutoff1_perc
    p2 <- if (is.null(input$spanCutoff2_perc) || is.na(input$spanCutoff2_perc)) 75 else input$spanCutoff2_perc
    
    new_abs1 <- round(quantile(stats$dists, p1 / 100, na.rm = TRUE, names = FALSE))
    new_abs2 <- round(quantile(stats$dists, p2 / 100, na.rm = TRUE, names = FALSE))
    
    cap <- max(1, stats$max_dist - 1)
    if (new_abs2 > cap) new_abs2 <- cap
    if (new_abs1 > new_abs2) new_abs1 <- new_abs2
    
    if (!isTRUE(all.equal(input$spanCutoff1_abs, new_abs1))) {
      updateNumericInput(session, "spanCutoff1_abs", value = new_abs1)
    }
    if (!isTRUE(all.equal(input$spanCutoff2_abs, new_abs2))) {
      updateNumericInput(session, "spanCutoff2_abs", value = new_abs2)
    }
  }, ignoreInit = TRUE)
  
  # Sync: Absolute -> Percentile
  observeEvent(c(input$spanCutoff1_abs, input$spanCutoff2_abs), {
    req(input$spanThresholdMode == "absolute")
    
    stats <- span_stats()
    req(stats)
    
    a1 <- if (is.null(input$spanCutoff1_abs) || is.na(input$spanCutoff1_abs)) 1 else input$spanCutoff1_abs
    a2 <- if (is.null(input$spanCutoff2_abs) || is.na(input$spanCutoff2_abs)) 2 else input$spanCutoff2_abs
    
    cap <- max(1, stats$max_dist - 1)
    
    # ---> THE FIX: Snap the UI back if the user types an impossible number
    safe_a2 <- min(a2, cap)
    safe_a1 <- min(a1, safe_a2)
    
    if (a2 > cap) {
      updateNumericInput(session, "spanCutoff2_abs", value = safe_a2)
    }
    if (a1 > safe_a2) {
      updateNumericInput(session, "spanCutoff1_abs", value = safe_a1)
    }
    
    # Calculate the new percentiles based on the safe numbers
    new_p1 <- round(stats$ecdf_fun(safe_a1) * 100)
    new_p2 <- round(stats$ecdf_fun(safe_a2) * 100)
    
    if (!isTRUE(all.equal(input$spanCutoff1_perc, new_p1))) {
      updateNumericInput(session, "spanCutoff1_perc", value = new_p1)
    }
    if (!isTRUE(all.equal(input$spanCutoff2_perc, new_p2))) {
      updateNumericInput(session, "spanCutoff2_perc", value = new_p2)
    }
  }, ignoreInit = TRUE)
  
  # --- Data Processing using the absolute thresholds ---
  span_analysis_data <- reactive({
    req(processed_data())
    pd <- processed_data()
    conn <- pd$connections_df
    moves_df <- pd$moves_df
    
    if(nrow(conn) == 0) {
      return(list(
        overall_stats = empty_table("No links available."),
        span_cats = empty_table("No links available."),
        move_stats = empty_table("No links available.")
      ))
    }
    
    distances <- conn$distance
    total_links <- length(distances)
    
    # Calculate exact frequencies for highly local spans
    span_1_count <- sum(distances == 1, na.rm = TRUE)
    span_2_count <- sum(distances == 2, na.rm = TRUE)
    span_3_count <- sum(distances == 3, na.rm = TRUE)
    
    span_1_pct <- round((span_1_count / total_links) * 100, 2)
    span_2_pct <- round((span_2_count / total_links) * 100, 2)
    span_3_pct <- round((span_3_count / total_links) * 100, 2)
    
    # Quartiles for the boxplot stats
    q1 <- quantile(distances, 0.25, na.rm = TRUE, names = FALSE)
    q3 <- quantile(distances, 0.75, na.rm = TRUE, names = FALSE)
    
    # 1. Overall Stats
    overall_stats <- tibble::tibble(
      Statistic = c(
        "Total Links",
        "Mean Linkspan", 
        "Median Linkspan", 
        "Standard Deviation", 
        "Minimum Span",
        "1st Quartile (Q1)",
        "3rd Quartile (Q3)",
        "Interquartile Range (IQR)", 
        "Maximum Span", 
        "Links with Span = 1",
        "Links with Span = 2",
        "Links with Span = 3"
      ),
      Value = c(
        as.character(total_links),
        as.character(round(mean(distances, na.rm=TRUE), 2)),
        as.character(round(median(distances, na.rm=TRUE), 2)),
        as.character(round(sd(distances, na.rm=TRUE), 2)),
        as.character(min(distances, na.rm=TRUE)),
        as.character(q1),
        as.character(q3),
        as.character(round(IQR(distances, na.rm=TRUE), 2)),
        as.character(max(distances, na.rm=TRUE)),
        paste0(span_1_count, " (", span_1_pct, "%)"),
        paste0(span_2_count, " (", span_2_pct, "%)"),
        paste0(span_3_count, " (", span_3_pct, "%)")
      )
    )
    
    # 2. Span Categories
    raw_c1 <- if(is.null(input$spanCutoff1_abs) || is.na(input$spanCutoff1_abs)) 1 else input$spanCutoff1_abs
    raw_c2 <- if(is.null(input$spanCutoff2_abs) || is.na(input$spanCutoff2_abs)) 2 else input$spanCutoff2_abs
    
    max_dist <- max(distances, na.rm = TRUE)
    cap <- max(1, max_dist - 1)
    actual_c2 <- min(raw_c2, cap)
    actual_c1 <- min(raw_c1, actual_c2)
    
    conn <- conn %>%
      dplyr::mutate(
        span_category = dplyr::case_when(
          distance <= actual_c1 ~ "Short",
          distance > actual_c1 & distance <= actual_c2 ~ "Medium",
          distance > actual_c2 ~ "Long",
          TRUE ~ "Unknown"
        )
      )
    
    span_cats <- conn %>%
      dplyr::count(span_category) %>%
      dplyr::mutate(Percentage = round(n / sum(n) * 100, 2)) %>%
      dplyr::rename(Category = span_category, Count = n)
    
    # 3. Move-Level Mastery Table
    fore <- conn %>%
      dplyr::group_by(move = to) %>%
      dplyr::summarise(
        n_forelinks = dplyr::n(),
        mean_forelink_span = round(mean(distance, na.rm=TRUE), 2),
        median_forelink_span = round(median(distance, na.rm=TRUE), 2),
        max_forelink_span = max(distance, na.rm=TRUE),
        long_forelinks = sum(span_category == "Long", na.rm=TRUE),
        medium_forelinks = sum(span_category == "Medium", na.rm=TRUE),
        short_forelinks = sum(span_category == "Short", na.rm=TRUE)
      )
    
    back <- conn %>%
      dplyr::group_by(move = from) %>%
      dplyr::summarise(
        n_backlinks = dplyr::n(),
        mean_backlink_span = round(mean(distance, na.rm=TRUE), 2),
        median_backlink_span = round(median(distance, na.rm=TRUE), 2),
        max_backlink_span = max(distance, na.rm=TRUE),
        long_backlinks = sum(span_category == "Long", na.rm=TRUE),
        medium_backlinks = sum(span_category == "Medium", na.rm=TRUE),
        short_backlinks = sum(span_category == "Short", na.rm=TRUE)
      )
    
    move_stats <- moves_df %>%
      dplyr::select(move) %>%
      dplyr::left_join(fore, by="move") %>%
      dplyr::left_join(back, by="move") %>%
      # Replace NA with 0 ONLY for the count-based metrics
      dplyr::mutate(
        dplyr::across(c(n_forelinks, long_forelinks, medium_forelinks, short_forelinks,
                        n_backlinks, long_backlinks, medium_backlinks, short_backlinks), 
                      ~tidyr::replace_na(.x, 0))
      ) %>%
      dplyr::mutate(
        total_links = n_forelinks + n_backlinks,
        net_directionality = n_forelinks - n_backlinks,
        max_span_overall = suppressWarnings(pmax(max_forelink_span, max_backlink_span, na.rm=TRUE)),
        max_span_overall = ifelse(is.infinite(max_span_overall), NA, max_span_overall),
        total_long_links = long_forelinks + long_backlinks,
        total_medium_links = medium_forelinks + medium_backlinks,
        total_short_links = short_forelinks + short_backlinks
      ) %>%
      dplyr::select(
        move,
        max_span_overall,
        total_links,
        net_directionality,
        n_forelinks, mean_forelink_span, median_forelink_span, max_forelink_span,
        long_forelinks, medium_forelinks, short_forelinks,
        n_backlinks, mean_backlink_span, median_backlink_span, max_backlink_span,
        long_backlinks, medium_backlinks, short_backlinks,
        total_long_links, total_medium_links, total_short_links
      ) %>%
      dplyr::arrange(dplyr::desc(max_span_overall))
    
    list(
      overall_stats = clean_df_names(overall_stats),
      span_cats = clean_df_names(span_cats),
      move_stats = clean_df_names(move_stats)
    )
  })
  
  # Render the new tables
  output$spanOverallStatsTable <- renderReactable({
    req(span_analysis_data())
    build_quant_table(span_analysis_data()$overall_stats, page_length = 15)
  })
  
  output$spanCategoriesTable <- renderReactable({
    req(span_analysis_data())
    build_quant_table(span_analysis_data()$span_cats, page_length = 10)
  })
  
  output$spanMoveLevelTable <- renderReactable({
    req(span_analysis_data())
    build_quant_table(span_analysis_data()$move_stats, page_length = 20)
  })
  
  
  # Update selectInput choices based on the columns of archiograph_moves_df
  observe({
    req(processed_data())
    
    # Access archiograph_moves_df from the processed data
    archiograph_moves_df <- processed_data()$archiograph_moves_df
    
    # Exclude "move" and unnamed columns (e.g., "...1", "...2", etc.)
    all_columns <- setdiff(names(archiograph_moves_df), c("from", "to", "move"))
    all_columns <- all_columns[!grepl("^\\.+", all_columns)]
    
    # Check if all_columns is empty
    if (length(all_columns) == 0) {
      all_columns <- "No eligible columns"  # Placeholder text
      shinyjs::disable("archiographColumn")  # Disable the selectInput if no columns are available
    } else {
      shinyjs::enable("archiographColumn")  # Enable the selectInput if columns are available
    }
    
    # Update the dropdown options in the UI
    updateSelectInput(session, "archiographColumn", choices = all_columns)
  })
  
  # Update Trace drop-downs and Crop Slider with available moves
  observe({
    req(processed_data())
    moves <- processed_data()$moves_df$move
    max_move <- max(moves, na.rm = TRUE)
    
    # Update Trace menus
    updateSelectizeInput(session, "traceForwardMoves", choices = moves, server = TRUE)
    updateSelectizeInput(session, "traceBackwardMoves", choices = moves, server = TRUE)
    
    # Update Crop Slider to span the full dataset by default
    updateSliderInput(session, "plotCropRange", max = max_move, value = c(1, max_move))
  })
  
  # Update the Move Classification dropdown exactly ONCE when new data is uploaded
  observeEvent(processed_data(), {
    moves <- processed_data()$moves
    exclude_cols <- c("move", "from", "to", "x", "y") 
    cat_cols <- setdiff(names(moves), exclude_cols)
    
    # Load them instantly using the new picker functions
    updatePickerInput(session, "quantGroupVars", choices = cat_cols, selected = cat_cols)
    updatePickerInput(session, "cmGroupVars", choices = cat_cols, selected = cat_cols) 
  })
  
  # Output the number of unique factors and dynamically generate color pickers
  output$numFactorsOutput <- renderText({
    req(input$archiographColumn)
    
    if (input$archiographColumn == "No eligible columns") {
      return("No eligible columns for Archiograph.")
    }
    
    archiograph_moves_df <- processed_data()$archiograph_moves_df
    selected_column <- archiograph_moves_df[[input$archiographColumn]]  # Extract selected column
    unique_factors <- unique(selected_column)  # Get unique factors
    
    # Concatenate factors into a string without leading spaces
    paste("Unique factors:\n", paste(unique_factors, collapse = "\n"))
  })
  
  # Dynamically generate color pickers for each unique factor
  output$colorPickersUI <- renderUI({
    req(input$archiographColumn)
    
    if (input$archiographColumn == "No eligible columns") {
      return(NULL)  # No color pickers if there are no eligible columns
    }
    
    archiograph_moves_df <- processed_data()$archiograph_moves_df
    selected_column <- archiograph_moves_df[[input$archiographColumn]]
    unique_factors <- unique(selected_column)
    
    # Generate color pickers for each unique factor
    picker_list <- lapply(unique_factors, function(factor) {
      colourpicker::colourInput(
        inputId = paste0("color_", gsub(" ", "_", factor)),  # Dynamic input ID for each factor
        label = paste("Color for", factor),
        value = "blue"  # Default color value (you can change this)
      )
    })
    
    do.call(tagList, picker_list)  # Combine all color pickers into a tagList
  })
  
  
  # Reactive expression to perform web and chunk analysis when the button is clicked
  web_chunk_data <- eventReactive(input$runWebChunk, {
    req(processed_data())
    
    patterns_100 <- identify_patterns(
      df = processed_data()$connections_df %>% select(from, to),
      min_moves = input$minMoves,
      max_moves = input$maxMoves,
      min_connection_percentage = input$minConnectionPercentage,
      max_connection_percentage = input$maxConnectionPercentage
    )
    
    patterns_100
  })
  
  # Reactive expression to add scaled mid_y to connections_df
  scaled_connections_df <- reactive({
    req(processed_data())
    connections_df <- processed_data()$connections_df
    connections_df <- connections_df %>%
      mutate(mid_y_scaled = mid_y * input$yendScale) %>%
      mutate(mid_y_scaled_arc = mid_y * input$yarcScale)# Create a new reactive column
    connections_df
  })
  
  # Output the Link Index
  output$linkIndexOutput <- renderText({
    req(processed_data())
    paste("Link Index (Total number of links/Total number of moves):", round(processed_data()$link_index, 2))
  })
  
  # Output the Total Number of Moves
  output$totalNumberMoves <- renderText({
    req(processed_data())
    paste("Total number of moves:", processed_data()$total_moves)
  })
  
  # Output Critical Forward Move A
  output$criticalForwardMoveA <- renderText({
    req(processed_data())
    paste("CM>A:", processed_data()$critical_move_a)
  })
  
  # Output Critical Forward Move B
  output$criticalForwardMoveB <- renderText({
    req(processed_data())
    paste("CM>B:", processed_data()$critical_move_b)
  })
  
  # Output Critical Forward Move C
  output$criticalForwardMoveC <- renderText({
    req(processed_data())
    paste("CM>C:", processed_data()$critical_move_c)
  })
  
  # Output Critical Backward Move A
  output$criticalBackwardMoveA <- renderText({
    req(processed_data())
    paste("CM<A:", processed_data()$critical_move_a)
  })
  
  # Output Critical Backward Move B
  output$criticalBackwardMoveB <- renderText({
    req(processed_data())
    paste("CM<B:", processed_data()$critical_move_b)
  })
  
  # Output Critical Backward Move C
  output$criticalBackwardMoveC <- renderText({
    req(processed_data())
    paste("CM<C:", processed_data()$critical_move_c)
  })
  
  ##### New Reactable Builder Function #####
  
  # This helper function passes any extra arguments (`...`) directly into reactable
  ##### New Reactable Builder Function #####
  
  # This helper function passes any extra arguments (`...`) directly into reactable
  build_quant_table <- function(data, page_length = 20, ...) {
    reactable(
      data,
      ...,
      pagination = TRUE,
      showPageSizeOptions = TRUE,                # <--- NEW: Turns on the dropdown
      pageSizeOptions = c(10, 20, 50, 100, 500), # <--- NEW: Gives options up to 500 rows
      defaultPageSize = page_length,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = TRUE,      
      resizable = TRUE, 
      defaultColDef = colDef(
        minWidth = 150  
      ),
      theme = reactableTheme(
        # Header styling
        headerStyle = list(
          backgroundColor = "#0A4F57",
          color = "#FFFFFF",
          textTransform = "uppercase",
          fontWeight = 850,
          fontSize = "0.74rem",
          letterSpacing = "0.055em",
          borderBottom = "4px solid #06AED5",
          padding = "11px 13px",
          whiteSpace = "normal", 
          wordBreak = "break-word", # <--- 2. Forces long column names with underscores to wrap!
          lineHeight = "1.3"
        ),
        # Body styling
        borderColor = "rgba(10, 79, 87, 0.08)",
        stripedColor = "#FAFBFC",
        highlightColor = "rgba(6, 174, 213, 0.08)",
        cellPadding = "8px 13px",
        style = list(
          fontFamily = "Inter, system-ui, -apple-system, sans-serif",
          fontSize = "0.88rem",
          color = "#2d3436"
        )
      )
    )
  }
  
  # Render the data summary table
  output$dataSummary <- renderReactable({
    req(processed_data())
    build_quant_table(processed_data()$moves_df, page_length = 10)
  })
  
  # Render the link span table
  output$linkSpanTable <- renderReactable({
    req(processed_data())
    build_quant_table(
      processed_data()$distance_table, 
      page_length = 20,
      # Pin the "Move" column to the left when scrolling horizontally!
      columns = list(
        Move = colDef(
          style = list(fontWeight = "800", color = "#0A4F57", backgroundColor = "#F6F7F9"),
          sticky = "left"
        )
      )
    )
  })
  
  output$descriptiveStatisticsTable <- renderReactable({
    req(processed_data())
    build_quant_table(processed_data()$descriptive_statistics, page_length = 10)
  })
  
  output$interIntraLinksTable <- renderReactable({
    req(processed_data(), input$quantGroupVars)
    df <- processed_data()$inter_intra_links_by_variable
    if ("Variable" %in% names(df)) df <- df[df$Variable %in% input$quantGroupVars, ]
    build_quant_table(df, page_length = 20)
  })
  
  output$moveDirectionCountsTable <- renderReactable({
    req(processed_data())
    build_quant_table(processed_data()$move_direction_counts, page_length = 10)
  })
  
  output$moveDirectionClassificationTable <- renderReactable({
    req(processed_data())
    build_quant_table(processed_data()$moves_with_direction_classification, page_length = 20)
  })
  
  output$moveDirectionByVariableTable <- renderReactable({
    req(processed_data(), input$quantGroupVars)
    df <- processed_data()$move_direction_by_variable_wide
    if ("Variable" %in% names(df)) df <- df[df$Variable %in% input$quantGroupVars, ]
    build_quant_table(df, page_length = 20)
  })
  
  output$directedLinkCountsTable <- renderReactable({
    req(processed_data(), input$quantGroupVars)
    df <- processed_data()$link_counts_between_categories_long
    if ("Variable" %in% names(df)) df <- df[df$Variable %in% input$quantGroupVars, ]
    build_quant_table(df, page_length = 20)
  })
  
  output$undirectedLinkCountsTable <- renderReactable({
    req(processed_data(), input$quantGroupVars)
    df <- processed_data()$undirected_link_counts_long
    if ("Variable" %in% names(df)) df <- df[df$Variable %in% input$quantGroupVars, ]
    build_quant_table(df, page_length = 20)
  })
  
  output$cmThresholdsTable <- renderReactable({
    req(processed_data())
    build_quant_table(processed_data()$directional_cm_thresholds, page_length = 15)
  })
  
  output$cmRankingTable <- renderReactable({
    req(processed_data())
    build_quant_table(processed_data()$directional_scores, page_length = 20)
  })
  
  output$cmMoveCountsTable <- renderReactable({
    req(processed_data())
    build_quant_table(processed_data()$directional_critical_move_counts, page_length = 20)
  })
  
  output$cmCountsByVariableTable <- renderReactable({
    # ---> NEW: Now strictly listens to the dropdown on its own tab
    req(processed_data(), input$cmGroupVars) 
    
    df <- processed_data()$directional_critical_moves_by_variable
    
    if ("Variable" %in% names(df)) {
      df <- df[df$Variable %in% input$cmGroupVars, ]
    }
    
    build_quant_table(df, page_length = 20)
  })
  
  # Master Export Function for all three tables
  generate_master_export <- function(file, pd, span_data) {
    stacked_undirected <- stack_tables_for_writexl(pd$undirected_link_matrices_by_variable)
    stacked_directed <- stack_tables_for_writexl(pd$link_matrices_by_variable)
    
    sheets_to_export <- list(
      "Summary" = pd$linkography_summary,
      "Descriptive Statistics" = pd$descriptive_statistics,
      "Span Overall Stats" = span_data$overall_stats,
      "Span Categories" = span_data$span_cats,
      "Span Move Stats" = span_data$move_stats,
      "Link Span Matrix" = pd$distance_table,
      "Inter Intra Links" = pd$inter_intra_links_by_variable,
      "Move Direction Counts" = pd$move_direction_counts,
      "Move Direction Class" = pd$moves_with_direction_classification,
      "Move Dir by Variable" = pd$move_direction_by_variable_wide,
      "CM Ranking List" = pd$directional_scores,
      "CM Threshold List" = pd$directional_cm_thresholds,
      "CM Move Counts" = pd$directional_critical_move_counts,
      "CM Counts by Variable" = pd$directional_critical_moves_by_variable,
      "Undirected Link Matrices" = stacked_undirected,
      "Directed Link Matrices" = stacked_directed
    )
    writexl::write_xlsx(sheets_to_export, path = file)
  }
  
  # Link Span Dedicated Download
  output$downloadSpanOutputs <- downloadHandler(
    filename = function() { paste0("linkography_outputs-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx") },
    content = function(file) {
      req(processed_data(), span_analysis_data())
      generate_master_export(file, processed_data(), span_analysis_data())
    }
  )
  
  # Move Classification Dedicated Download
  output$downloadClassOutputs <- downloadHandler(
    filename = function() { paste0("linkography_outputs-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx") },
    content = function(file) {
      req(processed_data(), span_analysis_data())
      generate_master_export(file, processed_data(), span_analysis_data())
    }
  )
  
  # Critical Moves Dedicated Download
  output$downloadQuantOutputs <- downloadHandler(
    filename = function() { paste0("linkography_outputs-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx") },
    content = function(file) {
      req(processed_data(), span_analysis_data())
      generate_master_export(file, processed_data(), span_analysis_data())
    }
  )
  
  selected_colors <- reactive({
    req(input$archiographColumn)
    
    # Return an empty list if no eligible columns are present
    if (input$archiographColumn == "No eligible columns") {
      return(list())
    }
    
    archiograph_moves_df <- processed_data()$archiograph_moves_df
    selected_column <- archiograph_moves_df[[input$archiographColumn]]
    unique_factors <- unique(selected_column)
    
    colors <- sapply(unique_factors, function(factor) {
      input[[paste0("color_", gsub(" ", "_", factor))]]  # Get the color input value for each factor
    }, simplify = FALSE)
    
    colors  # Return a list of colors mapped to their factors
  })
  
  # Reactive value to store the number of custom patterns
  customPatternCount <- reactiveVal(1)
  
  # Reactive value to store the current state of the patterns
  customPatternValues <- reactiveValues()
  
  # Add a new pattern UI when the "Add New Pattern" button is clicked
  observeEvent(input$addPattern, {
    # Save current values before adding a new pattern
    for (i in 1:customPatternCount()) {
      customPatternValues[[paste0("patternFrom_", i)]] <- input[[paste0("patternFrom_", i)]]
      customPatternValues[[paste0("patternTo_", i)]] <- input[[paste0("patternTo_", i)]]
      customPatternValues[[paste0("showPattern_", i)]] <- input[[paste0("showPattern_", i)]]
      customPatternValues[[paste0("patternColor_", i)]] <- input[[paste0("patternColor_", i)]]
      customPatternValues[[paste0("patternWeight_", i)]] <- input[[paste0("patternWeight_", i)]]
    }
    
    # Increment the pattern count
    customPatternCount(customPatternCount() + 1)
  })
  
  # Dynamically generate the UI for custom patterns
  output$customPatternsUI <- renderUI({
    pattern_ui <- lapply(1:customPatternCount(), function(i) {
      tagList(
        fluidRow(
          column(6, h4(paste("Pattern", i))),  # Top row with "Pattern i" text
          column(6, checkboxInput(paste0("showPattern_", i), "Show", value = customPatternValues[[paste0("showPattern_", i)]] %||% FALSE))  # Checkbox
        ),
        fluidRow(
          column(6, numericInput(paste0("patternFrom_", i), "From:", value = customPatternValues[[paste0("patternFrom_", i)]] %||% 1, min = 1)),  # From input
          column(6, numericInput(paste0("patternTo_", i), "To:", value = customPatternValues[[paste0("patternTo_", i)]] %||% 5, min = 1))  # To input
        ),
        fluidRow(
          column(6, colourpicker::colourInput(paste0("patternColor_", i), "Pattern Color:", value = customPatternValues[[paste0("patternColor_", i)]] %||% "purple")),
          column(6, sliderInput(paste0("patternWeight_", i), "Line Weight:", min = 0.5, max = 5, value = customPatternValues[[paste0("patternWeight_", i)]] %||% 1, step = 0.1))
        ),
        hr()
      )
    })
    do.call(tagList, pattern_ui)
  })
  
  # Reactive expression to gather custom patterns
  customPatterns <- reactive({
    lapply(1:customPatternCount(), function(i) {
      list(
        from = input[[paste0("patternFrom_", i)]],
        to = input[[paste0("patternTo_", i)]],
        show = input[[paste0("showPattern_", i)]],
        color = input[[paste0("patternColor_", i)]],
        weight = input[[paste0("patternWeight_", i)]]
      )
    })
  })
  
  # Dynamically generate UI controls for identified web/chunk patterns
  output$webChunkResultsUI <- renderUI({
    patterns <- web_chunk_data()
    
    if (length(patterns) == 0) {
      return(tags$p("No chunks found with current settings.", style = "color: #FA8072; font-weight: bold; font-size: 0.85rem;"))
    }
    
    # 1. UI for when Global Styling is OFF (Individual controls)
    chunk_ui <- lapply(seq_along(patterns), function(i) {
      pattern <- patterns[[i]]
      move_range <- paste(min(pattern$nodes), "-", max(pattern$nodes))
      
      conditionalPanel(
        condition = "!input.globalChunkStyle",
        div(
          style = "background: #F6F7F9; padding: 12px; border-radius: 12px; margin-bottom: 12px; border: 1px solid #E7EAEE;",
          h6(paste("Chunk", i, ": Moves", move_range), style = "margin-top: 0; color: var(--cater-midnight); font-weight: 800; font-size: 0.85rem;"),
          p(paste("Density:", round(pattern$connection_percentage, 1), "%"), style = "font-size: 0.75rem; margin-bottom: 10px; color: #5B6770;"),
          colourpicker::colourInput(paste0("chunkColor_", i), "Colour", value = "green"),
          sliderInput(paste0("chunkWeight_", i), "Line Weight", min = 0.5, max = 5, value = 2, step = 0.1)
        )
      )
    })
    
    # 2. UI for when Global Styling is ON (Just a clean list summary)
    list_ui <- conditionalPanel(
      condition = "input.globalChunkStyle",
      tags$ul(
        style = "padding-left: 20px; font-size: 0.82rem; color: var(--cater-midnight); font-weight: 600;",
        lapply(seq_along(patterns), function(i) {
          pattern <- patterns[[i]]
          tags$li(
            paste0("Chunk ", i, ": Moves ", min(pattern$nodes), "-", max(pattern$nodes), 
                   " (", round(pattern$connection_percentage, 1), "%)")
          )
        })
      )
    )
    
    tagList(list_ui, do.call(tagList, chunk_ui))
  })
  
  # Dynamically generate UI controls for identified Sawtooth patterns
  output$sawtoothResultsUI <- renderUI({
    req(processed_data())
    patterns <- processed_data()$sawtooth_patterns_list
    
    if (length(patterns) == 0) {
      return(tags$p("No sawtooth patterns found.", style = "color: #FA8072; font-weight: bold; font-size: 0.85rem;"))
    }
    
    # 1. UI for when Global Styling is OFF (Individual controls)
    chunk_ui <- lapply(seq_along(patterns), function(i) {
      pattern <- patterns[[i]]
      move_range <- paste(min(pattern), "-", max(pattern))
      
      conditionalPanel(
        condition = "!input.globalSawtoothStyle",
        div(
          style = "background: #F6F7F9; padding: 12px; border-radius: 12px; margin-bottom: 12px; border: 1px solid #E7EAEE;",
          h6(paste("Sawtooth", i, ": Moves", move_range), style = "margin-top: 0; color: var(--cater-midnight); font-weight: 800; font-size: 0.85rem;"),
          colourpicker::colourInput(paste0("sawtoothColor_", i), "Colour", value = "red"),
          sliderInput(paste0("sawtoothWeight_", i), "Line Weight", min = 0.5, max = 5, value = 2, step = 0.1)
        )
      )
    })
    
    # 2. UI for when Global Styling is ON (Clean list summary)
    list_ui <- conditionalPanel(
      condition = "input.globalSawtoothStyle",
      tags$ul(
        style = "padding-left: 20px; font-size: 0.82rem; color: var(--cater-midnight); font-weight: 600;",
        lapply(seq_along(patterns), function(i) {
          pattern <- patterns[[i]]
          tags$li(
            paste0("Sawtooth ", i, ": Moves ", paste(pattern, collapse = ", "))
          )
        })
      )
    )
    
    tagList(list_ui, do.call(tagList, chunk_ui))
  })
  
  # Render the interactive plot
  plotToExport <- reactive({
    req(scaled_connections_df())
    
    moves_df <- processed_data()$moves_df
    connections_df <- scaled_connections_df()
    
    # ---> NEW: Clean Crop Logic <---
    crop_min <- input$plotCropRange[1]
    crop_max <- input$plotCropRange[2]
    
    # Safety check for app load
    if (is.null(crop_min)) crop_min <- 1
    if (is.null(crop_max)) crop_max <- max(moves_df$move, na.rm = TRUE)
    
    # Filter out any connections that cross the boundary
    connections_df <- connections_df %>%
      dplyr::filter(
        from >= crop_min & from <= crop_max,
        to >= crop_min & to <= crop_max
      )
    
    # Filter the nodes/labels
    moves_df <- moves_df %>%
      dplyr::filter(
        move >= crop_min & move <= crop_max
      )
    # ---> END NEW LOGIC <---
    
    uni_forward_moves <- processed_data()$uni_forward_moves
    uni_backward_moves <- processed_data()$uni_backward_moves
    bidirectional_moves <- processed_data()$bidirectional_moves
    
    filtered_moves_df <- moves_df %>%
      filter(row_number() %% input$moveDisplayFrequency == 0)
    
    eligible_archiograph <- !is.null(input$archiographColumn) &&
      input$archiographColumn != "No eligible columns" &&
      input$archiographColumn %in% names(processed_data()$archiograph_moves_df)
    
    if (isTRUE(input$autoYAxis)) {
      # 1. Base extreme points (y = 0 is the baseline for moves)
      base_min <- min(connections_df$mid_y_scaled, na.rm = TRUE)
      base_max <- 0
      
      # Safety check if there are no links at all
      if (is.infinite(base_min)) base_min <- -5
      
      # 2. Check Archiograph bounds (which swing upwards)
      if (isTRUE(input$showArchiograph) && eligible_archiograph) {
        max_arc <- max(abs(connections_df$mid_y_scaled_arc), na.rm = TRUE) * 1.5
        base_max <- max(base_max, max_arc)
        base_min <- min(base_min, -max_arc)
      }
      
      # 3. Calculate total vertical span to create proportional padding
      span <- base_max - base_min
      if (span == 0) span <- 10
      
      # Use 5% proportional padding instead of a hardcoded flat "2" units
      padding <- span * 0.05
      
      # 4. Add specific space for CM Notation ONLY if it is actually turned on
      top_padding <- padding
      if (isTRUE(input$showCmNotation)) {
        # Ensure there is a safe buffer for the text symbols above the line
        notation_height <- (input$cmNotationY %||% 0.6) + 2.5
        top_padding <- max(padding, notation_height)
      }
      
      # 5. Set the final dynamic limits
      plot_ylim <- c(base_min - padding, base_max + top_padding)
    } else {
      plot_ylim <- input$yAxisRange
    }
    
    # Calculate visual boundaries. Expand left side if CM Notation is on to fit the text.
    plot_xlim <- c(crop_min - 0.5, crop_max + 0.5)
    if (isTRUE(input$showCmNotation)) {
      # Safely expand left boundary so the slider-adjusted text never gets cut off
      offset <- input$cmLabelXOffset %||% -0.75
      plot_xlim[1] <- crop_min + offset - 1 
    }
    
    p <- ggplot() +
      
      # Base linkograph layer
      geom_segment(
        data = connections_df,
        aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled),
        colour = input$segmentColor,
        linewidth = input$segmentSize,
        lineend = "round"
      ) +
      geom_segment(
        data = connections_df,
        aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled),
        colour = input$segmentColor,
        linewidth = input$segmentSize,
        lineend = "round"
      ) +
    
    coord_fixed(
      ratio = 1,
      xlim = plot_xlim,  # <--- Uses the dynamically expanded boundaries
      ylim = plot_ylim,
      clip = "on"
    )
    
    # Unidirectional forward highlight
    if (isTRUE(input$showUniForward)) {
      p <- p +
        geom_segment(
          data = filter(connections_df, from %in% uni_forward_moves | to %in% uni_forward_moves),
          aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled),
          colour = input$uniForwardColor,
          linewidth = input$uniForwardWeight,
          lineend = "round"
        )
    }
    
    # Unidirectional backward highlight
    if (isTRUE(input$showUniBackward)) {
      p <- p +
        geom_segment(
          data = filter(connections_df, from %in% uni_backward_moves | to %in% uni_backward_moves),
          aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled),
          colour = input$uniBackwardColor,
          linewidth = input$uniBackwardWeight,
          lineend = "round"
        )
    }
    
    # Bidirectional highlight
    if (isTRUE(input$showBidirectional)) {
      p <- p +
        geom_segment(
          data = filter(connections_df, from %in% bidirectional_moves | to %in% bidirectional_moves),
          aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled),
          colour = input$bidirectionalColor,
          linewidth = input$bidirectionalWeight,
          lineend = "round"
        ) +
        geom_segment(
          data = filter(connections_df, from %in% bidirectional_moves | to %in% bidirectional_moves),
          aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled),
          colour = input$bidirectionalColor,
          linewidth = input$bidirectionalWeight,
          lineend = "round"
        )
    }
    
    # Sawtooth highlight
    if (isTRUE(input$showSawtooth)) {
      patterns_st <- processed_data()$sawtooth_patterns_list
      
      for (i in seq_along(patterns_st)) {
        pattern <- patterns_st[[i]]
        
        # Determine styling based on the toggle
        if (isTRUE(input$globalSawtoothStyle)) {
          c_color <- input$sawtoothGlobalColor %||% "red"
          c_weight <- input$sawtoothGlobalWeight %||% 2
        } else {
          c_color <- input[[paste0("sawtoothColor_", i)]] %||% "red"
          c_weight <- input[[paste0("sawtoothWeight_", i)]] %||% 2
        }
        
        # Filter connections specifically for the moves in this sawtooth chunk
        chunk_df <- connections_df %>%
          filter(from %in% pattern & to %in% pattern)
        
        if (nrow(chunk_df) > 0) {
          p <- p +
            geom_segment(
              data = chunk_df,
              aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled),
              colour = c_color,
              linewidth = c_weight,
              lineend = "round"
            ) +
            geom_segment(
              data = chunk_df,
              aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled),
              colour = c_color,
              linewidth = c_weight,
              lineend = "round"
            )
        }
      }
    }
    
    # Trace Forward Highlights
    if (isTRUE(input$showTraceForward) && length(input$traceForwardMoves) > 0) {
      # For a forward trace, we find future moves that link BACK to the selected move(s)
      trace_fwd_df <- connections_df %>%
        filter(to %in% as.numeric(input$traceForwardMoves))
      
      if (nrow(trace_fwd_df) > 0) {
        p <- p +
          geom_segment(
            data = trace_fwd_df,
            # Only draw the half-line emanating from the 'to' node up to the midpoint
            aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled),
            colour = input$traceForwardColor, 
            linewidth = input$traceForwardWeight, 
            lineend = "round"
          )
      }
    }
    
    # Trace Backward Highlights
    if (isTRUE(input$showTraceBackward) && length(input$traceBackwardMoves) > 0) {
      # For a backward trace, we find past moves that the selected move(s) link BACK to
      trace_bwd_df <- connections_df %>%
        filter(from %in% as.numeric(input$traceBackwardMoves))
      
      if (nrow(trace_bwd_df) > 0) {
        p <- p +
          geom_segment(
            data = trace_bwd_df,
            # Only draw the half-line emanating from the 'from' node up to the midpoint
            aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled),
            colour = input$traceBackwardColor, 
            linewidth = input$traceBackwardWeight, 
            lineend = "round"
          )
      }
    }
    
    # Custom pattern highlights
    for (pattern in customPatterns()) {
      if (isTRUE(pattern$show)) {
        pattern_moves <- seq(pattern$from, pattern$to)
        
        pattern_df <- connections_df %>%
          filter(from %in% pattern_moves & to %in% pattern_moves)
        
        p <- p +
          geom_segment(
            data = pattern_df,
            aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled),
            colour = pattern$color,
            linewidth = pattern$weight,
            lineend = "round"
          ) +
          geom_segment(
            data = pattern_df,
            aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled),
            colour = pattern$color,
            linewidth = pattern$weight,
            lineend = "round"
          )
      }
    }
    
    # Archiograph overlay
    if (isTRUE(input$showArchiograph) && eligible_archiograph) {
      archiograph_moves_df <- processed_data()$archiograph_moves_df
      color_map <- unlist(selected_colors())
      
      connections_df <- connections_df %>%
        left_join(
          archiograph_moves_df %>%
            select(move, from_factor = all_of(input$archiographColumn)),
          by = c("from" = "move")
        ) %>%
        left_join(
          archiograph_moves_df %>%
            select(move, to_factor = all_of(input$archiographColumn)),
          by = c("to" = "move")
        )
      
      p <- p +
        geom_curve(
          data = connections_df,
          aes(
            x = from_x,
            xend = mid_x,
            y = 0,
            yend = mid_y_scaled_arc * -1,
            colour = from_factor
          ),
          linewidth = input$archiographWeight,
          curvature = 0.25,
          alpha = 0.85
        ) +
        geom_curve(
          data = connections_df,
          aes(
            x = mid_x,
            xend = to_x,
            y = mid_y_scaled_arc * -1,
            yend = 0,
            colour = to_factor
          ),
          linewidth = input$archiographWeight,
          curvature = 0.25,
          alpha = 0.85
        ) +
        scale_colour_manual(values = color_map, na.value = "grey50")
    }
    
    # Web and chunk overlay
    if (isTRUE(input$showWebChunk) && input$runWebChunk > 0) {
      patterns_100 <- web_chunk_data()
      
      for (i in seq_along(patterns_100)) {
        pattern <- patterns_100[[i]]
        
        # Determine styling based on the toggle
        if (isTRUE(input$globalChunkStyle)) {
          c_color <- input$webChunkGlobalColor %||% "green"
          c_weight <- input$webChunkGlobalWeight %||% 2
        } else {
          c_color <- input[[paste0("chunkColor_", i)]] %||% "green"
          c_weight <- input[[paste0("chunkWeight_", i)]] %||% 2
        }
        
        chunk_df <- connections_df %>%
          filter(from %in% pattern$nodes & to %in% pattern$nodes)
        
        if (nrow(chunk_df) > 0) {
          p <- p +
            geom_segment(
              data = chunk_df,
              aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled),
              colour = c_color,
              linewidth = c_weight,
              lineend = "round"
            ) +
            geom_segment(
              data = chunk_df,
              aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled),
              colour = c_color,
              linewidth = c_weight,
              lineend = "round"
            )
        }
      }
    }
    
    # Move points
    p <- p +
      geom_point(
        data = moves_df,
        aes(x = x, y = y),
        size = input$pointSize,
        colour = input$pointColor
      )
    
    # Optional midpoint points
    if (isTRUE(input$showMidpoints)) {
      p <- p +
        geom_point(
          data = connections_df,
          aes(x = mid_x, y = mid_y_scaled),
          size = input$midPointSize,
          colour = input$midPointColor
        )
    }
    
    # ---> NEW: Critical Move Notation Layer (Calculated Manually for 100% Reliability) <---
    if (isTRUE(input$showCmNotation)) {
      cm_thresh <- input$selectedCmThreshold %||% 2
      
      # We use the FULL UNFILTERED connections list from pd to get accurate counts
      # regardless of how the visual crop is set.
      full_conn <- processed_data()$connections_df
      
      # Raw Math: Count Forward links (how many future moves link BACK to this move)
      fwd_counts <- full_conn %>% dplyr::count(to, name = "fwd_score")
      
      # Raw Math: Count Backward links (how many past moves this move links BACK to)
      bwd_counts <- full_conn %>% dplyr::count(from, name = "bwd_score")
      
      # Build a clean table of just the moves inside the crop window
      cm_symbols_df <- data.frame(plot_x = crop_min:crop_max) %>%
        dplyr::left_join(fwd_counts, by = c("plot_x" = "to")) %>%
        dplyr::left_join(bwd_counts, by = c("plot_x" = "from")) %>%
        # Replace NAs with 0 for moves with no connections
        dplyr::mutate(
          fwd_score = ifelse(is.na(fwd_score), 0, fwd_score),
          bwd_score = ifelse(is.na(bwd_score), 0, bwd_score)
        ) %>%
        # Apply the threshold to find Critical Moves
        dplyr::mutate(
          is_fwd = fwd_score >= cm_thresh,
          is_bwd = bwd_score >= cm_thresh,
          cm_symbol = dplyr::case_when(
            is_fwd & is_bwd ~ "<>",
            is_fwd ~ ">",
            is_bwd ~ "<",
            TRUE ~ NA_character_
          )
        ) %>%
        dplyr::filter(!is.na(cm_symbol))
      
      # Pull exact coordinates and size from your UI sliders
      sym_y <- input$cmNotationY %||% 0.6
      lbl_x <- crop_min + (input$cmLabelXOffset %||% -0.75)
      sym_size <- input$cmNotationSize %||% 4.5 # <--- NEW: Dynamic font size
      
      # 1. Draw the Threshold Label on the left
      label_df <- data.frame(x = lbl_x, y = sym_y, lbl = paste0("CM^", cm_thresh))
      
      p <- p + geom_text(
        data = label_df,
        aes(x = x, y = y, label = lbl),
        parse = TRUE, fontface = "bold", color = "#0A4F57", size = sym_size, hjust = 1 # <--- Updated to sym_size
      )
      
      # 2. Draw the <, >, <> symbols above the moves
      if (nrow(cm_symbols_df) > 0) {
        p <- p + geom_text(
          data = cm_symbols_df,
          aes(x = plot_x, y = sym_y, label = cm_symbol),
          fontface = "bold", size = sym_size, color = "#0A4F57", vjust = 0.5 # <--- Updated to sym_size
        )
      }
    }
    
    # Move labels and final theme
    p <- p +
      geom_text(
        data = filtered_moves_df,
        aes(
          x = x + input$textOffsetX,
          y = y + input$textOffsetY,
          label = move
        ),
        vjust = -1,
        size = input$textSize
      ) +
      scale_x_continuous(
        breaks = NULL,
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      theme_void() +
      theme(
        plot.margin = margin(20, 20, 20, 20),
        legend.position = "bottom"
      )
    
    p
  })
  
  
  output$interactivePlot <- renderPlot({
    plotToExport()  # This will render the plot in the UI
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("linkgraphy-plot-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".pdf")
    },
    content = function(file) {
      ggsave(file, 
             plot = plotToExport(),
             device = "pdf",
             width = input$pdfWidth, 
             height = input$pdfHeight, 
             units = "mm",
             scale = input$pdfScale)
    }
  )
  
  output$downloadPlotJPG <- downloadHandler(
    filename = function() {
      paste0("linkgraphy-plot-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".jpg")
    },
    content = function(file) {
      ggsave(file, 
             plot = plotToExport(), 
             device = "jpeg",
             width = input$pdfWidth, 
             height = input$pdfHeight, 
             units = "mm",
             scale = input$pdfScale,
             dpi = input$imageDpi,
             bg = "white")
    }
  )
}

# Function to identify sawtooth sequences
identify_sawtooth_patterns <- function(df) {
  preliminary_sequences <- list()
  
  # Identify preliminary sawtooth sequences
  for (i in 1:nrow(df)) {
    current_move <- df$move[i]
    connected_values <- sort(unlist(df$connected_values[i]))
    
    if (length(connected_values) == 2 && all(connected_values == c(current_move - 1, current_move + 1))) {
      # Additional checks for current_move - 1 and current_move + 1
      move_before <- current_move - 1
      move_after <- current_move + 1
      
      # Check if move_before is not connected to any move greater than current_move
      connections_before <- sort(unlist(df$connected_values[df$move == move_before]))
      if (any(connections_before > current_move)) {
        next
      }
      
      # Check if move_after is not connected to any move less than current_move
      connections_after <- sort(unlist(df$connected_values[df$move == move_after]))
      if (any(connections_after < current_move)) {
        next
      }
      
      # If all checks are passed, add the sequence
      sequence <- c(move_before, current_move, move_after)
      preliminary_sequences <- c(preliminary_sequences, list(sequence))
    }
  }
  
  # Function to check and merge sequences
  merge_sequences <- function(sequences) {
    merged <- list(sequences[[1]])
    for (i in 2:length(sequences)) {
      last_merged <- merged[[length(merged)]]
      current_seq <- sequences[[i]]
      
      # If current sequence starts before or exactly after the last sequence ends, merge them
      if (min(current_seq) <= max(last_merged) + 1) {
        merged[[length(merged)]] <- unique(c(last_merged, current_seq))
      } else {
        merged <- c(merged, list(current_seq))
      }
    }
    return(merged)
  }
  
  # Merge overlapping or consecutive sequences
  if (length(preliminary_sequences) > 1) {
    sawtooth_sequences <- merge_sequences(preliminary_sequences)
  } else {
    sawtooth_sequences <- preliminary_sequences
  }
  
  return(sawtooth_sequences)
}

# Function to identify patterns (Optimized Sliding Window Approach)
identify_patterns <- function(df, min_moves, max_moves, min_connection_percentage, max_connection_percentage = 100) {
  require(igraph)
  
  # Validate parameters
  if (!is.data.frame(df)) stop("Input df must be a data frame.")
  if (min_moves < 2) stop("min_moves must be at least 2.")
  if (max_moves < min_moves) stop("max_moves must be greater than or equal to min_moves.")
  if (!all(c("from", "to") %in% colnames(df))) stop("Data frame must contain 'from' and 'to' columns.")
  
  df <- df[, c("from", "to")] 
  g <- graph_from_data_frame(df, directed = FALSE)
  
  # Extract numeric nodes and sort them
  nodes <- sort(as.numeric(V(g)$name))
  
  identified_patterns <- list()
  
  # Use a sliding window instead of combinatorial generation
  for (size in min_moves:max_moves) {
    # Skip if there aren't enough nodes to form a chunk of this size
    if (length(nodes) < size) next
    
    # Slide a window of 'size' across the sorted nodes
    for (i in 1:(length(nodes) - size + 1)) {
      combo <- nodes[i:(i + size - 1)]
      
      # 1. The nodes must be strictly sequential (no gaps in the move numbers)
      if (!all(diff(combo) == 1)) next
      
      # 2. The first and last nodes in the sequence must be directly connected
      first_node <- combo[1]
      last_node <- combo[size]
      
      first_last_connected <- any(
        (df$from == first_node & df$to == last_node) | 
          (df$to == first_node & df$from == last_node)
      )
      
      if (!first_last_connected) next
      
      # 3. Check connection percentage of the induced subgraph
      subg <- induced_subgraph(g, as.character(combo))
      actual_connections <- gsize(subg)
      possible_connections <- size * (size - 1) / 2
      connection_percentage <- (actual_connections / possible_connections) * 100
      
      if (connection_percentage >= min_connection_percentage && connection_percentage <= max_connection_percentage) {
        identified_patterns[[length(identified_patterns) + 1]] <- list(
          nodes = combo, 
          connection_percentage = connection_percentage
        )
      }
    }
  }
  
  # If nothing was found, return an empty list early
  if (length(identified_patterns) == 0) return(list())
  
  # Filter out patterns that are subsets of larger valid patterns
  # Sort patterns by length descending to make the subset check highly efficient
  pattern_lengths <- sapply(identified_patterns, function(x) length(x$nodes))
  identified_patterns <- identified_patterns[order(pattern_lengths, decreasing = TRUE)]
  
  final_patterns <- list()
  
  for (i in seq_along(identified_patterns)) {
    current_nodes <- identified_patterns[[i]]$nodes
    
    is_subset <- FALSE
    for (fp in final_patterns) {
      if (all(current_nodes %in% fp$nodes)) {
        is_subset <- TRUE
        break
      }
    }
    
    if (!is_subset) {
      final_patterns[[length(final_patterns) + 1]] <- identified_patterns[[i]]
    }
  }
  
  return(final_patterns)
}

# Run the application 
shinyApp(ui = ui, server = server)