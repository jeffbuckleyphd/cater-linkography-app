library(shiny)
library(DT)
library(igraph)
library(tidyverse)
library(ggplot2)
library(readxl)
library(shinyjs)

# Define UI for the application
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  titlePanel("Linkography Web App"),
  
  tabsetPanel(
    tabPanel("Plot",  # First tab for the interactive plot
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload Excel File", accept = c(".xlsx")),
                 
                 # Display the Link Index after file upload
                 verbatimTextOutput("linkIndexOutput"),
                 
                 hr(),
                 
                 # Numeric inputs for PDF export settings
                 numericInput("pdfWidth", "PDF Width (in mm):", value = 297, min = 1),
                 numericInput("pdfHeight", "PDF Height (in mm):", value = 210, min = 1),
                 downloadButton("downloadPlot", "Download Plot as PDF"),
                 
                 hr(),  # A horizontal line to separate the sections
                 h4("Adjust Base Plot Properties"),  # A header for the base plot adjustments
                 
                 fluidRow(
                   column(12,
                          sliderInput("textOffsetX", "Adjust Text Horizontal Position:", min = -2, max = 2, value = 0, step = 0.01),
                          sliderInput("textOffsetY", "Adjust Text Vertical Position:", min = -2, max = 2, value = 0, step = 0.01),
                          sliderInput("textSize", "Adjust Text Size:", min = 1, max = 10, value = 3, step = 0.5),
                          sliderInput("moveDisplayFrequency", "Display Every nth Move Label:", min = 1, max = 10, value = 1, step = 1),
                          
                   ),
                   column(12,
                          colourpicker::colourInput("pointColor", "Select Move Color:", value = "black"),
                          sliderInput("pointSize", "Select Move Size:", min = 1, max = 10, value = 3, step = 0.5),
                   ),
                   column(12,
                          colourpicker::colourInput("midPointColor", "Select Link Color:", value = "blue"),
                          sliderInput("midPointSize", "Select Link Size:", min = 1, max = 10, value = 3, step = 0.5)
                   ),
                   column(12,
                          colourpicker::colourInput("segmentColor", "Select Line Color:", value = "black"),
                          sliderInput("segmentSize", "Select Line Size:", min = 0.5, max = 5, value = 1, step = 0.1)
                   ),
                   column(12,
                          sliderInput("yendScale", "Adjust Vertical Scaling:", min = 0, max = 2, value = 1, step = 0.01),  # Slider for scaling mid_y
                          sliderInput("yAxisRange", "Set Y-Axis Range:", min = -50, max = 50, value = c(-10, 10))
                   )
                 )
               ),
               
               mainPanel(
                 plotOutput("interactivePlot", height = "700px"),
                 
                 hr(),  # A horizontal line to separate sections
                 h4("Adjust Move Type Visibility"),  # A header for the move visibility adjustments
                 
                 fluidRow(
                   column(3,
                          checkboxInput("showUniForward", "Show Unidirectional Forward Lines", value = FALSE),
                          colourpicker::colourInput("uniForwardColor", "Unidirectional Forward Color:", value = "deeppink"),
                          sliderInput("uniForwardWeight", "Unidirectional Forward Line Weight:", min = 0.5, max = 3, value = 2, step = 0.1),
                          hr(),
                          checkboxInput("showUniBackward", "Show Unidirectional Backward Lines", value = FALSE),
                          colourpicker::colourInput("uniBackwardColor", "Unidirectional Backward Color:", value = "darkgoldenrod2"),
                          sliderInput("uniBackwardWeight", "Unidirectional Backward Line Weight:", min = 0.5, max = 3, value = 2, step = 0.1),
                          hr(),
                          checkboxInput("showSawtooth", "Show Sawtooth Pattern", value = FALSE),
                          colourpicker::colourInput("sawtoothColor", "Sawtooth Pattern Color:", value = "red"),
                          sliderInput("sawtoothWeight", "Sawtooth Line Weight:", min = 0.5, max = 3, value = 2, step = 0.1)
                   ),
                   column(3,
                          checkboxInput("showWebChunk", "Show Web and Chunk Patterns", value = FALSE),
                          actionButton("runWebChunk", "Run Web and Chunk Analysis"),
                          colourpicker::colourInput("webChunkLowColor", "Web and Chunk Low Color:", value = "green"),
                          colourpicker::colourInput("webChunkHighColor", "Web and Chunk High Color:", value = "green"),
                          sliderInput("webChunkWeight", "Web and Chunk Base Line Weight:", min = 0.5, max = 3, value = 2, step = 0.1),
                          numericInput("minMoves", "Minimum Moves:", value = 3, min = 2),
                          numericInput("maxMoves", "Maximum Moves:", value = 8, min = 3),
                          numericInput("minConnectionPercentage", "Minimum Connection Percentage:", value = 50, min = 0, max = 100),
                          numericInput("maxConnectionPercentage", "Maximum Connection Percentage:", value = 100, min = 0, max = 100)
                   ),
                   column(3,
                          checkboxInput("showArchiograph", "Show Archiograph", value = FALSE),
                          selectInput("archiographColumn", "Select Categorical Column for Archiograph:", choices = NULL),
                          verbatimTextOutput("numFactorsOutput"),  # Output to display number of unique factors
                          uiOutput("colorPickersUI"),  # Placeholder for dynamic color pickers
                          sliderInput("archiographWeight", "Archiograph Line Weight:", min = 0.5, max = 5, value = 1, step = 0.1),
                          sliderInput("yarcScale", "Adjust Archiograph Scaling:", min = 0, max = 5, value = 1, step = 0.01),  # Slider for scaling mid_y
                   ),
                   column(3,  # New column for custom patterns
                          h4("Custom Patterns"),
                          actionButton("addPattern", "Add New Pattern"),
                          uiOutput("customPatternsUI")
                   )
                 ),
                 
                 hidden(
                   fluidRow(
                     column(3,
                            checkboxInput("showBidirectional", "Show Bidirectional Lines", value = FALSE),
                            colourpicker::colourInput("bidirectionalColor", "Bidirectional Color:", value = "green"),
                            sliderInput("bidirectionalWeight", "Bidirectional Line Weight:", min = 0.5, max = 3, value = 2, step = 0.1)
                     )
                   )
                 )
               )
             )
    ),
    
    tabPanel("Link Span",  # Second tab for the link span table
             DTOutput("linkSpanTable")),
    
    tabPanel("Move Classification",  # Third tab for the move classification table
             DTOutput("moveClassificationTable")),
    
    tabPanel("Critical Moves",  # Fourth tab for the critical move classification table
             sidebarLayout(
               sidebarPanel(
                 
                 # Display the total number of moves
                 verbatimTextOutput("totalNumberMoves"),
                 
                 # Display critical move thresholds
                 
                 verbatimTextOutput("criticalForwardMoveA"),
                 verbatimTextOutput("criticalForwardMoveB"),
                 verbatimTextOutput("criticalForwardMoveC"),
                 verbatimTextOutput("criticalBackwardMoveA"),
                 verbatimTextOutput("criticalBackwardMoveB"),
                 verbatimTextOutput("criticalBackwardMoveC")
               ),
               mainPanel(
                 DTOutput("criticalMoveSummaryTable")
               )
             )
             )
  )
)




# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read and process the uploaded data
  processed_data <- reactive({
    req(input$file)
    mydata <- read_excel(input$file$datapath)
    
    ##### Create list of moves #####
    edge_list <- mydata %>%
      pivot_longer(cols = starts_with("..."), names_to = "connection", values_to = "to") %>%
      select(-connection) %>%
      filter(!is.na(to)) %>%
      rename(from = move)
    
    # Calculate the Link Index
    link_index <- nrow(edge_list) / nrow(mydata)
    
    # Calculate total number of moves
    
    total_moves <- nrow(mydata)
    
    # Create a moves_df with horizontal positions for linkograph (x and y axis positions)
    moves_df <- data.frame(move = unique(c(mydata$move)))
    moves_df <- moves_df %>% arrange(move) %>% mutate(x = 1:n(), y = 0) # Assign x positions and y=0 for all
    
    # Prepare the connections with geometric calculations
    connections_df <- edge_list %>%
      left_join(moves_df, by = c("from" = "move")) %>%
      rename(from_x = x) %>%
      left_join(moves_df, by = c("to" = "move")) %>%
      rename(to_x = x) %>%
      mutate(distance = abs(from_x - to_x), # Calculate horizontal distance
             mid_x = (from_x + to_x) / 2, # Calculate midpoint for x
             mid_y = -distance) # The negative sign ensures the projection is downward
    
    
    # Identify Unidirectional Forward Moves
    move_connections_df <- lapply(unique(c(moves_df$move)), function(move) {
      connected_moves <- unique(c(
        connections_df$from[connections_df$to == move],
        connections_df$to[connections_df$from == move]
      ))
      connected_moves <- setdiff(connected_moves, move)
      data.frame(move = move, connected_moves = I(list(connected_moves)))
    }) %>% bind_rows()
    
    move_connections_df <- move_connections_df %>%
      rowwise() %>%
      mutate(connected_values = list(unlist(connected_moves)))
    
    uni_forward_moves <- move_connections_df %>%
      rowwise() %>%
      filter(length(connected_values) > 0) %>%
      filter(all(connected_values > move)) %>%
      pull(move)
    
    # Identify Unidirectional Backward Moves
    uni_backward_moves <- move_connections_df %>%
      rowwise() %>%
      filter(length(connected_values) > 0) %>%
      filter(all(connected_values < move)) %>%
      pull(move)
    
    # Identify Bidirectional Moves
    bidirectional_moves <- move_connections_df %>%
      rowwise() %>%
      filter(length(connected_values) > 0) %>%
      filter(any(connected_values > move) & any(connected_values < move)) %>%
      pull(move)
    
    # Identify Orphan Moves
    orphan_moves <- move_connections_df %>%
      rowwise() %>%
      filter(length(connected_values) == 0) %>%
      pull(move)
    
    # Identify Critical Moves Type 1
    
    critical_moves_analysis_df <- move_connections_df
    critical_moves_analysis_df$category <- NA
    # Assign the categories based on the moves identified earlier
    critical_moves_analysis_df$category[critical_moves_analysis_df$move %in% uni_forward_moves] <- "Unidirectional Forward"
    critical_moves_analysis_df$category[critical_moves_analysis_df$move %in% uni_backward_moves] <- "Unidirectional Backward"
    critical_moves_analysis_df$category[critical_moves_analysis_df$move %in% bidirectional_moves] <- "Bidirectional"
    critical_moves_analysis_df$category[critical_moves_analysis_df$move %in% orphan_moves] <- "Orphan"
    
    critical_moves_analysis_df$total_connections <- lengths(critical_moves_analysis_df$connected_values)
    
    # Join with moves_df to get the category information
    critical_moves_analysis_df <- critical_moves_analysis_df %>%
      left_join(moves_df, by = "move")
    
    top_10_percent_cutoff_overall <- quantile(critical_moves_analysis_df$total_connections, 0.9)
    critical_moves_analysis_df$top_10_percent_overall <- critical_moves_analysis_df$total_connections >= top_10_percent_cutoff_overall
    
    top_11_percent_cutoff_overall <- quantile(critical_moves_analysis_df$total_connections, 0.89)
    critical_moves_analysis_df$top_11_percent_overall <- critical_moves_analysis_df$total_connections >= top_11_percent_cutoff_overall
    
    top_12_percent_cutoff_overall <- quantile(critical_moves_analysis_df$total_connections, 0.88)
    critical_moves_analysis_df$top_12_percent_overall <- critical_moves_analysis_df$total_connections >= top_12_percent_cutoff_overall
    
    # Filter the dataframe to get only the rows for "Unidirectional Forward" moves
    uni_forward_data <- critical_moves_analysis_df[critical_moves_analysis_df$category == "Unidirectional Forward", ]
    # Calculate the cutoffs for Unidirectional Forward
    top_10_percent_cutoff_uni_forward <- quantile(uni_forward_data$total_connections, 0.9)
    top_11_percent_cutoff_uni_forward <- quantile(uni_forward_data$total_connections, 0.89)
    top_12_percent_cutoff_uni_forward <- quantile(uni_forward_data$total_connections, 0.88)
    uni_forward_data$top_10_percent_category <- uni_forward_data$total_connections >= top_10_percent_cutoff_uni_forward
    uni_forward_data$top_11_percent_category <- uni_forward_data$total_connections >= top_11_percent_cutoff_uni_forward
    uni_forward_data$top_12_percent_category <- uni_forward_data$total_connections >= top_12_percent_cutoff_uni_forward
    
    # Filter the dataframe to get only the rows for "Unidirectional Backward" moves
    uni_backward_data <- critical_moves_analysis_df[critical_moves_analysis_df$category == "Unidirectional Backward", ]
    # Calculate the cutoffs for Unidirectional Backward
    top_10_percent_cutoff_uni_backward <- quantile(uni_backward_data$total_connections, 0.9)
    top_11_percent_cutoff_uni_backward <- quantile(uni_backward_data$total_connections, 0.89)
    top_12_percent_cutoff_uni_backward <- quantile(uni_backward_data$total_connections, 0.88)
    uni_backward_data$top_10_percent_category <- uni_backward_data$total_connections >= top_10_percent_cutoff_uni_backward
    uni_backward_data$top_11_percent_category <- uni_backward_data$total_connections >= top_11_percent_cutoff_uni_backward
    uni_backward_data$top_12_percent_category <- uni_backward_data$total_connections >= top_12_percent_cutoff_uni_backward
    
    # Filter the dataframe to get only the rows for "Bidirectional" moves
    bidirectional_data <- critical_moves_analysis_df[critical_moves_analysis_df$category == "Bidirectional", ]
    # Calculate the cutoffs for Bidirectional
    top_10_percent_cutoff_bidirectional <- quantile(bidirectional_data$total_connections, 0.9)
    top_11_percent_cutoff_bidirectional <- quantile(bidirectional_data$total_connections, 0.89)
    top_12_percent_cutoff_bidirectional <- quantile(bidirectional_data$total_connections, 0.88)
    # Create new columns indicating whether each move is within the top 10%, 11%, or 12%
    bidirectional_data$top_10_percent_category <- bidirectional_data$total_connections >= top_10_percent_cutoff_bidirectional
    bidirectional_data$top_11_percent_category <- bidirectional_data$total_connections >= top_11_percent_cutoff_bidirectional
    bidirectional_data$top_12_percent_category <- bidirectional_data$total_connections >= top_12_percent_cutoff_bidirectional
    
    # Filter the dataframe to get only the rows for "Orphan" moves
    orphan_data <- critical_moves_analysis_df[critical_moves_analysis_df$category == "Orphan", ]
    orphan_data$top_10_percent_category <- NA
    orphan_data$top_11_percent_category <- NA
    orphan_data$top_12_percent_category <- NA
    
    critical_moves_analysis_df <- rbind(uni_forward_data, uni_backward_data, bidirectional_data, orphan_data)
    
    critical_moves_analysis_df <- critical_moves_analysis_df[order(as.numeric(critical_moves_analysis_df$move)), ]

    # Critical moves Type 2
    
    critical_move_a <- floor(nrow(mydata) / 10)
    critical_move_b <- critical_move_a + 1
    critical_move_c <- critical_move_a + 2
    
    ## Real Critical Moves Table
    
    criticalMoveSummaryTable <- reactive({
      req(processed_data())
      
      connections_df <- processed_data()$connections_df
      all_moves <- processed_data()$moves_df$move
      
      # Get all links from both perspectives
      forward_df <- connections_df %>%
        filter(as.numeric(to) > as.numeric(from)) %>%
        select(move = from) %>%
        bind_rows(
          connections_df %>%
            filter(as.numeric(from) > as.numeric(to)) %>%
            select(move = to)
        )
      
      backward_df <- connections_df %>%
        filter(as.numeric(to) < as.numeric(from)) %>%
        select(move = from) %>%
        bind_rows(
          connections_df %>%
            filter(as.numeric(from) < as.numeric(to)) %>%
            select(move = to)
        )
      
      forward_counts <- forward_df %>%
        count(move, name = "Forward Links")
      
      backward_counts <- backward_df %>%
        count(move, name = "Backward Links")
      
      # Build summary table
      summary_df <- tibble(Move = all_moves) %>%
        left_join(forward_counts, by = c("Move" = "move")) %>%
        left_join(backward_counts, by = c("Move" = "move")) %>%
        mutate(
          `Forward Links` = replace_na(`Forward Links`, 0),
          `Backward Links` = replace_na(`Backward Links`, 0),
          
          # Critical Forward Moves
          `CM&gt;A` = ifelse(`Forward Links` >= processed_data()$critical_move_a, "Yes", "No"),
          `CM&gt;B` = ifelse(`Forward Links` >= processed_data()$critical_move_b, "Yes", "No"),
          `CM&gt;C` = ifelse(`Forward Links` >= processed_data()$critical_move_c, "Yes", "No"),
          
          # Critical Backward Moves
          `CM&lt;A` = ifelse(`Backward Links` >= processed_data()$critical_move_a, "Yes", "No"),
          `CM&lt;B` = ifelse(`Backward Links` >= processed_data()$critical_move_b, "Yes", "No"),
          `CM&lt;C` = ifelse(`Backward Links` >= processed_data()$critical_move_c, "Yes", "No")
        )
      
      summary_df
    })

    
    output$criticalMoveSummaryTable <- renderDT({
      datatable(
        criticalMoveSummaryTable(),
        options = list(
          pageLength = 10,
          dom = 'tip',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE,
        escape = FALSE
      )
    })
    
    
   
    
    
    
    
    ##### Sawtooth Identification (Runs Automatically) #####
    sawtooth_patterns <- identify_sawtooth_patterns(move_connections_df)
    sawtooth_sequence <- unlist(sawtooth_patterns)  # Assuming only one sequence for simplicity
    
    # Mark connections based on their role in the sawtooth pattern
    connections_df <- connections_df %>%
      mutate(
        sawtooth_role = case_when(
          from %in% sawtooth_sequence & to %in% sawtooth_sequence & from == min(sawtooth_sequence) ~ "Forward from First",
          from %in% sawtooth_sequence & to %in% sawtooth_sequence & to == max(sawtooth_sequence) ~ "Backward to Last",
          from %in% sawtooth_sequence & to %in% sawtooth_sequence ~ "Middle Moves",
          TRUE ~ "Not in Sawtooth"
        )
      )
    
    # Create the link span table
    link_span_moves_df <- connections_df %>% select(from, to, distance)
    unique_moves <- unique(c(mydata$move))
    distance_df <- expand.grid(from = unique_moves, to = unique_moves)
    distance_df <- distance_df %>%
      left_join(link_span_moves_df, by = c("from", "to"))
    
    distance_table <- distance_df %>%
      spread(key = to, value = distance)
    
    distance_table[is.na(distance_table)] <- ""
    colnames(distance_table)[which(names(distance_table) == "from")] <- "Move"
    
    # Create the move classification table
    move_classification <- critical_moves_analysis_df %>%
      mutate(
        `Move Type` = case_when(
          move %in% uni_forward_moves ~ "Unidirectional Forward",
          move %in% uni_backward_moves ~ "Unidirectional Backward",
          move %in% bidirectional_moves ~ "Bidirectional",
          move %in% orphan_moves ~ "Orphan",
          TRUE ~ "Neutral"
        )
      ) %>%
      select(Move = move,
             `Move Type`,
             `Top 10% Overall` = top_10_percent_overall,
             `Top 11% Overall` = top_11_percent_overall,
             `Top 12% Overall` = top_12_percent_overall,
             `Top 10% Category` = top_10_percent_category,
             `Top 11% Category` = top_11_percent_category,
             `Top 12% Category` = top_12_percent_category)
    
    archiograph_moves_df <- mydata
    
    # Return processed data frames and the link index as a list
    list(
      moves_df = moves_df, 
      connections_df = connections_df, 
      uni_forward_moves = uni_forward_moves, 
      uni_backward_moves = uni_backward_moves,
      bidirectional_moves = bidirectional_moves,
      sawtooth_sequence = sawtooth_sequence,
      link_index = link_index,
      total_moves = total_moves,
      critical_move_a = critical_move_a,
      critical_move_b = critical_move_b,
      critical_move_c = critical_move_c,
      distance_table = distance_table,
      move_classification = move_classification,
      edge_list = edge_list,
      archiograph_moves_df = archiograph_moves_df
    )
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
  
  # Render the data summary table
  output$dataSummary <- renderDT({
    req(processed_data())
    datatable(processed_data()$moves_df, options = list(pageLength = 10))
  })
  
  # Render the link span table on the Link Span tab
  output$linkSpanTable <- renderDT({
    req(processed_data())
    datatable(processed_data()$distance_table, rownames = FALSE, options = list(pageLength = nrow(processed_data()$distance_table))) %>%
      formatStyle(
        columns = names(processed_data()$distance_table),  # Apply to all columns
        textAlign = 'center',  # Center all text
        target = 'cell'  # Applies centering to the body cells
      ) %>%
      formatStyle(
        columns = names(processed_data()$distance_table),  # Apply to all columns
        textAlign = 'center',  # Center the headers
        target = 'row'  # Applies centering to the header row
      ) %>%
      formatStyle(
        'Move',  # Replace 'Move' with the actual name of your first column if different
        fontWeight = 'bold'  # Make the first column bold
      )
  })
  
  # Render the move classification table on the Move Classification tab
  output$moveClassificationTable <- renderDT({
    req(processed_data())
    datatable(processed_data()$move_classification, rownames = FALSE, options = list(pageLength = 10)) %>%
      formatStyle(
        columns = "Move",  # Center the 'Move' column
        textAlign = 'center',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "Move Type",  # Center the 'Move Type' column
        textAlign = 'center'
      )
  })
  
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
  
  
  
  
  
  
  
  # Render the interactive plot
  plotToExport <- reactive({
    req(scaled_connections_df())  # Use the reactive scaled connections_df
    
    # If no eligible columns, render a plot without the Archiograph layers
    if (input$archiographColumn == "No eligible columns") {
      
      # Extract processed data
      moves_df <- processed_data()$moves_df
      connections_df <- scaled_connections_df()  # Use the reactive scaled connections_df
      uni_forward_moves <- processed_data()$uni_forward_moves
      uni_backward_moves <- processed_data()$uni_backward_moves
      bidirectional_moves <- processed_data()$bidirectional_moves
      sawtooth_sequence <- processed_data()$sawtooth_sequence
      
      # Filter moves_df to select every nth move
      filtered_moves_df <- moves_df %>% filter(row_number() %% input$moveDisplayFrequency == 0)
      
      # Create the base plot using ggplot2
      p <- ggplot() +
        geom_segment(data = connections_df, aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$segmentColor, size = input$segmentSize) +
        geom_segment(data = connections_df, aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$segmentColor, size = input$segmentSize) +
        coord_cartesian(ylim = input$yAxisRange)  # Fix the Y-axis range here
      
      # Overlay the unidirectional forward lines if toggled on
      if (input$showUniForward) {
        p <- p + 
          geom_segment(data = filter(connections_df, from %in% uni_forward_moves | to %in% uni_forward_moves), 
                       aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = input$uniForwardColor, size = input$uniForwardWeight)
      }
      
      # Overlay the unidirectional backward lines if toggled on
      if (input$showUniBackward) {
        p <- p + 
          geom_segment(data = filter(connections_df, from %in% uni_backward_moves | to %in% uni_backward_moves), 
                       aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = input$uniBackwardColor, size = input$uniBackwardWeight)
      }
      
      # Overlay the bidirectional lines if toggled on
      if (input$showBidirectional) {
        p <- p + 
          geom_segment(data = filter(connections_df, from %in% bidirectional_moves | to %in% bidirectional_moves), 
                       aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = input$bidirectionalColor, size = input$bidirectionalWeight) +
          geom_segment(data = filter(connections_df, from %in% bidirectional_moves | to %in% bidirectional_moves), 
                       aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = input$bidirectionalColor, size = input$bidirectionalWeight)
      }
      
      # Overlay the sawtooth pattern if toggled on
      if (input$showSawtooth) {
        p <- p +
          geom_segment(data = connections_df %>% filter(sawtooth_role == "Middle Moves"), 
                       aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = input$sawtoothColor, size = input$sawtoothWeight) +
          geom_segment(data = connections_df %>% filter(sawtooth_role == "Middle Moves"), 
                       aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = input$sawtoothColor, size = input$sawtoothWeight)
      }
      
      # Overlay custom patterns
      for (pattern in customPatterns()) {
        if (pattern$show) {
          pattern_moves <- seq(pattern$from, pattern$to)
          p <- p +
            geom_segment(data = filter(connections_df, from %in% pattern_moves & to %in% pattern_moves), 
                         aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                         color = pattern$color, size = pattern$weight) +
            geom_segment(data = filter(connections_df, from %in% pattern_moves & to %in% pattern_moves), 
                         aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                         color = pattern$color, size = pattern$weight)
        }
      }
      
      # Overlay the web and chunk patterns if toggled on and the analysis is performed
      if (input$showWebChunk && input$runWebChunk > 0) {
        patterns_100 <- web_chunk_data()
        for (pattern in patterns_100) {
          connections_df$pattern_id <- NA
          connections_df$pattern_id[connections_df$from %in% pattern$nodes & connections_df$to %in% pattern$nodes] <- pattern$connection_percentage
          p <- p +
            geom_segment(data = filter(connections_df, !is.na(pattern_id)), 
                         aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled, size = pattern_id, color = pattern_id)) +
            geom_segment(data = filter(connections_df, !is.na(pattern_id)), 
                         aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled, size = pattern_id, color = pattern_id))
        }
        
        # Apply gradient color scaling without legend
        p <- p + scale_color_gradient(low = input$webChunkLowColor, high = input$webChunkHighColor, guide = "none") +
          scale_size_continuous(range = c(input$webChunkWeight * 0.5, input$webChunkWeight * 2), name = "Connection Percentage") +
          guides(color = "none")  # Remove the gradient from the legend
      }
      
      # Add points and text on top of the lines
      p <- p +
        geom_point(data = moves_df, aes(x = x, y = y), size = input$pointSize, color = input$pointColor) +
        geom_point(data = connections_df, aes(x = mid_x, y = mid_y_scaled), size = input$midPointSize, color = input$midPointColor) +
        geom_text(data = filtered_moves_df, aes(x = x + input$textOffsetX, y = y + input$textOffsetY, label = move), vjust = -1, size = input$textSize) +
        theme_minimal() +
        theme(axis.line = element_blank(),
              axis.text.x = element_text(hjust = 1),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_blank()) +
        scale_x_continuous(breaks = NULL)
      
      # Return the plot
      return(p)
    }
    
    req(selected_colors())  # Ensure selected_colors is valid
    
    # Extract processed data
    connections_df <- scaled_connections_df()
    archiograph_moves_df <- processed_data()$archiograph_moves_df
    
    # Map colors to 'from' and 'to' moves in connections_df
    from_colors <- sapply(connections_df$from, function(move) {
      factor <- archiograph_moves_df[[input$archiographColumn]][archiograph_moves_df$move == move]
      color <- selected_colors()[[factor]]
      if (is.null(color)) "black" else color  # Default to "black" if color is NULL
    })
    
    to_colors <- sapply(connections_df$to, function(move) {
      factor <- archiograph_moves_df[[input$archiographColumn]][archiograph_moves_df$move == move]
      color <- selected_colors()[[factor]]
      if (is.null(color)) "black" else color  # Default to "black" if color is NULL
    })
    

    # Extract processed data
    moves_df <- processed_data()$moves_df
    connections_df <- scaled_connections_df()  # Use the reactive scaled connections_df
    uni_forward_moves <- processed_data()$uni_forward_moves
    uni_backward_moves <- processed_data()$uni_backward_moves
    bidirectional_moves <- processed_data()$bidirectional_moves
    sawtooth_sequence <- processed_data()$sawtooth_sequence
    
    # Filter moves_df to select every nth move
    filtered_moves_df <- moves_df %>% filter(row_number() %% input$moveDisplayFrequency == 0)
    
    # Create the base plot using ggplot2
    p <- ggplot() +
      geom_segment(data = connections_df, aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                   color = input$segmentColor, size = input$segmentSize) +
      geom_segment(data = connections_df, aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                   color = input$segmentColor, size = input$segmentSize) +
      coord_cartesian(ylim = input$yAxisRange)  # Fix the Y-axis range here
    
    # Overlay the unidirectional forward lines if toggled on
    if (input$showUniForward) {
      p <- p + 
        geom_segment(data = filter(connections_df, from %in% uni_forward_moves | to %in% uni_forward_moves), 
                     aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$uniForwardColor, size = input$uniForwardWeight)
    }
    
    # Overlay the unidirectional backward lines if toggled on
    if (input$showUniBackward) {
      p <- p + 
        geom_segment(data = filter(connections_df, from %in% uni_backward_moves | to %in% uni_backward_moves), 
                     aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$uniBackwardColor, size = input$uniBackwardWeight)
    }
    
    # Overlay the bidirectional lines if toggled on
    if (input$showBidirectional) {
      p <- p + 
        geom_segment(data = filter(connections_df, from %in% bidirectional_moves | to %in% bidirectional_moves), 
                     aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$bidirectionalColor, size = input$bidirectionalWeight) +
        geom_segment(data = filter(connections_df, from %in% bidirectional_moves | to %in% bidirectional_moves), 
                     aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$bidirectionalColor, size = input$bidirectionalWeight)
    }
    
    # Overlay the sawtooth pattern if toggled on
    if (input$showSawtooth) {
      p <- p +
        geom_segment(data = connections_df %>% filter(sawtooth_role == "Middle Moves"), 
                     aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$sawtoothColor, size = input$sawtoothWeight) +
        geom_segment(data = connections_df %>% filter(sawtooth_role == "Middle Moves"), 
                     aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                     color = input$sawtoothColor, size = input$sawtoothWeight)
    }
    
    # Overlay custom patterns
    for (pattern in customPatterns()) {
      if (pattern$show) {
        pattern_moves <- seq(pattern$from, pattern$to)
        p <- p +
          geom_segment(data = filter(connections_df, from %in% pattern_moves & to %in% pattern_moves), 
                       aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = pattern$color, size = pattern$weight) +
          geom_segment(data = filter(connections_df, from %in% pattern_moves & to %in% pattern_moves), 
                       aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled), 
                       color = pattern$color, size = pattern$weight)
      }
    }
    
    # Overlay the archiograph if toggled on
    if (input$showArchiograph) {
      p <- p +
        geom_curve(data = connections_df, aes(x = from_x, xend = mid_x, y = 0, yend = (mid_y_scaled_arc * -1)), 
                   color = from_colors, size = input$archiographWeight, curvature = 0.3) +
        geom_curve(data = connections_df, aes(x = mid_x, xend = to_x, y = (mid_y_scaled_arc * -1), yend = 0), 
                   color = to_colors, size = input$archiographWeight, curvature = 0.3)
    }
    
    # Overlay the web and chunk patterns if toggled on and the analysis is performed
    if (input$showWebChunk && input$runWebChunk > 0) {
      patterns_100 <- web_chunk_data()
      for (pattern in patterns_100) {
        connections_df$pattern_id <- NA
        connections_df$pattern_id[connections_df$from %in% pattern$nodes & connections_df$to %in% pattern$nodes] <- pattern$connection_percentage
        p <- p +
          geom_segment(data = filter(connections_df, !is.na(pattern_id)), 
                       aes(x = from_x, xend = mid_x, y = 0, yend = mid_y_scaled, size = pattern_id, color = pattern_id)) +
          geom_segment(data = filter(connections_df, !is.na(pattern_id)), 
                       aes(x = to_x, xend = mid_x, y = 0, yend = mid_y_scaled, size = pattern_id, color = pattern_id))
      }
      
      # Apply gradient color scaling without legend
      p <- p + scale_color_gradient(low = input$webChunkLowColor, high = input$webChunkHighColor, guide = "none") +
        scale_size_continuous(range = c(input$webChunkWeight * 0.5, input$webChunkWeight * 2), name = "Connection Percentage") +
        guides(color = "none")  # Remove the gradient from the legend
    }
    
    # Add points and text on top of the lines
    p <- p +
      geom_point(data = moves_df, aes(x = x, y = y), size = input$pointSize, color = input$pointColor) +
      geom_point(data = connections_df, aes(x = mid_x, y = mid_y_scaled), size = input$midPointSize, color = input$midPointColor) +
      geom_text(data = filtered_moves_df, aes(x = x + input$textOffsetX, y = y + input$textOffsetY, label = move), vjust = -1, size = input$textSize) +
      theme_minimal() +
      theme(axis.line = element_blank(),
            axis.text.x = element_text(hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
      scale_x_continuous(breaks = NULL)
    
    # Return the plot
    return(p)
  })
  output$interactivePlot <- renderPlot({
    plotToExport()  # This will render the plot in the UI
  })
  
  output$interactivePlot <- renderPlot({
    plotToExport()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Open a PDF device with user-specified settings
      ggsave(file, 
             plot = plotToExport(), 
             device = cairo_pdf, 
             width = input$pdfWidth, 
             height = input$pdfHeight, 
             units = "mm")
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

# Function to identify patterns
identify_patterns <- function(df, min_moves, max_moves, min_connection_percentage,  max_connection_percentage = 100) {
  require(igraph)
  
  # Validate parameters
  if (!is.data.frame(df)) stop("Input df must be a data frame.")
  if (min_moves < 2) stop("min_moves must be at least 2.")
  if (max_moves < min_moves) stop("max_moves must be greater than or equal to min_moves.")
  
  # Ensure the df has 'from' and 'to' columns and create a new df with only these columns
  if (!all(c("from", "to") %in% colnames(df))) stop("Data frame must contain 'from' and 'to' columns.")
  df <- df[, c("from", "to")]  # Select only 'from' and 'to' columns
  
  # Convert edge list to an undirected graph, ensuring numerical order of vertices
  g <- graph_from_data_frame(df, directed = FALSE)
  V(g)$name <- as.character(V(g)$name) # Ensure vertex names are treated as characters for sorting
  
  identified_patterns <- list()
  
  for (size in min_moves:max_moves) {
    combos <- combn(as.numeric(V(g)$name), size, simplify = FALSE) # Work with numeric vertex names directly
    for (combo in combos) {
      # Sort combo to ensure ascending order, crucial for sequences involving 1
      combo <- sort(combo)
      
      # Check for sequential order (if needed, based on previous discussions)
      is_sequential <- all(diff(combo) == 1)
      
      if (is_sequential) { # Proceed if combo is sequentially ordered
        subg <- induced_subgraph(g, V(g)$name %in% combo)
        actual_connections <- gsize(subg)
        possible_connections <- size * (size - 1) / 2
        connection_percentage <- (actual_connections / possible_connections) * 100
        
        # Check if the first and last node are directly connected
        first_last_connected <- nrow(df[df$from %in% c(combo[1], combo[length(combo)]) & df$to %in% c(combo[length(combo)], combo[1]),]) > 0
        
        if (connection_percentage >= min_connection_percentage && connection_percentage <= max_connection_percentage && first_last_connected) {
          identified_patterns[[length(identified_patterns) + 1]] <- list(nodes = combo, connection_percentage = connection_percentage)
        }
      }
    }
  }
  
  # Filter out patterns that are subsets of larger patterns
  filtered_patterns <- lapply(identified_patterns, function(pattern) pattern$nodes)
  larger_patterns_only <- Filter(function(x) !any(sapply(filtered_patterns, function(y) all(x %in% y) && length(x) < length(y))), filtered_patterns)
  
  # Convert back to original structure if needed
  final_patterns <- lapply(larger_patterns_only, function(nodes) {
    connection_percentages <- sapply(identified_patterns, function(pattern) {
      if(all(nodes %in% pattern$nodes)) {
        return(pattern$connection_percentage)
      } else {
        return(NA)
      }
    })
    mean_connection_percentage <- mean(connection_percentages, na.rm = TRUE) # Use na.rm here
    
    list(nodes = nodes, connection_percentage = mean_connection_percentage)
  })
  
  return(final_patterns)

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
