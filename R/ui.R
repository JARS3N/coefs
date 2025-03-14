#' Shiny UI layout
#' @export
ui <- function() {
  shinyUI(
    fluidPage(
      titlePanel("Retrieve Barcode Coefficients"),
      
      fluidRow(
        # Left Column - Main Selection and Output
        column(6,
               selectInput(
                 "Lot",
                 "Select Lot",
                 c('SELECT'),  # Default option
                 selected = FALSE,
                 multiple = FALSE
               ),
               p("Start typing the Lot in the dropdown to filter choices."),
               tableOutput('Lottable'),
               tableOutput('oxtable'),
               tableOutput('pHtable'),
               tableOutput('bftbl'),
               
               # Checkbox & Table for Matching Lots
               checkboxInput("show_matching_lots", "Show Lots with same coefficients", value = FALSE),
               tableOutput('matching_lots_table')
        ),
        
        # Right Column - Filtering Options
        column(6,
               h4("Filters"),
               selectInput(
                 "Type", "Filter by Type (Optional)", 
                 choices = c("All", "W", "B", "C"), 
                 selected = "All"
               ),
               textInput(
                 "Year", "Filter by Last 2 Digits of Lot Number (Year) (Optional)", 
                 placeholder = "Enter last 2 digits (e.g., 24 for 2024)"
               ),
               selectInput(
                 "YearFilter", "Year Filter Type",
                 choices = c("=", "≥", "≤"),
                 selected = "="
               ),
               checkboxInput("include_experimental", "Include Experimental Lots", value = FALSE)
        )
      )
    )
  )
}

