#' Shiny UI layout
#' @export
ui <- function() {
  shiny::shinyUI(
    shiny::fluidPage(
      shiny::titlePanel("Retrieve Barcode Coefficients"),
      shiny::selectInput(
        "Lot",
        "Select Lot",
        c('SELECT'),
        selected = FALSE,
        multiple = FALSE
      ),
      shiny::p("Start typing the Lot in the dropdown to filter choices."),
      shiny::tableOutput('Lottable'),
      shiny::tableOutput('oxtable'),
      shiny::tableOutput('pHtable'),
      shiny::tableOutput('bftbl')
    )
  )
}
