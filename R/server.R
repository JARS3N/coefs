#' Shiny server logic
#' @export
server <- function(input, output, session) {
  lotstuff <- coefs::lots()  # Fetch lot data

  # Ensure updateSelectInput() runs only when session is ready
  observe({
    shiny::updateSelectInput(session, 'Lot', choices = lotstuff$LotNumber)
  })

  # Observe selection of a lot
  observeEvent(input$Lot, {
    if (!is.null(input$Lot) && input$Lot != 'SELECT') {
      BMID <- lotstuff$BMID[lotstuff$LotNumber == input$Lot]
      info <- coefs::fetch(BMID)

      output$Lottable <- shiny::renderTable(data.frame(Lot = input$Lot))
      output$oxtable <- shiny::renderTable(
        dplyr::select(info, dplyr::contains('PH')) %>%
          dplyr::mutate(PH_A = as.character(round(PH_A, 0))),
        digits = 6
      )
      output$pHtable <- shiny::renderTable(
        dplyr::select(info, dplyr::contains('O2')) %>%
          dplyr::mutate(O2_A = as.character(round(O2_A, 0))),
        digits = 6
      )
      output$bftbl <- shiny::renderTable(
        if (info$BF == 0) {
          data.frame(Cartridge_BufferFactor = NA)
        } else {
          dplyr::select(info, Cartridge_BufferFactor = BF)
        },
        digits = 6
      )
    }
  })
}
