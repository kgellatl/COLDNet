#' Shiny_net
#'
#' This function will take the visnetwork and start Shiny
#'
#' @param input the ggraph plot
#' @export
#' @details
#' #
#' @examples
#'

shiny_net <- function(input_net) {


  ###########

  server <- function(input, output) {
    output$network <- renderVisNetwork({input_net
        })

  }

  ###########

  ui <- fluidPage(
    visNetworkOutput("network")

  )

  ###########

  shinyApp(ui = ui, server = server)

}