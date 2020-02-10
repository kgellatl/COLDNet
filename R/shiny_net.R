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

shiny_net <- function(input) {

  server <- function(input, output) {
    output$network <- renderVisNetwork({vis_obj %>%

        visEvents(click = "function(nodes){
                Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
        )

        })

    output$shiny_return <- renderPrint({
      visNetworkProxy("network") %>%
        visNearestNodes(target = input$click, addDist = F)
    })

  }

  ui <- fluidPage(
    visNetworkOutput("network")#,
    #visNetworkOutput("shiny_return")
  )

  shinyApp(ui = ui, server = server)

}