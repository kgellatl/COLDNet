#' build_visnetwork
#'
#' This function will build a visnetwork object
#'
#' @param input_igraph the ggraph plot
#' @export
#' @details
#' #
#' @examples
#'

build_visnetwork <- function(input_igraph) {
  #######################################################
  fun1_return <- make_visnet(input_igraph)
  vis_obj <- fun1_return[[1]]
  edges <- fun1_return[[2]]
  nodes <- fun1_return[[3]]
  #######################################################

  ui = fluidPage(
    # https://stackoverflow.com/questions/25340847/control-the-height-in-fluidrow-in-r-shiny
    fluidRow(
      column(8, visNetworkOutput("network")),
      column(4, visNetworkOutput("sub_net"))),
    fluidRow(
      column(2, actionButton("gosel", "Make Subplot!")),
      column(10, dataTableOutput("data_table")))

  )

  #######################################################
  server <- function(input, output, session) {
    output$network <- renderVisNetwork({
      # minimal example
      vis_obj %>%

        #
        visInteraction()
    })
    observe({
      input$gosel
      visNetworkProxy("network") %>% visGetSelectedNodes()
    })
    return_plot <- NULL
    observe({
      sel_node <- input$network_selectedNodes
      # sel_node <- "STRN"
      ind1 <- which(sel_node == edges$from)
      ind2 <- which(sel_node == edges$to)

      new_edges <- edges[c(ind1,ind2),]
      new_nodes <- unique(c(new_edges$from, new_edges$to))
      new_nodes <- as.data.frame(new_nodes)
      new_nodes$id <- new_nodes$new_nodes
      new_nodes$label <-  new_nodes$id

      vis_obj2 <- visNetwork(new_nodes, new_edges)

      return_plot <- vis_obj2
      if(!is.null(return_plot)){
        output$sub_net <- renderVisNetwork(return_plot)
        output$data_table <- renderDataTable(new_edges)
      }
    })
  }
  #######################################################
  shinyApp(ui = ui, server = server)
  #######################################################
}
