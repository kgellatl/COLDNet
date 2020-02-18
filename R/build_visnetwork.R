#' build_visnetwork
#'
#' This function will build a visnetwork object from and igraph object.
#'
#' @param input_igraph the igraph object
#' @export
#' @details
#' #
#' @examples
#' build_visnetwork(input_igraph = test_net)

build_visnetwork <- function(input_igraph) {
  #######################################################

  fun1_return <- make_visnet(input_igraph)
  vis_obj <- fun1_return[[1]]
  edges <- fun1_return[[2]]
  nodes <- fun1_return[[3]]

  #######################################################

  ui <- shinyUI(fluidPage(
    fluidRow(
      column(width = 8,
             fluidRow(visNetworkOutput("network"), style = "height:500px")),
      column(width = 4,
             fluidRow(actionButton("gosel", "Make Subplot!"), style = "height:50px"),
             fluidRow(visNetworkOutput("sub_net"), style = "height:450px"))),
    column(width = 12,
           fluidRow(dataTableOutput("data_table"), style = "height:250px"))
  ))

  #######################################################

  server <- function(input, output, session) {
    output$network <- renderVisNetwork({
      vis_obj %>%
        visInteraction()
    })
    observe({
      input$gosel
      visNetworkProxy("network") %>% visGetSelectedNodes()
    })
    return_plot <- NULL
    observe({
      sel_node <- input$network_selectedNodes
      ind1 <- which(sel_node == edges$from)
      ind2 <- which(sel_node == edges$to)
      new_edges <- edges[c(ind1,ind2),]
      new_nodes <- unique(c(new_edges$from, new_edges$to))
      new_nodes <- as.data.frame(new_nodes)
      new_nodes$id <- new_nodes$new_nodes
      new_nodes$label <-  new_nodes$id
      return_plot <- visNetwork(new_nodes, new_edges)
      if(!is.null(return_plot)){
        output$sub_net <- renderVisNetwork({return_plot %>%
          visOptions(height = "450px")
          })
        output$data_table <- renderDataTable(new_edges)
      }
    })
  }

  #######################################################
  shinyApp(ui = ui, server = server)
  #######################################################

}
