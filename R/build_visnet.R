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

build_visnetwork <- function(input_igraph, graph_params = NULL, edge_scale = 7) {
  #######################################################
  make_visnet <- function(input_igraph, graph_params, edge_scale){
    test_net_data_frame <- as_data_frame(input_igraph)
    nodes <- igraph::as_data_frame(input_igraph, what = "vertices")
    colnames(nodes) <- "id"
    nodes$label <- nodes$id
    edges <- igraph::as_data_frame(input_igraph, what = "edges")
    if(!is.null(graph_params)){
      edges$width <-  graph_params$edge_width*edge_scale
      edges$color <- graph_params$edge_color
      test_net_data_frame$edge_color <- edges$color
    }
    vis_obj <- visNetwork(nodes, edges) %>%
      visIgraphLayout("layout_with_fr") %>%
      visExport() %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE))
    results_make_vis <- vector(mode = "list", length = 3)
    results_make_vis[[1]] <- vis_obj
    results_make_vis[[2]] <- edges
    results_make_vis[[3]] <- nodes
    return(results_make_vis)
  }
  fun1_return <- make_visnet(input_igraph, graph_params, edge_scale)
  vis_obj <- fun1_return[[1]]
  edges <- fun1_return[[2]]
  nodes <- fun1_return[[3]]
  #######################################################

  ui = fluidPage(

    fluidRow(
      column(6, visNetworkOutput("network")),
      column(4, visNetworkOutput("sub_net"))),
    fluidRow(
      column(1, actionButton("gosel", "Make Subplot!")),
      column(11, dataTableOutput("data_table")))

  )

  #######################################################
  server <- function(input, output, session) {
    output$network <- renderVisNetwork({
      # minimal example
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
      # sel_node <- "STRN"
      ind1 <- which(sel_node == edges$from)
      ind2 <- which(sel_node == edges$to)

      new_edges <- edges[c(ind1,ind2),]
      new_edges <- new_edges[,c("from", "to", "edge_weight", "width", "color")]
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

# build_visnetwork(input_igraph = test_net, graph_params = plot_parameters)
