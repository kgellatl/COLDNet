#' build_visnetwork
#'
#' This function will build a visnetwork object from and igraph object.
#'
#' @param input_igraph the igraph object
#' @export
#' @details
#' #
#' @examples
#'

build_vis_subnetwork <- function(input_igraph) {
  #######################################################

  fun1_return <- make_visnet(input_igraph)
  vis_obj <- fun1_return[[1]]
  edges <- fun1_return[[2]]
  nodes <- fun1_return[[3]]
  # x_pos <- V(input_igraph)$x_pos
  # y_pos <- V(input_igraph)$y_pos
  # l_full <- matrix(c(x_pos, y_pos), ncol=2)

  #######################################################

  ui <- fluidPage(
    fluidRow(column(width = 6, textInput("gene_search", "Query Gene", nodes$id[1]), style = "height:75px"),
             column(width = 6, sliderInput("degree_opt", "Node Degree", 1, 3, 1))),
    fluidRow(visNetworkOutput("sub_net"), style = "height:500px"),
    fluidRow(dataTableOutput("data_table"))
    )

  #######################################################

  server <- function(input, output) {
    observe({
      if(input$gene_search %in% nodes$id){
        degree_sel <- input$degree_opt
        gene_search <- input$gene_search
        keep_edges <- as_data_frame(make_ego_graph(input_igraph, order = degree_sel, nodes = gene_search)[[1]])
        new_edges <- keep_edges
        new_nodes <- unique(c(new_edges$from, new_edges$to))
        new_nodes <- as.data.frame(new_nodes)
        new_nodes$id <- new_nodes$new_nodes
        new_nodes$label <-  new_nodes$id
        vis_obj2 <- visNetwork(new_nodes, new_edges)
        return_plot <- vis_obj2
#         ind <- match(new_nodes$id, names(V(input_igraph)))
#         l <- l_full[ind,]
        if(!is.null(return_plot)){
          output$sub_net <- renderVisNetwork({return_plot %>%
              visOptions(height = "500px") %>%
              visIgraphLayout("layout_nicely")
          })
          output$data_table <- renderDataTable(new_edges)
        }
      }
    })
  }

  #######################################################
  shinyApp(ui = ui, server = server)
  #######################################################
}
