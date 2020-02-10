#' build_visnetwork
#'
#' This function will build a visnetwork object
#'
#' @param input the ggraph plot
#' @export
#' @details
#' #
#' @examples
#'

build_visnetwork <- function(input, graph_params = NA, edge_scale = 7) {

  nodes <- igraph::as_data_frame(input, what = "vertices")
  colnames(nodes) <- "id"
  nodes$label <- nodes$id
  edges <- igraph::as_data_frame(input, what = "edges")

  if(!all(is.na(graph_params))){

    edges$width <-  graph_params$edge_width*edge_scale
    edges$color <- graph_params$edge_color

  }

  vis_obj <- visNetwork(nodes, edges, width = "100%", height = "1000px") %>%
    visIgraphLayout("layout_with_fr")

  return(vis_obj)

}
