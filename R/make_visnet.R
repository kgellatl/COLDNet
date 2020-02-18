#' make_visnet
#'
#' This function will build a visnetwork object from and igraph object
#'
#' @param input_igraph the igraph object
#' @param edge_scale Default = 7. Scale factor for difference between igraph and visnet edge sizes.
#' @export
#' @details
#' Returns 3 objects in a list, [[1]] the visnetwork object, [[2]] the edges, [[3]] the nodes.
#' @examples
#' fun1_return <- make_visnet(input_igraph)

make_visnet <- function(input_igraph, edge_scale = 7){

  test_net_data_frame <- as_data_frame(input_igraph)
  nodes <- igraph::as_data_frame(input_igraph, what = "vertices")
  colnames(nodes)[1] <- "id"
  nodes$label <- nodes$id
  l <- as.matrix(nodes[,c('x_pos', "y_pos")])
  edges <- igraph::as_data_frame(input_igraph, what = "edges")
  edges$width <-  test_net_data_frame$width*edge_scale
  edges$color <- test_net_data_frame$color
  vis_obj <- visNetwork(nodes, edges) %>%
    visIgraphLayout("layout.norm", layoutMatrix = l) %>%
    visOptions(height = "500px", highlightNearest = TRUE,
               nodesIdSelection = list(enabled = TRUE))
  results_make_vis <- vector(mode = "list", length = 3)
  results_make_vis[[1]] <- vis_obj
  results_make_vis[[2]] <- edges
  results_make_vis[[3]] <- nodes

  return(results_make_vis)

}