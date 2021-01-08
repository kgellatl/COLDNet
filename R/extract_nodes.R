#' extract nodes
#'
#' This function will plot a ggraph object from an igraph object
#'
#' @param input_igraph the igraph object
#' @param node which node to extract
#' @param layout whether or not to recalculate the layout
#' @export
#' @details
#'
#' @examples
#' test_net <- extract_node(input_igraph = test_net, node = "ACTB", layout = "F")

extract_node <- function(input_igraph,
                          node,
                          layout = F){

  subset <- make_ego_graph(input_igraph, nodes = node)
  subset <- subset[[1]]

  if(layout){
    l <- layout_with_fr(subset)
    V(subset)$x_pos <- l[,1]
    V(subset)$y_pos <- l[,2]
  }

  return(subset)


}
