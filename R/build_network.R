#' build_network
#'
#' This function will build a igraph network from a dataframe and calculate the layout.
#'
#' @param input the input dataframe
#' @export
#' @details
#' #
#' @examples
#' test_net <- build_network(input = test_net_dat)

build_network <- function(input) {
  gene_net <- graph_from_data_frame(input)
  l <- layout_nicely(gene_net)
  V(gene_net)$x_pos <- l[,1]
  V(gene_net)$y_pos <- l[,2]

  return(gene_net)

}
