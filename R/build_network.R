#' build_network
#'
#' This function will build a igraph network from a dataframe and calculate the layout.
#'
#' @param input the input dataframe
#' @export
#' @details
#' #
#' @examples
#'

build_network <- function(input, from, to, color, weight) {

  gene_net <- graph_from_data_frame(input)
  gene_net <- as.undirected(gene_net)

  E(gene_net)$edge_color <- input[, color]
  E(gene_net)$edge_weight <- input[, weight]

  return(gene_net)


}
