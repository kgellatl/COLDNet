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

build_network <- function(input) {
  gene_net <- graph_from_data_frame(Results_from_test_matrix_2000)
  gene_net <- as.undirected(gene_net)

  E(gene_net)$rho <- Results_from_test_matrix_2000$rho
  E(gene_net)$pval <- Results_from_test_matrix_2000$p_Bonferroni

  gene_net
  names(V(gene_net))
  length(V(gene_net))
  length(E(gene_net))

  l <- layout_with_fr(gene_net)
}
