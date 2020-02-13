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

  E(gene_net)$edge_color <- input[, color]
  E(gene_net)$edge_weight <- input[, weight]

  l <- layout_nicely(gene_net, weights = E(gene_net)$edge_weight)


  V(gene_net)$x_pos <- l[,1]
  V(gene_net)$y_pos <- l[,2]

  g <- ggraph(gene_net, layout = l) +
    geom_edge_link(aes(colour = edge_color, width = edge_weight)) +
    geom_node_point() +
    scale_edge_colour_gradient2(name = "Correlation") +
    scale_edge_width(name = "Absolute Correlation", range = c(0.5,3)) +
    geom_node_text(label = names(V(gene_net)), size = 2) +
    theme_void()
  plot(g)

  gene_net <- extract_ggraph(gene_net, g)
  return(gene_net)


}
