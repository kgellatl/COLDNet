#' plot_ggraph
#'
#' This function will plot a ggraph object from an igraph object
#'
#' @param input_igraph the igraph object
#' @param color color of edges
#' @param width width of edges
#' @param col_lab label of color legend
#' @param width_lab label of width legend
#' @param scale "continuous" or "diverge"
#' @param render Whether or not a plot will be rendered
#' @param colpal a vector of length 2 for colors
#' @param label whether or not to label nodes
#' @param label_nodes subset of nodes to label
#' @export
#' @details
#'
#' @examples
#' test_net <- plot_ggraph(input_igraph = test_net, color = "rho", width = "log_p", scale = "diverge")

plot_ggraph <- function(input_igraph,
                        edge_color = NULL,
                        edge_colpal = c("blue", "red"),
                        edge_width = NULL,
                        node_color = "black",
                        node_size = 1,
                        edge_col_lab = NULL,
                        edge_width_lab = NULL,
                        scale = "diverge",
                        render = T,
                        label = F,
                        label_nodes = NULL){

  if(is.null(edge_col_lab)){
    edge_col_lab <- edge_color
  }
  if(is.null(edge_width_lab)){
    edge_width_lab <- edge_width
  }

  input_igraph <- test_net

  l <- matrix(c(V(input_igraph)$x_pos, V(input_igraph)$y_pos), ncol = 2)


  if(!is.null(edge_color)){
    E(input_igraph)$edge_color <- as_data_frame(input_igraph)[,edge_color]
  }

  if(!is.null(edge_width)){
    E(input_igraph)$edge_width <- as_data_frame(input_igraph)[,edge_width]
  }

  g <- ggraph(input_igraph, layout = l)
  g <-  g + geom_edge_link(aes(colour = edge_color, width = edge_width))

  g <- g + geom_node_point(color = node_color, size = node_size)

  if(scale == "diverge"){
    g <- g + scale_edge_colour_gradient2(name = edge_col_lab, low = edge_colpal[1], high = edge_colpal[2])
  }
  if(scale == "continuous"){
    g <- g + scale_edge_colour_gradient(name = edge_col_lab, low = edge_colpal[1], high = edge_colpal[2])
  }
  g <- g + scale_edge_width(name = edge_width_lab, range = c(0.2,1))

  if(label){
    if(is.null(label_nodes)){
      g <- g + geom_node_text(label = names(V(input_igraph)),
                              size = 2)
    } else {
      vertices <- names(V(input_igraph))
      ind <- match(label_nodes, vertices)
      new_labels <- rep("", length(vertices))
      new_labels[ind] <- label_nodes
      g <- g + geom_node_text(label = new_labels,
                              size = 2)
    }
  }

  g <- g + theme_void()


  if(render){
    plot(g)
  }

  input_igraph <- extract_ggraph(input_igraph, g)

  return(input_igraph)

}
