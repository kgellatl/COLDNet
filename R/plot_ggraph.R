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
#' @export
#' @details
#' #
#' @examples
#'

plot_ggraph <- function(input_igraph, color, width, col_lab = NULL, width_lab = NULL, scale = "diverge", extract = T){

  if(is.null(col_lab)){
    col_lab <- color
  }

  if(is.null(width_lab)){
    width_lab <- width
  }


  E(input_igraph)$edge_color <- as_data_frame(input_igraph)[,color]
  E(input_igraph)$edge_weight <- as_data_frame(input_igraph)[,width]

  l <- matrix(c(V(input_igraph)$x_pos, V(input_igraph)$y_pos), ncol = 2)

  g <- ggraph(input_igraph, layout = l)
    g <-  g + geom_edge_link(aes(colour = edge_color, width = edge_weight))
    g <- g + geom_node_point()
    if(scale == "diverge"){
      g <- g + scale_edge_colour_gradient2(name = col_lab)
    }
  if(scale == "continuous"){
    g <- g + scale_edge_colour_gradient(name = col_lab, low = "blue", high = "red")
  }
  g <- g + scale_edge_width(name = width_lab, range = c(0.3,5))
    g <- g + geom_node_text(label = names(V(input_igraph)), size = 2)
    g <- g + theme_void()

    plot(g)

    if(extract){
      input_igraph <- extract_ggraph(input_igraph, g)
    }

    return(input_igraph)

}
