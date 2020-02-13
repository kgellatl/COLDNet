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

plot_ggraph <- function(input, color, width, col_lab = NULL, width_lab = NULL, scale = "diverge", extract = T){

  if(is.null(col_lab)){
    col_lab <- color
  }

  if(is.null(width_lab)){
    width_lab <- width
  }


  E(input)$edge_color <- as_data_frame(input)[,color]
  E(input)$edge_weight <- as_data_frame(input)[,width]

  l <- matrix(c(V(input)$x_pos, V(input)$y_pos), ncol = 2)

  g <- ggraph(input, layout = l)
    g <-  g + geom_edge_link(aes(colour = edge_color, width = edge_weight))
    g <- g + geom_node_point()
    if(scale == "diverge"){
      g <- g + scale_edge_colour_gradient2(name = col_lab)
    }
  if(scale == "continuous"){
    g <- g + scale_edge_colour_gradient(name = col_lab, low = "blue", high = "red")
  }
  g <- g + scale_edge_width(name = width_lab, range = c(0.3,5))
    g <- g + geom_node_text(label = names(V(input)), size = 2)
    g <- g + theme_void()

    plot(g)

    if(extract){
      input <- extract_ggraph(input, g)
    }

    return(input)

}
