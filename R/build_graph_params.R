#' plot_ggraph
#'
#' This function will assign color and width values to your igraph network
#'
#' @param input_igraph the igraph object
#' @param color color of edges
#' @param width width of edges
#' @param scale "continuous" or "diverge"
#' @param pal Rcolorbrewer pal name. Pick appropriately for diverging or continuous data
#' @export
#' @details
#' #
#' @examples
#'
build_graph_params <- function(input_igraph, color, width, scale = "diverge", pal = NULL){

  ############### WIDTH #################
  LinMap <- function(x, from, to) {
    # Shifting the vector so that min(x) == 0
    x <- x - min(x)
    # Scaling to the range of [0, 1]
    x <- x / max(x)
    # Scaling to the needed amplitude
    x <- x * (to - from)
    # Shifting to the needed level
    x + from
  }
  width_vals <- as_data_frame(input_igraph)[,width]
  E(input_igraph)$width <- LinMap(x = width_vals, from = 0.3, to = 5)

  ############### COLOR #################


  color_vals <- as_data_frame(input_igraph)[,color]
  rr <- range(color_vals)
  color_vals <- (color_vals-rr[1])/diff(rr)


  if(scale == "diverge"){
    if(is.null(pal)){
      col_vec <- RColorBrewer::brewer.pal(11, "RdBu")
    } else {
      col_vec <- RColorBrewer::brewer.pal(8, pal)
    }
  }

  if(scale == "continuous"){
    if(is.null(pal)){
      col_vec <- RColorBrewer::brewer.pal(8, "YlOrRd")
    } else {
      col_vec <- RColorBrewer::brewer.pal(8, pal)
    }
  }

  f <- colorRamp(c(col_vec))
  colors <- rgb(f(color_vals)/255)
  E(input_igraph)$color <- colors

  return(input_igraph)

}
