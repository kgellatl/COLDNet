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


  if(scale == "diverge"){

    col_vec <- rep("white", length(color_vals))
    neg <- which(color_vals < 0)
    pos <- which(color_vals > 0)
    neg_vals <- color_vals[neg]
    pos_vals <- color_vals[pos]

    if(is.null(pal)){
      col_vec <- RColorBrewer::brewer.pal(11, "RdBu")
    } else {
      col_vec <- RColorBrewer::brewer.pal(11, pal)
    }

    low_col <- col_vec[1:4]
    high_col <- col_vec[8:11]

    ###
    rr <- range(neg_vals)
    neg_vals_scale <- (neg_vals-rr[1])/diff(rr)
    f_low <- colorRamp(c(low_col))
    colors_low <- rgb(f_low(neg_vals_scale)/255)
    ###
    rr <- range(pos_vals)
    pos_vals_scale <- (pos_vals-rr[1])/diff(rr)
    f_high <- colorRamp(c(high_col))
    colors_high <- rgb(f_high(pos_vals_scale)/255)
    ###

    col_vec[neg] <- colors_low
    col_vec[pos] <- colors_high
    colors <- col_vec
  }

  if(scale == "continuous"){
    if(is.null(pal)){
      col_vec <- RColorBrewer::brewer.pal(8, "YlOrRd")
    } else {
      col_vec <- RColorBrewer::brewer.pal(8, pal)
    }
    f <- colorRamp(c(col_vec))

    color_vals
    rr <- range(color_vals)
    color_vals <- (color_vals-rr[1])/diff(rr)
    colors <- rgb(f(color_vals)/255)

  }


  E(input_igraph)$color <- colors

  return(input_igraph)

}
