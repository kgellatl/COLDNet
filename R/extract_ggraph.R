#' extract_ggraph
#'
#' This function will extract plotting parameters from the ggraph object
#'
#' @param input the ggraph plot
#' @export
#' @details
#' #
#' @examples
#'

extract_ggraph <- function(input) {

  build <- ggplot_build(input)
  build_dat <- build$data[[1]]

  build_dat$group <- as.character(build_dat$group)
  groups <- unique(build_dat$group)

  graph_parameters <- matrix(ncol = 2, nrow = length(groups))

  for (i in 1:length(groups)) {
    int_group <- groups[i]
    ind <- which(build_dat$group == int_group)

    graph_parameters[i,1] <- build_dat$edge_colour[ind][1]
    graph_parameters[i,2] <- build_dat$edge_width[ind][1]
  }

  colnames(graph_parameters) <- c("edge_color", "edge_width")
  graph_parameters <- as.data.frame(graph_parameters)
  graph_parameters[,1] <- as.character(graph_parameters[,1])
  graph_parameters[,2] <- as.numeric(as.character(graph_parameters[,2]))
  return(graph_parameters)
}
