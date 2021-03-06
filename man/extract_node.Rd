% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_nodes.R
\name{extract_node}
\alias{extract_node}
\title{extract nodes}
\usage{
extract_node(input_igraph, node, layout = F)
}
\arguments{
\item{input_igraph}{the igraph object}

\item{node}{which node to extract}

\item{layout}{whether or not to recalculate the layout}
}
\description{
This function will plot a ggraph object from an igraph object
}
\details{

}
\examples{
test_net <- extract_node(input_igraph = test_net, node = "ACTB", layout = "F")
}
