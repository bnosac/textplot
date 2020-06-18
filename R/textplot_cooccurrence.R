
#' @rdname textplot_cooccurrence
#' @export
textplot_cooccurrence <- function(x, ...){
  UseMethod("textplot_cooccurrence")
}

#' @rdname textplot_cooccurrence
#' @title Plot term cooccurrences as a network
#' @description Plot term cooccurrences in a graph structure
#' @param x a data.frame with columns term1, term2 and cooc indicating how many times 2 terms are occurring together
#' @param terms a character vector with terms to only plot. Prevails compared to using \code{top_n}
#' @param top_n integer indicating to show only the top n occurrences as in \code{head(x, n = top_n)}
#' @param title character string with the title to use in the plot
#' @param subtitle character string with the subtitle to use in the plot
#' @param vertex_color character with the color of the label of each node. Defaults to darkgreen.
#' @param edge_color character with the color of the edges between the nodes. Defaults to grey.
#' @param base_family character passed on to \code{theme_void} setting the base font family
#' @param ... other parameters passed on to \code{ggraph::geom_node_text}
#' @return an object of class ggplot
#' @export
#' @examples
#' \dontshow{
#' if(require(udpipe) && require(igraph) && require(ggraph) && require(ggplot2))
#' \{
#' }
#' library(udpipe)
#' library(igraph)
#' library(ggraph)
#' library(ggplot2)
#' data(brussels_reviews_anno, package = 'udpipe')
#' x <- subset(brussels_reviews_anno, xpos %in% "JJ" & language %in% "fr")
#' x <- cooccurrence(x, group = "doc_id", term = "lemma")
#'
#' textplot_cooccurrence(x, top_n = 25, subtitle = "showing only top 25")
#' textplot_cooccurrence(x, top_n = 25, title = "Adjectives",
#'                       vertex_color = "orange", edge_color = "black",
#'                       fontface = "bold")
#'
#' \dontshow{
#' \}
#' # End of main if statement running only if the required packages are installed
#' }
textplot_cooccurrence.default <- function(x, terms, top_n = 50,
                                          title = "Term cooccurrences", subtitle = list(),
                                          vertex_color = "darkgreen",
                                          edge_color = "grey",
                                          base_family = "", ...){
  ## R CMD check happy
  cooc <- name <- NULL
  requireNamespace("ggraph")
  requireNamespace("ggplot2")
  requireNamespace("igraph")

  stopifnot(is.data.frame(x))
  stopifnot(all(c("term1", "term2", "cooc") %in% colnames(x)))

  if(missing(terms)){
    x <- head(x, n = top_n)
  }else{
    x <- x[x$term1 %in% terms & x$term2 %in% terms, ]
    if(!missing(top_n)){
      x <- head(x, n = top_n)
    }
  }
  x <- igraph::graph_from_data_frame(x)
  ggraph::ggraph(x, layout = "fr") +
    ggraph::geom_edge_link0(ggplot2::aes(edge_alpha = cooc, edge_width = cooc), edge_color = edge_color) +
    ggraph::geom_node_text(ggplot2::aes(label = name), col = vertex_color, ...) +
    ggraph::theme_graph(base_family = base_family) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme(legend.position = "none")
}
