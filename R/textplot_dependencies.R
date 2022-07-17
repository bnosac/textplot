
#' @rdname textplot_dependencyparser
#' @export
textplot_dependencyparser <- function(x, ...){
  UseMethod("textplot_dependencyparser")
}


#' @rdname textplot_dependencyparser
#' @title Plot output of a dependency parser
#' @description Plot output of a dependency parser.
#' This plot takes one sentence and shows for the sentence,
#' the words, the parts of speech tag and the dependency relationship between the words.
#' @param x a data.frame as returned by a call to \code{\link[udpipe]{udpipe}} containing 1 sentence
#' @param title character string with the title to use in the plot
#' @param subtitle character string with the title to use in the plot
#' @param vertex_color character with the color of the label of each node. Defaults to darkgreen.
#' @param edge_color character with the color of the edges between the nodes. Defaults to red.
#' @param size size of the labels in the plot. Defaults to 3.
#' @param base_family character passed on to \code{theme_void} setting the base font family
#' @param layout the type of layout, defaults to 'linear', passed on to \code{\link[ggraph]{ggraph}}
#' @param ... not used yet
#' @return an object of class ggplot
#' @seealso \code{\link[udpipe]{udpipe}}
#' @export
#' @examples
#' \dontshow{
#' if(require(udpipe) && require(ggraph) && require(ggplot2) && require(igraph))
#' \{
#' }
#' library(udpipe)
#' library(ggraph)
#' library(ggplot2)
#' library(igraph)
#' \donttest{
#' x <- udpipe("The economy is weak but the outlook is bright", "english")
#' textplot_dependencyparser(x)
#'
#' x <- udpipe("His speech about marshmallows in New York is utter bullshit", "english")
#' textplot_dependencyparser(x, size = 4)
#'
#' x <- udpipe("UDPipe provides tokenization, tagging, lemmatization and
#'              dependency parsing of raw text", "english")
#' textplot_dependencyparser(x, size = 4)
#' }
#'
#' data("example_udpipe", package = "textplot")
#' textplot_dependencyparser(example_udpipe, size = 4)
#'
#' \dontshow{
#' \}
#' # End of main if statement running only if the required packages are installed
#' }
textplot_dependencyparser.default <- function(x,
                                              title = "Dependency Parser",
                                              subtitle = "tokenisation, parts of speech tagging & dependency relations",
                                              vertex_color = "darkgreen",
                                              edge_color = "red",
                                              size = 3,
                                              base_family = "",
                                              layout = "linear",
                                              ...){
  stopifnot(is.data.frame(x) & all(c("sentence_id", "token_id", "head_token_id", "dep_rel",
                                     "token_id", "token", "upos") %in% colnames(x)))

  ## R CMD check happy
  dep_rel <- token <- upos <- NULL
  requireNamespace("ggraph")
  requireNamespace("ggplot2")
  requireNamespace("igraph")

  x <- x[!is.na(x$head_token_id), ]
  x <- x[x$sentence_id %in% min(x$sentence_id), ]
  edges <- x[x$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
  edges$label <- edges$dep_rel
  g <- igraph::graph_from_data_frame(edges,
                                     vertices = x[, c("token_id", "token", "upos")],
                                     directed = TRUE)

  ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                  arrow = grid::arrow(length = ggplot2::unit(4, 'mm'), ends = "last", type = "closed"),
                  end_cap = ggraph::label_rect("wordswordswords"),
                  label_colour = edge_color, check_overlap = TRUE, label_size = size) +
    ggraph::geom_node_label(ggplot2::aes(label = token), col = vertex_color, size = size, fontface = "bold") +
    ggraph::geom_node_text(ggplot2::aes(label = upos), nudge_y = -0.35, size = size) +
    ggraph::theme_graph(base_family = base_family) +
    ggplot2::labs(title = title, subtitle = subtitle)
}




#' @name example_udpipe
#' @title Example annotation of text using udpipe
#' @description The object is a data.frame of the annotation of the text:
#' "UDPipe provides tokenization, tagging, lemmatization and dependency parsing of raw text"
#'
#' @docType data
#' @examples
#' data(example_udpipe)
#' str(example_udpipe)
NULL
