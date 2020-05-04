#' @name example_btm
#' @title Example Biterm Topic Model
#' @description The object is a BTM topic model created with the BTM package.
#' It was created on a subset of all CRAN packages, namely package which
#' are part of the NaturalLanguageProcessing and MachineLearning task views.\cr
#' Timepoint of creation was 2020-04-10.
#'
#' @docType data
#' @examples
#' library(BTM)
#' data(example_btm)
#' example_btm
#' str(example_btm)
NULL

#' @title Plot function for a BTM object
#' @description Plot biterms as a clustered graph.
#' The graph is constructed by assigning each word to a topic and within a topic of words biterm frequencies are shown.
#' @param x an object of class \code{\link[BTM]{BTM}} with a biterm topic model
#' @param biterms a data.frame with columns term1, term2, topic with all biterms and the topic these were assigned to. Defaults to the biterms used to construct the model.
#' @param top_n integer indicating to limit to displaying the top_n terms for each topic. Defaults to 7.
#' @param which integer vector indicating to display only these topics. See the examples.
#' @param labels a character vector of names. Should be of the same length as the number of topics in the data.
#' @param title character string with the title to use in the plot
#' @param subtitle character string with the subtitle to use in the plot
#' @param ... not used
#' @return an object of class ggplot
#' @export
#' @method plot BTM
#' @seealso \code{\link[BTM]{BTM}}, \code{\link{textplot_bitermclusters.default}}
#' @examples
#' library(igraph)
#' library(BTM)
#' library(ggraph)
#' data(example_btm, package = 'textplot')
#'
#' model <- example_btm
#' \donttest{
#' plot(model, title = "BTM model", top_n = 3)
#' plot(model, title = "BTM model", top_n = 3, labels = 1:model$K)
#' plot(model, title = "BTM model", which = 7:15)
#' plot(model, title = "BTM model", subtitle = "First 5 topics",
#'      which = 1:5, top_n = 10)
#' plot(model, title = "Biterm topic model", subtitle = "First 8 topics",
#'      which = 1:8, top_n = 7)
#' }
#'
#' topiclabels <- c("Garbage",
#'   "Data Mining", "Gradient descent", "API's",
#'   "Random Forests", "Stat models", "Text Mining / NLP",
#'   "GLM / GAM / Bayesian", "Machine learning", "Variable selection",
#'   "Regularisation techniques", "Optimisation", "Fuzzy logic",
#'   "Classification/Regression trees", "Text frequencies",
#'   "Neural / Deep learning", "Variable selection",
#'   "Text file handling", "Text matching", "Topic modelling")
#' plot(model, title = "Biterm topic model", subtitle = "some topics",
#'      top_n = 7,
#'      which = c(3, 4, 5, 6, 7, 9, 12, 16, 20),
#'      labels = topiclabels)
#'
#' \donttest{
#' library(BTM)
#' library(data.table)
#' library(udpipe)
#' ## Annotate text with parts of speech tags
#' data("brussels_reviews", package = "udpipe")
#' anno <- subset(brussels_reviews, language %in% "nl")
#' anno <- data.frame(doc_id = anno$id, text = anno$feedback, stringsAsFactors = FALSE)
#' anno <- udpipe(anno, "dutch", trace = 10)
#' ## Get cooccurrences of nouns / adjectives and proper nouns
#' biterms <- as.data.table(anno)
#' biterms <- biterms[, cooccurrence(x = lemma,
#'                                   relevant = upos %in% c("NOUN", "PROPN", "ADJ"),
#'                                   skipgram = 2),
#'                      by = list(doc_id)]
#' ## Build the BTM model
#' set.seed(123456)
#' x <- subset(anno, upos %in% c("NOUN", "PROPN", "ADJ"))
#' x <- x[, c("doc_id", "lemma")]
#' model <- BTM(x, k = 5, beta = 0.01, iter = 2000, background = TRUE,
#'              biterms = biterms, trace = 100)
#' plot(model)
#' }
plot.BTM <- function(x,
                     biterms = terms(x, type = "biterms")$biterms,
                     top_n = 7, which,
                     labels = seq_len(x$K),
                     title = "Biterm topic model", subtitle = list(), ...){
  requireNamespace("BTM")

  displayterms <- stats::terms(x, top_n = top_n)
  ok <- try({
    biterms <- force(biterms)
  }, silent = TRUE)
  if(inherits(ok, "try-error")){
    biterms <- x$biterms$biterms
  }
  if(!is.data.frame(biterms)){
    if(inherits(ok, "try-error")){
      warning(ok)
    }
    stop("Please provide in argument biterms a data.frame with columns term1, term2 and topic\nIf x is of class BTM and you are reloading a saved BTM model, you should have saved your biterms as well.")
  }
  if(missing(labels)){
    textplot_bitermclusters(x = displayterms, biterms = biterms, which = which, title = title, subtitle = subtitle, ...)
  }else{
    textplot_bitermclusters(x = displayterms, biterms = biterms, which = which, labels = labels, title = title, subtitle = subtitle, ...)
  }
}


#' @rdname textplot_bitermclusters
#' @export
textplot_bitermclusters <- function(x, ...){
  UseMethod("textplot_bitermclusters")
}

#' @rdname textplot_bitermclusters
#' @title Plot biterm cluster groups
#' @description Plot biterms as a clustered graph.
#' The graph is constructed by assigning each word to a topic and within a topic of words biterm frequencies are shown.
#' @param x a list of data.frames, each containing the columns token and probability corresponding to how good a token is emitted by a topic. The list index is assumed to be the topic number
#' @param biterms a data.frame with columns term1, term2, topic with all biterms and the topic these were assigned to
#' @param which integer vector indicating to display only these topics. See the examples.
#' @param labels a character vector of names. Should be of the same length as the number of topics in the data.
#' @param title character string with the title to use in the plot
#' @param subtitle character string with the subtitle to use in the plot
#' @param ... not used
#' @return an object of class ggplot
#' @export
#' @examples
#' library(igraph)
#' library(ggraph)
#' library(concaveman)
#' library(BTM)
#' data(example_btm, package = 'textplot')
#' group_terms   <- terms(example_btm, top_n = 3)
#' group_biterms <- example_btm$biterms$biterms
#'
#' \donttest{
#' textplot_bitermclusters(x = group_terms, biterms = group_biterms)
#' textplot_bitermclusters(x = group_terms, biterms = group_biterms,
#'                         title = "BTM model", subtitle = "Topics 7-15",
#'                         which = 7:15, labels = seq_len(example_btm$K))
#'
#' group_terms   <- terms(example_btm, top_n = 10)
#' textplot_bitermclusters(x = group_terms, biterms = group_biterms,
#'                         title = "BTM model", subtitle = "Topics 1-5",
#'                         which = 1:5, labels = seq_len(example_btm$K))
#' }
#' group_terms   <- terms(example_btm, top_n = 7)
#' topiclabels <- c("Garbage",
#'   "Data Mining", "Gradient descent", "API's",
#'   "Random Forests", "Stat models", "Text Mining / NLP",
#'   "GLM / GAM / Bayesian", "Machine learning", "Variable selection",
#'   "Regularisation techniques", "Optimisation", "Fuzzy logic",
#'   "Classification/Regression trees", "Text frequencies",
#'   "Neural / Deep learning", "Variable selection",
#'   "Text file handling", "Text matching", "Topic modelling")
#' textplot_bitermclusters(x = group_terms, biterms = group_biterms,
#'                         title = "Biterm topic model", subtitle = "some topics",
#'                         which = c(3, 4, 5, 6, 7, 9, 12, 16, 20),
#'                         labels = topiclabels)
textplot_bitermclusters.default <- function(x, biterms,
                                            which, labels = seq_len(length(table(biterms$topic))),
                                            title = "Biterm topic model", subtitle = list(), ...){
  requireNamespace("ggraph")
  requireNamespace("ggforce")
  requireNamespace("concaveman")
  requireNamespace("ggplot2")
  requireNamespace("igraph")
  donotdrawlabels <- missing(labels)

  ## We are only going to display biterms part of each topic which are using the top_n most emitted terms for each topic
  ## Assign each term to a topic for the time being
  if(inherits(x, "data.frame")){
    displayterms <- data.table::copy(x)
  }else if(is.list(x)){
    displayterms <- data.table::rbindlist(x, idcol = "topic")
    displayterms <- data.table::setDF(displayterms)
  }
  ## R CMD check happy
  topic <- .N <- term1 <- term2 <- select <- best_topic <- cooc <- name <- x <- y <- probability <- NULL

  if(!missing(which)){
    displayterms <- displayterms[displayterms$topic %in% which, ]
  }
  displayterms <- displayterms[base::order(displayterms$probability, decreasing = TRUE), ]
  displayterms <- displayterms[!base::duplicated(displayterms$token), ]

  labels <- force(labels)
  ## Get most occuring topic for each biterm
  biterms <- data.table::copy(biterms)
  biterms <- data.table::setDT(biterms)
  if(!missing(which)){
    biterms <- biterms[biterms$topic %in% which, ]
  }
  biterms <- biterms[, list(best_topic = utils::head(base::names(base::sort(base::table(topic), decreasing = TRUE)), 1),
                            cooc = .N), by = list(term1, term2)]

  biterms <- biterms[biterms$term1 %in% displayterms$token & biterms$term2 %in% displayterms$token, ]
  biterms <- biterms[base::order(biterms$cooc, biterms$best_topic, decreasing = TRUE), ]
  biterms <- biterms[, select := seq_len(.N), by = list(best_topic)]

  tt <- base::split(displayterms, displayterms$topic)
  biterms <- base::split(biterms, biterms$best_topic)
  biterms <- base::lapply(base::intersect(names(tt), names(biterms)), FUN = function(i){
    ## Only keep links within biterms topics!!!! really important to have 1 graph
    topictokens <- tt[[i]]
    topictokens <- as.character(topictokens$token)
    bi <- biterms[[i]]
    bi <- bi[bi$term1 %in% topictokens & bi$term2 %in% topictokens, ]
    bi <- bi[bi$term1 != bi$term2, ]
    bi
  })
  biterms <- data.table::rbindlist(biterms)

  nodes <- displayterms[displayterms$token %in% c(biterms$term1, biterms$term2), c("token", "topic", "probability")]
  nodes <- nodes[base::order(nodes$topic, nodes$token), ]
  nodes$topic        <- base::factor(nodes$topic, levels = seq_len(length(labels)), labels = labels)
  biterms$best_topic <- base::factor(biterms$best_topic, levels = seq_len(length(labels)), labels = labels)

  g <- igraph::graph_from_data_frame(biterms, vertices = nodes, directed = FALSE)
  g <- ggraph::ggraph(g, layout = 'igraph', algorithm = "fr") +
    ggraph::geom_edge_link0(ggplot2::aes(edge_alpha = cooc, edge_width = cooc, edge_colour = best_topic)) +
    ggraph::geom_node_text(ggplot2::aes(label = name, size = probability), col = "black") +
    ggplot2::theme_void() + ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = title, subtitle = subtitle)
  if(donotdrawlabels){
    g + ggforce::geom_mark_hull(ggplot2::aes(x, y, group = topic, fill = topic), concavity = 4, expand = ggplot2::unit(5, "mm"), alpha = 0.25)
  }else{
    g + ggforce::geom_mark_hull(ggplot2::aes(x, y, group = topic, fill = topic, label = topic), concavity = 4, expand = ggplot2::unit(5, "mm"), alpha = 0.25)
  }
}
