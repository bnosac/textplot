
#' @rdname textplot_correlation_glasso
#' @export
textplot_correlation_glasso <- function(x, ...){
  UseMethod("textplot_correlation_glasso")
}

#' @rdname textplot_correlation_glasso
#' @title Plot sparse term correlations as a graph structure
#' @description Plot sparse term correlations as a graph structure.
#' Uses the glasso procedure (\code{glasso::glassopath}) to reduce the correlation matrix to retain only the
#' relevant correlations and next visualises these sparse correlations.
#' @param x a correlation matrix
#' @param n sample size used in computing the sparse correlation matrix. Defaults to 1000.
#' @param exclude_zero logical indicating to exclude zero-correlations from the graph
#' @param label.cex passed on to \code{qgraph::qgraph}
#' @param node.width passed on to \code{qgraph::qgraph}
#' @param ... further arguments passed on to \code{qgraph::qgraph},
#' except layout which is set to 'spring', labels (taken from the colnames of \code{x}),
#' and borders which is set to FALSE.
#' @return an object of class ggplot
#' @export
#' @examples
#' library(udpipe)
#' data(brussels_reviews_anno, package = 'udpipe')
#' x <- subset(brussels_reviews_anno, xpos %in% "NN" & language %in% "fr" & !is.na(lemma))
#' x <- document_term_frequencies(x, document = "doc_id", term = "lemma")
#' dtm <- document_term_matrix(x)
#' dtm <- dtm_remove_lowfreq(dtm, maxterms = 60)
#'
#' m <- dtm_cor(dtm)
#' textplot_correlation_glasso(m, exclude_zero = TRUE)
#' \donttest{
#' textplot_correlation_glasso(m, exclude_zero = FALSE)
#' }
textplot_correlation_glasso.default <- function(x, n = 1000, exclude_zero = TRUE, label.cex = 1, node.width = 0.5, ...){
  m <- EBICglasso(x, n = n)
  m <- m$optnet
  idx <- apply(m, MARGIN=1, FUN=function(x) sum(x == 1) > 1 | any(x > 0 & x < 1))
  if(exclude_zero){
    if(sum(idx) > 0){
      m <- m[idx, idx]
    }
  }
  requireNamespace("qgraph")
  qgraph::qgraph(m,
                 layout = "spring",
                 labels = colnames(m),
                 borders = FALSE,
                 label.scale = FALSE,
                 label.cex = label.cex,
                 node.width = node.width,
                 ...)
}
