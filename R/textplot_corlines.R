#' @title Document/Term Correlation Plot graphical attributes
#' @description Document/Term Correlation Plot graphical attributes
#' @param fontsize size of the font. Defaults to 25
#' @return a list with graph visualisation elements used by \code{\link{textplot_correlation_lines}}
#' @export
#' @examples
#' textplot_correlation_lines_attrs()
textplot_correlation_lines_attrs <- function(fontsize = 25){
  attrs <- Rgraphviz::getDefaultAttrs()
  attrs$graph$rankdir <- "LR"
  attrs$node$shape <- "plaintext"
  attrs$node$fixedsize <- FALSE
  attrs$node$fontsize <- fontsize
  attrs$edge$color <- "steelblue"
  #attrs$edge$color <- rgb(red=58, green=96, blue=168, maxColorValue = 255)
  attrs$edge$color <- "#3A60A8"
  attrs$edge$fontcolor <- "darkred"
  attrs
}

sparse_cor <- function (x) {
  n <- nrow(x)
  covmat <- (as.matrix(Matrix::crossprod(x)) - n * Matrix::tcrossprod(Matrix::colMeans(x)))/(n - 1)
  cormat <- covmat/Matrix::tcrossprod(sqrt(Matrix::diag(covmat)))
  cormat
}

#' @title Document/Term Correlation Plot
#' @description Plots the highest occurring correlations among terms. \cr
#' This is done by plotting the terms into nodes and the correlations between the terms as lines between the nodes.
#' Lines of the edges are proportional to the correlation height.
#' This uses the plot function for graphNEL objects (using the Rgraphviz package)
#' @param x a document-term matrix of class dgCMatrix
#' @param terms a character vector with terms present in the columns of \code{x} indicating terms to focus on
#' @param threshold a threshold to show only correlations between the terms with absolute values above this threshold. Defaults to 0.05.
#' @param top_n an integer indicating to show only the top top_n correlations. This can be set to plot only the top correlations. E.g. set it to 20 to show only the top 20 correlations
#' with the highest absolute value.
#' @param attrs a list of attributes with graph visualisation elements passed on to the plot function of an object of class graphNEL.
#' Defaults to \code{\link{textplot_correlation_lines_attrs}}.
#' @param terms_highlight a vector of character \code{terms} to highlight or a vector of numeric values in the 0-1 range indicating
#' how much (in percentage) to increase the node font size. See the examples.
#' @param label logical indicating to draw the label with the correlation size between the nodes
#' @param cex.label cex of the label of the correlation size
#' @param col.highlight color to use for highlighted terms specified in \code{terms_highlight}. Defaults to red.
#' @param lwd numeric value - graphical parameter used to increase the edge thickness which indicates the correlation strength. Defaults to 1.
#' @param ... other arguments passed on to plot
#' @return invisibly the plot
#' @export
#' @examples
#' ## Construct document/frequency/matrix
#' library(Rgraphviz)
#' library(udpipe)
#' data(brussels_reviews_anno, package = 'udpipe')
#' exclude <- c(32337682L, 27210436L, 26820445L, 37658826L, 33661134L, 48756422L,
#'   23454554L, 30461127L, 23292176L, 32850277L, 30566303L, 21595142L,
#'   20441279L, 38097066L, 28651065L, 29011387L, 37316020L, 22135291L,
#'   40169379L, 38627667L, 29470172L, 24071827L, 40478869L, 36825304L,
#'   21597085L, 21427658L, 7890178L, 32322472L, 39874379L, 32581310L,
#'   43865675L, 31586937L, 32454912L, 34861703L, 31403168L, 35997324L,
#'   29002317L, 33546304L, 47677695L)
#' dtm <- brussels_reviews_anno
#' dtm <- subset(dtm, !doc_id %in% exclude)
#' dtm <- subset(dtm, xpos %in% c("NN") & language == "nl" & !is.na(lemma))
#' dtm <- document_term_frequencies(dtm, document = "doc_id", term = "lemma")
#' dtm <- document_term_matrix(dtm)
#' dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
#' dtm <- dtm_remove_tfidf(dtm, top = 500)
#'
#' ## Plot top 20 correlations, having at least a correlation of 0.01
#' textplot_correlation_lines(dtm, top_n = 25, threshold = 0.01)
#'
#' ## Plot top 20 correlations
#' textplot_correlation_lines(dtm, top_n = 25, label = TRUE, lwd = 5)
#'
#' ## Plot top 20 correlations and highlight some terms
#' textplot_correlation_lines(dtm, top_n = 25, label = TRUE, lwd = 5,
#'                            terms_highlight = c("prijs", "privacy"),
#'                            main = "Top correlations in topic xyz")
#'
#' ## Plot top 20 correlations and highlight + increase some terms
#' textplot_correlation_lines(dtm, top_n = 25, label = TRUE, lwd=5,
#'                            terms_highlight = c(prijs = 0.8, privacy = 0.1),
#'                            col.highlight = "red")
#'
#' ## Plot correlations between specific terms
#' w <- dtm_colsums(dtm)
#' w <- head(sort(w, decreasing = TRUE), 100)
#' textplot_correlation_lines(dtm, terms = names(w), top_n = 20, label = TRUE)
#'
#' attrs <- textplot_correlation_lines_attrs()
#' attrs$node$shape <- "rectangle"
#' attrs$edge$color <- "steelblue"
#' textplot_correlation_lines(dtm, top_n = 20, label = TRUE,
#'                            attrs = attrs)
textplot_correlation_lines <- function(x,
                                       terms = colnames(x),
                                       threshold = 0.05,
                                       top_n,
                                       attrs = textplot_correlation_lines_attrs(),
                                       terms_highlight,
                                       label = FALSE,
                                       cex.label = 1,
                                       col.highlight = "red",
                                       lwd = 1, ...){
  requireNamespace("graph")
  requireNamespace("Rgraphviz")

  terms <- terms[terms %in% colnames(x)]
  if(inherits(x, "dgCMatrix")){
    cormat <- sparse_cor(x[, terms, drop = FALSE])
  }else{
    cormat <- stats::cor(as.matrix(x[, terms, drop = FALSE]))
  }
  if(!missing(top_n)){
    threshold <- cormat
    threshold[lower.tri(cormat, diag=TRUE)] <- NA
    threshold <- as.vector(abs(threshold))
    threshold <- threshold[!is.na(threshold)]
    threshold <- sort(threshold, decreasing=TRUE)
    threshold <- threshold[min(length(threshold), top_n)]
  }
  cormat[abs(cormat) < threshold] <- 0
  cormat[is.na(cormat)] <- 0
  diag(cormat) <- 0
  keep <- apply(cormat, 1, max)
  keep <- names(keep)[abs(keep) >= threshold]
  if(!missing(top_n)){
    keep <- head(keep, n = top_n)
  }
  if(!missing(terms_highlight)){
    if(is.numeric(terms_highlight)){
      terms_highlight <- terms_highlight[names(terms_highlight) %in% terms]
      keep <- unique(c(keep, names(terms_highlight)))
    }else if(is.character(terms_highlight)){
      terms_highlight <- intersect(terms_highlight, terms)
      keep <- unique(c(keep, terms_highlight))
    }
  }
  cormat <- cormat[keep, keep]
  g <- as(abs(cormat), "graphNEL")

  p <- Rgraphviz::layoutGraph(g)
  labels <- round(cormat[lower.tri(cormat) & abs(cormat) >= threshold], 2)
  names(labels) <- graph::edgeNames(p)

  graphplotter <- methods::getMethod("plot", signature = c("graph", "ANY"), where = getNamespace("Rgraphviz"))
  if(!missing(terms_highlight)){
    nAttrs <- list()
    if(is.numeric(terms_highlight)){
      nAttrs$fontsize <- attrs$node$fontsize + (attrs$node$fontsize * terms_highlight)
      terms_highlight[seq_along(terms_highlight)] <- col.highlight
      nAttrs$fontcolor <- terms_highlight
    }else if(is.character(terms_highlight)){
      names(terms_highlight) <- terms_highlight
      terms_highlight[seq_along(terms_highlight)] <- col.highlight
      nAttrs$fontcolor <- terms_highlight
    }
    p <- graphplotter(g, edgeAttrs=list(label=labels), nodeAttrs = nAttrs, attrs=attrs, ...)
  }else{
    p <- graphplotter(g, edgeAttrs=list(label=labels), attrs=attrs, ...)
  }


  lw <- round(cormat[lower.tri(cormat) & abs(cormat) >= threshold] * lwd)
  i <- 1
  for (ae in Rgraphviz::AgEdge(p)) {
    Rgraphviz::lines(ae, drawlabel=label, cex.label=cex.label, lwd = lw[i], len = 1)
    i <- i + 1
  }
  invisible(p)
}



setMethod("lines", "AgEdge", function(x, drawlabel=FALSE, cex.label=1, ..., len, lty=graphics::par("lty"), lwd=graphics::par("lwd")) {
  z <- Rgraphviz::splines(x)
  edgeColor <- Rgraphviz::color(x)
  if (edgeColor == "") edgeColor <- "black"

  arrowSize <- Rgraphviz::arrowsize(x)
  if ( arrowSize == "" ) arrowSize = "1"

  if(length(x@lty)>0) lty=x@lty[1]
  if(length(x@lwd)>0) lwd=x@lwd[1]

  len <- len * as.numeric(arrowSize)
  mapply(lines, z, MoreArgs=list(len=len, col=edgeColor, lty=lty, lwd=lwd, ...))

  if ( x@dir == "both" || x@dir== "back" ) {
    tails = Rgraphviz::bezierPoints(z[[1]])
    tail_from = tails[2, ]
    tail_to   = tails[1, ]
    graphics::arrows(tail_from[1], tail_from[2], tail_to[1], tail_to[2], col=edgeColor, length=len, lty=lty, lwd=lwd)
  }
  if ( x@dir == "both" || x@dir == "forward" ) {
    heads = Rgraphviz::bezierPoints(z[[length(z)]])
    head_from = heads[nrow(heads)-1, ]
    head_to   = heads[nrow(heads),]
    graphics::arrows(head_from[1], head_from[2], head_to[1], head_to[2], col=edgeColor, length=len, lty=lty, lwd=lwd)
  }
  if(drawlabel){
    drawEdgeTxtLabel <- function (txtLabel, xLoc, yLoc, cex.label) {
      txt <- Rgraphviz::labelText(txtLabel)
      if (length(txt) > 1)
        stop("'labelText(txtLabel)' must have length 1.")
      if (length(txt) == 0)
        return(invisible(NULL))
      if (xor(missing(xLoc), missing(yLoc)))
        stop("'xLoc' and 'yLoc' must be either be both specified or both missing.")
      if (missing(xLoc)) {
        loc <- Rgraphviz::labelLoc(txtLabel)
        justMod <- switch(Rgraphviz::labelJust(txtLabel), l = 0, n = -0.5,
                          r = -1)
        xLoc <- Rgraphviz::getX(loc) + (justMod * Rgraphviz::labelWidth(txtLabel))
        yLoc <- Rgraphviz::getY(loc)
      }
      graphics::text(xLoc, yLoc, txt, col = Rgraphviz::labelColor(txtLabel), cex=cex.label)
    }
    drawEdgeTxtLabel(Rgraphviz::txtLabel(x), cex.label=cex.label)
  }
})




