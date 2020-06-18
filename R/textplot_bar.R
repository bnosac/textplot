rescaler <- function(x, from = range(x, na.rm = TRUE, finite = TRUE), to = c(0, 1)){
  has_no_range <- function (x) {
    if (length(x) == 1){
      return(TRUE)
    }else if(x[1] == x[2]){
      return(TRUE)
    }else{
      m <- min(abs(x))
      if(m == 0){
        return(FALSE)
      }
      abs((x[1] - x[2])/m) < .Machine$double.eps
    }
  }
  if (has_no_range(from) || has_no_range(to)) {
    mean(to)
  }else{
    (x - from[1]) / diff(from) * diff(to) + to[1]
  }
}

#' @rdname textplot_bar
#' @export
textplot_bar <- function(x, ...){
  UseMethod("textplot_bar")
}

#' @rdname textplot_bar
#' @title Barplot of a frequency table using lattice
#' @description Barplot of a frequency table using lattice
#' @param x a table to plot or a data.frame with the first column the label and the second column the frequency
#' @param panel character string what to put into the panel
#' @param total integer with the total. Defaults to sum(x). Is used to plot the table counts as a percentage. In which case this is divided by the total.
#' @param top integer indicating to plot only the first 'top' table elements. Defaults to 40.
#' @param col.panel color of the panel. Defaults to lightgrey.
#' @param col.line color of the line. Passed on to the col argument in \code{lattice::panel.lines}
#' @param lwd width of the line. Passed on to the lwd argument in \code{lattice::panel.lines}
#' @param cextext numeric with the cex of the text with the counts plotted. Passed on to \code{lattice::panel.text}.
#' @param addpct logical indicating to add the percent with \code{lattice::panel.text}
#' @param cexpct numeric with the cex of the text plotted when using addpct. Passed on to \code{lattice::panel.text}.
#' @param textpos passed on to the pos argument of panel.text to indicate where to put the text of the frequencies
#' @param pctpos passed on to the pos argument of panel.text to indicate where to put the text of the percentages
#' @param v passed on to \code{lattice::panel.abline} to draw a vertical line
#' @param col.abline passed on to \code{lattice::panel.abline} to draw a vertical line
#' @param ... other arguments passed on to \code{lattice::dotplot}
#' @return the result of a call to \code{lattice::dotplot}
#' @export
#' @examples
#' data(brussels_listings, package = 'udpipe')
#' x <- table(brussels_listings$neighbourhood)
#' x <- sort(x)
#' textplot_bar(x,
#'  panel = "Locations", col.panel = "darkgrey", xlab = "Listings",
#'  cextext = 0.75, addpct = TRUE, cexpct = 0.5)
#'
#'
#' x <- sample(LETTERS, 1000, replace = TRUE)
#' textplot_bar(sort(table(x)), panel = "Frequencies", xlab = "Frequency",
#'    cextext = 0.75, main = "Freq stats")
#' textplot_bar(sort(table(x)), panel = "Frequencies", addpct = TRUE, top = 15)
#'
#' ## x can also be a data.frame where the first column
#' ## is the label and the second column the frequency
#' x <- data.frame(l = LETTERS, amount = rnorm(26))
#' textplot_bar(x)
#' textplot_bar(x, v = 0)
textplot_bar.default <- function(x, panel = "Effect", total = sum(x), top = 40,
                         col.panel = "lightgrey", col.line="lightblue", lwd=3, cextext=0.5, addpct=FALSE, cexpct=0.75,
                         textpos = 3, pctpos = 1, v=NULL, col.abline = "red", ...){
  if(inherits(x, c("data.table", "data.frame"))){
    data <- data.frame(freq = x[, 1],
                       label = as.numeric(x[, 2]),
                       panel = panel,
                       stringsAsFactors = FALSE)
    data$freq <- factor(data$freq, levels = rev(data$freq))
    if(missing(total)){
      total <- sum(data$label, na.rm = TRUE)
    }
    data <- data[order(data$label, decreasing = TRUE), ]
    data <- head(data, n = top)
  }else{
    tmp <- x
    keep <- names(sort(abs(tmp), decreasing=TRUE))
    tmp <- tmp[names(tmp) %in% keep[seq_len(min(top, length(keep)))]]
    tmp <- force(tmp)
    data <- data.frame(freq = names(tmp),
                       label = as.numeric(tmp),
                       panel = panel,
                       stringsAsFactors = FALSE)
    data$freq <- factor(as.character(data$freq), levels = names(tmp))

  }

  dotplot(data$freq ~ data$label | data$panel,
          panel = function(x, y, ...){
            mapply(x, y, FUN=function(x, y){
              panel.lines(x=c(0, x), y = c(y, y), lwd = lwd, col = col.line)
            })
            panel.dotplot(x, y, cex=rescaler(x, to = c(1.25, 2)), ...)
            if(!is.null(v)){
              panel.abline(v = v, col.line = col.abline, lty = 2)
            }
            panel.text(x, y, round(x, 2), pos = textpos, col="black", cex=cextext)
            if(addpct){
              panel.text(x, y, sprintf("(%s%s)", round(100*x/total, 1), "%"), pos = pctpos, col="black", cex = cexpct)
            }
          },
          par.settings = list(strip.background=list(col = col.panel)), ...)
}

