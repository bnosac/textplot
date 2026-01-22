#' @name example_embedding
#' @title Example word embedding matrix
#' @description A matrix with 25-dimensional word embeddings, constructed upon the be_parliament_2020 dataset in the doc2vec R package
#' @docType data
#' @examples
#' data(example_embedding, package = 'textplot')
#' head(example_embedding)
NULL


#' @name example_embedding_clusters
#' @title Example words emitted in a ETM text clustering model
#' @description Example words emitted in a ETM text clustering model constructed upon the be_parliament_2020 dataset in the doc2vec R package
#' @docType data
#' @examples
#' data(example_embedding_clusters, package = 'textplot')
#' head(example_embedding_clusters)
#' terminology <- split(example_embedding_clusters, example_embedding_clusters$cluster)
#' lapply(terminology, head, n = 5)
NULL



#' @rdname textplot_embedding_2d
#' @export
textplot_embedding_2d <- function(x, ...){
  UseMethod("textplot_embedding_2d")
}


#' @rdname textplot_embedding_2d
#' @title Plot word embeddings in 2D
#' @description This plot displays words in 2 dimensions, optionally grouped by cluster.\cr
#' This allows to visualise embeddings which are reduced by dimensionality reduction techniques like UMAP, t-SNE, PCA or similar techniques.
#' It allows to highlight the words by groups and is a good way to visualise a small sets of word or topic embeddings.
#' @param x a data.frame with columns 'x', 'y', 'term' and optionally 'group' (color by group), 'weight' (size of the text / point shown), 'type' (pch used for the type of point)
#' @param title character string with the title to use in the plot
#' @param subtitle character string with the subtitle to use in the plot
#' @param encircle logical indicating to encircle all the points belonging to a group using \code{geom_encircle} from the ggalt package
#' @param alpha transparancy level passed on to \code{geom_encircle} from the ggalt package in case \code{encircle} is set to \code{TRUE}
#' @param points logical indicating to add points. Defaults to \code{FALSE}.
#' @param ... not used yet
#' @return an object of class ggplot
#' @export
#' @examples
#' \dontshow{
#' if(require(ggplot2) && require(ggrepel) && require(ggalt))
#' \{
#' }
#' library(ggplot2)
#' library(ggrepel)
#' library(ggalt)
#' ##
#' ## Generate some fake embeddings
#' ##   probably you want to use word2vec::word2vec(...) + uwot::umap(...)
#' embeddings <- matrix(runif(26 * 2), nrow = 26, ncol = 2, dimnames = list(letters))
#' x <- data.frame(term = rownames(embeddings), x = embeddings[, 1], y = embeddings[, 2])
#'
#' ## 2D plot
#' textplot_embedding_2d(x)
#'
#' ## 2D plot with groups
#' x$group <- sample(c("clustera", "clusterb", "clusterc"), size = 26, replace = TRUE)
#' textplot_embedding_2d(x)
#'
#' ## 2D plot with groups and weights for each word
#' x$weight <- runif(nrow(x))
#' textplot_embedding_2d(x)
#' textplot_embedding_2d(x, points = TRUE)
#'
#' ## 2D plot with groups and weights for each word and different types of points
#' x$type <- sample(c("word", "center"), size = 26, replace = TRUE)
#' x$type <- factor(x$type, levels = c("word", "center"))
#' textplot_embedding_2d(x, points = TRUE)
#' textplot_embedding_2d(x, title = "Embedding plot in 2D", subtitle = "example")
#'
#' ## Encircle the words belonging to each group
#' textplot_embedding_2d(x, title = "Embedding plot in 2D", subtitle = "example",
#'                       encircle = TRUE, alpha = 0.2)
#' \dontshow{
#' \}
#' # End of main if statement running only if the required packages are installed
#' }
textplot_embedding_2d.default <- function(x, title = "Embedding plot in 2D", subtitle = list(), encircle = FALSE, points = FALSE, alpha = 0.4, ...){
  stopifnot(is.data.frame(x) & all(c("x", "y", "term") %in% colnames(x)))
  term <- cluster <- weight <- type <- y <- NULL

  requireNamespace("ggplot2")
  requireNamespace("ggrepel")

  if("group" %in% colnames(x)){
    x$cluster <- factor(x$group)
  }else{
    if(!"cluster" %in% colnames(x)){
      x$cluster <- rep("all", times = nrow(x))
    }
    x$cluster <- factor(x$cluster)
  }
  if("weight" %in% colnames(x)){
  }else{
    weight <- 1
  }
  if("type" %in% colnames(x)){
    if(!is.factor(x$type)){
      x$type <- factor(x$type)
    }
    g <- ggplot2::ggplot(x, ggplot2::aes(x = x, y = y, label = term, color = cluster, cex = weight, pch = type))
  }else{
    g <- ggplot2::ggplot(x, ggplot2::aes(x = x, y = y, label = term, color = cluster, cex = weight))
  }
  g <- g +
    ggrepel::geom_text_repel(show.legend = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = title, subtitle = subtitle)
  if(encircle){
    requireNamespace("ggalt")
    g <- g + ggalt::geom_encircle(ggplot2::aes(group = cluster, fill = cluster), alpha = alpha, show.legend = FALSE)
  }
  if(points){
    g <- g + ggplot2::geom_point(show.legend = FALSE)
  }
  g
}
