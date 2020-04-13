# Adapted from https://github.com/SachaEpskamp/qgraph/blob/master/R/glasso_methods.R
EBICglasso <- function (S, n, gamma = 0.5, penalize.diagonal = FALSE, nlambda = 100,
          lambda.min.ratio = 0.01, returnAllResults = FALSE, checkPD = TRUE,
          penalizeMatrix, countDiagonal = FALSE, refit = FALSE, ...){
  requireNamespace("glasso")
  logGaus <- function (S, K, n) {
    KS = K %*% S
    tr = function(A) sum(diag(A))
    return(n/2 * (log(det(K)) - tr(KS)))
  }
  EBIC <- function (S, K, n, gamma = 0.5, countDiagonal = FALSE) {
    L <- logGaus(S, K, n)
    E <- sum(K[lower.tri(K, diag = countDiagonal)] != 0)
    p <- nrow(K)
    -2 * L + E * log(n) + 4 * E * gamma * log(p)
  }
  wi2net <- function (x) {
    x <- -stats::cov2cor(x)
    diag(x) <- 0
    x <- Matrix::forceSymmetric(x)
    return(x)
  }


  if (any(eigen(S)$values < 0)){
    S <- Matrix::nearPD(S, corr = TRUE, maxit = 1000)
    S <- as.matrix(S$mat)
  }

  S <- stats::cov2cor(S)
  lambda.max = max(max(S - diag(nrow(S))), -min(S - diag(nrow(S))))
  lambda.min = lambda.min.ratio * lambda.max
  lambda = exp(seq(log(lambda.min), log(lambda.max), length = nlambda))
  glas_path <- glasso::glassopath(S, lambda, trace = 0, penalize.diagonal = penalize.diagonal, ...)

  lik <- sapply(seq_along(lambda), function(i) {
    logGaus(S = S, K = glas_path$wi[, , i], n = n)
  })
  EBICs <- sapply(seq_along(lambda), function(i) {
    EBIC(S=S, K=glas_path$wi[, , i], n=n, gamma=gamma, countDiagonal = countDiagonal)
  })
  opt <- which.min(EBICs)
  net <- as.matrix(Matrix::forceSymmetric(wi2net(glas_path$wi[, ,
                                                      opt])))
  colnames(net) <- rownames(net) <- colnames(S)
  optwi <- glas_path$wi[, , opt]
  return(list(results = glas_path, ebic = EBICs, loglik = lik,
                optnet = net, lambda = lambda, optwi = optwi))
}
