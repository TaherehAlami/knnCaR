#'
#' Calculate distances between train data and new data for knn_s3 model
#'
#' This function computes pairwise distances between the training data stored in a \code{knn_s3} object
#' and new data points, using either sum of squared errors ("sse") or sum of absolute differences ("sad").
#' It supports matrix or data frame inputs for the new data.
#'
#' @param object An object of \code{knn_s3} \code{\link{class}}.
#' @param newdata A numeric matrix or data frame of new observations.
#' @param distmethod Distance metric to use: \code{"sse"} for sum of squared errors (default) or
#'   \code{"sad"} for sum of absolute differences.
#' @param ... Additional parameters.
#'
#' @return A numeric matrix of distances with \code{nrow(newdata)} rows and \code{nrow(object$train_x)} columns.
#'
#' @examples
#' \dontrun{
#' fit <- knn_s3(Species ~ Sepal.Length + Sepal.Width, data = iris, k = 5)
#' new_points <- iris[1:3, 1:2]
#' distances <- distance(fit, new_points, distmethod = "sse")
#' print(distances)
#' }
#'
#' @export

distance <- function(object, newdata, distmethod = c("sse", "sad"), ...) {
  UseMethod("distance")
}

#' @export
distance.knn_s3 <- function(object, newdata, distmethod = c("sse", "sad"), ...) {
  distmethod <- match.arg(distmethod)

  if (is.matrix(newdata)) {
    Xtest <- newdata
  } else if (is.data.frame(newdata)) {
    mf <- model.frame(delete.response(terms(object$formula)), newdata)
    Xtest <- model.matrix(delete.response(terms(object$formula)), mf)
  } else {
    stop("newdata must be either a matrix or a data.frame")
  }

  n <- nrow(object$train_x)
  m <- nrow(Xtest)
  dists <- matrix(0, m, n)

  for(j in seq_len(m)){
    if (distmethod == "sse") {
      dists[j, ] <- rowSums((object$train_x - matrix(Xtest[j, ], n, ncol(Xtest), byrow = TRUE))^2)
    } else {
      dists[j, ] <- rowSums(abs(object$train_x - matrix(Xtest[j, ], n, ncol(Xtest), byrow = TRUE)))
    }
  }
  dists
}

