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

