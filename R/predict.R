#' Predict method for knn_s3 objects
#'
#' Predicts responses for new data using a fitted \code{knn_s3} model. Supports both
#' classification and regression types. For classification, returns predicted classes;
#' for regression, returns numeric predictions.
#'
#' @param object A \code{knn_s3} model object.
#' @param newdata A numeric matrix or data frame of new observations.
#' @param method Character string specifying the prediction method; either \code{"R"} (default)
#'        or \code{"cpp"} (currently the same implementation).
#' @param ... Additional arguments (currently ignored).
#'
#' @return For classification models, a factor vector of predicted classes. For regression,
#' a numeric vector of predicted values.
#'
#' @examples
#' \dontrun{
#' fit <- knn_s3(Species ~ Sepal.Length + Sepal.Width, data = iris, k = 5)
#' new_obs <- iris[1:5, c("Sepal.Length", "Sepal.Width")]
#' predict(fit, newdata = new_obs, method = "R")
#' }
#'
#' @export
#'
predict.knn_s3 <- function(object, newdata, method = c("R", "cpp"), ...) {
  method <- match.arg(method)

  if (is.matrix(newdata)) {
    Xtest <- newdata
  } else if (is.data.frame(newdata)) {
    mf <- model.frame(delete.response(terms(object$formula)), newdata)
    Xtest <- model.matrix(delete.response(terms(object$formula)), mf)
  } else {
    stop("newdata must be matrix or data.frame")
  }

  n <- nrow(object$train_x)
  m <- nrow(Xtest)
  preds <- vector("list", m)

  for (j in seq_len(m)) {
    d2 <- rowSums((object$train_x - matrix(Xtest[j, ], n, ncol(Xtest), byrow = TRUE))^2)
    nn <- order(d2)[1:object$k]

    if (object$type == "regression") {
      preds[[j]] <- mean(object$train_y[nn])
    } else {
      votes <- table(object$train_y[nn])
      preds[[j]] <- names(which.max(votes))
    }
  }

  if (object$type == "regression") {
    unlist(preds)
  } else {
    factor(unlist(preds), levels = levels(object$train_y))
  }
}
