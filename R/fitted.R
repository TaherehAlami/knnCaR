
#' Fitted values for knn_s3 objects
#'
#' Returns fitted values on the training data using the \code{predict} method with \code{method="R"}.
#'
#' @param object A \code{knn_s3} model object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Vector of predictions on the training data.
#'
#' @examples
#' \dontrun{
#' fit <- knn_s3(Species ~ Sepal.Length + Sepal.Width, data = iris, k = 5)
#' fitted(fit)
#' }
#' @export
fitted.knn_s3 <- function(object, ...) {
  predict(object, newdata = object$train_x, method = "R")
}
