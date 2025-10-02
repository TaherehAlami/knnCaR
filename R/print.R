#
#' Print method for knn_s3 objects
#'
#' Print the model structure and key attributes of a \code{\link{knn_s3}} object.
#' @param object an object of class (\code{\link{knn_s3}}).
#' @param ... further arguments.
#'
#' @return Prints the \code{knn_s3} object structure and key attributes.
#'
#' @examples
#' \dontrun{
#' # knn regression model
#' data(mtcars)
#' fit_reg <- knn_s3(mpg ~ disp + hp, data = mtcars, k = 3)
#' print(fit_reg)
#'
#' # knn classification model
#' data(iris)
#' fit_class <- knn_s3(Species ~ Sepal.Length + Sepal.Width, data = iris, k = 3)
#' print(fit_class)
#' }
#' @export

print.knn_s3 <- function(object, ...) {
  cat("Model structure:\n")
  cat(" - Call: "); print(object$call)
  cat(" - Class: knn_s3 \n")
  cat(" - Model type:", object$type, "\n")
  cat(" - Formula: "); print(object$formula)
  cat(" - Number of training samples:", nrow(object$train_x), "\n")
  cat(" - Number of features:", ncol(object$train_x)-1, "\n")
  cat(" - Number of neighbors (k):", object$k, "\n")
  cat(" - Response type:", class(object$train_y), "\n")
  if (object$type == "classification") {
    cat(" - Response class levels:", paste(levels(object$train_y), collapse = ", "), "\n")
  }
  invisible(object)
}
