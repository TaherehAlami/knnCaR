#
#' Summarizing knn Classification and Regression Model Fits
#'
#' Provides a summary of a fitted knn Classification and Regression Model,
#' including error metrics for regression or accuracy and confusion matrix
#' for classification.
#'
#' @param object an object of class \code{\link{knn_s3}}.
#' @return A summary of error metrics for \code{knn_s3} Model.
#' \itemize{
#'  \item For regression: \code{MSE} (mean squared error) and \code{R2} (coefficient of determination).
#'  \item For classification: \code{Accuracy} and \code{ConfusionMatrix} table.
#' }
#'
#' @examples
#' \dontrun{
#' # knn regression model
#' data(mtcars)
#' fit_reg <- knn_s3(mpg ~ disp + hp, data = mtcars, k = 3)
#' print(fit_reg)
#' summary(fit_reg)
#'
#' # knn classification model
#' data(iris)
#' fit_class <- knn_s3(Species ~ Sepal.Length + Sepal.Width, data = iris, k = 3)
#' print(fit_class)
#' summary(fit_class)
#' }
#' @export

summary.knn_s3 <- function(object, ...) {
  preds <- fitted(object)
  obs <- object$train_y

  if (object$type == "regression") {
    mse <- mean((obs - preds)^2)
    r2 <- 1 - sum((obs - preds)^2) / sum((obs - mean(obs))^2)
    cat("Training performance:\n")
    cat("  MSE:", round(mse, 4), "   R²:", round(r2, 4), "\n")
    invisible(list(call = object$call, k = object$k, MSE = mse, R2 = r2))
  } else {
    accuracy <- mean(preds == obs)
    tab <- table(Predicted = preds, Actual = obs)
    cat("Training performance (classification):\n")
    cat("  Accuracy:", round(accuracy, 4), "\n")
    print(tab)
    invisible(list(call = object$call, k = object$k, Accuracy = accuracy, ConfusionMatrix = tab))
  }
}
