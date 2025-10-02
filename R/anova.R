
#' Compare knn_s3 models using error metrics
#'
#' This function performs an analysis of variance (ANOVA)-style comparison of multiple
#' \code{knn_s3} models based on their training error. For regression models, the mean
#' squared error (MSE) is used; for classification models, the misclassification error
#' rate (1 - accuracy) is calculated. The differences in error between successive models
#' are also reported.
#'
#' @param object an object of class \code{\link{knn_s3}}.
#' @param ... further arguments.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \code{Model}: the deparsed call for each model,
#'   \item \code{k}: number of neighbors used in each model,
#'   \item \code{Error}: training error (MSE for regression, error rate for classification),
#'   \item \code{DeltaError}: difference in error compared to the previous model.
#' }
#' Invisibly returns this data frame after printing it.
#'
#' @examples
#' \dontrun{
#' fit3 <- knn_s3(Species ~ ., data = iris, k = 3)
#' fit5 <- knn_s3(Species ~ ., data = iris, k = 5)
#' anova(fit3, fit5)
#' }
#'
#' @export
anova.knn_s3 <- function(object, ...) {
  models <- list(object, ...)
  calls <- sapply(models, function(m) deparse(m$call))
  errors <- sapply(models, function(m) {
    preds <- fitted(m)
    obs <- m$train_y
    if (m$type == "regression") {
      mean((obs - preds)^2)
    } else {
      1 - mean(preds == obs)
    }
  })
  df <- data.frame(
    Model = calls,
    k = sapply(models, `[[`, "k"),
    Error = errors
  )
  df$DeltaError <- c(NA, diff(df$Error))
  print(df, row.names = FALSE)
  invisible(df)
}
