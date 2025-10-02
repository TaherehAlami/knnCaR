#'
#' Plot method for knn_s3 objects
#'
#' Produces a plot comparing actual versus predicted values for a \code{knn_s3} model.
#' For regression models, it shows a scatter plot and For classification models, it
#' creates a bar plot of counts for each actual class filled by predicted classes.
#'
#' @param object A fitted \code{\link{knn_s3}} object.
#' @param ... further arguments.
#' @return Returns an object of \code{\link{ggplot}} class visualizing model predictions
#'  against actual values.
#'
#' @import ggplot2
#' @examples
#' \dontrun{
#' fit <- knn_s3(Species ~ Sepal.Length + Sepal.Width, data = iris, k = 5)
#' plot(fit)
#' }
#' @export
plot.knn_s3 <- function(object, ...) {
  preds <- fitted(object)
  obs <- object$train_y
  plot_data <- data.frame(Actual = obs, Predicted = preds)

  if (object$type == "regression") {
    ggplot(plot_data, aes(x = Actual, y = Predicted)) +
      geom_point(color = "blue") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(title = "k-NN Model: Actual vs Predicted", x = "Actual Values", y = "Predicted Values") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    ggplot(plot_data, aes(x = Actual, fill = Predicted)) +
      geom_bar(position = "dodge") +
      labs(title = "k-NN Model: Actual vs Predicted", x = "Actual Classes", y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
}

