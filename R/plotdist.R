
#' Plot histogram of distances in knn_s3 training data
#'
#'
#' @param object A fitted \code{\link{knn_s3}} object.
#' @param distmethod A character string specifying the distance method to use.
#'        One of \code{"sse"} (sum of squared errors) or \code{"sad"} (sum of absolute differences).
#' @return Returns an object of \code{\link{ggplot}} class showing the histogram of distances.
#'
#' @import ggplot2
#' @examples
#' \dontrun{
#' fit <- knn_s3(mpg ~ disp + hp, data = mtcars, k = 3)
#' plotdist.knn_s3(fit, distmethod = "sse")
#' }
#' @export
plotdist.knn_s3 <- function(object, distmethod = c("sse", "sad")) {
  distmethod <- match.arg(distmethod)
  dists <- distance(object, object$train_x, distmethod)
  dist_vec <- as.vector(dists[lower.tri(dists)])
  df <- data.frame(distance = dist_vec)
  ggplot(df, aes(x = distance)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", distmethod, "distances"),
         x = "Distance",
         y = "Frequency") +
    theme_minimal()
}
