#
#' k-NN Classification and Regression
#'
#' Create a k-nearest neighbors (k-NN) model object for classification or regression
#' using a formula interface. This function supports numeric responses for regression
#' and factor responses for classification.
#' @param formula n object of class \code{\link{formula}}, y ~ x1 + x2 + ...
#' @param data a data frame containing the variables used in the formula.
#' @param k number of neighbors

#' @return An object of \code{\link{class}} \code{knn_s3} containing the
#' following components:
#' the original function call, the used formula, values of features, the values of
#' the response variable, the model type (classification or regression),
#' and the number of neighbors \code{k}.
#'
#' @details
#' The response variable should be either numeric (for regression) or a factor (for classification).
#' The function throws an error if the response is not one of these types.
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' fit <- knn_s3(mpg ~ disp + hp, data = mtcars, k = 3)
#' class(fit)
#' }
#'
#' @export
#'
knn_s3 <- function(formula, data, k = 5L) {
  mf <- model.frame(formula, data)
  y <- model.response(mf)

  if (is.factor(y)) {
    type <- "classification"
  } else if (is.numeric(y)) {
    type <- "regression"
  } else {
    stop("Response variable must be numeric or factor")
  }

  X <- model.matrix(attr(mf, "terms"), mf)
  stopifnot(is.numeric(k), k >= 1, k <= nrow(X))

  structure(list(
    call = match.call(),
    formula = formula,
    train_x = X,
    train_y = y,
    k = as.integer(k),
    type = type
  ), class = "knn_s3")
}

