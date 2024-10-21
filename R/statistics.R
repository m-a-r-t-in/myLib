
#' rmse
#'
#' Function that returns Root Mean Squared Error
#' @param linear_model_residuals linear model residuals
#' @keywords rmse
#' @export
#' @examples
#' rmse()
rmse <- function(linear_model_residuals)
{
  sqrt(mean(linear_model_residuals^2))
}

#' mae
#'
#' Function that returns Mean Absolute Error
#' @param linear_model_residuals linear model residuals
#' @keywords mae
#' @export
#' @examples
#' mae()
mae <- function(linear_model_residuals)
{
  mean(abs(linear_model_residuals))
}


#' getmode
#'
#' return mode of numerical vector
#' @param v numeric vector
#' @keywords mode
#' @export
#' @examples
#' getmode()
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
