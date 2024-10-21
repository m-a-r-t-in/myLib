#' randomRows
#'
#' return n random rows from dataframe
#' @param df dataframe
#' @param n number of rows to be returned
#' @keywords random rows
#' @export
#' @examples
#' randomRows()
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

#' mround
#'
#' This function round number to the given base.
#' @param x number to round.
#' @param base round x to base, eg. base = 5, result rounded to the 0,5,10,15...
#                                   base = 10, result rounded to the 0,10,20,30...
#' @keywords round
#' @export
#' @examples
#' mround()
mround <- function(x,base){
  base*round(x/base)
}