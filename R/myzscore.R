#' Z-score calculator
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' myzscore(x = 1:20)
myzscore <- function(x) {
  (x-mean(x)) / sd(x)
}
