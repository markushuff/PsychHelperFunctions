#' Compute c
#'
#'
#' @param hit hit rate
#' @param fa false alarm rate
#' @return bias c
#' @description Compute the response bias measure c
#' @references MacmillanStanislaw, H., & Todorov, N. (1999). Calculation of signal detection theory measures. Behavior Research Methods, Instruments, & Computers, 31(1), 137–149.

#' @export
#'
cc <- function(hit,fa)
{
  if(any(hit >= 1) | any(fa <= 0))
  {
    stop(paste("d' is only defined for hits < 1 and fa > 0, the current values are hit =", hit,"fa =", fa))
  }
  else
  {
    c <- -(qnorm(hit) + qnorm(fa))/2
  }
  return(c)
}
