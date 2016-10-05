#' Compute A'
#'
#'
#' @param hit hit rate
#' @param fa false alarm rate
#' @return sensitivity A'
#' @description Compute the non-parameterical measure A' for sensitivity
#' @references Stanislaw, H., & Todorov, N. (1999). Calculation of signal detection theory measures. Behavior Research Methods, Instruments, & Computers, 31(1), 137–149.
#' @export
#'
A_prime <- function(hit,fa)
{
  if(any(hit >= 1) | any(fa <= 0))
  {
    stop(paste("A' is only defined for hits < 1 and fa > 0, the current values are hit =", hit,"fa =", fa))
  }
  else
  {
   A_prime <- 0.5 + (sign(hit - fa)) * ((hit - fa) ^ 2 + abs(hit - fa)) / 
              (4 * pmax(hit, fa) - 4 * hit * fa)
  }
  return(A_prime)
}

# A_prime <- function(hit,fa)
# {
#   ifelse((hit >= 1 | fa <= 0),
#          stop(paste("A' is only defined for hits < 1 and fa > 0, the current values are hit =", hit,"fa =", fa)),
#          0.5 + (sign(hit - fa)) * ((hit - fa) ^ 2 + abs(hit - fa)) / 
#            (4 * pmax(hit, fa) - 4 * hit * fa))
# }