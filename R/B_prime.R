#' Compute B''
#'
#'
#' @param hit hit rate
#' @param fa false alarm rate
#' @return sensitivity B''
#' @description Compute the non-parameterical measure B'' for response bias
#' @references Stanislaw, H., & Todorov, N. (1999). Calculation of signal detection theory measures. Behavior Research Methods, Instruments, & Computers, 31(1), 137–149.
#' @export
#'
B_prime <- function(hit,fa)
{
  ifelse((hit >= 1 | fa <= 0),
         stop(paste("B'' is only defined for hits < 1 and fa > 0, the current values are hit =", hit,"fa =", fa)),
         sign(hit - fa) * ((hit * (1 - hit) - fa * (1 - fa)) /
                             (hit * (1 - hit) + fa * (1 - fa))))
}

