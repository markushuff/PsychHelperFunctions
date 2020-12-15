#' Compute d'
#'
#'
#' @param hit hit rate
#' @param fa false alarm rate
#' @return sensitivity d'
#' @description Compute the parameterical measure d' for sensitivity
#' @references Macmillan, N. A., & Creelman, C. D. (2005). Detection theory: A user’s guide. Lawrence Erlbaum Associates.
#' @export
#'
d_prime <- function(hit,fa)
{
  if(any(hit >= 1) | any(fa <= 0))
  {
    stop(paste("d' is only defined for hits < 1 and fa > 0, the current values are hit =", hit,"fa =", fa))
  }
  else
  {
    d_prime <- qnorm(hit) - qnorm(fa)
  }
  return(d_prime)
}
