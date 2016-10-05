#' Compute A
#' 
#' 
#' @param hit hit rate
#' @param fa false alarm rate
#' @return sensitivity A
#' @description Compute the non-parameterical measure A for sensitivity
#' @references Zhang, J., & Mueller, S. T. (2005). A note on ROC analysis and non-parametric estimate of sensitivity. Psychometrika, 70(1), 203–212. doi:10.1007/s11336-003-1119-8
#' @export

A <-function(hit,fa)
{
  if(fa<=.5 & hit>=.5)
  {
    a <- .75 + (hit-fa)/4 - fa*(1-hit)
  } else if(fa<=hit & hit<=.5)
  {
    a <- .75 + (hit-fa)/4 - fa/(4*hit)
  } else {
    a <- .75 + (hit-fa)/4 - (1-hit)/(4 * (1-fa))
  } 
  return(a)
}
