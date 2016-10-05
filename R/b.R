#' Compute b
#' 
#' 
#' @param hit hit rate
#' @param fa false alarm rate
#' @return sensitivity b
#' @description Compute the non-parameterical measure b for bias
#' @references Zhang, J., & Mueller, S. T. (2005). A note on ROC analysis and non-parametric estimate of sensitivity. Psychometrika, 70(1), 203–212. doi:10.1007/s11336-003-1119-8
#' @export


b <- function(hit,fa)
{
  if(fa<=.5 & hit>=.5)
  {
    b <-(5-4*hit)/(1+4*fa)
  } else if(fa<=hit & hit<=.5)
  {
    b <-(hit^2+hit)/(hit^2+fa) 
  } else { 
    b <- ((1-fa)^2 + (1-hit))/((1-fa)^2 + (1-fa))
  } 
  return(b)
}