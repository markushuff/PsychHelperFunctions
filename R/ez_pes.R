#' ezANOVA wrapper to calculate 
#'
#'
#' @param ezANOVAResult ezANOVA result
#' @return ezANOVA with partial eta-squared
#' @details ezANOVA must be run with parameter 'detailed=T'.
#' @author Frank Papenmeier
#' @export
#'
ezANOVA.pes <- function (ezANOVAResult)
{
  if (is.null(ezANOVAResult$ANOVA$SSn) | is.null(ezANOVAResult$ANOVA$SSd))
  {
    stop("ezANOVA must be run with parameter 'detailed=T'.")
  }
  
  ezANOVAResult$ANOVA$pes <- ezANOVAResult$ANOVA$SSn/(ezANOVAResult$ANOVA$SSn+ezANOVAResult$ANOVA$SSd)
  
  return(ezANOVAResult)
}
