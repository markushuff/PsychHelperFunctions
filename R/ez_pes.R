#' ezANOVA wrapper to calculate 
#'
#'
#' @param ezANOVAResult ezANOVA result
#' @return ezANOVA with partial eta-squared
#' @details ezANOVA muss mit dem Parameter 'detailed=T' ausgeführt werden.
#' @author Frank Papenmeier
#' @export
#'
ezANOVA.pes <- function (ezANOVAResult)
{
  if (is.null(ezANOVAResult$ANOVA$SSn) | is.null(ezANOVAResult$ANOVA$SSd))
  {
    stop("ezANOVA muss mit dem Parameter 'detailed=T' ausgeführt werden.")
  }
  
  ezANOVAResult$ANOVA$pes <- ezANOVAResult$ANOVA$SSn/(ezANOVAResult$ANOVA$SSn+ezANOVAResult$ANOVA$SSd)
  
  return(ezANOVAResult)
}
