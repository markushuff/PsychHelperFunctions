#' Calculate partial eta squared of a given F statistic

#' @author Markus Huff
#' @param DFeffect Degrees of freedom of the effect
#' @param DFerror Degrees of freedom of the error
#' @param Feffect F value of the effect
#' @export

pes <- function(DFeffect,DFerror,Feffect)
{
  (DFeffect*Feffect) / ((DFeffect*Feffect)+DFerror)
}
