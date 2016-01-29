#' Converts partial eta squared to Cohen's f

#' @author Markus Huff
#' @param pes partial eta sqaured
#' @export
#' 
pes_to_f <- function(pes)
{
  sqrt(pes / ( 1 - pes ) )
}