#' Compute Cohen's w (omega) as effect size for Chi-square tests
#'
#'
#' @param chi_sq Chi-square value
#' @param n total count in all cells
#' @return Cohen's w (omega)
#' @description For each cell of a table containing m cells, there are two proportions considered: 
#' one specified by a null hypothesis and the other specified by the alternative hypothesis. 
#' Usually, the proportions specified by the alternative hypothesis are those occurring in the data. 
#' The effect size, w (omega), is calculated using this formula.
#' small: 0.1, medium: 0.3, large: 0.5
#' @references http://ncss.wpengine.netdna-cdn.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Chi-Square_Effect_Size_Calculator.pdf
#' @references https://de.wikipedia.org/wiki/Effektst%C3%A4rke
#' @export
#'
cohens_w <- function(chi_sq,n){
  sqrt(chi_sq/n)
}