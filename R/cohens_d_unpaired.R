#' Compute Cohen's d
#'
#'
#' @param t t-value of unpaired t-test
#' @param n2 sample size group 2
#' @param n1 sample size group 1
#' @return Cohen's d
#' @description Pustejovsky (2014) states for sample sizes using an unpaired
#' t-test with sample sizes n1 and n2 Cohen's d = t [Sqrt(1/n1 + 1/n2)]
#' When n1 = n2 Cohen's d = 2t / Sqrt(df) (See Rosenthal (1994) and Howell (2013), p.649).
#' This formula is used to compute Cohen's d from a t ratio via the cohens_d_unpaired function.
#' @references http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/td
#' @references Howell, D. C. (2013) Statistical methods for psychologists. 8th Edition.
#' International Edition. Wadsworth:Belmont, CA.
#' @references Pustejovsky, J. E. (2014) Converting from d to r to z when the design
#' uses extreme groups, dichotomization, or experimental control.
#' Psychological Methods 19(1) 92-112.
#' @references Rosenthal, R. (1994) Parametric measures of effect size. In H.
#' Cooper and L.V. Hedges (Eds.). The handbook of research synthesis. New York:
#' Russell Sage Foundation.
#' @export
#'
cohens_d_unpaired <- function(t,n1,n2){
  if(n1==n2) 2*t/sqrt(n1+n2-2)
  else t*sqrt(1/n1 + 1/n2)
}
