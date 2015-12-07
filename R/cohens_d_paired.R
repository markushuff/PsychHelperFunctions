#' Compute Cohen's d for paired samples / and one sample
#'
#'
#' @param t t-value of paired t-test
#' @param n sample size
#' @return Cohen's d
#' @description This value of Cohen's d is used by Lenth (2006) in his
#' one sample and (paired sample) t-test option. Notice than as n goes
#' up t will increase if the new observations are close to the mean which
#' should be the case if, as assumed, the response is normally distributed.
#' @references http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/td
#' @references Lenth, R. V. (2006) Java Applets for Power and Sample Size [Computer software].
#' Retrieved month day, year, from http://www.stat.uiowa.edu/~rlenth/Power.
#' @export
#'
cohens_d_paired <- function(t,n){
  t/sqrt(n)
}

