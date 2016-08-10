#' Test for overdispersion of glmer models (variance larger than the mean)
#'
#' @param model glmer model (fitted with the glmer() function of the lme4 package)
#' @return Overdispersion measure.
#' @description Overdispersion of a glmer model is given if the deviation is larger than the mean. If this is the case 
#' (the ratio is larger than 1), it is recommended to calculate a variable with a unique value for each observation 
#' (dat$obs_effect <- 1:nrow(dat)) and to include this variable as additional random term (+ (1|obs_effect)).
#' 
#' It is important to fit the glmer model on the raw data. Otherwise, this function throws an error message.
#' @references http://glmm.wikidot.com/faq
#' @references http://stats.stackexchange.com/questions/6989/how-do-i-fit-a-multilevel-model-for-over-dispersed-poisson-outcomes/9670#9670
#' @references http://stats.stackexchange.com/questions/83611/how-to-fix-an-overdispersion-in-a-poisson-glmm-with-lmer-function-in-r
#' @export
#'
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}