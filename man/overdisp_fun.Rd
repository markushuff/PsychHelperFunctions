% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overdisp_fun.R
\name{overdisp_fun}
\alias{overdisp_fun}
\title{Test for overdispersion of glmer models (variance larger than the mean)}
\usage{
overdisp_fun(model)
}
\arguments{
\item{model}{glmer model (fitted with the glmer() function of the lme4 package)}
}
\value{
Overdispersion measure.
}
\description{
Overdispersion of a glmer model is given if the deviation is larger than the mean. If this is the case 
(the ratio is larger than 1), it is recommended to calculate a variable with a unique value for each observation 
(dat$obs_effect <- 1:nrow(dat)) and to include this variable as additional random term (+ (1|obs_effect)).

It is important to fit the glmer model on the raw data. Otherwise, this function throws an error message.
}
\references{
http://glmm.wikidot.com/faq

http://stats.stackexchange.com/questions/6989/how-do-i-fit-a-multilevel-model-for-over-dispersed-poisson-outcomes/9670#9670

http://stats.stackexchange.com/questions/83611/how-to-fix-an-overdispersion-in-a-poisson-glmm-with-lmer-function-in-r
}
