#' Bargraph within-subject error bars
#'
#' @author Frank Papenmeier
#' @param sid subject identifier
#' @param x.factor a factor (required) whose levels will form the x axis.
#' @param response a numeric variable giving the response.
#' @param group grouping factor (optional) whose levels will form the traces.
#' @param type the type of plot: lines, points, or both. Defaults to both.
#' @param x.cont logical. Treat x.factor as a continuous variable?
#' @param legend logical. Should a legend be included?
#' @param trace.label overall legend label.
#' @param leg.lab legend labels for trace factors.
#' @param fixed	logical. Should the legend be in the order of the levels of 'trace.factor' or in the order of the traces at their right-hand ends?
#' @param x.leg, y.leg optional values to over ride the default legend placement.
#' @param cex.leg character expansion value for legend labels.
#' @param ncol number of columns to use for legend.
#' @param pch	a vector of plotting symbols or characters.
#' @param fun	the function to compute the summary statistic. Should return a single real value. Defaults to mean with NA values removed.
#' @param ci.fun the functions to compute the CI. Should return a vector of length 2 defining the lower and upper limits of the CI's. Defaults to the mean +/- 1 standard error, with NA values removed.
#' @param err.width	set width of whiskers for error bars.
#' @param err.col	color for error bars. Defaults to col.
#' @param err.lty	line type for error bars.
#' @param xlim, ylim range for x and y axes.
#' @param cex	overall plot character expansion value.
#' @param lwd	deterimines line width.
#' @param col	default color(s) for plot.
#' @param cex.axis character expansion value for axis labels.
#' @param xaxt should x-axis be drawn?
#' @param data an optional data frame.
#' @param subset an optional expression indicating the subset of the rows of 'data' that should be used in the plot.
#' @param ...	further graphical parameters.
#' @export
#'
#'

bargraph.CI.within <- function(x.factor, response, group = NULL, data = NULL, sid = NULL, subset = NULL, conf.level = 0.95, within.ci.method = c("baguley", "morey", "cousineau"), ...)
{
  if (is.null(data)) withinCI <- sciplot.CI.within.common(x.factor, response, group, data, sid, subset, conf.level, within.ci.method)
  else withinCI <- sciplot.CI.within.common(substitute(x.factor), substitute(response), substitute(group), data, substitute(sid), subset, conf.level, within.ci.method)

  #bargraph.CI(x.factor=x.factor,response=response.normalized,group=group,data=withinCI$daten,fun=withinCI$fun,ci.fun=withinCI$ci.fun,...)
  # Hack: Neues sciplot: fun nicht explizit definieren (eval Problem), sondern Annahme, dass Mittelwert ist
  sciplot::bargraph.CI(x.factor=x.factor,response=response.normalized,group=group,data=withinCI$daten,ci.fun=withinCI$ci.fun,...)
}
