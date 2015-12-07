#' Sciplot within-subject error bars
#'
#' @author Frank Papenmeier
#' @description Implemented within CI Methods: 
#' Cousineau-Morey intervals with adaption by Baguley (2012), Morey (2008), and Cousineau(2005)")
#' @export
#'
#'

sciplot.CI.within.common <- function(x.factor, response, group, data, sid, subset, conf.level, within.ci.method)
{
  print("NOTE: Current implementation assumes that all factors are within-subjects!")

  if (!is.null(data)) {
    sid <- eval(sid, envir = data)
    x.factor <- eval(x.factor, envir = data)
    response <- eval(response, envir = data)
    group <- eval(group, envir = data)
  }

  if (is.null(sid)) stop("subject identifier (sid) must not be NULL!")

  if (!is.null(subset)) stop("subset must be NULL (subset != NULL not implemented)")

  if (length(group[[1]]) > 1) stop("Definition of multiple grouping factors via list not supported. Use paste instead!")

  # Annahme: Alle Faktoren sind within --> wie einen Faktor mit vielen Stufen handhaben, siehe Morey(2008)
  groups <- paste(x.factor, group, sep=".")
  m <- length(levels(factor(groups)))
  morey.correction <- m/(m-1)

  # Baguley (2012)
  baguley.diff.factor <- 2^0.5/2

  # Cousineau (2005, zitiert nach Morey, 2008): Within Varianz entfernen, indem Daten normalisiert
  if (is.null(group)) daten <- data.frame(sid=sid,x.factor=x.factor,response=response) # alles in Datenframe, da durch merge umsortiert wird und sonst Zuordnung unklar ist
  else daten <- data.frame(sid=sid,x.factor=x.factor,response=response,group=group) # alles in Datenframe, da durch merge umsortiert wird und sonst Zuordnung unklar ist
  subj.means <- aggregate(list(response.subj.mean=response),list(sid=sid),mean)
  daten <- merge(daten, subj.means)
  daten$response.normalized <- daten$response - daten$response.subj.mean + mean(daten$response)

  # Within CI Method
  within.ci.method <- within.ci.method[1]
  if (within.ci.method == "baguley") {
    print("Within CI Method: Cousineau-Morey intervals with adaption by Baguley (2012)")
  } else if (within.ci.method == "morey") {
    print("Within CI Method: Morey (2008)")
    baguley.diff.factor <- 1
  } else if (within.ci.method == "cousineau") {
    print("Within CI Method: Cousineau(2005)")
    baguley.diff.factor <- 1
    morey.correction <- 1
  } else {
    stop("No valid Within CI Method declared")
  }

  # Funktionen fuer Plot und CI
  fun = function(x) mean(x, na.rm=TRUE)
  ci.fun = function(x) {
    n <- length(x[complete.cases(x)])
    ci <- sqrt(var(x, na.rm=T)*morey.correction/n) * qt(1 - (1 - conf.level)/2, n - 1) * baguley.diff.factor

    # Fuer Testzwecke auskommentieren (CI auf Konsole ausgeben)
    #print(c(fun(x)-ci, fun(x)+ci))

    return(c(fun(x)-ci, fun(x)+ci))
  }

  results <- list()
  results$daten <- daten
  results$fun <- fun
  results$ci.fun <- ci.fun
  return(results)
}


#
# demodaten <- data.frame(factor1=c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1),factor2=c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1),vp=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),pc=c(0.8,0.6,0.55,0.3,0.95,0.8,0.6,0.4,0.55,0.2,0.18,0.0,0.68,0.5,0.3,0.2))
# dev.new()
#
# # Normaler lineplot
# lineplot.CI(factor1,pc,factor2,data=demodaten)
#
# # Within CI: subject identifier (sid) mit angeben
# lineplot.CI.within(factor1,pc,factor2,data=demodaten,sid=vp)
#
# # Optional: Within CI Methode angeben
# lineplot.CI.within(factor1,pc,factor2,data=demodaten,sid=vp,within.ci.method="cousineau")
# lineplot.CI.within(factor1,pc,factor2,data=demodaten,sid=vp,within.ci.method="morey")
# lineplot.CI.within(factor1,pc,factor2,data=demodaten,sid=vp,within.ci.method="baguley")
#
# # Optional2: CI Groesse angeben
# lineplot.CI.within(factor1,pc,factor2,data=demodaten,sid=vp,conf.level=0.99)
#
