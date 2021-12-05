#' n most salient event boundaries
#'
#'
#' @param n number of the most salient event boundaries to return
#' @param segmag segmag object
#' @return dataframe including the n most salient event boundaries
#' @description Get a list including the n highest event boundaries of a segmag object (Papenmeier, 2016; https://cran.r-project.org/package=segmag). Sometimes it is necessary to get just a list of the n highest event boundaries w/o doing the time consuming bootstrapping method. Note, this function requires the segmag package.
#' @references Papenmeier, F., & Sering, K. (2014). segmag: Determine event boundaries in event segmentation experiments. R package version, 1(2).
#' @export
#'
get_n_eb_times <- function(n, segmag)
{
  eb_times <- segmag::get_eb_times(segmag,
                           cutoff = mean(segmag$data$segmentation_magnitude))
  
  out <- segmag$data[segmag$data$time %in% eb_times, ] 
  out <- out[order(out$segmentation_magnitude, decreasing = TRUE),]
  out[1:n,]
}

