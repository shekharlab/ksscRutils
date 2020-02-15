
#' humpMean: Mean of a numeric vector x restricted to only values x > min
#'
#' @param x numeric vector
#' @param min numeric value
#'
#' @return mean of entries of x greter than min (numeric)
#' @export
#'
#' @examples
#' \dontrun{
#' humpMean(c(0,1,1,3,0,5), min=0)
#' }
humpMean=function(x, min=0) {
  return(mean(x[x>min]))
}

#' humpVar: Variance of a numeric vector x restricted to only values x > min
#'
#' @param x numeric vector
#' @param min numeric value
#'
#' @return variance of entries of x greter than min (numeric)
#' @export
#'
#' @examples
#' \dontrun{
#' humpVar(c(0,1,1,3,0,5), min=0)
#' }
humpVar=function(x, min=0) {
  return(var(x[x>min]))
}

#' Calculate dispersion (variance/mean)
#'
#' @param x numeric vector
#'
#' @return dispersion value
#' @export
#'
#' @examples
#' #' \dontrun{
#' disp_x(c(0,1,1,3,0,5))
#' }
disp_x = function(x){
  return(var(x) / mean(x))
}


#' Calculate coefficient of variation (standard deviation/mean)
#'
#' @param x numeric vector
#'
#' @return coefficient of variation (CV) value
#' @export
#'
#' @examples
#' #' #' \dontrun{
#' cv_x(c(0,1,1,3,0,5))
#' }
cv_x = function(x){
  return(sd(x) / mean(x))
}
