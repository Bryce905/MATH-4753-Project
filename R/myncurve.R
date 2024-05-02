#' Creates a normal curve and displays the probability up to a certain point
#'
#' @param mu the mean
#' @param sigma the number to choose from
#' @param a the upper end of the area
#'
#' @return a curve
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist polygon segments text
#' @importFrom stats dnorm optimize pbinom pnorm qnorm quantile rbinom
#' @examples
#' myncurve(mu=10,sigma=2,a=7)

myncurve = function(mu=10, sigma=2,a=7){
  x=NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
  xcurve = seq(mu-3*sigma,a, length = 100)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(c(0,xcurve,a),c(0,ycurve,0),col="Red")
  area = pnorm(a,mu,sigma)
  area = round(area,4)
  text(x=mu-sigma,y=0.06, paste("Area = ", area, sep=""))
  list(mu = mu, sigma = sigma, a = a)
}
