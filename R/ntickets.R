#' A function determining number of tickets to be sold to optimize the overbooking problem
#'
#' @param N the number of available seats on the flight
#' @param p the probability that a person shows up
#' @param gamma the probability of overbooking
#'
#' @return A discrete plot, a continuous curve, and a table of values
#' @export
#'
#' @examples
#' ntickets(N=400,p=0.94,gamma=0.02)
#'




ntickets = function(N=200,p=0.95,gamma=0.02){
  #Discrete
  i = N
  while(1-gamma-pbinom(N,i,p) < 0){
    i = i + 1
  }
  i - 1
  j = i
  while(1-pbinom(N,j,p) < 0.99){
    j = j + 1
  }
  x = (N:j)
  main = paste0("Objective vs n to find optimal tickets sold \n (",i,") gamma=",gamma," N=",N," discrete")
  plot(x,1-gamma-pbinom(N,x,p),ylim = c(0,1),ylab="Objective",xlab="n",main=main)
  abline(v=i,col="red")
  abline(h=1-gamma-pbinom(N,i,p),col="red")

  #Continuous
  f = function(x,gamma,p) (abs(N-qnorm(1-gamma,mean=x*p,sd=sqrt(x*p*(1-p)))+0.5))
  h=optimize(f,interval=c(i-5,i+5),gamma=0.02,p=0.95)
  hv=h$minimum
  hh=h$objective
  main2= paste0("Objective vs n to find optimal tickets sold \n (",hv,") gamma=",gamma," N=",N," continuous")
  curve(01-gamma-pnorm(N+0.5,mean=(x*p),sd=sqrt(x*p*(1-p))),xlim=c(N,j),xlab="n",ylab="Objective",main=main2)
  abline(v=hv,col="red")
  abline(h=hh,col="red")

  #List
  list(nd=i,nc=hv,N=N,p=p)
}
