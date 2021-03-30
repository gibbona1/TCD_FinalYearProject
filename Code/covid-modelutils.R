modnorm <- function(x,modx) return(floor(sum(abs(x-modx))/length(x)))

xntoyn <- function(xn) return(cumsum(xn))

basicmodx <- function(x, pars, len = 0){
  q <- floor(pars[1])
  a <- pars[2]
  b <- pars[3]
  modx <- x[1:q]
  for(i in (q+1):(length(x)+len)){
    modx[i] <- (1-b)*modx[i-1] + a*modx[i-q]
  }
  return(modx)
}

norm <- function(par, x) return(modnorm(x,basicmodx(x, par)))

normy <- function(par, x, y) return(modnorm(y,xntoyn(basicmodx(x, par))))

normalize <- function(x) return((x-min(x))/(max(x)-min(x)))

basef <- function(lambda,par) {
  return(lambda^(par[1]+1)-(1-par[3])*lambda^(par[1])-par[2])
}

basefprime <- function(lambda,par) {
  return((par[1]+1)*lambda^(par[1])-(1-par[3])*par[1]*lambda^(par[1]-1))
} 

normC <- function(par,x,r){
  return(modnorm(x, par*r^(0:(length(x[!is.na(x)])-1))))
}

movingavg <- function(x){
  mavgx <- (x[1]+x[2])/2
  for(i in 2:(length(x)-1)){
    mavgx[i] <- sum(x[(i-1):(i+1)])/3
  }
  mavgx[length(x)] <- (x[length(x)-1]+x[length(x)])/2
  return(mavgx)
}

normper <- function(par, q, x) return(modnorm(x,modxper(par,q,x)))

modxper <- function(par, q, x, len = 0){
  #a,b,c1,c2,p1,p2,n1,n2
  an   <- par[1]*(1+par[3]*sin(2*pi*(1:(length(x)+len) - par[7])/par[5]))
  bn   <- par[2]*(1+par[4]*sin(2*pi*(1:(length(x)+len) - par[8])/par[6]))
  bn[bn>1] <- 1
  modx <- x[1:q]
  for(i in (q+1):(length(x)+len)){
    modx[i] <- (bn[i]*(1-bn[i-1]))*modx[i-1]/bn[i-1] +
      (an[i-q]*bn[i])*modx[i-q]/bn[i-q]
  }
  return(modx)
}