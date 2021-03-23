source("Code/covid-plotutils.R")
source("Code/covid-modelutils.R")

owiddat      <- read.csv("Data/owid-covid-data.csv")
owiddat$date <- as.Date(owiddat$date, tryFormats = c("%Y-%m-%d"))
multilist <- list()

multidates <- list(
  #"Italy"         = list(c("2020-12-16", "2021-01-01"),
  #                       c("2021-01-02", "2021-01-30")),
  #"United States" = list(c("2020-11-16", "2021-01-06"),
  #                       c("2021-01-07", "2021-01-30")),
  #"Ireland"       = list(c("2020-12-16", "2021-01-07"),
  #                       c("2021-01-08", "2021-01-30"))
  "Ireland"       = list(c("2020-02-29", "2020-04-15"),
                         c("2020-04-16", "2020-06-09"))
)

multiphasePlots <- function(country, dates, data){
  plots <- list()
  crows <- grep(country, data$location)
  countrydat <- data.frame(date = data$date[crows], 
                           xn = data$new_cases[crows], yn = data$total_cases[crows])
  if(nrow(countrydat[countrydat$date < dates[[1]][1],]) == 0)
    beforecumcases <- 0
  else
    beforecumcases <- sum(countrydat$xn[countrydat$date < dates[[1]][1]])
  
  countrydat  <- countrydat[countrydat$date >= dates[[1]][1],]
  countrydat  <- countrydat[countrydat$date <= dates[[length(dates)]][2],]
  latest_date <- countrydat$date[nrow(countrydat)]
  
  forecastlen <- 5
  
  multimodx <- function(x, multix, pars, oldp = rep(1,10), start=FALSE, len = 0){
    q <- floor(pars[1])
    a <- pars[2]
    b <- pars[3]
    fitstd <- length(multix)+1
    
    if(start){
      multix[fitstd:(fitstd+q-1)] <- x[1:q]
      for(i in (fitstd+q):(fitstd+length(x)+len-1)){
        multix[i] <- (1-b)*multix[i-1] + a*multix[i-q]
      }
    } else {
      for(i in (fitstd):(fitstd+length(x)+len-1)){
        multix[i] <- (1-b)*multix[i-1] + a*multix[i-q]
      }
    }
    return(multix)
  }
  
  multimodxper <- function(par, q=7, x, multix, oldp = rep(1,10), start=FALSE, len = 0){
    #a,b,c1,c2,p1,p2,n1,n2
    #first day of this phase
    fitstd <- length(multix)+1
    an  <- par[1]*(1+par[3]*sin(2*pi*(1:(fitstd+length(x)+len-1) - par[7])/par[5]))
    bn  <- par[2]*(1+par[4]*sin(2*pi*(1:(fitstd+length(x)+len-1) - par[8])/par[6]))
    
    if(start){
      multix[fitstd:(fitstd+q-1)] <- x[1:q]
      for(i in (fitstd+q):(fitstd+length(x)+len-1)){
        multix[i] <- (bn[i]*(1-bn[i-1]))*multix[i-1]/bn[i-1] +
          (an[i-q]*bn[i])*multix[i-q]/bn[i-q]
      }
    } else {
      for(i in fitstd:(fitstd+length(x)+len-1)){
        multix[i] <- (bn[i]*(1-bn[i-1]))*multix[i-1]/bn[i-1] +
          (an[i-q]*bn[i])*multix[i-q]/bn[i-q]
      }
    }
    return(multix)
  }
  
  #Specific dates
  multimodel  <- c()
  multimodelp <- c()
  phasepars   <- list()
  for(i in 1:length(dates)){
    phase    <- dates[[i]]
    phasedat <- countrydat[countrydat$date >= phase[1] & countrydat$date <= phase[2],]
    
    #get basic model for each phase first in order to get 
    # starting a and b to guess for periodic an and bn
    
    aseq    <- seq(from = 0.1, to = 2.5, length.out = 50)
    bseq    <- seq(from = 0.1, to = 0.9, length.out = 50)
    qseq    <- 6:8
    normdat <- expand.grid(q = qseq, a = aseq, b = bseq)   
    
    if(i == 1)
      abnorm <- apply(normdat, 1, function(x) modnorm(multimodx(phasedat$xn, multimodel, x, start=TRUE), phasedat$xn))
    else
      abnorm <- apply(normdat, 1, function(x) modnorm(multimodx(phasedat$xn, multimodel, x, oldp = phasepars[[i-1]])[length(multimodel) + 1:nrow(phasedat)], phasedat$xn))
    
    normalize <- function(x){
      return((x-min(x))/(max(x)-min(x)))
    }
    normdat$abnorm  <- abnorm
    
    tileoptim <- normdat[which.min(normdat$abnorm),1:3]
    optimpars <- c(tileoptim$q, tileoptim$a, tileoptim$b)
    
    q <- optimpars[1]
    a <- optimpars[2]
    b <- optimpars[3]
    
    phasepars[[i]] <- round(optimpars,3)
    
    if(i == 1 & i != length(dates))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, start=TRUE)
    if(i == 1 & i == length(dates))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, start=TRUE, len=forecastlen)
    if(i > 1 & i == length(dates))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, oldp = phasepars[[i-1]], len=forecastlen)
    if(length(dates) > 2 & i %in% 2:(length(dates)-1))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, oldp = phasepars[[i-1]])
    
    aseqper <- a*seq(from = 0.7, to = 1.3, length.out = 10)
    bseqper <- b*seq(from = 0.7, to = 1.3, length.out = 10)
    c_1seq  <- c_2seq <- seq(0.04, 0.2, length.out = 10)
    n_1seq  <- n_2seq <- c(1,7)
    p_1seq  <- p_2seq <- 6:7
    normdatp <- expand.grid(a  = aseqper, b  = bseqper,
                            c1 = c_1seq,  c2 = c_2seq,
                            p1 = p_1seq,  p2 = p_2seq,
                            n1 = n_1seq,  n2 = n_2seq)
    
    if(i == 1)
      pernorm <- apply(normdatp, 1, function(par) modnorm(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp, start=TRUE), phasedat$xn))
    else
      pernorm <- apply(normdatp, 1, function(par) modnorm(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp)[length(multimodelp)+1:nrow(phasedat)], phasedat$xn))
    
    normdatp$pernorm <- pernorm
    
    newnormdatp <- normdatp %>% top_n(pernorm, n = -25)
    
    if(i == 1)
      pernormy <- apply(newnormdatp, 1, function(par) modnorm(beforecumcases+xntoyn(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp, start=TRUE)), phasedat$yn))
    else
      pernormy <- apply(newnormdatp, 1, function(par) modnorm(beforecumcases+xntoyn(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp))[length(multimodelp)+1:nrow(phasedat)], phasedat$yn))
    
    newnormdatp$pernormy <- pernormy
    newnormdatp$combnorm <- normalize(newnormdatp$pernorm) + normalize(newnormdatp$pernormy)
    
    peroptim <- as.numeric(newnormdatp[which.min(newnormdatp$combnorm),1:8])
    
    phasepars[[i]] <- c(phasepars[[i]],round(peroptim,3))
    
    if(i == 1 & i != length(dates))
      multimodelp <- multimodxper(peroptim, q = q, phasedat$xn, multimodelp, start=TRUE)
    if(i == 1 & i == length(dates))
      multimodelp <- multimodxper(peroptim, q = q, phasedat$xn, multimodelp, start=TRUE, len=forecastlen)
    if(i > 1 & i == length(dates))
      multimodelp <- multimodxper(peroptim, q = q, phasedat$xn, multimodelp, len=forecastlen)
    if(length(dates) > 2 & i %in% 2:(length(dates)-1))
      multimodelp <- multimodxper(peroptim, q = q, phasedat$xn, multimodelp)
  }
  
  cols <- list(
    xn       = wes_palettes$Zissou1[1],
    yn       = wes_palettes$Darjeeling2[2],
    multixn  = wes_palettes$Darjeeling1[1],
    multiyn  = wes_palettes$Darjeeling1[1],
    multip   = "magenta4",
    x3       = wes_palettes$FantasticFox1[2]
  )
  
  labs <- list(
    xn = list(bquote(.(country)*","~x[n]*"*=new cases/day, actual till"~.(format.Date(latest_date,"%d.%m.%Y")))),
    yn = list(bquote(.(country)*","~y[n]*"*=cumulative cases, actual till"~.(format.Date(latest_date,"%d.%m.%Y"))))
  )
  
  multimodnormval  <- modnorm(multimodel[1:length(countrydat$xn)], countrydat$xn)
  multimodnormyval <- modnorm(beforecumcases+xntoyn(multimodel[1:length(countrydat$xn)]), countrydat$yn)
  
  b.roman <- function(x){ return(paste0("(", as.roman(x), ")"))}
  
  multilabd <- c()
  for(i in 1:length(dates)){
    multilabd <- c(multilabd, paste(b.roman(i), "from", format.Date(as.Date(as.character(dates[[i]][1])),"%d.%m")))
  }
  multilabd <- paste0(multilabd, collapse = "; ")
  
  multilabp <- c()
  for(i in 1:length(dates)){
    multilabp <- c(multilabp, paste0(b.roman(i), " a=", phasepars[[i]][[2]], ", b=", phasepars[[i]][[3]]))
  }
  multilabp <- paste0(multilabp, paste0("; q=", phasepars[[i]][[1]]), collapse = "; ")
  
  labs$multixn <- list(bquote("base"~.(length(dates))*"-phase model"~x[n]*"=new cases/day: "*
                                .(multilabd)*
                                "; ||x*-x||="*.(multimodnormval)*"  "*
                                .(multilabp)))
  labs$multiyn <- list(bquote("base"~.(length(dates))*"-phase model"~y[n]*"=cumulative cases: "*
                                .(multilabd)*
                                "; ||y*-y||="*.(multimodnormyval)*"  "*
                                .(multilabp)))
  
  mavgx1 <- movingavg(countrydat$xn[!is.na(countrydat$xn)])
  
  countrydat$mavgx3 <- movingavg(mavgx1)
  
  x3norm  <- modnorm(countrydat$xn,countrydat$mavgx3)
  labs$x3 <- list(bquote("moving average x*(3); ||x*(3)-x*||="*.(x3norm)))
  
  modeldat <- data.frame(date     = c(countrydat$date,as.Date(latest_date) + 1:forecastlen),
                         multixn  = multimodel,
                         multiyn  = beforecumcases + xntoyn(multimodel),
                         multipxn = multimodelp,
                         multipyn = beforecumcases + xntoyn(multimodelp))
  
  plots[["xn"]] <- plot_multixn(countrydat, modeldat, cols, labs)
  
  plots[["yn"]] <- plot_multiyn(countrydat, modeldat, cols, labs)
  
  multilabp <- c()
  for(i in 1:length(dates)){
    multilabp <- c(multilabp,  paste0(b.roman(i),  " a=", phasepars[[i]][[2]], ", b=", phasepars[[i]][[3]]))
  }
  multilabp <- paste0(multilabp, paste0("; q=", phasepars[[i]][[1]]), collapse = "; ")
  
  labs$multipxn <- list(bquote("periodic"~.(length(dates))*"-phase model"~x[n]*"=new cases/day: "*
                                 .(multilabd)*
                                 "; ||x*-x||="*.(multimodnormval)*"  "*
                                 .(multilabp)))
  labs$multipyn <- list(bquote("periodic"~.(length(dates))*"-phase model"~y[n]*"=cumulative cases: "*
                                 .(multilabd)*
                                 "; ||y*-y||="*.(multimodnormyval)*"  "*
                                 .(multilabp)))
  
  plots[["perxn"]] <- plot_multiperxn(countrydat, modeldat, cols, labs)
  
  plots[["peryn"]] <- plot_multiperyn(countrydat, modeldat, cols, labs)
  
  return(plots)
}

for(country in names(multidates)){
  multilist[[country]] <- multiphasePlots(country, multidates[[country]], owiddat)
}
