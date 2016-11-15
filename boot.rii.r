## RII bootstrapping calculations
## Ordinary bootstraping conducted using bias corrected accelerated 95% confidence intervals
##  DiCiccio, T.J. and Efron B. (1996) Bootstrap confidence intervals (withDiscussion). Statistical Science, 11, 189-228
## http://math.usask.ca/~longhai/doc/talks/slide-bootstrap.pdf
## Armas, C., Ordiales, R., & Pugnaire, F. I. (2004). Measuring plant interactions: a new comparative index. Ecology, 85(10), 2682-2686.


boot.rii <- function(x, treatment, treat.var, control.var, response, iter) {
  boot.function <- function(data, indices){
    
    s1 <- subset(data, data[,treatment] == treat.var, response)
    o1 <- subset(data, data[,treatment] == control.var, response)
    s1.samp <- s1[indices,1]
    o1.samp <- o1[indices,1]
    rii.vals <- (s1.samp - o1.samp) / (s1.samp+o1.samp)
    rii.vals[is.na(rii.vals)] <- 0
    coefs <-mean(rii.vals)
    return(coefs)
    
  }
  
  boot1 <- boot(x, statistic=boot.function, 999)
  boot.ci1 <- boot.ci(boot1, type="bca")
  return(data.frame(mean=boot1$t0,lower=boot.ci1$bca[4],upper=boot.ci1$bca[5]))
}