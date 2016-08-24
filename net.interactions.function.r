## Alex Filazzola and Christopher Lortie
## August 24 2016
#Introduction ####
#W is a common character for fitness in models, and we adopt herein to match notational trends.
#Background here: https://en.wikipedia.org/wiki/Fitness_(biology)
#Function and review published here: http://link.springer.com/article/10.1007/s11258-016-0604-y
#Lortie, C. J., Filazzola, A., Welham, C., & Turkington, R. A cost–benefit model for plant–plant interactions: a density-series tool to detect facilitation. Plant Ecology, 1-15.
#W is an estimate or probability of fitness. Here, we fully assume and model relative.
#W1 is an estimate of fitness derived from a single with only measure (i.e. biomass or seed production but not both)
#W2 is a net estimate of the likelihood of fitness when two responses to a factor such as density are measured.
#W3 is a net estimate when 3 or more reponses are measured.

#requirements####
#need column called mean
#need column called se
#load dplyr
#for pairwise comparisons, the response column is required. When comparing 3 or more responses, there is no need to specify column
require(dplyr)

W.net <- function(x, response){ 
  W.2 <- function(x, response){ ## function for two response estimates
    resp2 <- match(x[,response], levels(x[,response]))
    x1 <- subset(x, resp2==unique(resp2)[1])
    y1 <- subset(x, resp2==unique(resp2)[2])
    W <- x1$mean*y1$mean #calculate fitness metric
    var.w <- (((x1$mean^2)*y1$se)+((y1$mean^2)*x1$se))-(x1$se*y1$se) # calculate variance
    W.2 <- cbind(x1[,1:(ncol(x1)-3)],W,var.w) 
    return(W.2)
  }
  W.3 <- function(x) { ## function for three or more response estimates
    W <- x %>%  summarise(W=prod(mean)) #calculate fitness metric
    var.w <- mutate(x, var.W = se/mean)
    var.w <- var.w %>% summarise(var.w=sum(var.W))
    W <- inner_join(W, var.w)
    metrics<- mutate(W, var.w = W * var.w) # calculate variance
    metrics[is.na(metrics)] <- 0
    metrics <- as.data.frame(metrics)
    return(metrics)
  }
  
  if(missing(response))
    W.3(x) ## syntax: x = df %>% group_by(factor1, factor2, ... factori)
  else
    W.2(x, response) ## syntax:  x = df, response = "estimate column"
}
