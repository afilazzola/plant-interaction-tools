## Bootstrap RII
## Alex Filazzola
## August 24 2016
## Description
# RII is an effect estimate proposed by Armas et al 2004 to compare a plants performance with neighbours and without.
# It was generated from a similar index that compares a subjects success between control and treatment plots (t-c)/(t+c)
# Heterogeneity stats are typically available for comparisons, but for simpler, within study examples, permutations tests can be used with parametric statistics. 
# Michalet et al. 2014 conducted these analyses in a 2014 Functional Ecology paper between cushion and open microsites. 

##Citations
#Michalet, R., Schöb, C., Lortie, C. J., Brooker, R. W., & Callaway, R. M. (2014). Partitioning net interactions among plants along altitudinal gradients to study community responses to climate change. Functional Ecology, 28(1), 75-86.
#Armas, C., Ordiales, R., & Pugnaire, F. I. (2004). Measuring plant interactions: a new comparative index. Ecology, 85(10), 2682-2686.

## This function calculates RII by subsetting the data and randomizing the treatment and control plots. It will work with unequal samples.
# x = dataframe
# treatment = column with treatment and control variables
# treatment.var = treatment specified in quotations
# control.var = control specified in quotations
# variable = response variable of interest
# number of times to commit bootstrapping. 

boot.rii <- function(x, treatment, control.var, treat.var, variable,iteration){
s1 <- subset(x, x[,treatment] == treat.var, select=variable) ## subset the treatment group
o1 <- subset(x, x[,treatment] == control.var, select=variable)  ## subset the control group
min.samp <- min(length(s1[,1]),length(o1[,1]))  ## minimum number of samples
rii.avg.total <- c() ## set up blank mean vector
rii.se.total <- c() ## set up blank mean vector
for (i in 1:iteration){ ##loop the sampling of treatment and control groups and calculate RII
set.seed(i) ## control randomization to return same values
treat.samp<- sample(s1[,variable],min.samp)
control.samp<-sample(o1[,variable],min.samp)
return1 <- (treat.samp - control.samp) / (treat.samp+control.samp)
return1[is.na(return1)] <- 0
rii.avg <- mean(return1)
rii.se <- se(return1)
rii.avg.total <- c(rii.avg.total,rii.avg) ## bind all the means together
rii.se.total <- c(rii.se.total,rii.se) ## bind all the confidence intervals together
}
rii.avg <- mean(rii.avg.total)
rii.se <- mean(rii.se.total)
treat <- c(treatment)
rii.results <- data.frame(factor=treat,average=rii.avg,error=rii.se)
print(rii.avg.total)
}

## example
consumer.effect <- boot.rii(data2014, treatment="Exclosure", control.var="N", treat.var="Y", variable="Biomass", 999) ## consumer
microsite.effect <- boot.rii(dat2014, treatment="Microsite", control.var="Open", treat.var="Shrub", variable="Biomass", 999) ## microsite
water.effect <- boot.rii(dat2014, treatment="Watered", control.var="N", treat.var="Y", variable="Biomass", 999) ## water

rii.vals <- rbind(consumer.effect,microsite.effect,water.effect)
rii.vals[,"Year"] <- rep("2014",3)
rii.vals

