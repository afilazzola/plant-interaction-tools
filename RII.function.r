## Alex Filazzla
## August 24 2016
## function for rii
#Armas, C., Ordiales, R., & Pugnaire, F. I. (2004). Measuring plant interactions: a new comparative indedata. Ecology, 85(10), 2682-2686.

# x = dataframe
# j = columns to keep for the RII data set
# y = response variables for rii function to operate on
# need to replace "treat.control" with the column that contains the treatment and control for the function to operate on. 
# need to replace "treatment" and "control" with associated treatments and controls. 


rii <- function(x, j, var)
{
s1 <- subset(x, treat.control == "treatment", select=var)
o1 <- subset(x, treat.control == "control", select=var)
return1 <- (s1 - o1) / (s1+o1)
x1 <- x[seq(1, nrow(x), by = 2),]
return2 <- cbind(x1[j], return1)
return2[is.na(return2)] <- 0
return2
}
