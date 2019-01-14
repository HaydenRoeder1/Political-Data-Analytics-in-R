require(foreign)
set.seed(12345)
setwd("D:/School/RClass/Homework4")


#Question 1
data_sforce <- read.csv("sforce.csv")
data_dollars <- read.csv("dollars.csv")
data_mergedsales <- merge(data_sforce, data_dollars, by = c('region'), all = TRUE)



#Question 2
data_genders <- read.csv("genders.csv")
data_incomes <- read.csv("incomes.csv")
data_mergedgenderincome <- merge(data_genders, data_incomes, by = c('First.Name', 'Last.Name'), all = TRUE)



#Question3
data_gdp <- read.csv("gdp.csv")
require(reshape)
data_wide <- cast(data_gdp, state_long ~ year, mean, value = 'logGDPpc')
for(i in 2:length(names(data_wide))){ #loops through each year column
  names(data_wide)[i] <- paste("gdp", names(data_wide)[i], sep = "_")
}



#Question 4
data_long <- melt(data_wide, id = c('state_long'))
View(data_long)
names(data_long)[2] <- "logGDPpc"


#Bonus
leastFactorial <- function(n = 1){
  counter <- 2
  factorialSum <- 1
  while(factorialSum < n){ #Loops until the factorial "Sum" is greater than n
    factorialSum <- factorialSum * counter #Calculates the next factorial
    counter <- counter + 1
  }
  return(factorialSum)
}

leastFactorial(25)
