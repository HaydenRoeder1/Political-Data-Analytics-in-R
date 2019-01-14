require(foreign)
require(ggplot2)
set.seed(12345)
setwd("D:/School/RClass/Homework3")
#import data into R
data_new <- read.csv("assignment4.csv")
View(data_new)
names(data_new)

#Question 1, part a in text document
#SPEECH
summary(data_new$SPEECH)
table(data_new$SPEECH) #Mode = 1 
data_plot <- data_new[complete.cases(data_new$SPEECH),]
data_plot[which(data_plot$SPEECH == 0), 'speech_modified'] <- 'Low Freedom of Speech'
data_plot[which(data_plot$SPEECH == 1), 'speech_modified'] <- 'Medium Freedom of Speech'
data_plot[which(data_plot$SPEECH == 2), 'speech_modified'] <- 'High Freedom of Speech'
speech_plot <- ggplot(data_plot, aes(x = factor(speech_modified))) +
  geom_bar(aes(fill = speech_modified), position = 'dodge')
speech_plot
#totalprotest
summary(data_new$totalprotest)
table(data_new$totalprotest) #Mode = 0 
data_plot <- data_new[complete.cases(data_new$totalprotest),]
tprotest_plot <- ggplot(data_plot, aes(x = factor(totalprotest))) +
  geom_bar()
tprotest_plot
#protest
summary(data_new$protest)
table(data_new$protest) #Mode = 1 
data_plot <- data_new[complete.cases(data_new$protest),]
data_plot[which(data_plot$protest == 0), 'protest_modified'] <- 'No Protests'
data_plot[which(data_plot$protest == 1), 'protest_modified'] <- 'One or More Protests'
protest_plot <- ggplot(data_plot, aes(x = factor(protest_modified))) +
  geom_bar()
protest_plot


#Question 2
data_new$logGDP <- log(data_new$GDPpercapitacurrentUSNY)
gdppop_plot <- ggplot(data_new, aes(x = logGDP, y = PopulationgrowthannualSP)) +
  geom_point() 
gdppop_plot <- gdppop_plot + theme_bw()
gdppop_plot


#Question 3
data_plot <- data_new[complete.cases(data_new$polity2, data_new$totalprotest),]
data_plot[which(data_plot$totalprotest == 0), 'tprotest_modified'] <- 'No Protests'
data_plot[which(data_plot$totalprotest > 0 & data_plot$totalprotest < 6), 'tprotest_modified'] <- 'Few Protests'
data_plot[which(data_plot$totalprotest > 5), 'tprotest_modified'] <- 'Many Protests'

data_plot[which(data_plot$polity2 <= -6), 'polity_modified'] <- 'autocracy'
data_plot[which(data_plot$polity2 > -6 & data_plot$polity2<6), 'polity_modified'] <- 'anocracy'
data_plot[which(data_plot$polity2>=6), 'polity_modified'] <- 'democracy'
protest_plot <- ggplot(data_plot, aes(x = factor(tprotest_modified))) +
  geom_bar(aes(fill = polity_modified), position = 'dodge')
protest_plot


#Question 4
data_plot <- data_new[complete.cases(data_new$polity2, data_new$totalprotest),]
count <- 0
for(i in 1:length(data_plot$polity2)){
  if(data_plot$polity2[i] >= 6 & data_plot$totalprotest[i] >= 2){
    count <- count+1
  }
}
count #682



#Bonus
data_plot <- data_new[complete.cases(data_new$polity2),]
average <- 0
sum <- 0
for(i in 1:201){
  index <- floor(runif(1, min = 1, max = length(data_plot$polity2)))
  sum <- sum + data_plot$polity2[index]       
  average <- sum / i
}
average
summary(data_plot$polity2)
#Estimated is usually within .5 of the true value 