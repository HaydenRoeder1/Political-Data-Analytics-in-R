
require(foreign)
require(ggplot2)
set.seed(12345)
setwd("D:/School/RClass/Homework5")
data_new <- read.csv("lecture5.csv")
View(data_new)
#Quesiton 1
mean_val <- function(vec = c()){
  sum <- 0
  for(i in vec){
    sum <- sum + i
  }
  return(sum / length(vec))
}
test_vec <- c(1,4,8,9,20)
mean_val(test_vec) == mean(test_vec, na.rm = TRUE)



#Question 2
ref_bar2 <- ggplot(data_new, aes(refreal)) + 
  geom_bar(colour='white') + 
  labs(title = 'Responses to "Refugees are real refugees"', subtitle = 'ESS 2002 Wave',
       x=expression(italic('Strongly Agree to Strongly Disagree')), y = 'Number of responses') + 
  theme_bw() +
  theme(axis.line = element_line(color = "grey"),
        panel.border = element_blank(), 
        axis.title = element_text(face = 'italic'),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "red"))
ref_bar2



#Question 3
#Part A
data_plot <- data_new[complete.cases(data_new$immjob, data_new$immwage),]
median(data_plot$immjob) #Median = 5
median(data_plot$immwage) #Median = 3
table(data_plot$immjob) #Mode = 5
table(data_plot$immwage) #Mode = 2
#Part C
imm_graph <- ggplot(data_plot, aes(data_plot$immwage, data_plot$immjob)) +
geom_bar(colour='black', stat = 'summary', fun.y = 'median') +
theme_bw() +
labs(title = "Immigrant job market impact relative to immigrant wage impact opinions", x = ("Immigrants decrease wages -> Immigrants increase wages"), y = ("Immigrants take away jobs -> Immigrants create jobs"))
imm_graph
#Part D
data_plot <- data_new[complete.cases(data_new$immjob, data_new$immwage,data_new$edulvla),]
data_plot[which(data_plot$immjob < 4), 'immjob_modified'] <- 'Take Away Jobs'
data_plot[which(data_plot$immjob >= 4 & data_plot$immjob <= 6), 'immjob_modified'] <- 'Neutral Job Impact'
data_plot[which(data_plot$immjob > 6), 'immjob_modified'] <- 'Create Jobs'
imm_graph2 <- ggplot(data_plot, aes(data_plot$immwage)) + geom_bar(aes(fill = (data_plot$immjob_modified)), colour = "white", position = "dodge")





