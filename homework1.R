#Question 1
names <- c("Hayden", "Jordan", "Brian", "Laurel")
names


#Question 2
genders <- c("Male", "Male", "Male", "Female")
genders


#Question 3
people <- data.frame(NAMES = names, GENDERS = genders)
people


#Question 4
people$TOWNS <- c("Rochester", "Rochester", "Buffalo", "Syracuse")
people


#Question 5
#install.packages("ggplot2")
library("ggplot2")


#Question 6
test_data <- rnorm(10000, mean = 20, sd = 4)
hist(test_data)


#Question 7
#help(sprintf)
#help(paste)


#Question 8
movies <- c("Avengers", "Harry Potter", "Saving Private Ryan", "Incredibles", "Step Brothers")
actors <- c("Will Ferrell", "Tom Hanks")
concat <- c(movies,actors)
concat

#Question 9
sum_three <- function(num1, num2, num3) {
  return (num1 + num2 + num3)
}
#sum_three(1,3,5)


#Bonus
mutate_vector <- function(first, last, vector){
  vector[1] <- first 
  vector[length(vector)] <- last
  return(vector)
}
#mutate_vector(1,9, c(2,4,2,84,3,5,6))