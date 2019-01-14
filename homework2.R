#set.seed(12345)

#Question 1
min_val <- function(vector = c(0)) {
  min <- vector[1]
  for (x in vector[2:length(vector)]) {
    min <- min*(x > min) + x*(x <= min)
  }
  return(min)
}
income <- c(15000.67, 100567.55, 54436.31, 51000.54, 200034.93, 34000.79)
test <- c(5,8,2,1,3,1,9,4)
min_val(income)
min_val(test)



#Question 2
range <- function(vector = c(0)){
  min <- vector[1]
  max <- vector[1]
  for(x in vector[2:length(vector)]){
    min <- min*(x > min) + x*(x <= min)
    max <- max*(x < max) + x*(x >= max)
  }
  return(max-min)
}
income <- c(15000.67, 100567.55, 54436.31, 51000.54, 200034.93, 34000.79)
test <- c(5,8,2,1,3,1,9,4)
range(income)
range(test)



#Question 3
palindrome <- function(string){
  len <- nchar(string)
  substrLen <- as.integer(len / 2)
  str1 <- substr(string, 1, substrLen)#first half of string
  isPalindrome <- TRUE
  if(len %% 2 == 0) #length of string is even
  {
    str2 <- substr(string, substrLen + 1, len) #2nd half of string 
  } else{
    str2 <- substr(string, substrLen + 2, len)#2nd half of string excluding middle character which won't effect whether or not its a palindrome
    
  }
  for(x in substrLen){
    if(substr(str1, x, x) != substr(str2, substrLen - x + 1, substrLen - x + 1)){#Compares each character of the substrings, reverses str2
      isPalindrome <- FALSE
    }
  }
  return(isPalindrome)
}
test <- "racecar"
palindrome(test)
test <- "abcdef"
palindrome(test)



#Question 4
magicWell <- function(a = 1275, b = 362, n = 2){
  sum <- 0
  for(x in 1:n){
    sum <- sum + (a*b)
    a <- a + 1
    b <- b + 1
  }
  print(sum)
  return(sum)
}
magicWell()



#Question 5
abortion <- c()
astances <- c("Pro-Choice", "Pro-Life", "Undecided")
guncontrol <- c()
gstances <- c("For Gun Control", "Against Gun Control", "Don't Know")
party <- c()
pstances <- c("Democrat", "Republican", "Independent")
for(x in 1:100){
  abortion <- c(abortion, astances[max(1, rbinom(1, 3, 0.5))])
  guncontrol <- c(guncontrol, gstances[max(1, rbinom(1, 3, 0.5))])
  party <- c(party, pstances[max(1, rbinom(1, 3, 0.5))])
}
people = data.frame(Abortion_Stance = abortion, Gun_Control_Stance = guncontrol, Party = party)


#Question 6
sum_vector <- function(vector = c(0)) {
  total <- 0
  for (x in vector) total <- total + 1
  return(total)
}

AGCandPC <- c()
for(x in 1:length((abortion))){
  if(abortion[x] == "Pro-Choice" && guncontrol[x] == "Against Gun Control"){
    AGCandPC <- c(AGCandPC, x) #saves index of person with desired responses
  }
}
sumAGCandPC <- sum_vector(AGCandPC)
DemAGCPC <- c()
for(x in AGCandPC){
  if(party[x] == "Democrat"){
    DemAGCPC <- c(DemAGCPC, x)
  }
}
sumDem <- sum_vector(DemAGCPC)


#Question 7
data_toplot <- table(people$Party)
barplot(data_toplot)
data_toplot <- table(people$Abortion_Stance, people$Party)
barplot(data_toplot, legend.text = TRUE, beside = TRUE, xlab = "Party ID")

#Bonus
for (x in 1:length(letters)) {
  for (y in 1:length(letters)) {
    print(paste(letters[x], letters[length(letters) - y + 1], sep=""))
  }
}



