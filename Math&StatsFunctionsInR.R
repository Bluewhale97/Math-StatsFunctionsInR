#1. numerical and character functions
abs(x) #absolute value abs(-4) retruns 4
sqrt(x) #square root, sqrt(25) returns 5
ceiling(x) #smallest integer not less than x, ceiling(3.475) returns 4
floor(x) #largest integer not greater than x, floor(3.475) retrurns 3
trunc(x) #integer formed by truncating values in x toward 0, trunc(5.99) retruns 5
round(x, digits=n) #rounds x to the specified number of decimal places round(3.475, digits=2) returns 3.48
signif(x, digits=n) #rounds x to the specified number of significant digits, signif(3.475, digits=2) returns 3.5.
cos(x), sin(x), tan(x)
acos(x), asin(X), atan(X) #arc-cosine, arc-sine and arc-tangent
cosh(x), sinh(x), tanh(x) #hyperbolic cosine, sine, and tangent sinh(2) returns 3.627
acosh(x), asinh(x), atanh(x) #hyperbolic arc-cosine, arc-sine, and arc-tangent asinh(3.627) returns 2
log(x, base=n) #logarithm of x to the base n
log(x)
log10(x) #for convenience: log(x) is the natural logarithm, log10(x) is the common logarithm
exp(x) #e^x, exp(2.3026) returns 10
#when these functions are applied to numeric vectors, matrices or data frames, they operate on each individual value, for example, sqrt(c(4,15,25)) returns c(2,4,5)

#2 statistical functions
mean(x)
median(x)
sd(x)
var(x)
mad(x) #median absolute deviation, mad(c(1,2,3,4)) returns 1.48
mad(c(1,2,3,4)) #constant*cMedian(abs(x-center)), constant==1.48
quantile(x, probs) #quantile where x is the numeric vector, where quantiles are desired and probs is a numeric vector with probabilities in [0,1]. #30th and 80th is quantile(x, c(.3, .8))
range(x) #x<-c(1,2,3,4), range(x) returns c(1,4). diff(range(x)) returns 3
sum(x) #sum(c(1,2,3,4)) returns 10
diff(x, lag=n) #lagged differences, with lag indicating which lag to use, the default lag is 1. x<-c(1,5,23,29), diff(x) returns c(4,18,6)
min(x)
max(x)
scale(x, center=T, scale=T) #column center or standardize data object x.

#3. case study: calculating the mean and sd
x<-c(1,2,3,4,5,6,7,8)
mean(x)
sd(x)
n<-length(x)
meanx<-sum(x)/n
css <-sum((x-meanx)^2)
sdx <-sqrt(css/(n-1))
meanx
sdx
#standardizing data
#by default, the scale()function standardizes the specified columns of a matrix or data frame to a mean of 0 and a standard deviation of 1
newdata <-scale(mydata)
#to standardize each column to an arbitrary mean and sd, you can use 
newdata <- scale(mydata)*SD +M #M is the desired mean and SD is the desired sd
#scale() only uses for numeric one, if using for one column rather than an entire matrix or data frame, we can
newdata <- transform(mydata, myvar= scale(myvar)*SD +M)

#4.Probability functions take the form: [dpqr]distribution_abbreviation()
#where the first letter refers to the aspect of the distribution returned:
#d=density
#p=distribution function
#q=quantile function
#r=random generation(random deviates)


#5. probability distributions
#Distribution 1.Beta 2. Logistic 3.Binomial 4.Multinomial 5. Cauchy 6. Negative binomial 7.Chi-squared(noncentral) 8.Normal
#Abbreviation 1.beta 2. logis    3.binom    4.multinom    5. cauchy 6  nbinom            7.chisq                   8.norm

#Distribution 1.Exponential 2.Poisson 3. F 4. Wilcoxon signed rank 5. Gamma 6. T 7. Geometric 8.Uniform 
#Abbreviation 1.exp         2.pois    3. f 4. signrank             5. gamma 6. t 7. geom      8.unif

#Distribution 1.Hypergeometric 2. Weibull 3.Lognormal 4.Wilcoxon rank sum
#Abbreviation 1. hyper         2. weibull 3.lnorm     4.wilcox

#default mean=0, sd=1 for standard normal distribution
#6. case study: normal distribution functions
x<- pretty(c(-3,3),30)
y<-dnorm(x)
plot(x, y, type="l", xlab="Normal Deviate", ylab="Density", yaxs="i")

pnorm(1.96) #p(z<1.96)
qnorm(.9, mean=500, sd=100) #the value of the 90th percentile of a normal distribution with a mean of 500 and a sd of 100
rnorm(50, mean=50, sd=10) #generate 50 random normal deviates with a mean of 50 and sd of 10

#7. setting the seed for random number generation: making the results reproducible
set.seed()

runif(6)#generate pseudo-random numbers from a uniform distribution on the interval 0 to 1

set.seed(1234)
runif(6)

#8.draw data from a multivariate normal distribution with a given mean vector and covariance matrix
mvrnorm(n, mean, signma) #in the MASS package, n is the desired sample size, mean is the vector of means and signma is the variance-covariance(or correlation) matrix

library(MASS)
options(digits=3)
set.seed(1234)
mean <-c(230.7,146.7,3.6)
signma<-matrix(c(15360.8,6721.2,-47.1, 6721.2, 4700.9, -16.5, -47.1, -16.5, 0.3), nrow=3, ncol=3)
mydata <-mvrnorm(500, mean, signma)
mydata <-as.data.frame(mydata)
names(mydata) <-c("y","x1","x2")
dim(mydata)
head(mydata, n=10)


#9. character functions: character functions extract information from textual data or reformat textual data from printing and reporting
nchar(x) #counts the number of character of x
x<-c("ab","cde","fghij")
length(x)
nchar(x[3])

substr(x,start, stop) #extracts or replaces substrings in a character vectors
x<-"abcddf"
substr(x,2,4)
substr(x,2,4) <- "22222"
x

grep(pattern, replacement, ignore.case=F, fixed=F) #searches for pattern in x, if fixed=F, then pattern is a regular expression, if fixed= T, then pattern is a text string. returns the matching indices
grep("A", c("b","A","c",fixed=T))

sub(pattern, replacement, x, ignore.case=F, fixed=F)#finds pattern in x and substitutes the replacement text. if fixed=F, then the pattern is a regular expression, if fixed=T, then pattern is a text string. 
sub("\\s",".","Hello There") #\\s is a regular expression for finding whitespace, rather than using \s(R's escape character is \)

strsplit(x, split, fixed=F) #splits the elements of character vector x at split. if fixed=F, then pattern is a regular expression, otherwise a text string
y<-strsplit("abc","")
y

paste(...,sep="")#concatenates strings after using the sep string to separate them.
paste("x",1:3, sep="")
paste("x",1:3,sep="M")
paste("Today is", date())


toupper(x) #uppercase
toupper('abc')

tolower(x) #lowercase
tolower("ABC") #returns "abc"


#actually, regular expressions provide a clear and concise syntax for matching a pattern of text. 
#^[hc]?at matchs any string that starts with 0 or one occurrences of h or c, followd by at, like hat, cat, at

#10. other useful functions 
length(x)
x<-c(2,5,6,8) #returns 4

seq(from, to, by) #generates a sequence
indices <-seq(1,10,2)
indices #is c(1,3,5,7,9)

rep(x,n) #repeats x n times
y<-rep(1:3,2)
y

cut(x,n) #divide the continuous variable x into a factor with n levels. to create an ordered factor, include the option ordered_result =T

pretty(x,n) #creates pretty breakpoints. divides a continuous variable x into n intervals by selecting n+1 equally spaced rounded values. often used in plotting

cat(..., file="myfile", append=F)#concatenats the objects in.. and outputs them to the screen or to a file(if one is declared)
name <- c("Jane")
cat("Hello", name, "\n")

#11. the use of escape characters in printingm \n for newlines, \t for tabs, \'for a single quote, \b for backspace and ?Quote for more information
name<- "Bob"
cat ("Hello", name, "\b", ".\n","Isn\'t R", "\t", "GREAT?\n")
cat ("Hello", name, "\b.\nIsn\'t R\tGREAT?\n") #the space from \t is just 1, which is different from above, 4


#12. applying functions to matrices and data frames

apply(x, MARGIN, FUN,...)
mydata <- matrix(rnorm(30),nrow=6)
mydata
apply(mydata,1,mean)
apply(mydata,2,mean)
apply(mydata,2,mean, trim=.2) #trim means that the bottom 20% and top 20% of the values discarded

#lapply()
#sapply() which is a user friendly version of lapply()

#13. case study: a solution for the data management challenge in a small program
options(digits=2)
Student <-c("John Davis", "Angela Williams","Bullwinkle Moose",
            "David Jones", "Janice Markhammer", "Cheryl Cushing",
            "Reuven Ytzrhak","Greg Knox", "Joel England",
            "Mary Rayburn")
Math <- c(502,600,412,358,495,512,410,625,573,522)
Science <-c(95, 99,80,82,75,85,80,95,89,86)
English <-c(25,22,18,15,20,28,15,30,27,19)
roster <-data.frame(Student, Math, Science, English, stringsAsFactors = F)

#obtains the performance scores
z<- scale(roster[,2:4])
score <-apply(z,1,mean)
roster <-cbind(roster, score)

#grades the students
y<-quantile(score, c(.8,.6,.4,.2))
roster$grade[score >= y[1]]<-"A"
roster$grade[score <y[1] & score >= y[2]] <-"B"
roster$grade[score <y[2]&score >=y[3]]<-"C"
roster$grade[score<y[3]&score >=y[4]] <-"D"
roster$grade[score<y[4]]<-"F"

#extracts the last and the first names
name <- strsplit((roster$Student),"")
name
Lastname <-sapply(name,"[",2) # take the first element of each component and put it in a firstname vector, [ is a function that extracts part of an object

Firstname <- sapply(name,"[",1)
roster <- cbind(Firstname, Lastname, roster[,-1])
roster <- roster[order(Lastname, Firstname),]
roster


#14. Control flow
#statement is a single R statement or a compound statement(a group of R statements enclosed in curly braces {} and separated by semicolons)
#cond is an expression that resolves to TRUE or FALSE
#expr is a statement that evaluates to a number or character string
#seq is a sequence of numbers or character strings

#15. repetition and looping

#for : for loop executes a statement repetitively until a variable's value is no loner contained in the sequence seq.
for(var in seq) statement

for(i in 1:10) print("Hello") # word Hello is printed 10 times

#WHILE: executes a statement repetitively until the condition is no longer true
while(cond) statement

i<10
while(i>0) {print("Hello"; i<- i-1)}

#loop constrcture is time comsumming, it is better to use the builtin numerical and character functions in conjunction with the apply family of functions

#16. conditional execution
#IF-ELSE
if(cond) statement
if(cond) statement1 else statement2

if(is.character(grade)) grade <-as.factor(grade)
if(!is.factor(grade)) grade <-as.factor(grade) else print("Grade already is a factor")

#IFELSE: compact and vectorized version of the if-else construct
ifelse(cond, statement1, statement2) #if cond is T, execut the statement1, else execute the statement2

ifelse(score>0.5, print("Passed"), print("Failed"))
outcome <-ifelse(score>0.5, "Passed","Failed")
#use ifelse when you want to take a binary action or when you want to input and output vectors from the construct


#SWITCH
switch(list, expr,...) #where ... represents statements tied to the possible outcome values of expr.

feelings <-c("sad", "afraid")
for(x in feelings)
  print(
    switch(x,
           happy = "I am gald you are happy",
           afraid = "There is nothing to fear",
           sad="Cheer up",
           angry = "Calm down now")
  )

#17. user-written functions
myfunction <- function(arg1, arg2,...){
  statements
  return(object)
}


mystats <-function(x, parametric=T, print=F){ #we also can change the logic value of parametric and print
  if(parametric){
    center <-mean(x); spread<-sd(x)
  } else {
    center <-median(x); spread <-mad(x)
  }
  if(print & parametric){
    cat("Mean=", center, "\n","SD", spread, "\n")
  } else if(print & !parametric){
    cat("Median=", center, "\n", "MAD=", spread,"\n")
  }
  result <-list(center=center, spread=spread)
  return(result)
}
set.seed(1234)
x <-rnorm(500)
y<-mystats(x)
y


#a user written function that uses the switch construct
mydate <- function(type="long"){
  switch(type, long= format(Sys.time(), "%A %B %d %Y"),
         short =format(Sys.time(),"%m-%d-%y"),
         cat(type,"is not a recognized type\n"))
}
mydate("long")
mydate("short")
mydate()
mydate("1") #cat() function is executed only if nothing is matched

#18. warning(), message(), stop()
warning() #to generate a warning message
message() #to generate a diagnosic message
stop() #to stop execution of the current expression and carry out an error action

#Appendix B describes how to customize the R environment so that user written functions are loaded automatically at startup

#19. aggregation and reshaping
#transposing(reversing rows and columns)
t() #to transpose a matrix or a data frame
cars <- mtcars[1:5, 1:4]
cars
t(cars)
#reshapre2 package gets more flexible way of transposing data 

#aggregating data
aggregate(x, by, FUN) #by is a list of variables that will be crossed to form the new observationsm and FUN is the scalar function used to calculate summary statistics that will make up the new observation values

options(digits=3)
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl, gear), FUN=mean, na.rm=T)
aggdata#group1 is for cyl and group2 is for gears

#20. reshape2 package
#tremendously versatile approach to both restructuring and aggregating dataset
install.packages("reshape2")
library(reshape2)
#melt data: so that each row is a unique ID-variable combination
#cast data into any shape we desire

md <-melt(mydata, id=c("ID","Time"))
md
dcast(md ,formula, fun.aggregate) #recast the data into any shape,md is the melted data, formula describes the desired end result and fun.aggregate is the optional aggregating function, The formula takes the form: rowvar1+rowvar2+...~colvar1+colvar2+...\

dcast(md, ID~variable, mean)
dcast(md, Time~variable, mean)
dcast(md, ID~Time, mean)
dcast(md, ID+Tine~variable)
dcast(md,ID+variable~Time)
dcast(md,ID~variable+Time) #the variable before "~" is like primary key but the variable after that should be splited to several values
