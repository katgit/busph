## -------------------------------------------- ##
##
##   Programming in R
##
##   Research Computing Services
##   Katia Bulekova
##
## --------------------------------------------- ##


#--------------------
# Control Flow
#--------------------
# if()  / if()  else  / ifelse()
# for / while / repeat
# break / next 


# Logical Operator Meaning
# ==       Equal to
# !=       Not equal to
# <, <=    Less than, less than or equal to
# >, >=    Greater than, greater than or equal to
# &        Logical AND
# |        Logical OR 
# %in%     is part of a set    



#********************************

# --- if (condition) action_T ---

#********************************

set.seed(1234)
x <- sample (1:100, 1)
if ( x < 50 ) print("This value is less than 50")

# --- if (condition) action_T else action_F ---

set.seed(1234)
x <- sample (1:100, 1)
if ( x < 50 ) { 
  print("This value is less than 50") 
} else {
  print("This value is equal or greater than 50") 
}  



#-------------------------#
#    exercise
#-------------------------#
# Load dataset from COVID19 package
#install.packages("COVID19")
library(COVID19)
covid <- covid19(verbose = FALSE)

# Using function Sys.Date() get the current date
# Calculate the latest date in covid dataset
# If these two values are equal, print "the data is up to date"
# Otherwise, print "The data is out of date"

today  <- Sys.Date()
max.date <- max( covid$date )
if ( today == max.date){ 
  print("up to date")
}else if (today=="yesterday"){
  

} else {
  print("out of date")
}


#*************************************************

# --- ifelse ( condition, value_T, value_F ) ---

#*************************************************

set.seed(1234)
x <- sample (1:100, 50)
print(x)

y <- ifelse ( x %% 2 == 0, "even", "odd" ) 
print(y)



#-------------------------#
#    exercise
#-------------------------#

# Using the same covid dataset create a new column "highRate"
# Using ifelse() function, set the value to be TRUE if
# the "death" divided by "population" is greater than 0.001

covid$highRate <- ifelse( covid$deaths/covid$population > 0.001, TRUE, FALSE )
head(covid$highRate)
table(covid$highRate, useNA="ifany")
summary(covid$highRate)

#*************************************************

# --- case_when ( ) 

#*************************************************
library(dplyr)


table(covid$administrative_area_level_1) 

covid$region <- case_when(covid$administrative_area_level_1 %in% c("United States", "Canada")  ~ "North America", 
                          covid$administrative_area_level_1 %in% c("Bahrain", "Egypt", "Iran", "Iraq", "Israel", "Jordan", "Palestine", "Kuwait", "Lebanon", "Libya", "Morocco")  ~ "Middle East",
                          covid$administrative_area_level_1 %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua","Panama") ~ "Central America",
                          is.na(covid$administrative_area_level_1) ~ "Unknown", 
                          .default ="Other")

table(covid$region)


# We could also do the above operation using mutate() function from dplyr package
covid <- covid |> 
  mutate(region = case_when(administrative_area_level_1 %in% c("United States", "Canada")  ~ "North America", 
                            administrative_area_level_1 %in% c("Bahrain", "Egypt", "Iran", "Iraq", "Israel", "Jordan", "Palestine", "Kuwait", "Lebanon", "Libya", "Morocco")  ~ "Middle East",
                            administrative_area_level_1 %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua","Panama") ~ "Central America",
                            is.na(administrative_area_level_1) ~ "Unknown", 
                            .default ="Other"))
table(covid$region)




#*****************************************

# --- for (item in vector) { actions } ---

#*****************************************

# Iterate over a range of integer values
for (i in 1:3) {
  print(i)
}


# Iterate over a character vector
for (i in month.name) {
  print(i)
}

# get the names of all files in the specified directory
my.files <- list.files(path = ".", pattern = "*")

# list information about each file
for ( ifile in my.files) {
  print(file.info( ifile ) )
}


# For loops should NOT be used when the operation could be
# vectorised
# Compare:

system.time({
  
x <- numeric( nrow(covid) )
for ( i in 1:nrow(covid) ) {
  x[i] <- covid$deaths[i] / covid$population[i]
}

})

system.time({
# vectorised approach
x <- covid$deaths/ covid$population
})


#-------------------------#
#    exercise
#-------------------------#

# using a for() loop calculate N factorial 
N=5
result <- 1
for (i in 2:N) 
{
   result <- result * i
}
print(result)


## "next" - proceed to the next iteration
for (i in 1:20 ) {
  print( paste ("Start of iteration #", i))
  if(i <= 10) {
    ## Skip the first 10 iterations
    next                 
  }
  print( paste ("End of iteration #", i))
}


## "break" - exit a loop
result <- 1
for (i in 1:20 ) {
  print( paste ("Start of iteration #", i))
  
  result <- result * i
  if( result > 1000) {
    ## Exit the loop
    print (paste("Final value of result is", result))
    break                 
  }
  print( paste ("At the end of iteration #", i, "result =", result))
}


#******************************************************

# ------------------ Functions ------------------------

# --- fun <- function ( arg1, arg2, ...) { actions } ---

#******************************************************

# For input vector x calculate, mean, median, standard deviation and length
mySummary <- function ( x ) {
  v1 <- mean( x , na.rm=T)
  v2 <- median( x , na.rm=T)
  v3 <- sd ( x , na.rm=T)
  v4 <- length ( x) 
  
  c( mean = v1, median = v2, std = v3, length = v4 )
}

set.seed(1234)
a <- sample (1:10, 1000, replace = TRUE)
mySummary(a)

mySummary (covid$recovered)


# Function can have any number of arguments

mySummary <- function ( x, verbose = FALSE ) {
  v1 <- mean( x )
  v2 <- median( x )
  v3 <- sd ( x )
  v4 <- length ( x )
  
  if (verbose) {
    c( mean = v1, median = v2, std = v3, length = v4 )
  }else{
    c( mean = v1, median = v2 )
  }
}

# Now mySummary function can be called with 1 or 2 arguments
mySummary(x=a)
mySummary(x=a, verbose=T)

mySummary(verbose = T , x = a)


# The ...  argument
myPlot <- function ( x, y, ... ) {

  z <- x + y
  hist( z, ... )
}

set.seed(1234)
a <- sample (1:10, 1000, replace = TRUE)
b <- sample (1:10, 1000, replace = TRUE)
myPlot ( a, b )
myPlot ( a, b, col = "skyblue", main="Histogram of a+b" )



#-------------------------#
#    exercise
#-------------------------#

# Write a function that converts Fahrenheit  to Celsius
#  ( x - 32 ) / 9 * 5

F_to_C <- function ( x ) {
  ( x - 32 ) / 9 * 5
}

x <- 32:212
F_to_C (x)

#**************************************************

# --- Warning and Error messages ---

#**************************************************

## Messages:
print("Some output")
message("Some output")

## Use stop() function to catch errors
my_func <- function(x){
  
  message ("Start of the function")
  stop ("Some error occured")
  message ("This message will not be printed")
  
}

my_func(1)


## Use warning() function to print a warning message
my_func <- function(x){
  
  message ("Start of the function")
  warning ("Some unusual value is detected")
  message ("End of the fucntion")
  
}

my_func(1)

#**************************************************

# --- apply / lapply / sapply / tapply / mapply ---

#**************************************************

#Apply Functions Over Array Margins
x <- matrix ( 1 : 30, nrow = 5 )
x
apply( x, 1, max )
apply( x, 2, max )

colSums(x)
rowSums(x)



#Apply a Function over a List or Vector
x <- list( a = 1 : 10, 
           b = seq(as.Date("2020/3/26"), as.Date("2020/6/11"), "days") )
lapply(x, mean ) 

# Simplified version of lapply returning a vector
x <- list( a = 1 : 10, b = 20 : 15 )
lapply(x, mean)
sapply(x, mean)
sapply(x, which.max)




# Apply a function to each group in array
tapply( covid$tests, covid$id, sum, na.rm=T )

covid |> group_by(id) |> summarize(sum(tests, na.rm=T))

# Apply a function to multiple list or vector argument
mapply(rep, 
       times = c(1:12), 
       x = month.name)


