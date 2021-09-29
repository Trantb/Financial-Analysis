library(xts)
library(zoo)
library(lubridate)
library(tibble)
library(ggplot2)
library(reshape)
library(dplyr)

setwd('C:/Users/HP/Desktop/Study/side project')
df <- read.csv('F-F_Research_Data_Factors_daily.csv', header = T)


####Cleaning data
#Remove titles and upper notes
df <- df[-c(1:6, 25048:25049),] #25048:25049 is for ending note & copyrights
colnames(df)[1:5] <- c('Date', 'Mkt-RF', 'SMB', 'HML', 'RF') #column name

#Convert from character to numeric data
for (i in 1:ncol(df)){
  df[,i] <- as.numeric(df[,i])
}
rm(i)
#convert column date to Date format
df$Date <- ymd(df$Date)
df$Mkt = df$`Mkt-RF` + df$RF #recreate Market return column
df <- df[c(1,5,6)] #keep date, market return and free risk return

df[,2] <- df[,2]/100 #divide by 100 to obtain proportion
df[,3] <- df[,3]/100 #same as above

#convert rownames to date index
df <- xts(df[,2:3], order.by = as.Date(df[,1]))
options(scipen = 999) #remove scientific notations.

####problem 1
basicStat <- matrix(NA, nrow = 5, ncol = 2)
colnames(basicStat) <- c('Free Risk','Market')

#include mean, median, standard deviation, minimum and maximum
rownames(basicStat) <- c('mean','median','sd','min','max')

basicStat[1,1] <- mean(df$RF);basicStat[1,2] <- mean(df$Mkt)
basicStat[2,1] <- median(df$RF); basicStat[2,2] <- median(df$Mkt)
basicStat[3,1] <- sd(df$RF); basicStat[3,2] <- sd(df$Mkt)
basicStat[4,1] <- min(df$RF); basicStat[4,2] <- min(df$Mkt)
basicStat[5,1] <- max(df$RF); basicStat[5,2] <- max(df$Mkt)
rm(basicStat)

#plot histogram for return
ggplot(df, aes(x = RF)) + geom_histogram(bins = 30) + coord_flip()
ggplot(df, aes(x = Mkt)) + geom_histogram(bins = 30) + coord_flip()

#Plot return over time
plot(df$RF)
plot(df$Mkt)

####problem 2
invest_fr <- 1 + df$RF #return after one time period with the first period is 1
invest_fr_fv <- cumprod(invest_fr) #compound function
plot(invest_fr_fv, type = 'l', col = 'red', lwd = 2)

invest_mkt <- 1 + df$Mkt
invest_mkt_fv <- cumprod(invest_mkt)
plot(invest_mkt_fv, type = 'l', col = 'red', lwd = 2)

rm(invest_fr); rm(invest_fr_fv); rm(invest_mkt); rm(invest_mkt_fv)

####problem 3
###3a
return_daily <- function(typeOfReturn){ #calculate daily return
  year <- data.frame(sample(1926:2021, 25, replace = T)) #pick 25 years randomly
  return <- data.frame(NULL)
  temp <- data.frame(NULL)
  for (i in 1:nrow(year)){
    temp <- df[,typeOfReturn][paste(year[i,], sep = '')]
    return <- rbind(return, temp) #stack all observations on a dataset
  }
  interest <- return + 1 #return after one time period with the first period is 1
  value <- 1 #base value
  for (j in 1:nrow(interest)){
    value <- value*interest[j,1] #loop through to calculate the amount at year 25
  }
  return(value)  
}
#typeOfReturn: 1 for risk free, 2 for market



#bootstrap with 1000 times
re <- replicate(n = 1000, return_daily(2))
re <- data.frame(re) 
ggplot(re, aes(x = re)) + geom_histogram(bins = 15) + coord_flip() #histogram
rm(re)

#Calculate annually return given daily data
returnByyear <- function(){
  #Create a matrix with 96 rows and 3 cols
  returnByYear <- data.frame(matrix(NA, nrow = 96, ncol = 3))
  #similar to original data with each collumn for data, free risk and market
  #but this time, it is year and not date
  colnames(returnByYear) <- c('year', 'RF', 'Mkt') 
  returnByYear[,1] <- c(1926:2021) #year from 1926:2021
  #Calculate annual Rf return
  for (i in 1:nrow(returnByYear)){ #96 rows for 96 years
    temp <- df[,1][paste(returnByYear[i,1], sep = '')] # each i is a year
    temp <- temp + 1 #return at one period after given the original time is 1
    value <- 1 #base value
    for (j in 1:nrow(temp)){
      value <- value*as.numeric(temp[j,1]) #compound for daily in a year
    }
    value <- value - 1 #annual return
    returnByYear[i,2] <- value #store in column 2
  }
  #Calculate annual Mkt return following the same logic
  for (i in 1:nrow(returnByYear)){
    temp <- df[,2][paste(returnByYear[i,1], sep = '')]
    temp <- temp + 1
    value <- 1
    for (j in 1:nrow(temp)){
      value <- value*as.numeric(temp[j,1])
    }
    value <- value - 1
    returnByYear[i,3] <- value
  }
  return(returnByYear)
}
returnByYear <- data.frame(returnByyear())
returnByYear <- xts(returnByYear[,2:3], 
                    order.by = as.yearmon(returnByYear[,1])) #change to ts data
#row names as Jan 1926 to Jan 2021, but it is actually annual return

return_annually <- function(typeOfReturn){ #using same logic as the daily return
  year <- data.frame(sample(1926:2021, 25, replace = T))
  return <- data.frame(NULL)
  temp <- data.frame(NULL)
  for (i in 1:nrow(year)){
    temp <- returnByYear[,typeOfReturn][paste(year[i,], sep = '')]
    return <- rbind(return, temp)
  }
  interest <- return + 1
  value <- 1
  for (j in 1:nrow(interest)){
    value <- value*interest[j,1]
  }
  return(value)  
}
#1 for free risk, 2 for market

re <- replicate(n = 1000, return_annually(2))
re <- data.frame(re)
ggplot(re, aes(x = re)) + geom_histogram(bins = 15) + coord_flip()

rm(re)
rm(return_daily) #use annual return for now, no need for this
rm(df) #use annual return for now, no need for this

###3b
#using yearly return for less calculation. 
#create a function to generate return given 1 dollar for any 25 years
createData <- function(typeOfReturn){
  year <- data.frame(sample(1926:2021, 25, replace = T)) #random 25 years
  return <- data.frame(NULL)
  temp <- data.frame(NULL)
  for (i in 1:nrow(year)){
    temp <- returnByYear[,typeOfReturn][paste(year[i,], sep = '')]
    return <- rbind(return, temp) #stack all data over each other
  }
  interest <- return + 1 
  interest <- cumprod(interest)
  value <- as.numeric(interest[,1])
  return(value)  
}


#We use this one for multiple following up questions
data <- matrix(data = c(1:25), nrow = 25, ncol = 1)
temp <- data.frame(NULL)
for (i in 1:1000){ #create 1000 observations
  temp <- createData(2)
  data <- cbind(data, temp)
}

graph <- data[,-1]
colnames(graph)[1:1000] <- paste('X', 1:1000, sep = '')
#change data from the lat format to the long format
graph <- melt(graph, id.vars = 'year') 
colnames(graph) <- c('year','X2','return')

#create a vector controlling color
cols <- replicate(1000, 'grey50')

ggplot(graph, aes(x = year, y = return, color = X2)) + geom_line() +
  scale_y_log10() + guides(color = F) + scale_color_manual(values = cols)

rm(temp); rm(i); rm(cols); rm(graph)

##3c
graph <- data[,-1] #reuse the data for this probelm
colnames(graph)[1:1000] <- paste('X', 1:1000, sep = '')
carg <- data.frame(NULL)
#calculate carg base on the link
for (i in 1:ncol(graph)){
  carg[i,1] <- (as.numeric(last(graph[,i]))/as.numeric(graph[1,i]))^(1/25) - 1  
}
colnames(carg)[1] <- 'carg'
ggplot(carg, aes(x = carg)) + geom_histogram(bins = 30) + coord_flip()

rm(i); rm(graph)
#keep carg object for problem#4

##3d 
graph <- data[,-1]
colnames(graph)[1:1000] <- paste('X', 1:1000, sep = '')

#find the position of the 5th and 95th percentile
position <- data.frame(graph[25,]) #create a vector for return of the final year
position <- position[order(position, decreasing = F),] #sort
position5 <- match(position[50], graph[25,]) #the index 50 should be the 5th
position95 <- match(position[950], graph[25,]) #the index 950 should be the 9th
cols <- replicate(1000, 'grey80') #create a vector of color
cols[999:1000] <- 'red'#the last two will be drawn last, so I assign this value
#and swap column below


graph <- data.frame(graph)
graph <- graph %>% #find the position then swap after the last column
  relocate(paste('X',c(position5,position95), sep = ''), .after = X1000)
colnames(graph)[1:1000] <- paste('X', 1:1000, sep = '') #rename again

graph <- data.matrix(graph)

graph <- melt(graph, id.vars = 'year') #change format to draw
colnames(graph) <- c('year','X2','return')
ggplot(graph, aes(x = year, y = return, color = X2)) + geom_line() +
  scale_y_log10() + guides(color = F) + scale_color_manual(values = cols) +
  scale_alpha_manual(values = alpha)

rm(position5); rm(position95); rm(graph); rm(cols); rm(position)

##3i
#Calculate consecutive year. 
#pick year function to pick 25 years consecutive ly
pickyear <- function(){
  firstyear <- sample(1926:2021, 1, replace = T)
  if (firstyear >= 1997){ #1997 to 2021 is 25 year. Any year after that will
    #not have enough 25 years, so we reset
    1997:2021
  } else {
    firstyear:(firstyear + 24)
    }
}

#function to return 25 years continuously
actualData <- function(typeOfReturn){
  year <- data.frame(pickyear())
  return <- data.frame(NULL)
  temp <- data.frame(NULL)
  for (i in year[1,]:year[25,]){
   temp <- returnByYear[,typeOfReturn][paste(i, sep = '')]
    return <- rbind(return, temp)
  }
  interest <- return + 1
  interest <- cumprod(interest)
  value <- as.numeric(interest[,1])
  return(value)
}

#create some observations given real data
dataReal <- matrix(data = c(1:25), nrow = 25, ncol = 1)
temp <- data.frame(NULL)
for (i in 1:3){ #create 3 observations
  temp <- actualData(2)
  dataReal <- cbind(dataReal, temp)
}
dataReal <- dataReal[,-1]

#Below following same logic as graphing above
graph <- data[,-1]
colnames(graph)[1:1000] <- paste('X', 1:1000, sep = '')


position <- data.frame(graph[25,])  
position <- position[order(position, decreasing = F),]
position5 <- match(position[50], graph[25,])
position95 <- match(position[950], graph[25,])


graph <- data.frame(graph)
graph <- graph %>% 
  relocate(paste('X',c(position5,position95), sep = ''), .after = X1000)

graph <- cbind(graph, dataReal) #3 last column will be actual data
colnames(graph)[1:1003] <- paste('X', 1:1003, sep = '') #rename

cols <- replicate(1003, 'grey80')
cols[999:1000] <- 'red' #column 999 and 1000 is 5th and 95th percentile
# so give it red color
cols[1001:1003] <- 'blue' #these three are actual data so give it blue color

graph <- data.matrix(graph)

graph <- melt(graph, id.vars = 'year')
colnames(graph) <- c('year','X2','return')
ggplot(graph, aes(x = year, y = return, color = X2)) + geom_line() +
  scale_y_log10() + guides(color = F) + scale_color_manual(values = cols) +
  scale_alpha_manual(values = alpha)

rm(position5); rm(position95); rm(cols); rm(i); rm(position); rm(temp)
rm(pickyear); rm(actualData); rm(dataReal); rm(graph)

####problem 4
#Ho: CAGR =< 0%
#Ha: CAGR > 0%
#one tail test
(mean(carg[,1]) - 0)/(sd(carg[,1])/sqrt(1000))
rm(carg)
#t score is roughly 76.51 which is above any value in t table. Therefore,
#we can safely say with above 99% confidence that investing in the market 
#in 25 years will results in a CARG > 0%.


##problem 5
asset <- function(base, pctInMkt){#pctInmkt should be the percentage in market
  #base is the original amount
  rf <- return_annually(1) #free risk return
  mkt <- return_annually(2) #annual return
  
  asset_mkt <- base*pctInMkt #percent in market
  asset_rf <- base*(1 - pctInMkt) #percent in free risk
  
  returnFromMkt <- asset_mkt*mkt
  returnFromRf <- asset_rf*rf
  final <- returnFromMkt + returnFromRf #sum
  return(final)
}

ave_mkt_return <- replicate(n = 1000, asset(1,1))
ave_mkt_return <- data.frame(ave_mkt_return)
ggplot(ave_mkt_return, aes(x = ave_mkt_return)) + geom_histogram(bins = 15) +
  coord_flip()

ave_rf_return <- replicate(n = 1000, asset(1, 0.5))
ave_rf_return <- data.frame(ave_rf_return)
ggplot(ave_rf_return, aes(x = ave_rf_return)) + geom_histogram(bins = 15) + 
  coord_flip()


compare <- matrix(NA, nrow = 5, ncol = 2)
colnames(compare) <- c('Half&Half','Market')
rownames(compare) <- c('mean','median','sd','min','max')

compare[1,1] <- mean(ave_rf_return[,1]);compare[1,2] <- mean(ave_mkt_return[,1])
compare[2,1] <- median(ave_rf_return[,1]); compare[2,2] <- median(ave_mkt_return[,1])
compare[3,1] <- sd(ave_rf_return[,1]); compare[3,2] <- sd(ave_mkt_return[,1])
compare[4,1] <- min(ave_rf_return[,1]); compare[4,2] <- min(ave_mkt_return[,1])
compare[5,1] <- max(ave_rf_return[,1]); compare[5,2] <- max(ave_mkt_return[,1])

rm(ave_mkt_return); rm(ave_rf_return); rm(compare)



