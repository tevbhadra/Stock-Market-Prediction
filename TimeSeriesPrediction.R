getwd()
setwd("C:/Users/ahmed/Desktop")
library(readxl)
Boeing <- read_excel("C:/Users/ahmed/Downloads/Structured_ Data_Boeing(Edited).xlsx")

#To check the dimensionality
dim(Boeing)

#To check the data structure
str(Boeing)


#library DataExplorer for exploring the data.
library(DataExplorer)

plot_missing(Boeing) ## To find missing values

plot_histogram(Boeing) ## To show histogram


  ## To find  distribution of each continuous variable
library(DataExplorer)
library(graphics)

plot_boxplot(Boeing, by = "Adj_High", nrow = 2L, ncol = 2L)
plot_correlation(Adj_High)
plot_boxplot(Boeing , by = "Adj_High")

plot_boxplot(Boeing$Volume)
plot(Boeing[,7:13], col = 4) 

boxplot(Boeing$Open, main="Adjusted Volume Boxplot")

Boeing$Volume <- Boeing$Volume[-ootvalues ]
ootvalues <- boxplot(Boeing$Volume)$out
ootvalues
Boeing$Volume <- Boeing$Volume[-ootvalues ]
boxplot(Boeing$Volume)$out
max(Boeing$Volume)
mean(Boeing$High)
mean(Boeing$Low)
mean(Boeing$Open)
mean(Boeing$Close)
mean(Boeing$Dividend)
var(Boeing$High)
var(Boeing$Low)
cor(Boeing$High ,Boeing$Dividend)
cor(Boeing$Adj_High, Boeing$Dividend)



library(ggplot2)
par(mfrow=c(2,2))
ggplot(Boeing , aes(y=High ,x=Dividend))+geom_point()+geom_smooth(method=lm ,se=TRUE)
ggplot(Boeing , aes(y=Low ,x=Dividend))+geom_point()+geom_smooth(method=lm ,se=TRUE)
ggplot(Boeing , aes(y=Adj_High ,x=Dividend))+geom_point()+geom_smooth(method=lm ,se=TRUE)
ggplot(Boeing , aes(y=Dividend ,x=Low))+geom_point()+geom_smooth(method=lm ,se=TRUE)

# assignment 3

cor(Boeing[,-1])
correlation <- corr
str(Boeing)


# Edited


install.packages("e1071")
library(e1071)

#skewnwss
skewness(Boeing$Open)
SKEWVALUES <- apply(Boeing[,-1], 2, skewness)
SKEWVALUES

#kurtosis
kurtosisvalues <- apply(Boeing[,-1], 2, kurtosis)
kurtosisvalues

#summary of dataset before transformation
summary(Boeing)

#Checking the missing values

sum(is.na(Boeing))
sum(is.null(Boeing))

uniquedata <- unique(Boeing)
dim(uniquedata)
dim(Boeing)

#Outliers
library(ddpcr)
remove_outliers(Boeing$Volume)


# Box Cox Transformation for volume
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
ChiAreaTrans <- BoxCoxTrans(Boeing$Volume)
ChiAreaTrans

predict(ChiAreaTrans, head(Boeing$Volume))

skewness(predict(ChiAreaTrans, (Boeing$Volume)))
kurtosis(predict(ChiAreaTrans, (Boeing$Volume)))

x <-predict(ChiAreaTrans, (Boeing$Volume))
dim(predict(ChiAreaTrans, (Boeing$Volume)))
dim.data.frame(x)
str(x)
head(x)
Boeing$Volume <- x
Boeing$Volume
dim(Boeing)
names(Boeing)
skewness(Boeing$Volume)


## Box Cox Transformation for Dividend
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
Boeing$Dividend
ChiAreaTrans <- BoxCoxTrans(Boeing$Dividend)
ChiAreaTrans


## Box Cox Transformation for adj_volume
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
ChiAreaTrans <- BoxCoxTrans(Boeing$Adj_Volume)
ChiAreaTrans

predict(ChiAreaTrans, head(Boeing$Adj_Volume))

skewness(predict(ChiAreaTrans, (Boeing$Adj_Volume)))
kurtosis(predict(ChiAreaTrans, (Boeing$Adj_Volume)))

x <-predict(ChiAreaTrans, (Boeing$Adj_Volume))
dim(predict(ChiAreaTrans, (Boeing$Adj_Volume)))
dim.data.frame(x)
str(x)
head(x)
Boeing$Adj_Volume <- x

skewness(Boeing$Adj_Volume)

#PCA
my.pca <- prcomp(Boeing[,-c(1,2)])
summary(my.pca)
a<- my.pca$x
a
cor(my.pca$x)
#Biplot
biplot(my.pca)
#Screeplot
plot(my.pca, type="l")

cor(Boeing[,-1])

####################################Model####################################################################

#############Visualize the time series#####################################################################

library(readxl)
Boeing <- read_excel("C:/Users/ahmed/Downloads/Structured_ Data_Boeing(Edited).xlsx")
as.Date(Boeing$Date, format= "%m/%d/%Y")
head(Boeing)

plot(Boeing[,-(3:13)])

#abline(reg=lm(Boeing$Open~time(Boeing$Open)))
#plot(aggregate(Boeing$Open,FUN=mean))

#boxplot(Boeing$Open~cycle(Boeing$Open))

##############################Stationarize the series#####################################################
library(tseries)
adf.test(diff(log(Boeing$Open)), alternative="stationary", k=0)

#This will give you the result of the Augmented Dickey-Fuller Test
#Augmented Dickey-Fuller Test

#data:  diff(log(AirPassengers))
#Dickey-Fuller = -9.6003, Lag order = 0, p-value = 0.01
#alternative hypothesis: stationary

#Since p < 0.05 reject Null hypothesis and conclude that the series 
#diff(log(AirPassengers)) is stationary


plot(diff(log(Boeing$Open)))

#####################################Plot ACF/PACF charts and find optimal parameters##################

acf(diff(log(AirPassengers)))




pacf(diff(log(AirPassengers)))



################################Build ARMA model#########################

(fit <- arima(log(AirPassengers), c(1, 1, 0),seasonal = list(order = c(1, 1, 0), period = 12)))







#####################Make predictions###############################

pred <- predict(fit, n.ahead = 10*12)
pred # get the values for each month for the next 10 years
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3)) 



























AP<-ts(Boeing$Open,frequency = 12, start=c(2016,4))
class(AP) # AP is a ts object
AP1 <- as.matrix(AP)
AP1

#Convert the ts object to a dataframe
data.frame(AP = c(AP), time = c(time(AP)))

# Convert time series data to csv
ts2csv <- function(x) {
  fname <- paste0(deparse(substitute(x)), ".csv")
  if (NCOL(x) == 1L) {
    # Univariate time series
    readr::write_csv(
      as.data.frame(tsibble::as_tsibble(x)),
      fname)
  } else {
    # Multivariate time series
    readr::write_csv(
      as.data.frame(tsibble::spread(tsibble::as_tsibble(x), key, value)),
      fname)
  }
}



ts2csv(AP)




