#Clear workspace
rm(list=ls())
#Clear plots
dev.off()

#Installing required packages
install.packages("moments")
library(moments)
library(ggplot2)
require("class")
require("datasets")

#Set working directory
setwd("D:/ANLY506_Exploratory_Data_Analysis")
#Read in data
airquality <- airquality

#Dataset first look
colnames(airquality)
head(airquality)
tail(airquality)
dim(airquality)
length(airquality)
str(airquality)
summary(airquality)
library(ggpubr)

#The data set has 153 observations with 6 variables; Ozone, Solar.R, Wind, Temp, Month, Day.The Ozone variable shows the mean ozone in parts per billion from 1 PM to 3 PM at Roosevelt Island. The Solar.R variable records the solar radiation in Langleys in the frequency band 4000-7700 Angstroms from 8 AM to 12 AM at Central Park. Wind variable shows the average wind speed in miles-per-hour at 7 AM and 10 AM at LaGuardia Airport. Temp records the maximum temperature in degrees F at LaGuardia airport. Month and Day variables show the month and date on which the observations were individually recorded. All variables have integer data type except Wind, which has a double data type.

#A quick look of the data shows that the observations do, indeed, have the correct variable type. 

#We can see that the average temperature is 77.88235 degree F with a median of 79 degree F. We could also see some of the simple statistics of the Ozone variable.

##Querying the number of missing values in each column of the data set
colSums(is.na(airquality))
#Creating a function to calculate the missing values percentage of each variable
percent_missing <- function(aq){colSums(is.na(airquality)) / nrow(airquality) * 100}
percent_missing(airquality)

#Out of 153 observations, there are 37 (24%) missing (NA) Ozone values, and 7 (4%) missing Solar.R values.  

#With Solar.R variable, the missing values can be substituted by the variable's mean as there are only 7 NA values. However, for the Ozone variable, as there are 37 missing values in a rather small data set of 153 observations, the integrity of the data set would be affected. For example, should we simply remove the 37 observations or substitute the Ozone's mean into observations with missing Ozone values, the analysis may be inaccurate.

#Missing values can be handled by the mean imputation method. 

#Imputing the Ozone variable's monthly mean
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i, "Ozone"])){
    airquality[i, "Ozone"] <- mean(airquality[which(airquality[,"Month"] == airquality[i,"Month"]),"Ozone"],
                             na.rm = TRUE)
  }
  
  #Imputing the Solar.R variable's monthly mean
  if(is.na(airquality[i, "Solar.R"])){
    airquality[i, "Solar.R"]<- mean(airquality[which(airquality[,"Month"] == airquality[i,"Month"]),"Solar.R"],
                              na.rm = TRUE)
  }
}  

#Replacing observations' data with normalized data
head(airquality)
tail(airquality)
summary(airquality)

#Temperature descriptive statistics
mean(airquality[, 4])
median(airquality$Temp)
summary(airquality$Ozone)

#Visualizing all attributes
boxplot(airquality)

#Ozone Histogram
summary(airquality$Ozone)
o <- ggplot(data=airquality) 
o + geom_histogram(mapping=aes(Ozone))
skewness(airquality$Ozone, na.rm = T)
kurtosis(airquality$Ozone, na.rm = T)
#The Ozone variable's distribution is heavily skewed to the right.

#Solar.R Histogram
summary(airquality$Solar.R)
s <- ggplot(data = airquality)
s + geom_histogram(mapping = aes(Solar.R))
skewness(airquality$Solar.R, na.rm = T)
kurtosis(airquality$Solar.R, na.rm = T)
#The distribution of Solar.R variable looks to be multi-modal and slightly left-skewed.

#Temperature histograms
summary(airquality$Temp)
t <- ggplot(data = airquality)
t + geom_histogram(mapping = aes(Temp))
skewness(airquality$Temp, na.rm = T)
kurtosis(airquality$Temp, na.rm = T)
#Temperature is slightly skewed to the left, but have an almost normal distribution.

#Wind Histogram
summary(airquality$Wind)
w <- ggplot(data = airquality)
w + geom_histogram(mapping = aes(Wind))
skewness(airquality$Wind, na.rm = T)
kurtosis(airquality$Wind, na.rm = T)
#Wind has a distribution close to the normal distribution but slightly skewed to the right.

#Month and Day are not quantitative variables, so we cannot perform distribution analysis.

#Boxplot of temperature (Temp) in relation to Month
with(airquality, boxplot(Temp ~ Month))

#Adding color to the Temp vs. Month boxplot
with(airquality, boxplot(Temp ~ Month, col = c(1,2,3,4,5)))

#Forcing R to consider the Month variable as factors
with(airquality, as.factor(Month))
#Distinct factors of the Month variable
levels(with(airquality, as.factor(Month)))

#Temp vs. Month boxplot, colored by levels
with(airquality,
     boxplot(Temp ~ Month,
             col = levels(with(airquality,
                               as.factor(Month)))))

#Scatter plot to show the relationship between the Ozone and Temperature variables
with(airquality,plot(Ozone ~ Temp))

#Month levels
month_levels <- levels(with(airquality, as.factor(Month)))

#Month levels as color and number scatter plot
with(airquality, plot(Ozone ~ Temp,
                      pch = month_levels,
                      col = month_levels))

#Month levels as color and number scatter plot with different shapes
with(airquality, plot(Ozone ~ Temp,
                      pch = as.numeric(month_levels),
                      col = month_levels))


#Month vs Temp plot with Month as factor
qplot(Month, Temp,
      data = airquality,
      geom = "boxplot",
      color = as.factor(Month))

airquality2 <- airquality
airquality2$Month <- factor(airquality2$Month,
                            levels = 5:9,
                            labels = month.abb[5:9],
                            ordered = TRUE)

#Checking Month classes between the original 
class(airquality$Month)
class(airquality2$Month)
#levels
levels(airquality$Month)
levels(as.factor(airquality$Month))
levels(airquality2$Month)
qplot(Month, Temp, data = airquality2, geom = "boxplot", color = Month) +
  theme(legend.position = "none")

ggarrange(
  qplot(Month, Ozone, data = airquality2, geom = "boxplot", color = Month),
  qplot(Month, Solar.R, data = airquality2, geom = "boxplot", color = Month),
  qplot(Month, Temp, data = airquality2, geom = "boxplot", color = Month),
  qplot(Month, Wind, data = airquality2, geom = "boxplot", color = Month),
  labels = c("A", "B", "C", "D"),
  ncol = 2, nrow = 2)

##Scatter plots
qplot(Temp, Ozone, data = airquality2, col = Month)

qplot(x = Temp, y = Ozone, data = airquality2, 
      col= Month, 
      geom = c("point", "smooth"), 
      method = "lm", 
      se = FALSE)

qplot(x=Temp, y=Ozone, data=airquality2, 
      col=as.numeric(Month), 
      geom=c("point", "smooth"), 
      method="lm", 
      se = T)

qplot(x=Temp, y=Ozone, data=airquality2, 
      col=as.numeric(Month), 
      geom=c("point", "smooth"), 
      se = T) + 
  theme(legend.position="none")

#Simple Linear Regression Model
linreg <- with(airquality, lm(Ozone ~ Temp))
linreg
str(linreg)
linreg$coefficients
with(airquality, plot(Ozone ~ Temp, pch = month_levels, col = month_levels))
abline(linreg, col = "red", lwd = 3)

