############################################## TASK 1 ##############################################

#Reading NOAA data downloaded in textfile format
#Cleaning the data in the text file and arranging the data into a 3-column table
raw.data.in.txt.format.file.table <- 
  read.table("~/Desktop/Raw Dataset for assignment.txt", sep = ",", header = TRUE, skip = 4)

#Discarding Anomaly column from raw data set  
raw.data.without.ANOMALY.Column.table <- raw.data.in.txt.format.file.table[,-3]

#Converting table into data frame
library(tidyverse)
convert.table.to.data.frame<- 
  as.data.frame.matrix(raw.data.without.ANOMALY.Column.table)

#checking if there are any missing values in Column VALUE (Average Temperature) of the raw data set.
VALUE.column.data <- convert.table.to.data.frame$Value
missing.values.check <- is.na(VALUE.column.data)

#excluding missing values from Column VALUE (Average Temperature) if there are any missing values.
Exclude.missing.values <- na.exclude(VALUE.column.data)

#checking if the values in column VALUE (Average Temperature) are all of numeric data type
numeric.values.check <- is.numeric(VALUE.column.data)

#Break Date Values into 2 Columns: Year and Month_In_Numeric
semi.preprocessed.data.with.YEAR.AND.MONTH.Column.Table <-
  transform(convert.table.to.data.frame, Year = substr(convert.table.to.data.frame$Date, 1, 4),
            Month = substr(convert.table.to.data.frame$Date, 5, 6))

#Discard Date Column
semi.preprocessed.data.without.DATE.Column.Table <- 
  semi.preprocessed.data.with.YEAR.AND.MONTH.Column.Table[,-1]

#Convert Month from numeric format to Character format e.g. 01 to January
semi.preprocessed.data.without.DATE.Column.Table$Month <- 
  month.name[semi.preprocessed.data.without.DATE.Column.Table$Month]

#rearrange table columns
clean.preprocessed.data.Table <- semi.preprocessed.data.without.DATE.Column.Table[c(2,3,1)]

#convert clean pre-processed table into csv file.
write.csv(clean.preprocessed.data.Table, file = "Clean Preprocessed Data.csv", row.names = FALSE )



################################################ TASK 2 ########################################################

#read data from CSV file created in Task 1
clean.data.in.csv.format <- read.csv("Clean Preprocessed Data.csv", header = TRUE)
VALUE.Column.Data <- clean.data.in.csv.format$Value

#print out arithmetic mean for average temperatures of all months from 1959 to 1979
arithmetic.mean.of.average.temperature.for.all.years <- mean(VALUE.Column.Data)

#print out variance for average temperatures of all months from 1959 to 1979
variance.of.average.temperature.for.all.years <- var(VALUE.Column.Data)

#print out Standard Deviation for average temperatures of all months from 1959 to 1979
standard.deviation.of.average.temperature.for.all.years <- 
  sd(VALUE.Column.Data)

median.of.average.temperature.for.all.years <-
  median(VALUE.Column.Data)

plot(VALUE.Column.Data, type = "p")
#plotting histogram from 1959 to 1979
hist(VALUE.Column.Data, breaks = 24,
     xlab = "MONTHLY AVERAGE TEMPERATURE (FAHRENHEIT)", 
     ylab = "NUMBER OF MONTHS", 
     main = "HISTOGRAM OF MONTHLY AVERAGE TEMPERATURE DISTRIBUTION IN USA (1959-1979)",
     col = "blue", border = "red")

#compute skewness of histogram
library(e1071)
skewness(VALUE.Column.Data)

################################################ TASK 4 ########################################################

#read data from CSV file created in Task 1
clean.data.in.csv.format <- read.csv("~/Desktop/Clean Preprocessed Data.csv", header = TRUE)
VALUE.Column.Data <- clean.data.in.csv.format$Value

#getting the log of the proportional change
log.of.proportional.change.of.Average.Temperature <- 
  log((head(VALUE.Column.Data, -1))/tail(VALUE.Column.Data))  
#plotting histogram for log of the proportional change
hist(log.of.proportional.change.of.Average.Temperature, 
     main = "Histogram for the distribution of the log 
     of the proportional change in Average Temperature",
     ylab = "Number of Occurences",
     xlab = "Log of proportional change of Average Temperature")

################################################ TASK 5 ########################################################

#read data from CSV file created in Task 1
clean.data.in.csv.format <- read.csv("~/Desktop/Clean Preprocessed Data.csv", header = TRUE)
VALUE.Column.Data <- clean.data.in.csv.format$Value

#computing log of proportional change of Average Temperature
log.of.proportional.change.of.Average.Temperature <- 
  log((head(VALUE.Column.Data, -1))/tail(VALUE.Column.Data)) 

#calculate monthly drift rate
drift.rate <- mean(log.of.proportional.change.of.Average.Temperature)
#calculate monthly volatility rate
volatility.rate <- sd(log.of.proportional.change.of.Average.Temperature)

#add missing value for the first proportional change as zero 
log.of.proportional.change.of.Average.Temperature <- c(0,log.of.proportional.change.of.Average.Temperature)

#add new column for log_of_proportional_change_of_Average_Temperature
clean.data.in.csv.format$log_of_proportional_change_of_Average_Temperature <- log.of.proportional.change.of.Average.Temperature

#drop column month
clean.data.in.csv.format <- 
  clean.data.in.csv.format[,-2]

#drop column Value
clean.data.in.csv.format <- 
  clean.data.in.csv.format[,-2]

#calculate drift rate year by year and presenting into a table
library(tidyverse)
Drift.rate.table <- aggregate(clean.data.in.csv.format,
          by = list(clean.data.in.csv.format$Year), FUN = mean)
Drift.rate.table[,-1]
names(Drift.rate.table)[names(Drift.rate.table) 
                        == "log_of_proportional_change_of_Average_Temperature"] <- "Drift_Rate_by_each_year"
Drift.rate.table

#calculate volatility rate year by year and presenting into a table
Volatility.rate.table <- aggregate(clean.data.in.csv.format,
          by = list(clean.data.in.csv.format$Year), FUN = sd)
Volatility.rate.table[,-2]
names(Volatility.rate.table)[names(Volatility.rate.table)
                             == "log_of_proportional_change_of_Average_Temperature"] <- "Volatility_Rate_by_each_year"
Volatility.rate.table

plot(Drift.rate.table$Year, Drift.rate.table$Drift_Rate_by_each_year, type = 'o', xlab = "Year", ylab = "Drift Rate", col="red", main = "Drift Rate between 1959-1979")
plot(Volatility.rate.table$Group.1,Volatility.rate.table$Volatility_Rate_by_each_year, type = 'o', col= "blue", xlab = "Year", ylab = "Volatility Rate", main = "Volatility Rate between 1959-1979")
