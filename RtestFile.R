# Here I will write the script, that will be later transfered to Rmarkup file.

# 1. Setup of the analysis

setwd('C:/Users/Nika.Nika-Dell.000/Documents/GitHub/RepData_PeerAssessment1')

# Chack for the datafile. If not available - download and unzip.
if (!file.exists("activity.csv")) {
  if(!file.exists("activity.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip")
  }
  unzip("activity.zip")
}

# read the data into the dataframe
data<-read.csv("activity.csv",header=T)

# transform the date into proper format. 
data$date<-as.Date(data$date)

# make intervals 4 digit
data$interval<-paste("000",data$interval,sep="")
for (i in 1:nrow(data)) {
  data[i,"interval"]<-substr(data[i,"interval"],start=nchar(data[i,"interval"])-3,stop=nchar(data[i,"interval"]))
}

# create new variable for agregated time
# data$time<-strptime(paste(data$date,data$interval,sep=" "),format="%Y-%m-%d %H%M")

# Setup is compleate

# 1. What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
#    1. Make a histogram of the total number of steps taken each day
#    2. Calculate and report the mean and median total number of steps taken
#       per day

# calculate and plot the total number of steps taken each day

totalSteps<-sapply(split(data$steps,data$date),sum,na.rm=T)
hist(totalSteps,breaks=16,main="Total number of steps taken per day",col="blue",xlab="total number of steps")
abline(v=mean(totalSteps,na.rm=T),lwd=3,col="red")
text (8000,10,"mean")

cat("MEAN total number of steps per day is: ",mean(totalSteps,na.rm=T))
cat("MEDIAN total number of steps per day is: ",median(totalSteps,na.rm=T))

# Done with part 1.

intervalMean<-sapply(split(data$steps,data$interval),mean,na.rm=T)
intervals<-strptime(names(intervalMean),format="%H%M")

plot(intervals,intervalMean,type="l", col="blue",main="Average number of steps per interval", xlab="5 min intervals", ylab="average steps taken")
abline(h=max(intervalMean),col="red")
abline(v=intervals[intervalMean==max(intervalMean)]+0,col="red")
text(intervals[intervalMean==max(intervalMean)]-16000,max(intervalMean)-10, "Max (08:35, 206)")
# Which 5 minute interval contains most steps in average?
format(intervals[intervalMean==max(intervalMean)],"%H:%M")
# What is the maximum number of steps taken per period in average?
max(intervalMean)

# Done with task #2

# The number of missing values in the dataset
sum(is.na(data$steps))
table(complete.cases(data))

#------
# the NA handling methodology: average of the interval for whole dataset.
# Average number of steps per interval is calculated for the period, discarding the missing
# values. Next the apropriate values are inserted instead of missing values.

# Calculating avarage number of steps per interval (again!).

intervalMean<-sapply(split(data$steps,data$interval),mean,na.rm=T)

# Clone the database
newData<-data
# Find the missing values and insert apropriate average.
for (i in which(is.na(data$steps))) {
  newData[i,"steps"]<-intervalMean[data[i,"interval"]]
}

# calculate total number of steps and plot the histogram.

newTotSteps<-sapply(split(newData$steps,newData$date),sum)
TotSteps<-sapply(split(data$steps,data$date),sum,na.rm=T)
hist(newTotSteps,breaks=16,col="green")

mean(newTotSteps)

median(newTotSteps)

# ----------------------------------------------------------

# Question 4: Are there differences in activity patterns between weekdays and weekends?

for (i in 1:nrow(data)) {
  day<-weekdays(newData[i,"date"])
  ifelse (day=="Sunday" | day=="Saturday", newData[i,"day"]<-"weekend",newData[i,"day"]<-"weekday")
}
newData$day<-as.factor(newData$day)

library(lattice)


