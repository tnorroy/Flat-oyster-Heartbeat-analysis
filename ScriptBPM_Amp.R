#This file is the main script. 
#It reads all the data,
#selects a time interval in each file,
#over this interval, calculates the average period of the 10 oysters (the bpm),
#the average maximum amplitude and the average minimum amplitude.
#
#The results are presented in a data frame.
#The results can be imported in a csv file
#
#The time interval over which each calculation is performed can be set by the user. 

library("lubridate")
library("dplyr")
library("zoo")
library(ggplot2)

setwd("/Users/tomnorroy/Desktop/Rscript Local")
source("rolling_avg - Copy.R")

#Data frame with all the data files ordered
fnames <- order_folder()

interval<- 60
window_size_running_average <- 601
timestring <- "2022-11-05 13:00:00"

period1 <- list()
period2 <- list()
period3 <- list()
period4 <- list()
period5 <- list()
period6 <- list()
period7 <- list()
period8 <- list()
period9 <- list()
period10 <- list()

amplitude1 <- list()
amplitude2 <- list()
amplitude3 <- list()
amplitude4 <- list()
amplitude5 <- list()
amplitude6 <- list()
amplitude7 <- list()
amplitude8 <- list()
amplitude9 <- list()
amplitude10 <- list()

datetime_list <- list()
element <-3

#Loop to browse into the folder
for (element in 1:(length(fnames[,2]))){
  
  print(element)
  timestring <- fnames[element,2]
  second(timestring) <- 1
  
  #if (hour(timestring) >= 20 | hour(timestring) <= 8){}
    data_file_csv <- find_read_convert(timestring,fnames)
    datetime_list[length(datetime_list)+1] <- as.character(timestring)
    
    timestring <- data_file_csv[1,c("Time")] 
    print(element)
    data_time_interval <- select_interval(interval, ymd_hms(timestring), data_file_csv)
  
    #Check if there is no missing data in the interval
    for (k in 1:(length(data_time_interval[,1])-1)) { 
      if (ymd_hms(data_time_interval[k+1,1]) - ymd_hms(data_time_interval[k,1]) > 2) {
      
        timestring <- data_time_interval$datetime[length(data_time_interval$datetime)]
        data_time_interval <- select_interval(interval, ymd_hms(timestring), data_file_csv)
      }
    }
 
    #Loop to calculate each parameters for every oyster during the interval
    for (i in 2:ncol(data_file_csv)){
      if (i>11) {break}
      
      amplitude_max_list <- list()
      amplitude_min_list <- list()
      data_time_interval$roll_avg <- rollmean(as.numeric(data_time_interval[[i]]),window_size_running_average, fill=NA, align='center')
    
      intersection_points <- intersection_point_calculation(data_time_interval,as.numeric(data_time_interval[[i]]) )
      period <- periods_calculation(intersection_points)
      amplitude_signal <- amplitude_calculation(data_time_interval, intersection_points,as.numeric(data_time_interval[[i]]) )
    
      for (m in 1:(length(amplitude_signal)-1)){
        if (amplitude_signal[m] > amplitude_signal[m+1]){
          amplitude_max_list[length(amplitude_max_list)+1] <- amplitude_signal[m]
        }
        if (amplitude_signal[m]< amplitude_signal[m+1]){
          amplitude_min_list[length(amplitude_min_list)+1] <- amplitude_signal[m]
        }
      }
      
      sum_max_amplitude <- numeric(1)
      for (l in 1:(length(amplitude_max_list))){
        sum_max_amplitude <- sum_max_amplitude + as.numeric(amplitude_max_list[l])
      }
      
      sum_min_amplitude <- numeric(1)
      for (l in 1:(length(amplitude_min_list))){
        sum_min_amplitude <- sum_min_amplitude + as.numeric(amplitude_min_list[l])
      }
      
      
      if (i==2){
        period1[length(period1)+1] <- period
        amplitude1[length(amplitude1)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
        
      }
      if (i==3){
        period2[length(period2)+1] <- period
        amplitude2[length(amplitude2)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==4){
        period3[length(period3)+1] <- period
        amplitude3[length(amplitude3)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==5){
        period4[length(period4)+1] <- period
        amplitude4[length(amplitude4)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==6){
        period5[length(period5)+1] <- period
        amplitude5[length(amplitude5)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==7){
        period6[length(period6)+1] <- period
        amplitude6[length(amplitude6)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==8){
        period7[length(period7)+1] <- period
        amplitude7[length(amplitude7)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==9){
        period8[length(period8)+1] <- period
        amplitude8[length(amplitude8)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==10){
        period9[length(period9)+1] <- period
        amplitude9[length(amplitude9)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      if (i==11){
        period10[length(period10)+1] <- period
        amplitude10[length(amplitude10)+1] <- (sum_max_amplitude/length(amplitude_max_list))-(sum_min_amplitude/length(amplitude_min_list))
      }
      
      print(timestring)
    }
  #}
}



df_period_result <- data.frame(
  DateTime = unlist(datetime_list),
  Period1 = unlist(period1),
  Period2 = unlist(period2),
  Period3 = unlist(period3),
  Period4 = unlist(period4),
  Period5 = unlist(period5),
  Period6 = unlist(period6),
  Period7 = unlist(period7),
  Period8 = unlist(period8),
  Period9 = unlist(period9),
  Period10 = unlist(period10)
)

df_amplitude_result <- data.frame(
  DateTime = unlist(datetime_list),
  Amplitude1 = unlist(amplitude1),
  Amplitude2 = unlist(amplitude2),
  Amplitude3 = unlist(amplitude3),
  Amplitude4 = unlist(amplitude4),
  Amplitude5 = unlist(amplitude5),
  Amplitude6 = unlist(amplitude6),
  Amplitude7 = unlist(amplitude7),
  Amplitude8 = unlist(amplitude8),
  Amplitude9 = unlist(amplitude9),
  Amplitude10 = unlist(amplitude10)
)

print(df_period_result)

df_period_result$DateTime <- as.Date(df_period_result$DateTime)
df_period_result$BPM1 <- 60 /df_period_result$Period1
df_period_result$BPM2 <- 60 /df_period_result$Period2
df_period_result$BPM3 <- 60 /df_period_result$Period3
df_period_result$BPM4 <- 60 /df_period_result$Period4
df_period_result$BPM5 <- 60 /df_period_result$Period5
df_period_result$BPM6 <- 60 /df_period_result$Period6
df_period_result$BPM7 <- 60 /df_period_result$Period7
df_period_result$BPM8 <- 60 /df_period_result$Period8
df_period_result$BPM9 <- 60 /df_period_result$Period9
df_period_result$BPM10 <- 60 /df_period_result$Period10


ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM1)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 1") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 1")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM2)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 2") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 2")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM3)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 3") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 3")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM4)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 4") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 4")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM5)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 5") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 5")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM6)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 6") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 6")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM7)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 7") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 7")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM8)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 8") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 8")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM9)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 9") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 9")

ggplot(df_period_result, aes(x = factor(lubridate::isoweek(DateTime)), y = BPM10)) +
  geom_boxplot() +
  labs(x = "Week", y = "BPM Oyster 10") +
  ggtitle("Boxplot of the BPM through weeks for Oyster 10")


df_amplitude_result$DateTime <- as.Date(df_amplitude_result$DateTime)

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude1)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 1") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 1")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude2)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 2") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 2")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude3)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 3") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 3")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude4)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 4") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 4")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude5)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 5") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 5")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude6)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 6") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 6")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude7)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 7") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 7")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude8)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 8") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 8")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude9)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 9") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 9")

ggplot(df_amplitude_result, aes(x = factor(lubridate::isoweek(DateTime)), y = Amplitude10)) +
  geom_boxplot() +
  labs(x = "Week", y = "Amplitude Oyster 10") +
  ggtitle("Boxplot of the amplitude through weeks for Oyster 10")

OystersXaxis <- c("NO HW 1", "NO HW 2", "NO HW 3", "NL HW  4", "NL HW  5", "CR HW 6", "CR HW 7", "NO 25 8", "NO 25 9", "NO 25 10")
CI <- c(1.60, 1.34, 1.32, 1.80, 1.68, 1.72, 1.90, 2.10, 0.92, 1.54)

barplot(CI, names.arg = OystersXaxis, main = "Barplot", xlab = "Oysters", ylab = "CI")

write.csv(df_period_result, "D:/Rscript Local/period_results.csv", row.names=FALSE)
write.csv(df_amplitude_result, "D:/Rscript Local/amplitude_results.csv", row.names=FALSE)

