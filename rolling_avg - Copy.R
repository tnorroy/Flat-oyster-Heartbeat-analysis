#This R file contain functions to :
# => Read the data
# => Select a time interval from the requested date
# => Find the intersection points between the heartbeat and the rolling average of it
# => Calculate the period and the max and min amplitude

library("lubridate")
library("dplyr")
library("zoo")

order_folder <- function(){
  
  #Put every data in a data frame and remove the file extension in all the names
  fnames <- data.frame(fname = list.files(path ="/Users/tomnorroy/Desktop/Rscript Local/data_Local/weeks",full.names=FALSE,recursive=TRUE))
  fnames$datetime <- gsub("min.CSV","",fnames$fname)
  
  #Convert the name of every file into datetime and order the file from the oldest to the latest
  fnames$datetime <- ymd_hm(fnames$datetime)
  fnames <- fnames[order(fnames$datetime),]
  
  return(fnames)
}

find_read_convert <- function(string,fnames){
  #Convert the requested date in a datetime
  
  Start <- ymd_hms(string)
  
  #Find the index of the file with the requested date in the list
  Index <- findInterval(Start,fnames$datetime)
  Filename <- fnames[Index,c("fname")]
  #Read the data, skip the headline and convert the data "Time" in the file into a datetime
  df <- read.csv(paste0("/Users/tomnorroy/Desktop/Rscript Local/data_Local/weeks/",Filename), skip = 21)
  df$datetime <- ymd_hms(df$Time)
  op <- options(digits.secs=6)
  return(df)
}

select_interval <- function(interval,start,df){
  
  #Select in the data the time interval requested and return it
  End <- start + interval
  selection <- filter(df,datetime >= start & datetime <= End)
  return(selection)
}


RootLinearInterpolant <- function (x, y, y0 = 0) {
  if (is.unsorted(x)) {
    ind <- order(x)
    x <- x[ind]; y <- y[ind]
  }
  z <- y - y0
  ## which piecewise linear segment crosses zero?
  k <- which(z[-1] * z[-length(z)] < 0)
  ## analytically root finding
  xk <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
  xk
}

intersection_point_calculation <- function(data, Oester){
  #Then first calculate the difference between the measured value and the rolling average:
  data$diff <- Oester - data$roll_avg

  #You can then calculate the intersections with the x-axis by
  Intersects <- RootLinearInterpolant(data$datetime, data$diff, y0 = 0)
  return(Intersects)
}

periods_calculation <- function(intersection){
  #Periods calculation
  diff_periods <- intersection[length(intersection)] - intersection[1]
  diff_periods_numeric <- as.numeric(diff_periods)
  periods <- (length(intersection))/2/diff_periods_numeric
  return(periods)
}


amplitude_calculation <- function(data, intersection, Oester){
# Amplitude calculation between all the intersection points
  amplitudes_between_intersects <- numeric(length(intersection) - 1)
  data$diff <- Oester - data$roll_avg
  
  for (i in 1:(length(intersection) - 1)) {
    # Find the index of the closest intersection point in test$datetime
    index1 <- which.min(abs(as.numeric(data$datetime) - as.numeric(intersection[i])))
    index2 <- which.min(abs(as.numeric(data$datetime) - as.numeric(intersection[i + 1])))
    index_mid <- (index1+index2)%/%2
    
    #Find the max amplitude between the 2 intersection points
    local_max_idx <- which.max(Oester[index1:index2]) + index1 - 1
    local_max_amplitude <- Oester[local_max_idx]
    
    if (data$diff[index_mid] <0){
      # If it is a minimum :
      local_max_idx <- which.min(Oester[index1:index2]) + index1 - 1
      local_max_amplitude <- Oester[local_max_idx]
    }
    amplitudes_between_intersects[i] <- local_max_amplitude
  }
  return(amplitudes_between_intersects)
}

