load_power_data <- function(directory,
                            file_name,
                            date_filter_begin=as.Date("2007/02/01",format="%Y/%m/%d"),
                            date_filter_end=as.Date("2007/02/02",format="%Y/%m/%d"),
                            date_format="%d/%m/%Y",
                            time_format="%H:%M:%S",
                            sep=";",
                            na.strings=c("?")) {
  power_data <- read.csv(file.path(directory,file_name),header=TRUE,sep=sep,na.strings=na.strings)
  power_data$Time <- strptime(power_data$Time,format=time_format)
  power_data$Date <- as.Date(power_data$Date,format=date_format)
  power_data <- subset(power_data,power_data$Date>=date_filter_begin&power_data$Date<=date_filter_end)
}

build_time_series <- function(data,
                              xlab,
                              ylab,
                              height=480,
                              width=480,
                              freq=365,
                              file_name="plot2.png") {
  axis_label_locations = c(0,length(data)/2,length(data)) 
  png(filename=file_name,
      height=height,
      width=width)
  ts(data,frequency=freq)
  plot.ts(data,xlab="",ylab=ylab,xaxt="n")
  axis(side=1,at=axis_label_locations,labels=xlab)
  dev.off()
}

#Load the power data and generate a histogram
print("Loading power data")
power_data<-load_power_data("power_consumption","household_power_consumption.txt")
print("Generating time-series plot from Global Active Power data")
build_time_series(power_data$Global_active_power,c("Thurs","Fri","Sat"),"Global Active Power(kilowatts)")