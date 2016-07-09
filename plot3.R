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
                              file_name="plot3.png") {
  axis_label_locations = c(0,length(data$Sub_metering_1)/2,length(data$Sub_metering_1)) 
  png(filename=file_name,
      height=height,
      width=width)
  par(mfrow=c(1,1))
  ts(data$Sub_metering_1,frequency=freq)
  ts(data$Sub_metering_2,frequency=freq)
  ts(data$Sub_metering_3,frequency=freq)
  data_to_plot <- cbind(data$Sub_metering_1,data$Sub_metering_2,data$Sub_metering_3)
  plot.ts(data_to_plot,xlab="",ylab=ylab,xaxt="n",plot.type="s",col=c(1,2,4))
  axis(side=1,at=axis_label_locations,labels=xlab)
  legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),col=c(1,2,4))
  dev.off()
}

#Load the power data and generate a histogram
print("Loading power data")
power_data<-load_power_data("power_consumption","household_power_consumption.txt")
print("Generating multivariate time-series plot from Global Active Power data")
build_time_series(power_data,c("Thurs","Fri","Sat"),"Energy sub metering")