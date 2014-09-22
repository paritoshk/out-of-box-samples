#Abu Nayeem
#Plot 4

#download and unzip file and MAKE sure file is in working directory

fh<-file("household_power_consumption.txt","r");
Power<- read.table(text = grep("^[1,2]/2/2007",readLines(fh),value=TRUE), sep=";",na.strings="?")
colnames(Power)<- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

# Convert Data and Time together
DateTime <-paste(Power$Date,Power$Time)
Power$DateTime <-strptime(DateTime, "%d/%m/%Y %H:%M:%S")

# Plot [Note some of the coding is from previous assignments]
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(Power,{
  plot(DateTime, Global_active_power, type="l", ylab="Global Active Power")
  plot(DateTime, Voltage, type="l")
  plot(DateTime,Sub_metering_1, type="l", ylab="Energy Sub Metering",col="grey")
  points(Power$DateTime,Power$Sub_metering_2, type="l",col="red")
  points(Power$DateTime,Power$Sub_metering_3, type="l",col="blue")
  legend("topright",lty=1,bty="n",cex=0.4, col=c("grey","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  plot(DateTime, Global_reactive_power, type="l")
})
dev.copy(png,filename="plot4.png")
dev.off()

# cex adjusts the the size of legend words, bty removes boundaries