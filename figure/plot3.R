#Abu Nayeem
#Plot 3

#download and unzip file and MAKE sure file is in working directory

fh<-file("household_power_consumption.txt","r");
Power<- read.table(text = grep("^[1,2]/2/2007",readLines(fh),value=TRUE), sep=";",na.strings="?")
colnames(Power)<- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

# Convert Data and Time together
DateTime <-paste(Power$Date,Power$Time)
Power$DateTime <-strptime(DateTime, "%d/%m/%Y %H:%M:%S")

#Plot
with(Power,plot(DateTime,Sub_metering_1, type="l", ylab="Energy Sub Metering",col="grey"))
points(Power$DateTime,Power$Sub_metering_2, type="l",col="red")
points(Power$DateTime,Power$Sub_metering_3, type="l",col="blue")
legend("topright",lty=1,col=c("grey","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.copy(png,filename="plot3.png")
dev.off()

#note lty makes it a line in the legend