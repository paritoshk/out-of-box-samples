# Abu Nayeem
#Plot 2

#download and unzip file and MAKE sure file is in working directory

fh<-file("household_power_consumption.txt","r");
Power<- read.table(text = grep("^[1,2]/2/2007",readLines(fh),value=TRUE), sep=";",na.strings="?")
colnames(Power)<- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

# Convert Data and Time together
DateTime <-paste(Power$Date,Power$Time)
Power$DateTime <-strptime(DateTime, "%d/%m/%Y %H:%M:%S")

#Plot 2
plot(Power$DateTime, Power$Global_active_power)
with(Power,plot(DateTime, Global_active_power, type="l", ylab="Global Active Power (kilowatts)"))
dev.copy(png,filename="plot2.png")
dev.off()



