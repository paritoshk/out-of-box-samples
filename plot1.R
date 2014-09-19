#Abu Nayeem
#Plot 1

#download and unzip file and MAKE sure file is in working directory

fh<-file("household_power_consumption.txt","r");
Power<- read.table(text = grep("^[1,2]/2/2007",readLines(fh),value=TRUE), sep=";",na.strings="?")
colnames(Power)<- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

# Convert Data and Time together
DateTime <-paste(Power$Date,Power$Time)
Power$DateTime <-strptime(DateTime, "%d/%m/%Y %H:%M:%S")

#Plot 1
hist(Power$Global_active_power, xlab="Global Active Power (kilowatts)",main="Global Active Power",col="red",)
dev.copy(png,filename="plot1.png")
dev.off()