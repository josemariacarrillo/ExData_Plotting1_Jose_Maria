
plot4 <- function (input){
        
        library(colbycol)
        
        i.can <- cbc.read.table(input, header = TRUE, sep = ";")
        
        mydata <- as.data.frame(i.can)
        
        mydata$Datetime<-as.POSIXct(paste(mydata$Date,mydata$Time),format="%d/%m/%Y %H:%M:%S")
        
        i<-c("31/1/2007 24:00:00")
        i<-as.POSIXct(i,format="%d/%m/%Y %H:%M:%S")
        
        j<-c("3/2/2007 00:00:00")
        j<-as.POSIXct(j,format="%d/%m/%Y %H:%M:%S")
        
        dat<-subset(mydata,mydata$Datetime>=i & mydata$Datetime<=j)
        
        dat$Global_active_power<-as.numeric(dat$Global_active_power)
        dat$Sub_metering_1<-as.numeric(dat$Sub_metering_1)
        dat$Sub_metering_2<-as.numeric(dat$Sub_metering_2)
        dat$Sub_metering_3<-as.numeric(dat$Sub_metering_3)
        
        par(mfrow=c(2,2))
        p4.1<-with(dat,plot(dat$Datetime,dat$Global_active_power, main="", ylab="Global Active Power (kilowatts)",xlab="",cex.lab=0.8, pch=".")) + lines(dat$Datetime,dat$Global_active_power,lty=1)
        p4.2<-with(dat,plot(dat$Datetime,dat$Voltage, main="", ylab="Voltage",xlab="datetime",cex.lab=0.8, pch=".")) + lines(dat$Datetime,dat$Voltage,lty=1)
        p4.3<-with(dat,plot(dat$Datetime,dat$Sub_metering_1, main="", ylab="Energy sub metering",xlab="",cex.lab=0.8, pch=".")) + lines(dat$Datetime,dat$Sub_metering_1,lty=1,cex.lab=0.8,col="purple") + lines(dat$Datetime,dat$Sub_metering_2,lty=1,cex.lab=0.8,col="orange") + lines(dat$Datetime,dat$Sub_metering_3,lty=1,cex.lab=0.8,col="blue")
        p4.3<-legend("topright",col=c("purple","orange","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd=1,pt.cex = 1,cex=0.5)
        p4.4<-with(dat,plot(dat$Datetime,dat$Global_reactive_power, main="", ylab="Global_reactive_power",xlab="datetime",cex.lab=0.8, pch=".")) + lines(dat$Datetime,dat$Global_reactive_power,lty=1)

        dev.copy(png,"./plot4.png")
        dev.off()
        
        return (plot4)  
}
