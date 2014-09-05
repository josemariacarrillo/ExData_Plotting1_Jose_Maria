
plot2 <- function (input){
        
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
        
        plot2<-with(dat,plot(dat$Datetime,dat$Global_active_power, main="", ylab="Global Active Power (kilowatts)",xlab="",cex.lab=0.8, pch=".")) + lines(dat$Datetime,dat$Global_active_power,lty=1)
        dev.copy(png,"./plot2.png")
        dev.off()
        
        return (plot2)  
}
