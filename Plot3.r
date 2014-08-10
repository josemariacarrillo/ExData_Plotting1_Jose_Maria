
plot3 <- function (){
        
        if (!file.exists("coursera_output")){
                dir.create("coursera_output")
        }
        
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","./coursera_output/exdata-data-household_power_consumption.zip.")
        
        mydata_file_name<-unzip("exdata-data-household_power_consumption.zip")
        
        i.can <- cbc.read.table(mydata_file_name, header = TRUE, sep = ";")
        
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
        
        plot3<-with(dat,plot(dat$Datetime,dat$Sub_metering_1, main="", ylab="Energy sub metering",xlab="",cex.lab=0.8, pch=".")) + lines(dat$Datetime,dat$Sub_metering_1,lty=1,cex.lab=0.8,col="purple") + lines(dat$Datetime,dat$Sub_metering_2,lty=1,cex.lab=0.8,col="orange") + lines(dat$Datetime,dat$Sub_metering_3,lty=1,cex.lab=0.8,col="blue")
        plot3<-legend("topright",col=c("purple","orange","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd=1,pt.cex = 1,cex=0.5)
        dev.copy(png,"./coursera_output/plot3.png")
        dev.off()
        
        return (plot3)  
}