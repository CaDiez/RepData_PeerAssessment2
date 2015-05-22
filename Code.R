##packsUsed<-c("R.utils", "data.table")
##sapply(packsUsed, require, character.only=TRUE, quietly=TRUE)
library ("data.table", quietly=TRUE)
library ("R.utils")

cache = TRUE
## Check and load data
if (!"StormData.csv.bz2" %in% dir("./Data Science Specialization/05 Reproducible Research/RepData_PeerAssessment2")) {
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "stormData.csv.bz2")
}
if(!file.exists("StormData.csv"))
        bunzip2("stormData.csv.bz2", overwrite=T, remove=F)        
stormData <- read.csv("stormData.csv", sep = ",")

## Review data structure
dim(stormData)
head(stormData, n=2)

#Preprocces fieldnames and structure for further analysis
old <- names(stormData)
new <- tolower(old)
setnames(stormData, old, new)

# Define multipliers for valid exponents
powerExp <- data.frame(c("", "0", "H", "K", "M", "B"), c(1, 1, 10^2, 10^3,10^6, 10^9))
colnames(powerExp) <- c("validexp", "PowerBy")
# Subset data retaining only records with valid exponent
stormDataExp <- subset(stormData, (cropdmgexp %in% powerExp$validexp) & (propdmgexp %in% powerExp$validexp))
# Get the final economics in crops & props by multiplying factor by value
colnames(powerExp) <- c("validexp", "propdmgPower")
stormDataExp <- merge(stormData, powerExp, by.x = "propdmgexp", by.y = "validexp")
stormDataExp$propdmg <- (stormDataExp$propdmg * stormDataExp$propdmgPower)
colnames(powerExp) <- c("validexp", "cropdmgPower")
stormDataExp <- merge(stormDataExp, powerExp, by.x = "cropdmgexp", by.y = "validexp")
stormDataExp$cropdmg <- (stormDataExp$cropdmg * stormDataExp$cropdmgPower)

##Create a function to summarize any column of the dataset
selectColumnMetric<-function(field)
{
        ##summarize the field & create a data frame with titles
        tableColumn <- tapply(stormDataExp[,field],stormDataExp$evtype,sum)
        tableColumnSumOrd <- data.frame(eventtype=names(tableColumn), 
                             metric=as.numeric(tableColumn))
        #order by column in descending fashion
        tableColumnSumOrd <- tableColumnSumOrd[order(tableColumnSumOrd$metric,decreasing=TRUE),]
        # Take only top 20 & return
        tableColumnSumOrd <- tableColumnSumOrd[1:20,]   
        return(tableColumnSumOrd)
}

injuries<-selectColumnMetric("injuries")
fatalities<-selectColumnMetric("fatalities")

par(las=3, cex=0.3, mfrow=c(1,2))
barplot(fatalities$metric, names.arg=fatalities$eventtype, col=fatalities$eventtype,       
        ylab="Fatalities",
        main="Top 20 Fatalities/Event Type")
title(xlab = "Event Type", line=11)
barplot(injuries$metric, names.arg=injuries$eventtype, col=injuries$eventtype,       
        ylab="Injuries",
        main="Top 20 Injuries/Event Type")
title(xlab = "Event Type", line=11)

