Data processing
---------------

``` r
if (!"datafile.csv.bz2" %in% dir("./")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","datafile.csv.bz2")
}

if(!"weatherdata" %in% ls()) {
        weatherdata <- read.csv("datafile.csv.bz2")
        
}

library(ggplot2)
```

Creating data frames for the event type, fatalities and inuries
===============================================================

``` r
weatherdataclean <- data.frame(weatherdata$EVTYPE,weatherdata$FATALITIES, weatherdata$INJURIES)
colnames(weatherdataclean) = c("EVTYPE", "FATALITIES", "INJURIES")
```

Creating data frames for event type, property damage and crop damage
====================================================================

``` r
damagedataclean <- data.frame(weatherdata$EVTYPE,weatherdata$PROPDMG, weatherdata$PROPDMGEXP, weatherdata$CROPDMG, weatherdata$CROPDMGEXP)
colnames(damagedataclean) = c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
```

Derriving the damage amount based on metric summary (K = 1,000, M = 1,000,000, B = 1,000,000,000). Create new metric for combined property + crop damage.
=========================================================================================================================================================

``` r
damagedataclean$PROPDMGMult <- ifelse (damagedataclean$PROPDMGEXP == "K", 1000, ifelse (damagedataclean$PROPDMGEXP == "M", 1000000, ifelse (damagedataclean$PROPDMGEXP == "B", 1000000000, 0)))

damagedataclean$PROPDMGAMT <- damagedataclean$PROPDMG*damagedataclean$PROPDMGMult

damagedataclean$CROPDMGMult <- ifelse (damagedataclean$CROPDMGEXP == "K", 1000, ifelse (damagedataclean$CROPDMGEXP == "M", 1000000, ifelse (damagedataclean$CROPDMGEXP == "B", 1000000000, 0)))

damagedataclean$CROPDMGAMT <- damagedataclean$CROPDMG*damagedataclean$CROPDMGMult

damagedataclean$TOTALDMGAMT <- damagedataclean$PROPDMGAMT+damagedataclean$CROPDMGAMT
```

Results
-------

Question 1: Across the United States, which types of events are most harmful with respect to population health?
===============================================================================================================

In this analysis "Harmful" will be interpreted as having the most injuries or most fatalities, which will be separated into two outputs below. In terms of "types of events", each will be examined individuually by event types and not by group of event types.

Fatalities
==========

The histogram below named "top 10 weather events by \# Fatalities"" is a summary of events, based on total numbers of fatalties by event type. The histogram is limited by the top 10 events.

``` r
weatherfatalities <- aggregate(weatherdataclean$FATALITIES, by = list(weatherdataclean$EVTYPE), FUN = sum, na.rm = TRUE)
colnames(weatherfatalities) = c("EVTYPE", "FATALITIES")
weatherfatalities <- weatherfatalities[order(-weatherfatalities$FATALITIES),]
topweatherfatalities <- weatherfatalities[1: 10, ]

p<- ggplot(topweatherfatalities, aes(x=reorder(EVTYPE, FATALITIES), y=FATALITIES))
p+geom_bar(stat = "identity", fill = "black")+ ggtitle("Top 10 Weather Events by # Fatalities")+labs(x = "Event Type", y="#Fatalities") +theme(axis.text.x = element_text(angle=45, hjust=1)) 
```

![](Reproudicble_research_project_2_files/figure-markdown_github/unnamed-chunk-5-1.png)

Based on the histogram "top 10 weather events by \# Fatalities" Tornados are the most harmful events to population health based on total number fatalities.

Injuries
========

The histogram below named "top 10 weather events by \# injuries" is a summary of events, based on total numbers of injuries by event type. The histogram is limited by the top 10 events.

``` r
weatherinjury <- aggregate(weatherdataclean$INJURIES, by = list(weatherdataclean$EVTYPE), FUN = sum, na.rm = TRUE)
colnames(weatherinjury) = c("EVTYPE", "INJURIES")
weatherinjury <- weatherinjury[order(-weatherinjury$INJURIES),]
topweatherinjury <- weatherinjury[1: 10, ]
q<- ggplot(topweatherinjury, aes(x=reorder(EVTYPE, INJURIES), y=INJURIES))
q+geom_bar(stat = "identity", fill = "black")+ ggtitle("Top 10 Weather Events by # Injuries")+labs(x = "Event Type", y="#Injuries") +theme(axis.text.x = element_text(angle=45, hjust=1)) 
```

![](Reproudicble_research_project_2_files/figure-markdown_github/unnamed-chunk-6-1.png) Based on the histogram "top 10 weather events by \# Injuries" Tornados are the most harmful events to population health based on total number fatalities.

Question 2: Across the United States, which types of events have the greatest economic consequences?
----------------------------------------------------------------------------------------------------

In this analysis "Economic consequences" will be interpreted as having the most Fatalities. In terms of "types of events", will be examined individuually by event types and not by group of event types.

Economic concequences
=====================

The histogram below named "top 10 weather events by total damage" is a summary of events, based on total damage in dollars by event type. The histogram is limited by the top 10 events.

``` r
TOTALDMGAMT <- aggregate(damagedataclean$TOTALDMGAMT, by = list(damagedataclean$EVTYPE), FUN = sum, na.rm = TRUE)
colnames(TOTALDMGAMT) = c("EVTYPE", "TOTALDMGAMT")
TOTALDMGAMT <- TOTALDMGAMT[order(-TOTALDMGAMT$TOTALDMGAMT),]
TOPTOTALDMGAMT <- TOTALDMGAMT[1: 10, ]

r<- ggplot(TOPTOTALDMGAMT, aes(x=reorder(EVTYPE, TOTALDMGAMT/1000000000), y=TOTALDMGAMT/1000000000))
r+geom_bar(stat = "identity", fill = "green")+ ggtitle("Top 10 Weather Events by Total Damage (in $ Billions)")+labs(x = "Event Type", y="Total Damage (in $ Billions)") +theme(axis.text.x = element_text(angle=45, hjust=1)) 
```

![](Reproudicble_research_project_2_files/figure-markdown_github/unnamed-chunk-7-1.png) Based on the histogram "top 10 weather by total damage" floods have the greates economic consequenses based on total damage by event type.

Conclusion
----------

The analysis shows that tornados are in terms of fatalities and injuries the most harmul event to population health. While Floods have the greatest economic consequences.
