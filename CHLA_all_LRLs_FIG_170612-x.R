###  UPHL LIMS EDD workup -- 170105 ###
###
###  Looking at LRLs for CHLA data

library(ggplot2)
library(openxlsx)
library(reshape2)
options(scipen = 999) 
options(digits=3)
library(gdata)
library(plyr)
options(max.print = 1500)
###


filename <- choose.files(caption="Select Data File [xlsx]", multi=FALSE)
path.filename <- dirname(filename)
setwd(path.filename)
DATA.1 <- read.xlsx(filename, sheet=1, colNames=TRUE, rowNames=FALSE,
                    detectDates=FALSE)

##  Summarize DATAFILE  ##
data <- DATA.1
# DATA.1 <- data

data.Vars <- data.frame()  ## prepare DF
for (i in 1:length(names(data))) {
    class_i <- class(data[,i])
    name_i <- names(data[i])
    num.NAs <- sum(is.na(data[,i]))
    count_levels <- length(unique(data[,i]))
    num.blanks <- length(data[data[,i] == "",c(i)])
    data.Vars <- rbind(data.Vars, data.frame(i, name_i, class_i, num.NAs, 
                                             count_levels,num.blanks))   }

#######################
unique(DATA.1$Characteristic.Name)
unique(DATA.1$Detection.Condition)
#######################
plot.data <- DATA.1[!is.na(DATA.1$Detection.Condition),c("MLID", "Monitoring.Location.Type",
                "YEAR", "Characteristic.Name", "Detection.Condition", 
                "Detection/Quantitation.Limit.Type1", "Detection/Quantitation.Limit.Value1",
                "Detection/Quantitation.Limit.Unit1","Analytical.Method.ID" ),
                                drop=TRUE]
plot.data <- plot.data[plot.data$`Detection/Quantitation.Limit.Unit1` != "mg",,drop=TRUE]
plot.data <- plot.data[plot.data$`Characteristic.Name` != "Pheophytin a",,drop=TRUE]

names(plot.data) <- c("MLID","MonLocType","YEAR",   
                      "Characteristic.Name", "Detection.Condition" ,"DetLimitType1", 
                      "DetLimitVal1", "DetLimitUnit1" , "Analytical.Method.ID" )

plot.data[plot.data$DetLimitVal1 < 0.05,"DetLimitVal1"] <- 0.5
### plot LRL x YEAR
plot.data

z0 <- ggplot(data = plot.data) + theme_bw()
z1 <- z0 + geom_point(aes(x=YEAR, y=DetLimitVal1, col=Analytical.Method.ID), na.rm=T) + 
    scale_y_log10(breaks = c(0.05,0.1,0.5,2,5,10,20,50,100)) + ylab("CHLA conc. [ug/L]") +
    ggtitle("Lower Reporting Limit concentration over time") + 
    scale_x_continuous(breaks=c(1985,1990,1995,2000,2005,2010,2015))
z1
ggsave(file="CHLA_nonDet_conc_Fig3.jpeg", width=6.5, height=5, dpi=500) ## okay (170612)

##

z0 <- ggplot(data = plot.data) + theme_bw()
z1 <- z0 + geom_jitter(aes(x=YEAR, y=DetLimitVal1, col=Analytical.Method.ID), na.rm=T, height = 0) + 
    scale_y_log10(breaks = c(0.05,0.1,0.5,2,5,10,20,50,100)) + ylab("CHLA conc. [ug/L]") +
    ggtitle("Lower Reporting Limit concentration over time") + 
    scale_x_continuous(breaks=c(1985,1990,1995,2000,2005,2010,2015))
z1
ggsave(file="CHLA_nonDet_conc_Fig3b.jpeg", width=6.5, height=5, dpi=500)


###
#z0 <- ggplot(data = plot.data) + theme_bw()
#z1 <- z0 + geom_point(aes(x=YEAR, y=DetLimitVal1, col=MonLocType), na.rm=T) + 
#    scale_y_log10(breaks = c(0.05,0.1,0.5,2,5,10,20,50,100)) + ylab("CHLA conc. [ug/L]") +
#    ggtitle("Concentration of 'Non-Detect' Samples over time") + 
#    scale_x_continuous(breaks=c(1985,1990,1995,2000,2005,2010,2015))
#z1
#ggsave(file="CHLA_nonDet_conc_Fig2b.jpeg", width=6.5, height=5, dpi=500)
## this one has too many levels to be informative...

