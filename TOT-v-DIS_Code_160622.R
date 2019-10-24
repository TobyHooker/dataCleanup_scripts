###  Total vs. Dissolved N and P concentrations ::
###  Figure and Data Evaluation

##  Project:  NPS Long-term Monitoring Sites and Data

##  Work_160621_v2
##  Compile AWQMS and 2015 Data for Total versus Dissolved Nutrients

library(openxlsx)
options(digits=3)
options(scipen = 999) 
library(ggplot2)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(plyr)
library(stats)

## Step 1 ::  Need to Import AWQMS file ::  ####

filename <- choose.files(caption="Select xlsx Data File to be Examined", multi=FALSE)
AWQMS.dat <- read.xlsx(filename);

## Step 2 ::  New to Import UPHL 2015 file ::

filename2 <- choose.files(caption="Select xlsx Data File to be Examined", multi=FALSE)
UPHL2015.dat <- read.xlsx(filename2)

## Step 3 ::  Select only total-P and total-N parameters...

AWQMS.NP <- AWQMS.dat[(AWQMS.dat$Characteristic.Name == "Phosphate-phosphorus" |
                      AWQMS.dat$Characteristic.Name == "Nitrogen"),]

UPHL2015.NP <- UPHL2015.dat[(UPHL2015.dat[,"Param#Description"] == "Phosphate, Tot. Dig. (as P)" |
                             UPHL2015.dat[,"Param#Description"] == "Total Nitrogen"),]

## Step 4 ::  Trim Variables and Combine Datasets  ####
# Step 4.1 :: Describe Variables
AWQMS.Vars <- data.frame()  ## prepare DF
for (i in 1:length(names(AWQMS.NP))) {
  class_i <- class(AWQMS.NP[,i])
  name_i <- names(AWQMS.NP[i])
  num.NAs <- sum(is.na(AWQMS.NP[,i]))
  count_levels <- length(unique(AWQMS.NP[,i]))
  num.blanks <- length(AWQMS.NP[AWQMS.NP[,i] == "",c(i)])
AWQMS.Vars <- rbind(AWQMS.Vars, data.frame(i, name_i, class_i, num.NAs, count_levels,num.blanks)) }

UPHL2015.Vars <- data.frame()  ## prepare DF
for (i in 1:length(names(UPHL2015.NP))) {
  class_i <- class(UPHL2015.NP[,i])
  name_i <- names(UPHL2015.NP[i])
  num.NAs <- sum(is.na(UPHL2015.NP[,i]))
  count_levels <- length(unique(UPHL2015.NP[,i]))
  num.blanks <- length(UPHL2015.NP[UPHL2015.NP[,i] == "",c(i)])
UPHL2015.Vars <- rbind(UPHL2015.Vars, 
                       data.frame(i, name_i, class_i, num.NAs, count_levels,num.blanks)) }

## Step 5 ::  Select NPS Sites   ####

AWQMS.NP.NPS <- AWQMS.NP[(as.numeric(AWQMS.NP$Monitoring.Location.ID) %in% NPS.sites[,1]),]

UPHL2015.NP.NPS <- UPHL2015.NP[(as.numeric(UPHL2015.NP[,"Station#ID"]) %in% NPS.sites[,1]),]

## Step 5.1 ::  Trim Variables

data <- AWQMS.NP.NPS

AWQMS.Var2 <- data.frame()  ## prepare DF
for (i in 1:length(names(data))) {
  class_i <- class(data[,i])
  name_i <- names(data[i])
  num.NAs <- sum(is.na(data[,i]))
  count_levels <- length(unique(data[,i]))
  num.blanks <- length(data[data[,i] == "",c(i)])
AWQMS.Var2 <- rbind(AWQMS.Var2, data.frame(i, name_i, class_i, num.NAs, count_levels,num.blanks)) }

AWQMS.NP.NPS2 <- AWQMS.NP.NPS[,c(3,10,27,29,30,31,32,40)] ## datafile

data <- UPHL2015.NP.NPS

UPHL2015.Var2 <- data.frame()  ## prepare DF
for (i in 1:length(names(data))) {
  class_i <- class(data[,i])
  name_i <- names(data[i])
  num.NAs <- sum(is.na(data[,i]))
  count_levels <- length(unique(data[,i]))
  num.blanks <- length(data[data[,i] == "",c(i)])
UPHL2015.Var2 <- rbind(UPHL2015.Var2, data.frame(i, name_i, class_i, num.NAs, count_levels,num.blanks)) }

UPHL2015.NP.NPS2 <- UPHL2015.NP.NPS[,c(10,12,31,34,37,38,40)] ## datafile

## Step 5.2 ::  Align Var names   ####

names(AWQMS.NP.NPS2) <- c("MLID", "Samp.Date", "Param", "Matrix", "Detect.Cond", "Res.Flag",
                          "Result.Val", "Detect.Limit")
AWQMS.NP.NPS2 <- droplevels(AWQMS.NP.NPS2)

names(UPHL2015.NP.NPS2) <- c("MLID", "Samp.Date", "Param", "Matrix", "Res.Flag",
                          "Result.Val", "Detect.Limit")

UPHL2015.NP.NPS2 <- droplevels(UPHL2015.NP.NPS2)

UPHL2015.NP.NPS2$Detect.Cond  <- NA
UPHL2015.NP.NPS2 <- UPHL2015.NP.NPS2[,c(1,2,3,4,8,5,6,7)]

NP.NPSdat <- rbind(AWQMS.NP.NPS2, UPHL2015.NP.NPS2)

## Step 6 ::  Examine Variable Classes ..  ####
data <- NP.NPSdat

Var.Class <- data.frame()  ## prepare DF
for (i in 1:length(names(data))) {
  class_i <- class(data[,i])
  name_i <- names(data[i])
  num.NAs <- sum(is.na(data[,i]))
  count_levels <- length(unique(data[,i]))
  num.blanks <- length(data[data[,i] == "",c(i)])
Var.Class <- rbind(Var.Class, data.frame(i, name_i, class_i, num.NAs, count_levels,num.blanks)) }

## Revise Var Classes

NP.NPSdat$MLID <- as.numeric(NP.NPSdat$MLID)

NP.NPSdat$Samp.Date <- convertToDate(NP.NPSdat$Samp.Date)
NP.NPSdat$Year <- format(NP.NPSdat$Samp.Date, "%Y")
NP.NPSdat$Year <- as.numeric(NP.NPSdat$Year)
NP.NPSdat$Samp.Date <- as.character.Date(NP.NPSdat$Samp.Date)

NP.NPSdat$Result.Val <- as.numeric(NP.NPSdat$Result.Val)

## Step 7 ::  Need to re-classify the MATRIX levels... ####

NP.NPSdat$Matrix2 <- mapvalues(NP.NPSdat$Matrix, 
                              from = c("Dissolved","Total","Water","Water, Dissolved"), 
                              to = c("Dissolved", "Total","Total","Dissolved"))

NP.NPSdat$Param <- mapvalues(NP.NPSdat$Param,
                             from=c("Nitrogen","Phosphate-phosphorus",
                                    "Phosphate, Tot. Dig. (as P)","Total Nitrogen"),
                             to=c("Nitrogen", "Phosphorus","Phosphorus","Nitrogen"))

## Should be okay -- remember that only non-U flagged data are used here...

## Step 8 ::  Cross-tab (Cast) data by Param & Matrix  ####
# This trims out any obs that have missing data (either not collected or below DL)
NP.dat <- NP.NPSdat[(is.na(NP.NPSdat$Detect.Cond) | is.na(NP.NPSdat$Res.Flag)),]

NP.NPS.data1 <- dcast(NP.dat[,c(1:3,10,7,9)], 
                      MLID + Samp.Date + Year + Param ~ Matrix2,
                      value.var="Result.Val", drop=T, mean)

nrow(NP.NPS.data1[(!is.na(NP.NPS.data1$Dissolved) & !is.na(NP.NPS.data1$Total)),]) # check on size

NPS.TNDN <- NP.NPS.data1[(NP.NPS.data1$Param == "Nitrogen"),] ## No data in this dataset...
NPS.TPDP <- NP.NPS.data1[(NP.NPS.data1$Param == "Phosphorus"),] ## 514 cases

#####  These data (NPS.xxx) should be ready to go...

### Phosphorus Data ::  NPS.TPDP  ####
nrow(NPS.TPDP[(!is.na(NPS.TPDP$Dissolved) & !is.na(NPS.TPDP$Total)),])

Data.Summ <- ddply(NPS.TPDP, .(Param), numcolwise(quantile, probes=c(0.1,0.25,0.5,0.75,0.9)),
                   na.rm=T, names=T)
Data.Summ$Var <- c("10%","25%", "median", "75%", "90%")
Data.Summ <- Data.Summ[,c(-2,-3)]
x1 <- count(!is.na(NPS.TPDP$Dissolved))
x1[1,1] <- "missing"
x1[2,1] <- "Value"
x2 <- count(!is.na(NPS.TPDP$Total))
x2[1,1] <- "missing"
x2[2,1] <- "Value"

xx <- cbind(x1, x2)
xx <- xx[,-3]
names(xx) <- c("Var", "Dissolved", "Total")
xx <- cbind(xx, rep("Phosphorus",2))
xx <- xx[,c(4,1,2,3)]
names(xx) <- c("Param", "Var", "Dissolved", "Total")

Data.Summ <- rbind(Data.Summ, xx)

write.xlsx(Data.Summ,"NPS.TPDP_varSumm.xlsx")

##  Data for figure - modeling only  #####
    TPDP.dat <- data.frame()
        DP <- runif(15, min=0.003, max=4.5)
        TP <- runif(15, min=0.003, max=4.5)
    TPDP.dat <- data.frame(DP, TP)
#  FIGURE 1
z0 <- ggplot(data=TPDP.dat) + theme_bw()
z1 <- z0 + geom_point(data=NPS.TPDP, aes(x=Total, y=Dissolved), na.rm=TRUE) + 
    coord_cartesian(xlim=c(0,0.5), ylim=c(0,0.5)) +
    geom_abline(slope=1, intercept=0, col="black", lty=5, size=0.6) + 
    ylab("Dissolved") + xlab("Total")

    tri <- data.frame(x=c(0.0001,4.999,4.999), y=c(0.0001,4.999,0.0001))
    lobar <- data.frame(x=c(0,0,0.006,0.003), y=c(0.003,0.006,0.006,0.003))
    bar <- data.frame(x=c(0,5,5,0), y=c(0.006, ((5*1.1)+0.006),5,0))
    blanks <- data.frame(x=c(0,0,0.016,0.006), y=c(0.006,0.016,0.016,0.006))

z2 <- z1 + geom_hline(yintercept=0.003, color="red", lty=2) +
        geom_vline(xintercept=0.003, color="red", lty=2) +
        geom_polygon(data=tri, aes(x=x, y=y), fill = "gray60", alpha=0.5) +
    annotate("text",x=0.3,y=0.2,hjust=0,vjust=0,label="OK ! :: TP > DP",fontface="bold",size=5) +
    annotate("text", x=0.005, y=0.45, hjust=-0.1, vjust=0, label="MRL = 0.003", col="red") +
    annotate("text", x=0.38, y=0.38, hjust=-0.20, vjust=1.5, label="1:1 Line", col="black",
             fontface="bold")

z3 <- z2 + geom_polygon(data=lobar, aes(x=x, y=y), fill="red")

z4 <- z3 + geom_polygon(data=bar, aes(x=x, y=y),fill="darkgreen", alpha=0.5) +
    annotate("text", x=0.37, y=0.38, hjust=1, vjust=-1.2, 
             label="10% allowable error\n on 1:1 Line", col="darkgreen", fontface="bold")

z5 <- z4 + geom_polygon(data=lobar, aes(x=x, y=y), fill="red")
z6 <- z5 +geom_polygon(data=blanks, aes(x=x, y=y), fill="red", alpha=0.5)
z6

ggsave(file="TPDP.figure1.NPSdata.jpeg", width=5, height=5, dpi=500)

### Figure 2 :: w/ Zoom to 0.15 mg P/L
z0 <- ggplot(data=TPDP.dat) + theme_bw()
z1 <- z0 + geom_point(data=NPS.TPDP, aes(x=Total, y=Dissolved), na.rm=TRUE) + 
    coord_cartesian(xlim=c(0,0.15), ylim=c(0,0.15)) +
    geom_abline(slope=1, intercept=0, col="black", lty=5, size=0.6) + 
    ylab("Dissolved") + xlab("Total")

    tri <- data.frame(x=c(0.0001,4.999,4.999), y=c(0.0001,4.999,0.0001))
    lobar <- data.frame(x=c(0,0,0.006,0.003), y=c(0.003,0.006,0.006,0.003))
    bar <- data.frame(x=c(0,5,5,0), y=c(0.006, ((5*1.1)+0.006),5,0))
    blanks <- data.frame(x=c(0,0,0.016,0.006), y=c(0.006,0.016,0.016,0.006))

z2 <- z1 + geom_hline(yintercept=0.003, color="red", lty=2) +
        geom_vline(xintercept=0.003, color="red", lty=2) +
        geom_polygon(data=tri, aes(x=x, y=y), fill = "gray60", alpha=0.5) +
    annotate("text",x=0.08,y=0.06,hjust=0,vjust=0,label="OK ! :: TP > DP",
             fontface="bold",size=5) +
    annotate("text", x=0.005, y=0.14, hjust=-0.1, vjust=0, label="MRL = 0.003", col="red") +
    annotate("text", x=0.11, y=0.11, hjust=-0.20, vjust=1.5, label="1:1 Line", col="black",
             fontface="bold")

z3 <- z2 + geom_polygon(data=lobar, aes(x=x, y=y), fill="red")

z4 <- z3 + geom_polygon(data=bar, aes(x=x, y=y),fill="darkgreen", alpha=0.5) +
    annotate("text", x=0.08, y=0.08, hjust=1, vjust=-1.2, 
             label="10% allowable error\n on 1:1 Line", col="darkgreen", fontface="bold")

z5 <- z4 + geom_polygon(data=lobar, aes(x=x, y=y), fill="red")
z6 <- z5 + geom_polygon(data=blanks, aes(x=x, y=y), fill="red", alpha=0.5)
z6

ggsave(file="TPDP.figure2.NPSdata.jpeg", width=5, height=5, dpi=500)

#
### Figure 3 :: w/ Zoom to 0.08 mg P/L
z0 <- ggplot(data=TPDP.dat) + theme_bw()
z1 <- z0 + geom_point(data=NPS.TPDP, aes(x=Total, y=Dissolved), na.rm=TRUE) + 
    coord_cartesian(xlim=c(0,0.08), ylim=c(0,0.08)) +
    geom_abline(slope=1, intercept=0, col="black", lty=5, size=0.6) + 
    ylab("Dissolved") + xlab("Total")

    tri <- data.frame(x=c(0.0001,4.999,4.999), y=c(0.0001,4.999,0.0001))
    lobar <- data.frame(x=c(0,0,0.006,0.003), y=c(0.003,0.006,0.006,0.003))
    bar <- data.frame(x=c(0,5,5,0), y=c(0.006, ((5*1.1)+0.006),5,0))
    blanks <- data.frame(x=c(0,0,0.016,0.006), y=c(0.006,0.016,0.016,0.006))

z2 <- z1 + geom_hline(yintercept=0.003, color="red", lty=2) +
        geom_vline(xintercept=0.003, color="red", lty=2) +
        geom_polygon(data=tri, aes(x=x, y=y), fill = "gray60", alpha=0.5) +
    annotate("text",x=0.051,y=0.03,hjust=0,vjust=-0.2,label="OK ! :: TP > DP",
             fontface="bold",size=4.5) +
    annotate("text", x=0.004, y=0.075, hjust=-0.1, vjust=0, label="MRL = 0.003", col="red") +
    annotate("text", x=0.066, y=0.066, hjust=-0.20, vjust=1.5, label="1:1 Line", col="black",
             fontface="bold")

z3 <- z2 + geom_polygon(data=lobar, aes(x=x, y=y), fill="red")

z4 <- z3 + geom_polygon(data=bar, aes(x=x, y=y),fill="darkgreen", alpha=0.5) +
    annotate("text", x=0.04, y=0.045, hjust=1, vjust=-1.3, 
             label="10% allowable error\n on 1:1 Line", col="darkgreen", fontface="bold")

z5 <- z4 + geom_polygon(data=lobar, aes(x=x, y=y), fill="red")
z6 <- z5 + geom_polygon(data=blanks, aes(x=x, y=y), fill="red", alpha=0.5)
z6

ggsave(file="TPDP.figure3.NPSdata.jpeg", width=5, height=5, dpi=500)














