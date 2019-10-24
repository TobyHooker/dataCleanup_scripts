## Working with UPHL Lab Data ... ##########
# see also:  R_LabData_fix_150813.txt
# Step1:  Import data from Lenora S into Excel
# Step2:  Select appropriate Trip IDs
# Step3:  Evaluate data (flat)
    # Data flags and Analysis Comments
    # Use previous versions to populate Parameter Names (PARAM.1 and PARAM.METH.4)
    # Recalculate RESULT values (as RESULT.val), building on LRL and URLs, 
    # Check for Zeros (are they real ?)
# Step4:  Export data as *.csv
# Step5:  Import into R [here]
# Step6:  < If all Result.calculations and Parameter.names have been adjusted...
    # Cross-tab the data (DCAST)
# *** Don't worry about other (subsequent) calcualtions or operations...these results will be 
# split into data groups (by project), combined with field and other data, before being re-
# imported back into R for analysis...

###########################################
### 151117 Lab Data (DWQ / USU / UGS)
LabData_2015_151117_t1 <- read.csv("L:/NewDAta/LabData_2015_151117_t1.csv")
data <- LabData_2015_151117_t1
data.nam <- as.data.frame(names(data))

## DCAST based on San Juan dataset (Aug 2015)
Lab.DAT0 <- dcast(data, Station.ID + Sample.Date + Sample.Time + Trip.ID + Sample.Description ~ PARAM.METH.4,
                 value.var="RESULT.val",length);
Lab.DAT1 <- dcast(data, Station.ID + Sample.Date + Sample.Time + Trip.ID + Sample.Description ~ PARAM.METH.4,
                 value.var="RESULT.val",mean);
write.csv(Lab.DAT1, file="Lab.DAT151117.csv", row.names=F)

###  <UPDATE 151120 to Data>  #####
# Fixed several coding errors...
LabData_2015_151120_t2 <- read.csv("L:/NewDAta/LabData_2015_151120_t2.csv")
data <- LabData_2015_151120_t2
Lab.DAT0 <- dcast(data, Station.ID + Sample.Date + Sample.Time + Trip.ID + Sample.Description ~ PARAM.METH.4,
                 value.var="RESULT.val",length);

Lab.DAT1 <- dcast(data, Station.ID + Sample.Date + Sample.Time + Trip.ID + Sample.Description ~ PARAM.METH.4,
                 value.var="RESULT.val",mean);

write.csv(Lab.DAT1, file="Lab.DAT151120.csv", row.names=F)
 
####################################
#### 151120 Lab Data (IW Historical)
HistLabDat1 <- read.csv("C:/Users/tobyhooker/Dropbox/REF Stds 2014+15/HistLabDat1.csv")
  Hist.DAT0 <- dcast(HistLabDat1, STORET + Date ~ PARAM4, value.var="RESULT.VALUE", length);
  Hist.DAT1 <- dcast(HistLabDat1, STORET + Date ~ PARAM4, value.var="RESULT.VALUE", mean);
write.csv(Hist.DAT1, file="HistLab.DAT151119.csv", row.names=F)

#### O T H E R   D A T A   T Y P E S   #######
## Historical Data ##
#  [1] Invertebrates   ########################
InvH.dat0 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/Inv.hist.dat0.csv")
names(InvH.dat0)

InvH.dat1.len <- dcast(InvH.dat0, STORET + Date + Sample.Type ~ Taxon.Metric,
                   value.var="Value", length);

InvH.dat1 <- dcast(InvH.dat0, STORET + Date + Sample.Type ~ Taxon.Metric,
                   value.var="Value", mean);

InvH.dat1 <- InvH.dat1[with(InvH.dat1, order(STORET, Date, Sample.Type)),]
names(InvH.dat1)

write.csv(InvH.dat1, file="Hist.InvertData.151125_v2.csv", row.names=F)
#####
  InvH.metrics <- subset(InvH.dat1, Type=="metric");
  InvH.metrics <- droplevels(InvH.metrics)
  names(InvH.metrics)
InvH.metrics1 <- InvH.metrics[,c(1:4,12,14,23,41,51,62,67,68,76)]

#####
## pull out the Taxa only...
names(InvH.dat1)
InvH.taxa <- InvH.dat1
  InvH.taxa1 <- InvH.taxa[,c(-11,-13,-22,-40,-50,-61,-66,-67,-75)]
names(InvH.taxa1)
names(InvH.taxa1)[names(InvH.taxa1) == "Simuliidae sp."] <- "Simuliidae"
## okay.
write.csv(InvH.taxa1, file="Hist.InvertData-X.151125_v3.csv", row.names=F)

#####################

## [2] SAV
# data == SAVcov.dat0

# use plyR to aggregate results (by STORET x Date)
names(SAVcov.dat0)
summary(SAVcov.dat0)

# [1] remove 999 from Alg.SAV.class --> to NA
#   dat <- SAVcov.dat0
#   dat[(dat$Alg.SAV.class==999),16] <- NA
SAVcov.dat0[(SAVcov.dat0$Alg.SAV.class==999),16] <- NA  ## works

# [2] trim columns 
SAVcov.dat1 <- SAVcov.dat0[,c(3,6,8,13,14,16:30)]
class(SAVcov.dat1$Date)

SAVcov.dat1$Year <- as.numeric(format(as.Date(SAVcov.dat1$Date, "%m/%d/%Y"),"%Y"))  # works !!
SAVcov.dat1$Month <- as.numeric(format(as.Date(SAVcov.dat1$Date, "%m/%d/%Y"),"%m"))  # works !!
SAVcov.dat1$DateX <- as.Date(SAVcov.dat1$Date, "%m/%d/%Y")
SAVcov.dat1$DOY <- as.numeric(SAVcov.dat1$DateX - as.Date("2008-01-01"))

#  Summarize data by Site (STORET and Date)
  SAVcov.dat2 <- SAVcov.dat1
SAVcov.dat2.mean <- ddply(SAVcov.dat2, .(TRT, STORET, Date), numcolwise(mean, na.rm=T))

SAVcov.dat2.mean <- SAVcov.dat2.mean[with(SAVcov.dat2.mean, order(Date, STORET, TRT)),]

write.csv(SAVcov.dat2.mean, file="Hist.SAVcovAvg.151125_v1.csv", row.names=F)


### Look at 2008 Carp Exclusion Experiment  (I can't find a write up...)
# data ==
SAV2008 <- subset(SAVcov.dat2.mean, Year==2008)
# [plot 1]

z0 <- ggplot(data=subset(SAV2008, Month < 10), aes(x=TRT)) + theme_bw();
z1 <- z0 + geom_boxplot(aes(y=SAV_COV), na.rm=T) + ylab("% Cover of SAV") + xlab(NULL) +
          scale_x_discrete(labels=c("Untreated", "Carp Excluded"));
z2 <- z1 + geom_dotplot(aes(y=SAV_COV), binaxis="y", stackdir="center", na.rm=T,
                        dotsize=0.5, fill="darkgreen");
z2

# [plot 2]

v0 <- ggplot(data=subset(SAV2008, Month < 10), aes(x=as.factor(Month), col=TRT)) + theme_bw();
v1 <- v0 + geom_boxplot(aes(y=SAV_COV), na.rm=T) + xlab("Month of Year (2008)") + ylab("% Cover of SAV");
v1

# [plot 3]
data=subset(SAV2008, Month < 10)
dataX <- droplevels(data)
# trim the dataset some more (drop size w/ few Obs)
dataXX <- subset(dataX, STORET != 4985620 & STORET != 4985650)
dataXX <- droplevels(dataXX)

b0 <- ggplot(data=dataXX, aes(x=DOY, col=TRT)) + theme_bw();
b1 <- b0 + geom_point(aes(y=SAV_COV), na.rm=T) + xlab("Month of Year (2008)") + ylab("% Cover of SAV")

b2 <- b1 + facet_wrap(~STORET, scales="fixed", ncol=3);
b2

## could try to lump the specific sites a bit more...
## Not too excited about a glm model to do this...given the wide range of sampling intensity...

# try plot3 for sites w/ at least 3 obs...
dataXXX <- subset(dataXX, STORET != 4985350 & STORET != 4985465)
dataXXX <- droplevels(dataXXX)

b0 <- ggplot(data=dataXXX, aes(x=DOY, col=TRT)) + theme_bw();
b1 <- b0 + geom_point(aes(y=SAV_COV), na.rm=T) + xlab("Month of Year (2008)") + ylab("% Cover of SAV")
b2 <- b1 + facet_wrap(~STORET, scales="fixed", ncol=3);
b2

### go ahead w/ these...genearlize by variable ID (num)
# 4 / 5 / 6 / 10 / 11 / 18 / 19 / 20
var.list <- c(4,5,6,10,11,18,19,20)
dataXXX <- dataXXX[with(dataXXX, order(STORET, Date, TRT)),]
i = 11

for (i in var.list) {
b0 <- ggplot(data=dataXXX, aes(x=DOY, col=TRT)) + theme_bw();
b1 <- b0 + geom_point(aes(y=dataXXX[,i]), na.rm=T) + xlab("Day of Year (2008)") +
          ylab(names(dataXXX[i])) + geom_line(aes(y=dataXXX[,i]), na.rm=T)
b2 <- b1 + facet_wrap(~STORET, scales="fixed", ncol=3);
b2

ggsave(file=paste("Hist_CarpEfx_",names(dataXXX[i]),"_0.jpeg", sep=""),
          width=6, height=8, dpi=500);

}
## color is a little tacky...but okay for now...




