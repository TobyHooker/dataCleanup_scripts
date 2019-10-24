## Water Chemistry Data Workup
# 151214
WChem_AllDat0 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/WChem_Alldat_151208.csv")

WChem0 <- WChem_AllDat0
WChem0.nam <- as.data.frame(names(WChem0))

summary(WChem0)

# STEP 1 :: FIX Dates  ######
  class(WChem0$Date)
WChem0$Date2 <- as.Date(WChem0$Date, format="%m/%d/%Y")
WChem0$Year <- as.numeric(format(WChem0$Date2, "%Y"))
WChem0$Month <- as.numeric(format(WChem0$Date2, "%m"))
WChem0$DOY <- yday(WChem0$Date2)
# STEP 2 :: Index Periods (IPs)  ######
WChem0$IP_calc <- ifelse(WChem0$DOY<166,NA,
                    ifelse(WChem0$DOY<228,1,
                    ifelse(WChem0$DOY>274,NA,
                    ifelse(WChem0$DOY>229,2,NA))));
# STEP 3 :: Add Water Depth   ######
WCh.wat <- WChem0[,c(3,74,73,2)]
SAV.wat <- SAVdat_dat1[,c(5,55,16)]
  WCh.wat$iden <- interaction(WCh.wat$MLID, WCh.wat$Date2, sep=".")
  SAV.wat$iden <- interaction(SAV.wat$STORET, SAV.wat$Date2, sep=".")
write.csv(WChem0, "WChem0_151214_v0.csv", row.names=F)
# fixed water depth data (as available), in excel...
WChem1 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/WChem0_151214_waterZ_v1.csv")
# STEP 4 :: Add more descriptive Dataset Names..."SET.name" ######
levels(WChem1$SET)
WChem1$SET.name <- WChem1$SET
levels(WChem1$SET.name) <- c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
                           "Willard Spur (2011-3)")
##
####### SUMMARIZE VARIABLES ####
summary(WChem1)
## Dump this back into Excel, calculate:
# 1) derived nutrient variables (e.g. DIN, TOC:TN, etc.)
# 2) Metals Exceedences (Prob. of Exceedence for each dataset [PoEx])
# 
  write.csv(WChem1, "WChem1_dat_151215.csv", row.names=F)
## Calculated derived NUTR vars && Metals Exceedences -- DONE !  ###########
WChem2 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/REFSTD_R/WChem2_dat_151215.csv")
#############################################################################
################################ MASTER WATER CHEM DATA == WChem2 ###########
######## Trim Variables ##
# data == WChem2
Wch2.nam <- as.data.frame(names(WChem2))
WChem3 <- WChem2
WChem3 <- WChem3[with(WChem3, order(iden)),]
names(WChem3)[names(WChem3) == "W.Temp.C."] <- "W.TempC"
names(WChem3)[names(WChem3) == "TIN.TIN"] <- "TIN.TN"
WCh3.nam <- as.data.frame(names(WChem3))

WChem3X <- WChem3[,c(74,70,72,75,62,64,9,11,12,10,13,14,15,17,63,
                     19,20,23,26,27,29,30,31,32,41,44,42,53,47,54,56,57,66,7,115,112)] # omit DOC
Wch3x.nam <- as.data.frame(names(WChem3X))
WChem3X$DO.p <- as.numeric(WChem3X$DO.p)
summary(WChem3X)
#names(WChem3X)[names(WChem3X) == "W.Temp.C."] <- "W.TempC"
#names(WChem3X)[names(WChem3X) == "TIN.TIN"] <- "TIN.TN"
## Apparently, No blanks !!
WChem4 <- WChem3X
Wch4.nam <- as.data.frame(names(WChem4))

## scan through all vars and report back CLASS
i = 4
WCh4.var <- data.frame()
for (i in 1:length(names(WChem4))){
  class_i <- class(WChem4[,i])
  name_i <- names(WChem4[i])
  num.NAs <- sum(is.na(WChem4[,i]))
  WCh4.var <- rbind(WCh4.var, data.frame(i, name_i, class_i, num.NAs))  
#  WChem4[,i] <- as.numeric(WChem4[,i])
}

write.csv(WChem4, "WChem4_151216.csv")
# omit SE from dataset (too many NAs...)
# omit TVS.TSS
WChem5 <- WChem4
WChem5 <- WChem5[,c(-24,-32)]
## SE_D and TVS.TSS removed.

## omit rows w/ > 19 missing variables (of 31 from WChem5)
WChem6 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/REFSTD_R/WChem6_dat.csv")
##########################  PCA !! -- Too many Missing values ####
## labdsv :: Package
dat <- WChem6[,c(-1)]
dat <- as.matrix(dat)
pca(dat, cor=TRUE, dim=min(nrow(dat), ncol(dat)))
prcomp(dat, retx=TRUE, center=TRUE, scale.=TRUE)
class(dat)
sum(is.finite(dat))
sum(is.na(dat))  
##############################################
######################################################################
######################################################################
###  W A T E R   C H E M  -  P H Y S I C A L  C O N D I T I O N  #####
######################################################################
data = WChem3   # n = 1654; k = 115 vars (via WChem2_dat_151215.csv)
WChem3 <- droplevels(WChem3)
## examine variables ##
WChem3 <- WChem3[with(WChem3, order(SET,Month)),]
WCh3.var <- data.frame() # reports class of all vars
for (i in 1:length(names(WChem3))){
  class_i <- class(WChem3[,i])
  name_i <- names(WChem3[i])
  num.NAs <- sum(is.na(WChem3[,i]))
  WCh3.var <- rbind(WCh3.var, data.frame(i, name_i, class_i, num.NAs))  }

####  Water Fig 1: Water Depth  [Water.Z - 75 ]  #####
# VAR x month [SET] Facets (jitter)  [****]
data <- subset(WChem3, !is.na(Water.Z))

q0 <- ggplot(data, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=Water.Z), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,130), breaks=seq(0,125,30), oob=squish) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Water Depth (cm)") + 
      ggtitle("Seasonal patterns in Pond Water Depth\n");
## Run functions below - before - plotting these extra points ####
# Geomean
Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=Water.Z, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_WaterZ-x-Mnth_2.jpeg", width=7, height=7, dpi=500)   ##  This one is okay

####   Water Depth (FIG 1.5)   CDF plot (Fix from Surface Mat Code) :: 151230  #####
##[#5: Water Depth  CDF  (by SET) w/ Marginal Density Pot (x) ######
data3 <- subset(WChem3, !is.na(Water.Z) & !is.na(IP_calc))
data3 <- data3[with(data3, order(SET.name, Date2)),]
#
x0 <- ggplot(data=data3, aes(x=Water.Z, col=SET.name), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=Water.Z, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,125), breaks=seq(0,125,25)) + 
      xlab("Water Depth (cm)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot

margin.x3 <- ggplot(data=data3, aes(x=Water.Z, col=SET.name)) + 
  stat_density(aes(y=(..density..)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.9),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,125), breaks=seq(0,125,25)) + 
  scale_y_continuous(labels=percent) +
  scale_color_tableau(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
          "Willard Spur (2011-3)"));
margin.x3  ## this is the density plot(s)   + theme( axis.text.y = element_blank())   [marg [L]=1.4
# Use blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
   panel.border = element_blank(),panel.background = element_blank(),
   axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), 
   axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line = element_blank()   );
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="AllIw_wat-z_xSETgrop_CDF+MARG_5B.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
             main=textGrob("Cumulative Distribution of Pond Water Depths\n", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
####

#####  Summary Functions  #######
gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
cnt <- function(x) count(x);
nmissing <- function(x) sum(is.na(x));
nDiff <- function(x) (length(x)-nmissing(x));
gm_mean2 <- function(x, na.rm=T, zero.propagate=FALSE) 
      { if(any(x<0, na.rm=T)) {   return(NaN) }
      if(zero.propagate) { if(any(x==0, na.rm=T)){ return(1) }
      exp(mean(log(x), na.rm=na.rm))  }   
      else { exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))   }   }
Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
Summ.Geomean1$GRP <- "geomean1"
Summ.Geomean2 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean2, na.rm=T));
Summ.NA <- ddply(data, .(SET.name, Year, Month), numcolwise(nmissing));
Summ.NA$GRP <- "NA's"
Summ.Cnt <- ddply(data, .(SET.name, Year, Month), numcolwise(length));
Summ.Cnt$GRP <- "NObs"
Summ.Mean <- ddply(data, .(SET.name, Year, Month), numcolwise(mean, na.rm=T));
Summ.Mean$GRP <- "mean"
Summ.Med <- ddply(data, .(SET.name, Year, Month), numcolwise(median, na.rm=T));
Summ.Med$GRP <- "median"
Summ.dat3 <- rbind(Summ.Geomean1, Summ.NA, Summ.Cnt, Summ.Mean, Summ.Med)
Summ.dat3 <- Summ.dat3[,c(length(names(Summ.dat3)),1:(length(names(Summ.dat3))-1))]
Summ.dat3 <- Summ.dat3[with(Summ.dat3, order(SET.name, Year, Month, GRP)),]
# get specific summary from table
a <- as.data.frame(Summ.dat3[(Summ.dat3$GRP == "NObs"),c(1,2,3,4,72)]);
########### Another set of Calcs -- by Dataset Only ######
Summ2.Geomean1 <- ddply(data, .(SET.name), numcolwise(gm_mean1, na.rm=T));
Summ2.Geomean1$GRP <- "geomean1"
Summ2.NA <- ddply(data, .(SET.name), numcolwise(nmissing));
Summ2.NA$GRP <- "NA's"
Summ2.Cnt <- ddply(data, .(SET.name), numcolwise(length));
Summ2.Cnt$GRP <- "NObs"
Summ2.Mean <- ddply(data, .(SET.name), numcolwise(mean, na.rm=T));
Summ2.Mean$GRP <- "mean"
Summ2.Med <- ddply(data, .(SET.name), numcolwise(median, na.rm=T));
Summ2.Med$GRP <- "median"
Summ2.dat3 <- rbind(Summ2.Geomean1, Summ2.NA, Summ2.Cnt, Summ2.Mean, Summ2.Med)
Summ2.dat3 <- Summ2.dat3[,c(length(names(Summ2.dat3)),1:(length(names(Summ2.dat3))-1))]
# get specific summary from table
aa <- as.data.frame(Summ2.dat3[(Summ2.dat3$GRP == "NObs"),c(1,2,3,4,72)]);
########################################################

## Water Fig 2: Water Temp  [  W.TempC - 62 ]
# VAR x month [SET] Facets (jitter)  [****]
data <- subset(WChem3, !is.na(W.TempC))
q0 <- ggplot(data, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F")) +
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=W.TempC), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(NA,32), oob=squish) +
      scale_x_discrete(labels=c("Jan", "Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec")) +
      geom_hline(yintercept=27, lty=2, col="black");
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Water Temperature (C)") + 
      ggtitle("Seasonal patterns in Pond Water Temperature\n");
q2
## Run functions below - before - plotting these extra points # Geomean ####
Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=W.TempC, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_WaterTempxMnth_2b.jpeg", width=7, height=7, dpi=500)   ##  This one is okay

## Water Fig 2-B: Water Temp  [  W.TempC - 62 ]
# VAR x CDF w/ marginal plots...[****]
data2 = subset(data, !is.na(IP_calc))
x0 <- ggplot(data=data2, aes(x=W.TempC, col=SET), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=W.TempC, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,32), oob=squish, breaks=seq(0,32,5)) + 
      xlab("Water Temperature (C)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black") +
      geom_vline(xintercept=27, lty=2, col="blue");
x1   ## this is the CDF plot
margin.x3 <- ggplot(data=data2, aes(x=W.TempC, col=SET)) + 
  stat_density(aes(y=(..density..)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.9),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,32), oob=squish, breaks=seq(0,32,5)) + 
  scale_y_continuous(labels=percent) +
  scale_color_tableau(breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
  labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
          "Willard Spur (2011-3)"));
margin.x3  ## this is the density plot(s)   + theme( axis.text.y = element_blank())   [marg [L]=1.4
# Use blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
   panel.border = element_blank(),panel.background = element_blank(),
   axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), 
   axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line = element_blank()   );
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="AllIW_WATER-TEMP_xSETgrop_CDF+MARG_6.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
      main=textGrob("Distribution of Pond Water Temperature During Summer Index Periods", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
######

## Water Fig 3: Water Salinity (as EC25)  [  EC25 - 64 ]
# VAR x month [SET] Facets (jitter)  [****]
data <- subset(WChem3, !is.na(EC25))
q0 <- ggplot(data, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F")) +
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=EC25), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) + scale_y_continuous(limits=c(NA,25000), oob=squish,
            labels=comma, breaks=seq(0,24000, 8000)) +
      scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Specific Conductance (uS/cm @ 25C)") + 
      ggtitle("Seasonal patterns in Pond Specific Conductance\n");
q2
## Run functions below - before - plotting these extra points # Geomean ####
Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=EC25, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_WaterEC25xMnth_1C.jpeg", width=7, height=7, dpi=500)   ##  Updated 151230

## Water Fig 3-B: Water Salinity (as EC25)  [  EC25 - 64 ]
# VAR x CDF w/ marginal plots...[****]
data2 = subset(data, !is.na(IP_calc))
x0 <- ggplot(data=data2, aes(x=EC25, col=SET), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.8, 0.17), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=EC25, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,25000), oob=squish, breaks=seq(0,24000, 8000)) + 
      xlab("Specific Conductance (uS/cm @ 25C)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot   ####
margin.x3 <- ggplot(data=data2, aes(x=EC25, col=SET)) + 
  stat_density(aes(y=(..density..)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.4),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,25000), oob=squish, breaks=seq(0,24000, 8000)) + 
  scale_y_continuous(labels=percent) +
  scale_color_tableau(breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
  labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
          "Willard Spur (2011-3)"));
margin.x3  ## this is the density plot(s)   + theme( axis.text.y = element_blank())   [marg [L]=1.4
# Use blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
   panel.border = element_blank(),panel.background = element_blank(),
   axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), 
   axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line = element_blank()   );
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="AllIW_WATER-EC25_xSETgrop_CDF+MARG_6.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
      main=textGrob("Distribution of Pond Specific Conductance During Summer Index Periods", vjust=1),
             left=textGrob("Proportion of data  below x-value", rot=90, vjust=1))
dev.off()
######
## Water Fig 3-C: Water Salinity (as EC25)  [  EC25 - 64 ] xx BOXPLOT  xx151230xx #####
data2 = subset(data, !is.na(IP_calc))
  w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=EC25), na.rm=T, outlier.shape=NA)
  w2 <- w1 + geom_jitter(aes(y=EC25, col=factor(Year)), 
                         position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("Specific Conductance (uS/cm @ 25C)") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,25000), oob=squish, breaks=c(0,2000,4000,8000,16000,24000), 
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=EC25, col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,25000), oob=squish, breaks=c(0,2000,4000,8000,16000,24000), 
                         labels=comma) + 
   scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728"), 
                      labels=c("Historical (2007-11)", "IW Survey (2012)", 
                      "West Desert IWs (2014-15)", "Willard Spur (2011-3)")) + coord_flip();
  margin.x2  ## this is the density plot(s)  data6

  legendX <- gtable_filter(ggplot_gtable(ggplot_build(margin.x2 + 
          theme(legend.position=c(0.42,2.6), legend.key.size=unit(0.25, "cm"), 
                legend.text=element_text(size=8),legend.margin=unit(0.00, "cm"),
                legend.background=element_rect()))), "guide-box")
# grid.draw(legendX)
# Use blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
   panel.border = element_blank(), panel.background = element_blank(),
   axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), 
   axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()         )
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="EC25_Box+Margin_v1C.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Pond Specific Conductance", vjust=1),legendX)
dev.off()
#### 151216 figures...


#  <Tox Metals:  See new Work-Code...
#  Major Ions
#  Nutrients...

#### 151221 Reload Workspace and Write data files as CSV...  #####
write.csv(WChem7, "WChem7_151221.csv", row.names=F)
write.csv(MajorIons2, "MajorIons2_151221.csv", row.names=F)


############    151230  More Data Exploration    ##############
### data = WChem3   (better than WCHem7...cleaner)   #####
## Functions:  
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   cnt <- function(x) count(x);
   nmissing <- function(x) sum(is.na(x));
   nDiff <- function(x) (length(x)-nmissing(x));
   gm_mean2 <- function(x, na.rm=T, zero.propagate=FALSE) 
      { if(any(x<0, na.rm=T)) {   return(NaN) }
      if(zero.propagate) { if(any(x==0, na.rm=T)){ return(1) }
      exp(mean(log(x), na.rm=na.rm))  }   
      else { exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))   }   }
#####
# Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
# Summ.Geomean1$GRP <- "geomean1"
# Summ.Geomean2 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean2, na.rm=T));

data <- WChem3

Summ.NA <- ddply(data, .(SET.name, Year), numcolwise(nmissing));
Summ.NA$GRP <- "NA's"
Summ.NA2 <- ddply(data, .(SET.name), numcolwise(nmissing));

Summ.Len <- ddply(data, .(SET.name, Year), numcolwise(length));
Summ.Len$GRP <- "Length"
Summ.Len2 <- ddply(data, .(SET.name), numcolwise(length));

Summ.Cnt <- ddply(data, .(SET.name, Year), numcolwise(cnt));
Summ.Cnt$GRP <- "Count"

Summ.nDiff <- ddply(data, .(SET.name, Year), numcolwise(nDiff));
Summ.nDiff$GRP <- "Nobs"

Summ.nDiff2 <- ddply(data, .(SET.name), numcolwise(nDiff));

#Summ.Mean <- ddply(data, .(SET.name, Year, Month), numcolwise(mean, na.rm=T));
#Summ.Mean$GRP <- "mean"
#Summ.Med <- ddply(data, .(SET.name, Year, Month), numcolwise(median, na.rm=T));
#Summ.Med$GRP <- "median"
#Summ.dat3 <- rbind(Summ.Geomean1, Summ.NA, Summ.Cnt, Summ.Mean, Summ.Med)
#Summ.dat3 <- Summ.dat3[,c(length(names(Summ.dat3)),1:(length(names(Summ.dat3))-1))]
#Summ.dat3 <- Summ.dat3[with(Summ.dat3, order(SET.name, Year, Month, GRP)),]  #####
Summ.nDiff <- ddply(data, .(SET.name, Year), numcolwise(nDiff));
#####

##################################################################################
### S A L I N I T Y ::  T D S = EC25  ############################################
####################  FROM MAJORION Code  ########################################
#  use WChem3 instead
WChem3[!is.na(WChem3$TDS) & WChem3$TDS==204000,]$TDS <- NA
data <- WChem3
data2 <- subset(data, (!is.na(TDS) & !is.na(EC25)))
###  Subset data by Dataset
Hist.dat <- subset(data2, SET=="HIST", select=c(SET,TDS,EC25))
IW12.dat <- subset(data2, SET=="IW12", select=c(SET,TDS,EC25))
WDes.dat <- subset(data2, SET=="REF1415", select=c(SET,TDS,EC25))
WSpur.dat <- subset(data2, SET=="WSpur", select=c(SET,TDS,EC25))
## Linear models x Dataset
qplot(x=EC25, y=TDS, data=Hist.dat, geom=c("point","smooth"))
qplot(x=EC25, y=TDS, data=IW12.dat, geom=c("point","smooth"))
qplot(x=EC25, y=TDS, data=WDes.dat, geom=c("point","smooth"))
qplot(x=EC25, y=TDS, data=WSpur.dat, geom=c("point","smooth"))

## Linear estimation of TDS ! EC25 slopes...All data  #######
mod <- lm(TDS ~ EC25, data2)
   b0 <- mod$coeff[1]
   b1 <- mod$coeff[2]
   rsq <- summary(mod)$adj.r.squared

mod0 <- lm(TDS ~ EC25 - 1, data2)
   b0.0 <- mod0$coeff[1]
   b1.0 <- mod0$coeff[2]
   rsq.0 <- summary(mod0)$adj.r.squared

hist.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="HIST"))
IW12.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="IW12"))
WDes.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="REF1415"))
WSpur.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="WSpur"))

   summary(WSpur.lm0)

hist.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="HIST"))
IW12.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="IW12"))
WDes.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="REF1415"))
WSpur.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="WSpur"))

   summary(WSpur.lm)

LM.summ <- data.frame()
hist.pars <- c(hist.lm0$coeff[1], hist.lm0$coeff[2], summary(hist.lm0)$adj.r.squared)
iw12.pars <- c(IW12.lm0$coeff[1], IW12.lm0$coeff[2], summary(IW12.lm0)$adj.r.squared)
wdes.pars <- c(WDes.lm0$coeff[1], WDes.lm0$coeff[2], summary(WDes.lm0)$adj.r.squared)
wspur.pars <- c(WSpur.lm0$coeff[1], WSpur.lm0$coeff[2], summary(WSpur.lm0)$adj.r.squared)

LM.summ <- as.data.frame(rbind(hist.pars, iw12.pars, wdes.pars, wspur.pars))
LM.summ$SET <- c("HIST","IW12","REF1415", "WSpur"  )
   names(LM.summ)[names(LM.summ) == "(Intercept)"] <- "Intercept"
   names(LM.summ)[names(LM.summ) == "V3"] <- "Rsq"
   names(LM.summ)[names(LM.summ) == "slope"] <- "Slope"

hist0.pars <- c(0,hist.lm$coeff[1], summary(hist.lm)$adj.r.squared,"HIST")
iw120.pars <- c(0,IW12.lm$coeff[1], summary(IW12.lm)$adj.r.squared,"IW12")
wdes0.pars <- c(0,WDes.lm$coeff[1], summary(WDes.lm)$adj.r.squared,"REF1415")
wspur0.pars <- c(0,WSpur.lm$coeff[1], summary(WSpur.lm)$adj.r.squared,"WSpur")

LM.summ2 <- as.data.frame(rbind(LM.summ, hist0.pars, iw120.pars, wdes0.pars, wspur0.pars))

############

## PLOTTING ##
z0 <- ggplot(data2, aes(x=EC25, y=TDS, col=SET.name), na.rm=T) + theme_bw() + 
      scale_color_manual(values=c("darkgreen", "red", "blue", "purple"))
z1 <- z0 + geom_point(na.rm=T) + stat_smooth(aes(group=1),formula = y ~ x, col="black",
                                             linetype=2, size=0.8,method=lm, se=F, na.rm=T) +
      scale_x_continuous(labels=comma, limits=c(0,75000), oob=squish) +
      scale_y_continuous(labels=comma, limits=c(0,45000), oob=squish)
z2 <- z1 + annotate("text", size=6, x=5000, y=43000, hjust=0.1, vjust=0, color = "black",
            label="Total Dissolved Solids ~ Specific Conductance", show_guide=F) +
           annotate("text", size=5, x=5000, y=41000, hjust=0, vjust=0.2, color = "black",
            label=paste("slope = ", format(b0.0, nsmall=2, digits=3)), show_guide=F) +
           annotate("text", size=5, x=5000, y=39000, hjust=0, vjust=0.4, color = "black",
            label=paste("r^2 == ", format(rsq.0, nsmall=2, digits=2)), parse=T, show_guide=F);
z3 <- z2 + xlab("Specific Conductance (uS/cm @ 25C)") + labs(col="Dataset") + 
           ylab("Total Dissolved Solids (mg/L)") +
           theme(legend.position=c(0.825, 0.13));
z3

ggsave(file="AllIW_TDS-vs-EC25_fig1.jpeg", width=7, height=6.5, dpi=500);
#####


##### Salinty vs. Water Depth ####
data2 <- subset(WChem3, (!is.na(TDS) & !is.na(EC25) & !is.na(Water.Z)))

z0 <- ggplot(data=data2, aes(x=Water.Z)) + theme_bw() + 
      facet_wrap(~SET.name, scales="fixed", ncol=2) + scale_colour_tableau();
z1 <- z0 + geom_point(aes(y=EC25), na.rm=T) + scale_y_continuous(labels=comma, limits=c(0,25000))
z1
##### Not super interesting ####
##### TDS vs. EC25 ####
data3 <- subset(WChem3, (!is.na(TDS) & !is.na(EC25) ))

z0 <- ggplot(data=data3, aes(x=EC25)) + theme_bw() + 
      facet_wrap(~SET.name, scales="fixed", ncol=2) + scale_colour_tableau();
z1 <- z0 + geom_point(aes(y=TDS), na.rm=T) + scale_y_continuous(labels=comma, limits=c(0,15000)) +
      scale_x_continuous(labels=comma, limits=c(0,25000)) + geom_smooth(aes(y=TDS),method=lm, se=F)
z1
#####
##### TDS vs. EC25 #2   #####
data3 <- subset(WChem3, (!is.na(TDS) & !is.na(EC25) ))

z0 <- ggplot(data=data3, aes(x=EC25, col=SET.name)) + theme_bw() + scale_colour_tableau();
z1 <- z0 + geom_point(aes(y=TDS), na.rm=T) + scale_y_continuous(labels=comma, limits=c(0,15000)) +
      scale_x_continuous(labels=comma, limits=c(0,25000)) + geom_smooth(aes(y=TDS),method=lm, se=F)
z1
ggsave(file="AllIW_TDS-vs-EC25_fig2XX.jpeg", width=9, height=6.5, dpi=500)

###### EC25 Aggregated CDF #######
data2 = subset(data, !is.na(IP_calc))
x0 <- ggplot(data=data2, aes(x=EC25), na.rm=T) + theme_bw() + 
      guides(color=guide_legend(override.aes=list(size=1.2)));      
x1 <- x0 + stat_ecdf(aes(x=EC25), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,28000), oob=squish, breaks=seq(0,24000, 7500)) + 
      xlab("Specific Conductance (uS/cm @ 25C)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75,0.9,0.95,0.99), lty=2, col="black") +
      geom_vline(xintercept=c(7500,22500),lty=2, col="blue")
x1   ## this is the CDF plot   ####
ggsave(file="AllDATA_agg-CDF_fig4.jpeg", width=9, height=6.5, dpi=500)

####
































