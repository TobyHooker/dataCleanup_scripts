## Surface Mats ## IW Ref Std Sites [IW-RSS]
## 151210
## data::
SAV_Field_151210 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/REFSTD_R/SAVcov_Alldat_151208.csv")

SAVdat.0 <- SAV_Field_151210

######### B E S T   F I G U R E S  (are at the bottom of Doc) ###########

# Master site list and STORET to Site.Name conversion.  (Coordinates not yet perfect)
Sites_MASTER_151210 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/REFSTD_R/Sites_MASTER_151210.csv")
Sites.master <- Sites_MASTER_151210[,c(1:7)]

#######################################
## Class variables
summary(SAVdat.0)
SAVdat.0.nam <- as.data.frame(names(SAVdat.0))

# deal w/ missing or imcomplete values...
levels(SAVdat.0$TRT)

# SAVdat.0[(SAVdat.0$TRT==""),3] <- NA  ## It works, but is INAPPROPRIATE FOR CLASS VARS...
SAVdat.0[(is.na(SAVdat.0$TRT)),3] <- "control" # fiexed 151210 in evening...
SAVdat.0 <- droplevels(SAVdat.0)

summary(unique(as.factor(SAVdat.0$STORET)))
length(unique(SAVdat.0$STORET))

# fixing dates...
class(SAVdat.0$Date)
  dateX <- as.data.frame(SAVdat.0$Date)
  names(dateX)[names(dateX) == "SAVdat.0$Date"] <- "DATE"

class(dateX$DATE)
class(dateX$Date2)

dateX$Date2 <- as.Date(dateX$DATE, format="%m/%d/%Y")
dateX$Year <- as.numeric(format(dateX$Date2, "%Y"))
dateX$Month <- as.numeric(format(dateX$Date2, "%m"))

SAVdat.0$Date2 <- as.Date(SAVdat.0$Date, format="%m/%d/%Y")
SAVdat.0$Year <- as.numeric(format(SAVdat.0$Date2, "%Y"))
SAVdat.0$Month <- as.numeric(format(SAVdat.0$Date2, "%m"))
SAVdat.0$DOY <- yday(SAVdat.0$Date2)

length(unique(SAVdat.0$Date2))
##
#### Fix Index Periods for other obs...
SAVdat.0$IP_calc <- ifelse(SAVdat.0$DOY<166,NA,
                    ifelse(SAVdat.0$DOY<228,1,
                    ifelse(SAVdat.0$DOY>274,NA,
                    ifelse(SAVdat.0$DOY>229,2,NA))));
## works nicely...

### SAV and field cover metrics -- response Vars  ########
# SAV.tot
# Pond_mat
# Z.All
# SAV_cond
# SAV.Ht
# FAV_COV (??)
# BareCov (as complement of SAV.tot)...
# POND_SAV (corr vs SAV.tot)                ##########

## Clarify Dataset (SET) Names...
data <- SAVdat.0

levels(data$SET)
data$SET.name <- data$SET
levels(data$SET.name) <- c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
                           "Willard Spur (2011-3)")
# SAV total cover by month...
data2 <- subset(data, !is.na(IP_calc))  # ok, should probably remove the nocarp Exp obs
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## works fine [better]
data4 <- subset(data, TRT != "nocarp")  # Nocarp TRT removed...

####### PRELIMINARY FIGs ##################################
# [1] SAV x month [Year]
q0 <- ggplot(data, aes(x=factor(Month), fill=factor(Year))) + theme_bw() +
      scale_color_manual(values=rainbow(8));
q1 <- q0 + geom_dotplot(aes(y=SAV.tot), shape=21, na.rm=T, binaxis="y", 
                        stackdir="center", binwidth=1) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(fill="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover\n");
q2

ggsave(file="AllIW_SAVxMnth_1.jpeg", width=7, height=5, dpi=500)

# [2] SAV x month [SET] Boxplot
q0 <- ggplot(data, aes(x=factor(Month), fill=SET)) + theme_bw() +
      scale_color_brewer(palette="Set1");
q1 <- q0 + geom_boxplot(aes(y=SAV.tot), na.rm=T) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(fill="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover\n");
q2

ggsave(file="AllIW_SAVxMnth_2.jpeg", width=7, height=5, dpi=500)

# [3] SAV x month [SET] Facets
q0 <- ggplot(data, aes(x=factor(Month), fill=factor(Year))) + theme_bw() +
      scale_fill_brewer(palette="Set1") + facet_wrap(~SET, scales="fixed", ncol=1);
q1 <- q0 + geom_dotplot(aes(y=SAV.tot), dotsize=0.5, na.rm=T, binaxis="y", 
                        stackdir="center", binwidth=10) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(fill="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover\n");
q2

ggsave(file="AllIW_SAVxMnth_3.jpeg", width=7, height=9, dpi=500)

# [4] SAV x month [SET] Facets (jitter)
q0 <- ggplot(data, aes(x=factor(Month), fill=factor(Year))) + theme_bw() +
      scale_fill_brewer(palette="Set1") + facet_wrap(~SET, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=SAV.tot, fill=factor(Year)), shape=21, na.rm=T, size=3, 
                       position=position_jitter(w=0.15)) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(fill="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover\n");
q2

ggsave(file="AllIW_SAVxMnth_4.jpeg", width=7, height=9, dpi=500)

# [4b] SAV x month [SET] Facets (jitter)  [****]
data4 <- subset(data, TRT != "nocarp") 

q0 <- ggplot(data4, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=SAV.tot, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover\n");
q2

ggsave(file="AllIW_SAVxMnth_4bXX.jpeg", width=7, height=7, dpi=500)   ##  This one is okay

# [5] SAV x IP_calc [SET] Facets (Lines = geom_path)   [skipit]#####
  data2 = subset(data, !is.na(IP_calc))

q0 <- ggplot(data2, aes(x=factor(Month), col=factor(Year), group=STORET)) + theme_bw() +
      scale_color_manual(values=c(rainbow(4),"darkgreen", "darkred", "blue", "purple")) + 
      facet_wrap(~SET, scales="fixed", ncol=1);
q1 <- q0 + geom_path(aes(y=SAV.tot, group=STORET), na.rm=T) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) + 
      scale_x_discrete(labels=c("Jun", "Jul", "Aug", "Sep"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover during the growing season\n");
q2
ggsave(file="AllIW_SAVxMnth_5b.jpeg", width=6.5, height=6.5, dpi=500)   ##  #6 is better ####

# [6] SAV x IP_calc [SET] Facets (Lines = geom_path)   [*****]
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## IP != NA & TRT = control

q0 <- ggplot(data3, aes(x=DOY, col=factor(Year), group=STORET)) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_path(aes(y=SAV.tot, group=STORET), na.rm=T) + geom_point(aes(y=SAV.tot),size=0.8, 
      col="black") + scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_continuous(breaks=c(166,182,213,244,265), labels=c("mid-June", "July", "Aug", "Sept", 
      "lat-Sept")) + theme(legend.key.size=unit(0.7, "cm"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover during the growing season\n");
q2

ggsave(file="AllIW_SAVxMnth_6Xx.jpeg", width=7, height=7, dpi=500)   ## Better (*now*)

####
###
############  Comparison of pond-scale vs. quadrat-scale SAV  ***  ####
### [7]   SAV.tot (quadrats) vs. POND_SAV (whole pond scale)
data

z0 <- ggplot(data, aes(x=POND_SAV, y=SAV.tot, col=SET.name)) + theme_bw() + 
      scale_color_manual(values=c("darkgreen", "red", "blue", "purple"));
z1 <- z0 + geom_point(na.rm=T) + 
  stat_smooth(formula= y ~ x-1 ,color="black", linetype=2, size=0.6, method=lm, se=F, na.rm=T);
z <- lm(SAV.tot ~ POND_SAV, data=data, na.exclude=T)    ## formula:  y ~ model
    b0 <- z$coeff[2]  
    r2 <- summary(z)$adj.r.squared
z2 <- z1 + geom_text(size=3, data=NULL, x=5, y=93, hjust=0, vjust=0, color = "black", 
                     label="SAV (quadrat) ~ SAV (pond-scale)", show_guide  = F ) +
          geom_text(size=3, data=NULL, x=5, y=87, hjust=0, vjust=0.2, color="black",
                label=paste("slope = ", format(b0, nsmall=2, digits=2)), show_guide  = F ) +
          geom_text(size=3, data=NULL, x=5, y=83, hjust=0, vjust=0.4, color="black",
                label=paste("r^2 ==  ", format(r2, digits=2)), parse=T, show_guide  = F);
z3 <- z2 + xlab("SAV cover estimated for whole pond") + labs(col="Dataset") + 
            ylab("SAV cover estiamted from quadrats") + 
      ggtitle("Relationship between pond vs. quadrat SAV cover \n");
z3

ggsave(file="AllIW_SAV-plot-vs-Pond_1X.jpeg", width=7, height=5, dpi=500);
###

####### CDFs of SAV.tot #########
data <- data[with(data, order(SET, STORET, Date2)),]

# PLOT 1 - CDFs by SET  (updated down below [#4])
data2 <- subset(data, !is.na(IP_calc))  ## works fine

p0 <- ggplot(data=data2, aes(x=SAV.tot, col=SET)) + theme_bw() +
      scale_colour_tableau(name="Dataset", breaks=c("Hist", "IW12", "REF1415", "WSpur"),
              labels=c("Historical (2007-11)", "IW Survey (2012)",
                          "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
p1 <- p0 + stat_ecdf(aes(col=SET), na.rm=T)
p2 <- p1 + labs(col="Datasets") + ylab("Proportion of data occuring below x-value") +
      xlab("Total SAV Cover") + ggtitle("Cumulative Distribution Function of Total SAV Cover") +
      scale_y_continuous(labels=percent) + scale_x_continuous(limits=c(NA,100), breaks=seq(0,100,20));
p3 <- p2 + geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black")
p3

ggsave(file="AllIW_SAVcov_CDF_1.jpeg", width=7.5, height=6, dpi=500)
#

## PLOT 2 - All data combined
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## works fine [better]

p0 <- ggplot(data=data3, aes(x=SAV.tot)) + theme_bw() +
      scale_colour_tableau(name="Dataset", breaks=c("Hist", "IW12", "REF1415", "WSpur"),
              labels=c("Historical (2007-11)", "IW Survey (2012)",
                          "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
p1 <- p0 + stat_ecdf(na.rm=T)
p2 <- p1 + ylab("Proportion of data occuring below x-value") +
      xlab("Total SAV Cover") + ggtitle("Cumulative Distribution Function of Total SAV Cover") +
      scale_y_continuous(labels=percent) + scale_x_continuous(limits=c(NA,100), breaks=seq(0,100,20));
p3 <- p2 + geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black")
p3

ggsave(file="AllIW_SAVcov_CDF_2.jpeg", width=7.5, height=6, dpi=500)

## PLOT 3  ### COMBO PLOT ::  CDF w/ Marginal Density Plot (x) ########
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## works fine [better]

q0 <- ggplot(data=data3, aes(x=SAV.tot)) + theme_bw() + theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"))
q1 <- q0 + stat_ecdf(na.rm=T) + scale_y_continuous(labels=percent) + 
  scale_x_continuous(limits=c(NA,100), breaks=seq(0,100,20)) + xlab("Total SAV Cover") + labs(y=NULL);
q1
#  data.plot <- data3[,c(2,11)]
margin.x <- ggplot(data.plot, aes(x=SAV.tot, col=SET)) + geom_density(aes(col=SET), na.rm=T, alpha=0.5) + theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        plot.margin = unit(c(0.5,1,0,0.5),"cm"), legend.position="top", legend.key.size=unit(0.3,"cm")) + 
  labs(x=NULL, y=NULL, col="") + scale_x_continuous(limits=c(NA,NA)) + 
  scale_color_manual(values=c("darkgreen", "red", "blue", "purple"),breaks=c("Hist", "IW12", "REF1415",
                    "WSpur"), labels=c("Historical (2007-11)", "IW Survey (2012)", 
                    "West Desert IWs (2014-15)", "Willard Spur (2011-3)"))
# have both plots...
legend.position = "none", 
# Create a blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(), 
   panel.border = element_blank(),
   panel.background = element_blank(),
   axis.title.x = element_blank(),
   axis.title.y = element_blank(),
   axis.text.x = element_blank(), 
   axis.text.y = element_blank(),
   axis.ticks = element_blank(),
   axis.line = element_blank()
     )
#####

jpeg(filename="AllIW_SAVcovxSET_CDF_3.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x, q1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(2.5,5),
             main=textGrob("Cumulative Distribution Function of Total SAV Cover", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
############

###  PLOT 4  ### COMBO PLOT [2] :: CDF (by SET) w/ Marginal Density Pot (x) ######
#data2 <- subset(SAVdat.0, TRT != "nocarp") ## data w/o 'nocarp' treatment
data3
x0 <- ggplot(data=data3, aes(x=SAV.tot, col=SET)) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("Hist", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(col=SET), na.rm=T) + scale_y_continuous(labels=percent) + labs(y=NULL) +
      scale_x_continuous(limits=c(NA,100), breaks=seq(0,100,20)) + xlab("Total SAV Cover") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot

margin.x2 <- ggplot(data=data3, aes(x=SAV.tot, col=SET)) + geom_density(aes(col=SET), na.rm=T, alpha=0.5) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
      plot.margin = unit(c(0.5,1,0,0.5),"cm"), legend.position="none") + 
      labs(x=NULL, y=NULL, col="") + scale_x_continuous(limits=c(NA,NA)) + 
      scale_color_tableau(breaks=c("Hist", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
            "Willard Spur (2011-3)"))
margin.x2  ## this is the density plot(s)
# Use blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(), 
   panel.border = element_blank(),
   panel.background = element_blank(),
   axis.title.x = element_blank(),
   axis.title.y = element_blank(),
   axis.text.x = element_blank(), 
   axis.text.y = element_blank(),
   axis.ticks = element_blank(),
   axis.line = element_blank()
     )
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="AllIW_SAVcovxSETgrop_CDF_4.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x2, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
             main=textGrob("Cumulative Distribution of Total SAV Cover", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
############  This one is the good one !!!    ####

###  F I G U R E   T E M P L A T E S   ####

############################################################
############################################################
######   S C A T T E R  P L O T   T E M P L A T E   ########
############################################################
###########  Comparison of pond-scale vs. quadrat-scale SAV  ***  ####
#data
z0 <- ggplot(data, aes(x=POND_SAV, y=SAV.tot, col=SET.name)) + theme_bw() + 
      scale_color_manual(values=c("darkgreen", "red", "blue", "purple"));
z1 <- z0 + geom_point(na.rm=T) + 
  stat_smooth(formula= y ~ x-1 ,color="black", linetype=2, size=0.6, method=lm, se=F, na.rm=T);
z <- lm(SAV.tot ~ POND_SAV, data=data, na.exclude=T)    ## formula:  y ~ model
    b0 <- z$coeff[2]  
    r2 <- summary(z)$adj.r.squared
z2 <- z1 + geom_text(size=3, data=NULL, x=5, y=93, hjust=0, vjust=0, color = "black", 
                     label="SAV (quadrat) ~ SAV (pond-scale)", show_guide  = F ) +
          geom_text(size=3, data=NULL, x=5, y=87, hjust=0, vjust=0.2, color="black",
                label=paste("slope = ", format(b0, nsmall=2, digits=2)), show_guide  = F ) +
          geom_text(size=3, data=NULL, x=5, y=83, hjust=0, vjust=0.4, color="black",
                label=paste("r^2 ==  ", format(r2, digits=2)), parse=T, show_guide  = F);
z3 <- z2 + xlab("SAV cover estimated for whole pond") + labs(col="Dataset") + 
            ylab("SAV cover estiamted from quadrats") + 
      ggtitle("Relationship between pond vs. quadrat SAV cover \n");
z3
ggsave(file="AllIW_SAV-plot-vs-Pond_1X.jpeg", width=7, height=5, dpi=500);
###
############################################################

############################################################
######   F A C E T E D   J I T T E R   #####################
############################################################
# [4b] SAV x month [SET] Facets (jitter)  [****]
data4 <- subset(data, TRT != "nocarp") 

q0 <- ggplot(data4, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=SAV.tot, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover\n");
q2
ggsave(file="AllIW_SAVxMnth_4bXX.jpeg", width=7, height=7, dpi=500)   ##  This one is okay
############################################################

############################################################
#####   F A C E T E D   L I N E S [segments]   #############
############################################################
# [6] SAV x DOY [SET] Facets (Lines = geom_path)
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## IP != NA & TRT = control

q0 <- ggplot(data3, aes(x=DOY, col=factor(Year), group=STORET)) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_path(aes(y=SAV.tot, group=STORET), na.rm=T) + geom_point(aes(y=SAV.tot),size=0.8, 
      col="black") + scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_continuous(breaks=c(166,182,213,244,265), labels=c("mid-June", "July", "Aug", "Sept", 
      "lat-Sept")) + theme(legend.key.size=unit(0.7, "cm"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover during the growing season\n");
q2
ggsave(file="AllIW_SAVxMnth_6Xx.jpeg", width=7, height=7, dpi=500)   ## Better (*now*)
############################################################

############################################################
#####   F A C E T E D   L I N E S  x  IPs   ################
############################################################
# [7] SAV x IP_calc [SET] Facets (Lines = geom_path)  ## 151229 UPDATE #####
data <- SAVdat_dat1
data <- data[with(data, order(Year, SET.name, STORET, IP_calc)),]
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## IP != NA & TRT = control
Summ.MeanDat3 <- ddply(data3, .(SET.name, Year, STORET, IP_calc), numcolwise(mean, na.rm=T));
data3X <- Summ.MeanDat3
data3XX <- data3XX[with(data3XX, order(SET.name, STORET, Year, IP_calc)),]
# data3XX <- data3X[,c(1:4,10)]
data3XX.cast <- dcast(data3XX, SET.name + Year + STORET ~ IP_calc, value.var="SAV.tot")
data3XX.cast$PLOT <- as.character("no")
data3XX.cast[(!is.na(data3XX.cast[4]) & !is.na(data3XX.cast[5])),6] <- "yes"
data3XX2 <- data3XX.cast
data3XX2.melt <- melt(data3XX2, id=c(1:3,6), value.name="SAV.tot", variable.name="IP_calc")

data3XX.2 <- subset(data3XX2.melt, !is.na(SAV.tot))
data3XX.2 <- data3XX.2[with(data3XX.2, order(SET.name, Year, STORET, IP_calc)),]
data3XX.3 <- subset(data3XX.2, PLOT =="yes")
###

q0 <- ggplot(data=data3, aes(x=factor(IP_calc), col=factor(Year), group=STORET)) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_point(aes(y=SAV.tot),size=0.8, col="black", 
                      position=position_jitter(width=0.01), na.rm=T) +
      geom_path(data=data3XX.3, aes(x=factor(IP_calc), y=SAV.tot, group=STORET), na.rm=T, alpha=0.75) + 
      scale_y_continuous(limits=c(0,100), 
      breaks=seq(0,100,25)) + scale_x_discrete(labels=c("IP #1: mid-summer", "IP #2: early-autumn")) +
      theme(legend.key.size=unit(0.7, "cm"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover over IW Index Periods\n");
q2
ggsave(file="AllIW_SAVxIP_7G.jpeg", width=7, height=7, dpi=500)   ## Better (*now*)

############################################################

############################################################
#####   C D F's  w/  M A R G I N A L   D E N S I T Y  ######
############################################################
###  PLOT 4  ### COMBO PLOT [2] :: CDF (by SET) w/ Marginal Density Pot (x) ######
#data2 <- subset(SAVdat.0, TRT != "nocarp") ## data w/o 'nocarp' treatment
data3
x0 <- ggplot(data=data3, aes(x=SAV.tot, col=SET)) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("Hist", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(col=SET), na.rm=T) + scale_y_continuous(labels=percent) + labs(y=NULL) +
      scale_x_continuous(limits=c(0,100), breaks=seq(0,100,20)) + xlab("Total SAV Cover") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot

margin.x2 <- ggplot(data=data3, aes(x=SAV.tot, col=SET)) + 
      stat_density(aes(y=(..density..),col=SET), position="identity", geom="line", na.rm=T) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
      plot.margin = unit(c(0.5,1,0,0.5),"cm"), legend.position="none") + 
      labs(x=NULL, y=NULL, col="") + scale_x_continuous(limits=c(0,100)) + 
      scale_color_tableau(breaks=c("Hist", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
            "Willard Spur (2011-3)"))
margin.x2  ## this is the density plot(s)

# Use blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
   panel.border = element_blank(),panel.background = element_blank(),
   axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), 
   axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line = element_blank()   );
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="AllIW_SAVcovxSETgrop_CDF_4b.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x2, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
             main=textGrob("Cumulative Distribution of Total SAV Cover", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
############  This one is the good one !!!    ####

###################################################################
###################################################################
###################################################################
####   C A L C   S A V    I N D E X E S   #########################
###################################################################
###################################################################
# Use: data = data3 (IP_calc (1, 2) & TRT != 'nocarp')  n=347
  write.csv(data3, "SAV_data3.csv", row.names=F)
data3X_SAVcalcs <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/SAV_data3X_calcs_151211.csv")
data3X <- data3X_SAVcalcs[,c(1:63)]  # some duplicate vars...remove them now.
data3X <- data3X[,c(1:49,52:63)]  # now, rename SAV_SCR vars...
  names(data3X)[names(data3X) == "SAV_SCR.1"] <- "SAV_SCR"
  names(data3X)[names(data3X) == "SAV_SCR_X.1"] <- "SAV_SCR_X"
data3X.name <- as.data.frame(names(data3X))

# Vars include:
# SAV.set (y or n; whether SAV-scores are calculated)
# SAV_SCR ==> 2xIP2(cover) + IP1(cover) :: 0-300
# SAV_SCR_X ==> 2 x (IP2(cov) x IP2(cond)) + IP1(cov) :: 0-700
# rel_SAV_SCR_X ==> SAV_SCR_X / 700  ::  0-1
# interval ==> # of days between measurement periods in IP2 vs. IP1
# ** some sites had > 1 observation during either IP1 or IP2; the IP1 values were averaged, while 
# the SAV_SCR values were calculated for each IP2 observation that had an appropriate IP1 observation
# (This doesn't lose as much data as simply averaging across all observations within an IP, but allows
# for potential differences in the timing (data of IP2 measurement) or the interval between the obs.).

dat <- data3X[,c(2,57)]
table(dat)
summary(dat)
##### All set !!  #####
## subset data for only obs where SAV.set == y
data5X <- subset(data3X, SAV.set == "y")  ## only data w/ SAV_SCR calculations...
data5X <- droplevels(data5X)  ## remove unused factor levels...
data5X$rel_SAV_SCR <- data5X$SAV_SCR/300


#####  F I G U R E S   F O R    S A V   S C O R E S   ########
## PLOT  [1] :  boxplot x SET.name  @@ SAV_SCR / 300 (relativized)

w0 <- ggplot(data=data5X, aes(x=SET.name)) + theme_bw() + scale_color_tableau()
w1 <- w0 + geom_boxplot(aes(y=SAV_SCR/300))  ## value relativized to MAX (300)
w2 <- w1 + geom_jitter(aes(y=SAV_SCR/300, col=factor(Year)), position=position_jitter(width=0.15))
w3 <- w2 + xlab(NULL) + ylab("Relativized SAV Index Score") +
      ggtitle("SAV Cover Index Scores (relative to max value)\n") + labs(col="Sampling Year") + 
      scale_y_continuous(labels=percent) + theme(legend.position="none") +
      scale_x_discrete(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs \n(2014-15)",
                                "Willard Spur \n(2011-3)"));
w3
      ## Could also consider adding Y-axis marginal density curves...
ggsave(file="SAV_SCR-rel_Box1.jpeg", width=6.5, height=5, dpi=500) 

## PLOT  [2] :  boxplot x SET.name   @@ SAV_SCR_X / 700 (relativized)

w0 <- ggplot(data=data5X, aes(x=SET.name)) + theme_bw() + scale_color_tableau();
w1 <- w0 + geom_boxplot(aes(y=SAV_SCR_X/700)) ## value relativized to MAX (700)
w2 <- w1 + geom_jitter(aes(y=SAV_SCR_X/700, col=factor(Year)), position=position_jitter(width=0.15))
w3 <- w2 + xlab(NULL) + ylab("Relativized SAV Index Score") +
      ggtitle("SAV Cover x Condition Scores (relative to max value)\n") + labs(col="Sampling Year") + 
      scale_y_continuous(labels=percent) + theme(legend.position="none") +
      scale_x_discrete(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs \n(2014-15)",
                                "Willard Spur \n(2011-3)"));
w3
      ## Could also consider adding Y-axis marginal density curves...
ggsave(file="SAV_SCRxCOND-rel_Box2.jpeg", width=6.5, height=5, dpi=500) 

##
### PLOT  [3]  score comparisons ##** SCATTERPLOT  **##  #########
x0 <- ggplot(data=data5X, aes(x=SAV_SCR, y=SAV_SCR_X, col=SET.name)) + geom_point() + scale_color_tableau()
df <- data.frame(x=c(0,300), y=c(0,700))
x1 <- x0 + geom_path(data=df, aes(x=x, y=y, col="black"),col="black", lty=2)
## a little too complicated to be aided by a single figure...

#########################################
######  Re-do of SAV_SCR Boxplot (#1) w/ Marginal Density Plots (y-axis; right) #####
## PLOT  [1] :  boxplot x SET.name  @@ SAV_SCR / 300 (relativized)
  w0 <- ggplot(data=data5X, aes(x=SET.name)) + theme_bw() + scale_color_tableau()
  w1 <- w0 + geom_boxplot(aes(y=SAV_SCR/300))  ## value relativized to MAX (300)
  w2 <- w1 + geom_jitter(aes(y=SAV_SCR/300, col=factor(Year)), position=position_jitter(width=0.15))
  ww3 <- w2 + xlab(NULL) + ylab("Relativized SAV Index Score") + theme(legend.position="none",
      plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(labels=percent) + 
      scale_x_discrete(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs \n(2014-15)",
                                "Willard Spur \n(2011-3)"));
  ww3
## marginal plot ####
  margin.x2 <- ggplot(data=data5X, aes(x=SAV_SCR/300, col=SET.name)) + 
    geom_density(aes(x=SAV_SCR/300, col=SET.name), na.rm=T, alpha=0.9) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + scale_x_continuous(limits=c(0,1)) + 
      scale_color_tableau(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
            "Willard Spur (2011-3)")) + coord_flip();
  margin.x2  ## this is the density plot(s)
# , legend.position="none"
  legendX <- gtable_filter(ggplot_gtable(ggplot_build(margin.x2 + 
          theme(legend.position=c(0.42,2.6), legend.key.size=unit(0.25, "cm"), 
                legend.text=element_text(size=8),legend.margin=unit(0, "cm"),
                legend.background=element_rect()))), "guide-box")
# grid.draw(legendX)
  # legend.direction="horizontal", 
# Use blank placeholder plot :  #######
  blankPlot <- ggplot()+geom_blank(aes(1,1))+theme(plot.background = element_blank(), 
     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
     panel.border = element_blank(), panel.background = element_blank(),
     axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), 
     axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()         )
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="SAV_SCR-rel_Box1+Margin_v1.jpeg", width=7.5, height=6, units="in", res=500)
    grid.arrange(ww3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, widths=c(5,1.4),
    heights=c(5,0.2), main=textGrob("SAV Cover Index Scores (relative to max value)", vjust=1),legendX)
dev.off()
#######

######  Re-do of SAV_SCR_X Boxplot (#2) w/ Marginal Density Plots (y-axis; right) #####
## PLOT  [2] :  boxplot x SET.name   @@ SAV_SCR_X / 700 (relativized)
  w0 <- ggplot(data=data5X, aes(x=SET.name)) + theme_bw() + scale_color_tableau()
  w1 <- w0 + geom_boxplot(aes(y=SAV_SCR_X/700))  ## value relativized to MAX (300)
  w2 <- w1 + geom_jitter(aes(y=SAV_SCR_X/700, col=factor(Year)), position=position_jitter(width=0.15))
  w3 <- w2 + xlab(NULL) + ylab("Relativized SAV x Condition Index Score") + theme(legend.position="none",
      plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(labels=percent) + 
      scale_x_discrete(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs \n(2014-15)",
                                "Willard Spur \n(2011-3)"));
  w3
## marginal plot  ####
  margin.x2 <- ggplot(data=data5X, aes(x=SAV_SCR_X/700, col=SET.name)) + 
    geom_density(aes(x=SAV_SCR_X/700, col=SET.name), na.rm=T, alpha=0.9) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + scale_x_continuous(limits=c(0,1)) + 
      scale_color_tableau(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
            "Willard Spur (2011-3)")) + coord_flip();
  margin.x2  ## this is the density plot(s)
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
jpeg(filename="SAV_SCR_X-rel_Box2+Margin_v2.jpeg", width=7.5, height=6, units="in", res=500)
    grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, widths=c(5,1.4),
    heights=c(5,0.2), main=textGrob("SAV Cover x Condition Scores (relative to max value)", 
                                    vjust=1),legendX)
dev.off()
####


##########################################################################
########### C D F 's  O F   SAV_SCR  #########################################
#####   C D F's  w/  M A R G I N A L   D E N S I T Y  ######
############################################################
###  PLOT 5  ### CDF of SAV_SCR (by SET) w/ Marginal Density Pot (x) ######
data5X
x0 <- ggplot(data=data5X, aes(x=rel_SAV_SCR, col=SET), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("Hist", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=rel_SAV_SCR, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2), labels=percent) + 
      xlab("SAV Index Score") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot

margin.x3 <- ggplot(data=data5X, aes(x=rel_SAV_SCR, col=SET)) + 
  stat_density(aes(y=(..density..)/100),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.9),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) + 
  scale_y_continuous(labels=percent) +
  scale_color_tableau(breaks=c("Hist", "IW12", "REF1415", "WSpur"), 
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
jpeg(filename="AllIW_SAV_SCRxSETgrop_CDF_5x.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
             main=textGrob("Cumulative Distribution of SAV Index Scores", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
############ Not entirely appetizing, the marginal density plot is scaled vs. the X-axis (values)     ####
## Current method (y=..scaled..) provides the probability density relative to the maximum probability



### other plots.
gg <- ggplot(data=data5X, aes(x=interval, y=SAV_SCR, col=SET)) + theme_bw() +
      geom_point() + geom_smooth(method="lm", aes(fill=SET))


