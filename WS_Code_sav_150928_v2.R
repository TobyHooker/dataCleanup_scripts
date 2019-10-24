# OW + SAV data     //      150928
# OWSAV7.dat <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur.2015/OWSAV7.dat.csv")
# data  =   OWSAV7 <- OWSAV7.dat2
# write.csv(OWSAV7, "OWSAV7.dat2.csv", row.names=F)
OWSAV7
###########################
#### Working w/ SAV and site-cover data
###########################
# variable names...
# OW7.nm <- as.data.frame(names(OWSAV7))    //  UPDATED....OW7.name2
# write.csv(OW7.nm, "OW7.names.csv", row.names=F)
# write.csv(OW7.name, "OW7.name2.csv", row.names=F)
# OW7.name <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/OW7.names2.csv")
# OW7.name2 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/OW7.name2.csv")
#    names(OW7.name2)[names(OW7.name2) == "names.OWSAV7."] <- "Var.Name"

## Pattern of SAV Cover over time ## ##############
## Cover x Month (x Year) for all (|) SITES
## 
SAVCov <- OWSAV7[,c(2,4,8:15,86,87,61)]
    SAVCov2 <- subset(SAVCov, SiteSAV != "")  # Leaves the NAs, but removes the "" in Text(Sitenames)
SAVCov2 <- droplevels(SAVCov2)

SAVCov.melt <- melt(SAVCov2, id.vars=c(1:10), measure.vars=c(11:13), variable.name="Metric");

SAVCov.cast <- dcast(SAVCov.melt, SiteSAV + Year + Month ~ Metric)
    length(unique(SAVCov.cast$SiteSAV))
SAVCov.cast.Len <- droplevels(SAVCov.cast)
SAVCov.cast.1 <- dcast(SAVCov.melt, SiteSAV + Year + Month ~ Metric, mean)
SAVCov.cast.1 <- droplevels(SAVCov.cast.1)

### Make Plots      //  ##################
# RE-ORDER FACTOR LEVELS
SAVCov.cast.1 <- transform(SAVCov.cast.1, SiteName=factor(SiteSAV, 
                    levels=c("OUTFALL-CNFL", "OUT-WB-TAILRACE", "WS-1",
                    "WS-2", "WS-3", "WS-4", "WS-5", "OUT-HC-WMA-DD", 
                    "WS-6", "WS-7", "WS-8", "WS-9", "WS-10", "WS-11", "WS-12")));

SAVCov.cast.1 <- SAVCov.cast.1[with(SAVCov.cast.1, order(SiteSAV, Year, Month)),]

i=3
SAV.data <- SAVCov.cast.1
SAV.sitename <- as.data.frame(unique(SAV.data$SiteSAV));
# SAV Cover MAX     /   #############
for (i in 1:length(unique(SAV.data$SiteSAV))) {
    site_i <- unique(SAV.data$SiteSAV) [i];
    data_i <- SAV.data[SAV.data$SiteSAV==site_i,];
        
    x0 <- ggplot(data=data_i, aes(x=Month, col=factor(Year))) + theme_bw();
    x1 <- x0 + geom_point(aes(y=SAVcov.max), na.rm=T) + 
        scale_x_continuous(limits=c(3,12), breaks=seq(from=3, to=12, by=1));
    x2 <- x1 + geom_line(aes(y=SAVcov.max), na.rm=T) ;
    x3 <- x2 + xlab("Month") + ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.max",3])) +
        ggtitle(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.max",2], "for Site:", site_i)) +
        labs(col="Sampling Year");

x3
    ggsave(file=paste("WS15-SAV_",i,"_SitesXMonth_1.jpeg", sep=""), 
           width=10, height=6, dpi=500);
}
# SAV Cover AVG     /   ##################
for (i in 1:length(unique(SAV.data$SiteSAV))) {
    site_i <- unique(SAV.data$SiteSAV) [i];
    data_i <- SAV.data[SAV.data$SiteSAV==site_i,];
        
    x0 <- ggplot(data=data_i, aes(x=Month, col=factor(Year))) + theme_bw();
    x1 <- x0 + geom_point(aes(y=SAVcov.avg), na.rm=T) + 
        scale_x_continuous(limits=c(3,12), breaks=seq(from=3, to=12, by=1));
    x2 <- x1 + geom_line(aes(y=SAVcov.avg), na.rm=T) ;
    x3 <- x2 + xlab("Month") + ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.avg",3])) +
        ggtitle(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.avg",2], "for Site:", site_i)) +
        labs(col="Sampling Year");

x3
    ggsave(file=paste("WS15-SAVavg_",i,"_SitesXMonth_2.jpeg", sep=""), 
           width=10, height=6, dpi=500);
}
i=3
# Water Depths      /   ###############
for (i in 1:length(unique(SAV.data$SiteSAV))) {
    site_i <- unique(SAV.data$SiteSAV) [i];
    data_i <- SAV.data[SAV.data$SiteSAV==site_i,];
        
    x0 <- ggplot(data=data_i, aes(x=Month, col=factor(Year))) + theme_bw();
    x1 <- x0 + geom_point(aes(y=Depth.cm), na.rm=T) + 
        scale_x_continuous(limits=c(3,12), breaks=seq(from=3, to=12, by=1));
    x2 <- x1 + geom_line(aes(y=Depth.cm), na.rm=T) ;
    x3 <- x2 + xlab("Month") + ylab(paste(OW7.name2[OW7.name2$Var.Name=="Depth.cm",3])) +
        ggtitle(paste(OW7.name2[OW7.name2$Var.Name=="Depth.cm",2], "for Site:", site_i)) +
        labs(col="Sampling Year");

x3
    ggsave(file=paste("WS15-WaterZ_",i,"_SitesXMonth_3.jpeg", sep=""), 
           width=10, height=6, dpi=500);
}

## redo Water Depth PLOTS as 3x5 matrix (facet?)    ################
WatZ.dat <- subset(OWSAV7.1, SiteSAV != "")
WatZ.dat <- droplevels(WatZ.dat);
#WatZ.dat <- transform(WatZ.dat, SiteName=factor(SiteSAV, 
#                    levels=c("OUTFALL-CNFL", "OUT-WB-TAILRACE", "WS-1",
#                    "WS-2", "WS-3", "WS-4", "WS-5", "OUT-HC-WMA-DD", 
#                    "WS-6", "WS-7", "WS-8", "WS-9", "WS-10", "WS-11", "WS-12")));
WatZ.dat$SiteSAV <- factor(WatZ.dat$SiteSAV, levels=c("OUTFALL-CNFL", "OUT-WB-TAILRACE", "WS-1",
                    "WS-2", "WS-3", "WS-4", "WS-5", "OUT-HC-WMA-DD", 
                    "WS-6", "WS-7", "WS-8", "WS-9", "WS-10", "WS-11", "WS-12"));
### The above line works... ##
WatZ.dat <- WatZ.dat[with(WatZ.dat, order(SiteORDER, Year, Month)),]

v0 <- ggplot(data=WatZ.dat, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~SiteSAV, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=Depth.cm), na.rm=T) + scale_y_continuous(limits=c(0,NA), 
                    breaks=seq(0,120,30)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_line(aes(y=Depth.cm), na.rm=T)  + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + labs(col="Sampling Year") + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="Depth.cm",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="Depth.cm",3],")")) +
    ggtitle("Observed Water Depths in Willard Spur (2011-13)");
v3

ggsave(file=paste("WS15-WatDepth_ALLSitesXMon_4x.jpeg", sep=""), 
           width=10, height=11, dpi=500);
########  try again w/ 2 cols ... ############;
v0 <- ggplot(data=WatZ.dat, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~SiteSAV, scales="fixed", ncol=2);
v1 <- v0 + geom_point(aes(y=Depth.cm), na.rm=T) + scale_y_continuous(limits=c(0,NA), 
                    breaks=seq(0,120,30)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_line(aes(y=Depth.cm), na.rm=T)  + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + labs(col="Sampling Year") + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="Depth.cm",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="Depth.cm",3],")")) +
    ggtitle("Observed Water Depths in Willard Spur (2011-13)");
v3
ggsave(file=paste("WS15-WatDepth_ALLSitesXMon-x2_4xx.jpeg", sep=""), 
           width=10, height=11, dpi=500);
##############

### 150929:  Better SAV plots and SAV-metrics ::  Season x Year (Box plots)? #####
#  data = OWSAV7.1
v0 <- ggplot(data=WatZ.dat, aes(x=factor(Season, levels=c("Spring", "Summer", "Autumn")), 
        col=factor(Year))) + theme_bw() + scale_colour_manual(values=rainbow(3)) + 
        facet_wrap(~SiteSAV, scales="fixed", ncol=2);
v1 <- v0 + geom_jitter(aes(y=SAVcov.max), na.rm=T, position=position_jitter(w=0.1)) + 
    scale_y_continuous(limits=c(0,NA)) ;
    
v3 <- v1 + labs(col="Sampling Year") + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.max",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="SAVcov.max",3],")")) +
    ggtitle("Max SAV Cover: Willard Spur (2011-13)");
v3
ggsave(file=paste("WS15-MaxSAVCov_ALLSites-Seasn_5.jpeg", sep=""), 
           width=9, height=11, dpi=500);
## Not a great plot -- need to aggregate the sites a bit more...

##  SAVcovMax..    ###############
WatZ.dat <- WatZ.dat[with(WatZ.dat, order(SiteORDER, Year, Month)),]

v0 <- ggplot(data=WatZ.dat, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~SiteSAV, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAVcov.max), na.rm=T) + scale_y_continuous(limits=c(0,NA), 
                    breaks=seq(0,90,20)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_line(aes(y=SAVcov.max), na.rm=T)  + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + theme(legend.position="none", axis.text.x=element_text(size=rel(0.8))) + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.max",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="SAVcov.max",3],")")) +
    ggtitle("Max SAV Cover: Willard Spur (2011-13)");
v3
# legend removed
ggsave(file=paste("WS15-SAVCovMAx_Sites_5x.jpeg", sep=""), 
           width=7.5, height=9, dpi=500);

## SAVcov.avg    #################
v0 <- ggplot(data=WatZ.dat, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~SiteSAV, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAVcov.avg), na.rm=T) + scale_y_continuous(limits=c(0,NA), 
                    breaks=seq(0,90,20)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_line(aes(y=SAVcov.avg), na.rm=T)  + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + theme(legend.position="none", axis.text.x=element_text(size=rel(0.8))) + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.avg",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="SAVcov.avg",3],")")) +
    ggtitle("Mean SAV Cover: Willard Spur (2011-13)");
v3
# legend removed
ggsave(file=paste("WS15-SAVCovAVG_Sites_5x.jpeg", sep=""), 
           width=7.5, height=9, dpi=500);

## SAV.COND.avg    #############
v0 <- ggplot(data=WatZ.dat, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~SiteSAV, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAV.COND.avg), na.rm=T) + 
    scale_y_continuous(limits=c(0.8,NA), breaks=c(NA,1,2,3)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_line(aes(y=SAV.COND.avg), na.rm=T)  ;
v3 <- v2 + theme(legend.position="none", axis.text.x=element_text(size=rel(0.8))) + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAV.COND.avg",2])) +
    ggtitle("Mean SAV Condition Class: Willard Spur (2011-13)");
v3
# legend removed
ggsave(file=paste("WS15-SAVCONDavg_Sites_5x.jpeg", sep=""), 
           width=7.5, height=9, dpi=500);

## SurfMat    #############
v0 <- ggplot(data=WatZ.dat, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~SiteSAV, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SurfMat), na.rm=T) + 
    scale_y_continuous(limits=c(0.8,NA)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_line(aes(y=SurfMat), na.rm=T)  ;
v3 <- v2 + theme(legend.position="none", axis.text.x=element_text(size=rel(0.8))) + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="SurfMat",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="SurfMat",3],")")) +
    ggtitle("Mean Surface Mat Cover: Willard Spur (2011-13)");
v3
# legend removed
ggsave(file=paste("WS15-SurfMat_Sites_5x.jpeg", sep=""), 
           width=7.5, height=9, dpi=500);

## Algmat.avg    #############
v0 <- ggplot(data=WatZ.dat, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~SiteSAV, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=Algmat.avg), na.rm=T, position=position_jitter(w=0.2)) + 
    scale_y_continuous(limits=c(0.8,NA)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_line(aes(y=Algmat.avg), na.rm=T)  ;
v3 <- v2 + theme(legend.position="none", axis.text.x=element_text(size=rel(0.8))) + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="Algmat.avg",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="Algmat.avg",3],")")) +
    ggtitle("Mean Algal Mat Cover: Willard Spur (2011-13)");
v3
# legend removed
ggsave(file=paste("WS15-algalMat_Sites_5x.jpeg", sep=""), 
           width=7.5, height=9, dpi=500);
######################

## SAV and Cover metrics...collapsing into 5 sites... ############
#  data = WatZ.dat  ;

sites.x <- as.character(rep("",nrow(WatZ.dat)));
sites.x[WatZ.dat[,"SiteSAV"]=="OUTFALL-CNFL"] <- "Eastern Margin"
sites.x[WatZ.dat[,"SiteSAV"]=="OUT-WB-TAILRACE"] <- "Eastern Margin"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-1"] <- "Eastern Margin"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-2"] <- "WS-2 to WS-4"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-3"] <- "WS-2 to WS-4"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-4"] <- "WS-2 to WS-4"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-5"] <- "Mid-Spur"
sites.x[WatZ.dat[,"SiteSAV"]=="OUT-HC-WMA-DD"] <- "Mid-Spur"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-6"] <- "Mid-Spur"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-7"] <- "WS-7 to WS-9"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-8"] <- "WS-7 to WS-9"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-9"] <- "WS-7 to WS-9"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-10"] <- "Western Margin"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-11"] <- "Western Margin"
sites.x[WatZ.dat[,"SiteSAV"]=="WS-12"] <- "Western Margin"
WatZ.dat2 <- data.frame(WatZ.dat, SiteGROUP=sites.x)
WatZ.dat2 <- WatZ.dat2[with(WatZ.dat2, order(SiteGROUP, Year, DOY)),]
WatZ.dat2 <- WatZ.dat2[,c(1:6, 94, 7:(length(names(WatZ.dat2))-1))]
##############

####   Repeate SAV cover data figures (SYSTAT) from 2012/2013 ###########
## Facet x Year; xaxis = Month
# data = OWSAV7.dat2 (full data)
####  THESE ARE DOT / "JITTER" Plots 
OWSAV7.dat2 <- OWSAV7.dat2[with(OWSAV7.dat2, order(Year, DOY)),]

p0 <- ggplot(data=OWSAV7.dat2, aes(x=factor(Month), fill=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3)) + facet_wrap(~Year, scales="fixed", ncol=1);
p1 <- p0 + geom_dotplot(aes(y=SAVcov.avg), shape=21, size=3, na.rm=T, 
                        binaxis="y", stackdir="center",dotsize=1.5) + 
    scale_y_continuous(limits=c(0,NA), breaks=seq(0,100,25)) +
    scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
p2 <- p1 + labs(fill="Sampling Year") + xlab(NULL) + 
    ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.avg",2])) +
    ggtitle("Seasonal patterns in SAV cover");
p2
    ggsave(file="WS15_SAVxMnth_6.jpeg", width=7.5, height=5, dpi=500);
######

#####################################
## SAVcov.avg   SiteGROUP #############
WatZ.dat2$SiteGROUP <- factor(WatZ.dat2$SiteGROUP, levels=c("Eastern Margin", "WS-2 to WS-4", 
                            "Mid-Spur", "WS-7 to WS-9", "Western Margin"));

v0 <- ggplot(data=WatZ.dat2, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3), name="Sampling Year\n") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAVcov.avg), na.rm=T) + 
    scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAVcov.avg",2], "(", 
               OW7.name2[OW7.name2$Var.Name=="SAVcov.avg",3],")")) + 
    ggtitle("Mean SAV Cover: Willard Spur (2011-13)\n");
# geom_smooth(aes(y=SAVcov.avg), na.rm=T, se=F)  + 
####  Recalculate the Mean/Median (month) for each year, by the SiteGROUP classes ####
    gm_mean <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
    cnt <- function(x) count(x);
    nmissing <- function(x) sum(is.na(x));
    nDiff <- function(x) (length(x)-nmissing(x));
    gm_mean2 <- function(x, na.rm=T, zero.propagate=FALSE) 
        { if(any(x<0, na.rm=T)) {   return(NaN) }
        if(zero.propagate) { if(any(x==0, na.rm=T)){ return(1) }
        exp(mean(log(x), na.rm=na.rm))  }   
        else { exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))   }   }       ###########
####### GROUP Wise CALCS ###  ## #######
    z1 <- ddply(WatZ.dat2, .(SiteGROUP, Year, Month), numcolwise(gm_mean, na.rm=T));
    z2x <- ddply(WatZ.dat2, .(SiteGROUP, Year, Month), numcolwise(gm_mean2, na.rm=T));
    z3 <- ddply(WatZ.dat2, .(SiteGROUP, Year, Month), numcolwise(nmissing));
    z4 <- ddply(WatZ.dat2, .(SiteGROUP, Year, Month), numcolwise(length));
    z5 <- ddply(WatZ.dat2, .(SiteGROUP, Year, Month), numcolwise(mean, na.rm=T));
    z6 <- ddply(WatZ.dat2, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));
    dat.z6 <- z6[,c(1:3, 11, 55:58, 64:74)]     ## ######
v5 <- v3 + geom_point(data=z6, aes(x=Month, y=SAVcov.avg , col=factor(Year)), shape=19, size=4,
                      na.rm=T) + geom_line(data=z6, aes(x=Month, y=SAVcov.avg , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.8)), legend.position=c(0.85,0.25))
v6
####  UPDATED 10/7/2015 and fixed some calc issues ##
    ggsave(file=paste("WS15-SAVCovAVG_Sites_6.jpeg", sep=""), width=7.5, height=6, dpi=500);
#####################################################

### Dot plot + MEDian values (by SiteGROUP [k=5]) for SAV COND CLASS...  #######
######  SAV - COND - CLASS ######################
v0 <- ggplot(data=WatZ.dat2, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3), name="Sampling Year\n") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAV.COND.avg), na.rm=T) + 
    scale_y_continuous(limits=c(0.8,NA), breaks=c(NA,1,2,3)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste(OW7.name2[OW7.name2$Var.Name=="SAV.COND.avg",2], "(", 
            OW7.name2[OW7.name2$Var.Name=="SAV.COND.avg",3],")")) + 
            ggtitle("SAV Cover Class: Willard Spur (2011-13)\n");
v5 <- v3 + geom_point(data=z6, aes(x=Month, y=SAV.COND.avg , col=factor(Year)), shape=19, size=4,
                      na.rm=T) + geom_line(data=z6, aes(x=Month, y=SAV.COND.avg , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.8)), legend.position=c(0.85,0.25))
v6
    ggsave(file=paste("WS15-SAV.CONDCLASS_Sites_6.jpeg", sep=""), width=7.5, height=6, dpi=500);
##

##### New Variables (composites)
# data == WatZ.dat2
WatZ.dat3 <- WatZ.dat2
WatZ.dat3$SAV.biovol <- WatZ.dat3$SAVcov.avg*WatZ.dat3$SAV.z.avg
WatZ.dat3$SAV.SCR <- WatZ.dat3$SAVcov.avg*WatZ.dat3$SAV.COND.avg
WatZ.dat3$SAV.Vigor <- WatZ.dat3$SAVcov.avg*WatZ.dat3$SAV.COND.avg*WatZ.dat3$SAV.z.avg
summary(WatZ.dat3[,c(88, 86, 84,96:98)])
WatZ.d3.nm <- as.data.frame(names(WatZ.dat3))

###  SAV.BIOVOLUME
z6.3 <- ddply(WatZ.dat3, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));

v0 <- ggplot(data=WatZ.dat3, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3), name="Sampling Year\n") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAV.biovol), na.rm=T) + 
            scale_y_continuous(limits=c(NA,NA)) +
            scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste("SAV Biovolume (Cover x Height)")) + 
            ggtitle("SAV Biovolume: Willard Spur (2011-13)\n");
v5 <- v3 +  geom_point(data=z6.3, aes(x=Month, y=SAV.biovol , col=factor(Year)), shape=19, size=4,
                      na.rm=T) + 
            geom_line(data=z6.3, aes(x=Month, y=SAV.biovol , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.8)), legend.position=c(0.85,0.25))
v6
    ggsave(file=paste("WS15-SAV.BioVol-x-SiteGRP_7.jpeg", sep=""), width=7.5, height=6, dpi=500);

##
WZ3.trim <- WatZ.dat3[,c(7,10:12, 18, 53,63,65,67,68,84,86,88,62,95,96,97,98)]
##
###  SAV.SCR
z6.3 <- ddply(WatZ.dat3, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));

v0 <- ggplot(data=WatZ.dat3, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3), name="Sampling Year\n") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAV.SCR), na.rm=T) + 
            scale_y_continuous(limits=c(NA,NA)) +
            scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste("SAV Condition Score (Cover x Condition Class)")) + 
            ggtitle("SAV Condition Score: Willard Spur (2011-13)\n");
v5 <- v3 +  geom_point(data=z6.3, aes(x=Month, y=SAV.SCR , col=factor(Year)), shape=19, size=4,
                      na.rm=T) + 
            geom_line(data=z6.3, aes(x=Month, y=SAV.SCR , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.8)), legend.position=c(0.85,0.25))
v6
    ggsave(file=paste("WS15-SAV.SCR-x-SiteGRP_7.jpeg", sep=""), width=7.5, height=6, dpi=500);

###  SAV.Vigor
z6.3 <- ddply(WatZ.dat3, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));

v0 <- ggplot(data=WatZ.dat3, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3), name="Sampling Year\n") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=SAV.Vigor), na.rm=T) + 
            scale_y_continuous(limits=c(NA,NA)) +
            scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste("SAV Vigor  (Cover x Condition Class x Plant Ht)")) + 
            ggtitle("SAV Vigor Score: Willard Spur (2011-13)\n");
v5 <- v3 +  geom_point(data=z6.3, aes(x=Month, y=SAV.Vigor , col=factor(Year)), shape=19, size=4,
                      na.rm=T) + 
            geom_line(data=z6.3, aes(x=Month, y=SAV.Vigor , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.8)), legend.position=c(0.85,0.25))
v6
    ggsave(file=paste("WS15-SAV.VIGOR-x-SiteGRP_7.jpeg", sep=""), width=7.5, height=6, dpi=500);

###  CHLA
z6.3 <- ddply(WatZ.dat3, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));

v0 <- ggplot(data=WatZ.dat3, aes(x=Month, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3), name="Sampling Year\n") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=3);
v1 <- v0 + geom_point(aes(y=CHLA), na.rm=T) + 
            scale_y_continuous(limits=c(NA,NA)) +
            scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste(OW7.name2[OW7.name2$Var.Name=="CHLA",2], "(", 
            OW7.name2[OW7.name2$Var.Name=="CHLA",3],")")) + 
            ggtitle("Chlorophyll-a: Willard Spur (2011-13)\n");
v5 <- v3 +  geom_point(data=z6.3, aes(x=Month, y=CHLA , col=factor(Year)), shape=19, size=4,
                      na.rm=T) + 
            geom_line(data=z6.3, aes(x=Month, y=CHLA , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.8)), legend.position=c(0.85,0.25))
v6
    ggsave(file=paste("WS15-CHLA-x-SiteGRP_7.jpeg", sep=""), width=7.5, height=6, dpi=500);


##### *new* SAVcov.avg   SiteGROUP ######  151018  #######
# data = WatSAV9
dataX <- WatSAV9
    dataX$SiteGROUP <- factor(dataX$SiteGROUP, levels=c("Eastern Margin", "WS-2 to WS-4", 
                            "Mid-Spur", "WS-7 to WS-9", "Western Margin"));
## PLOTTING ##
v0 <- ggplot(data=dataX, aes(x=Month, col=factor(Year))) + theme_bw() + 
    labs(col="Sampling Year") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=5);
v1 <- v0 + geom_point(aes(y=SAVcov.avg), na.rm=T) + 
    scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste("Average SAV Cover")) + 
    ggtitle("Mean SAV Cover: Willard Spur (2011-13)");
# geom_smooth(aes(y=SAVcov.avg), na.rm=T, se=F)  + 
####  Recalculate the Mean/Median (month) for each year, by the SiteGROUP classes ####
    gm_mean <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
    cnt <- function(x) count(x);
    nmissing <- function(x) sum(is.na(x));
    nDiff <- function(x) (length(x)-nmissing(x));
    gm_mean2 <- function(x, na.rm=T, zero.propagate=FALSE) 
        { if(any(x<0, na.rm=T)) {   return(NaN) }
        if(zero.propagate) { if(any(x==0, na.rm=T)){ return(1) }
        exp(mean(log(x), na.rm=na.rm))  }   
        else { exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))   }   }       ###########
####### GROUP Wise CALCS ###  ## #######
    z1 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(gm_mean, na.rm=T));
    z2x <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(gm_mean2, na.rm=T));
    z3 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(nmissing));
    z4 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(length));
    z5 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(mean, na.rm=T));
    z6 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));
    dat.z6 <- z6[,c(1:3, 11, 55:58, 64:74)]     ## ######
v5 <- v3 + geom_point(data=z6, aes(x=Month, y=SAVcov.avg , col=factor(Year)), shape=19, size=3,
                      na.rm=T) + geom_line(data=z6, aes(x=Month, y=SAVcov.avg , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.65)), legend.text=element_text(size=rel(0.65)),
                 legend.position=c(0.06,0.25), legend.background=element_rect(fill="transparent"));
v6
######
    ggsave(file=paste("WS15-SAVCovAVG_Sites_x5_v7.jpeg", sep=""), width=10, height=3.5, dpi=500);
#####################################################
# scale_colour_manual(values=rainbow(3), name="Sampling Year\n") ;
###

##### *new* ** SAV.COND.avg   SiteGROUP ######  151018  #######
# data = WatSAV9
dataX <- WatSAV9
    dataX$SiteGROUP <- factor(dataX$SiteGROUP, levels=c("Eastern Margin", "WS-2 to WS-4", 
                            "Mid-Spur", "WS-7 to WS-9", "Western Margin"));
## PLOTTING ##
v0 <- ggplot(data=dataX, aes(x=Month, col=factor(Year))) + theme_bw() + 
    labs(col="Sampling Year") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=5);
v1 <- v0 + geom_point(aes(y=SAV.COND.avg), na.rm=T) + 
    scale_y_continuous(limits=c(0.8,NA), breaks=c(NA,1,2,3)) +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste("Average SAV Condition Class")) + 
    ggtitle("Mean SAV Condition Class: Willard Spur (2011-13)");
# geom_smooth(aes(y=SAVcov.avg), na.rm=T, se=F)  + 
####  Recalculate the Mean/Median (month) for each year, by the SiteGROUP classes ####
    gm_mean <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
    cnt <- function(x) count(x);
    nmissing <- function(x) sum(is.na(x));
    nDiff <- function(x) (length(x)-nmissing(x));
    gm_mean2 <- function(x, na.rm=T, zero.propagate=FALSE) 
        { if(any(x<0, na.rm=T)) {   return(NaN) }
        if(zero.propagate) { if(any(x==0, na.rm=T)){ return(1) }
        exp(mean(log(x), na.rm=na.rm))  }   
        else { exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))   }   }       ###########
####### GROUP Wise CALCS ###  ## #######
    z1 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(gm_mean, na.rm=T));
    z2x <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(gm_mean2, na.rm=T));
    z3 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(nmissing));
    z4 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(length));
    z5 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(mean, na.rm=T));
    z6 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));
    dat.z6 <- z6[,c(1:3, 11, 55:58, 64:74)]     ## ##################
v5 <- v3 + geom_point(data=z6, aes(x=Month, y=SAV.COND.avg , col=factor(Year)), shape=19, size=3,
                      na.rm=T) + geom_line(data=z6, aes(x=Month, y=SAV.COND.avg , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.65)), legend.text=element_text(size=rel(0.65)),
                 legend.position=c(0.06,0.25), legend.background=element_rect(fill="transparent"));
v6
######
    ggsave(file=paste("WS15-SAVCOND_Sites_x5_v7.jpeg", sep=""), width=10, height=3.5, dpi=500);
##

##### *new* ** SAV.biovol   SiteGROUP ######  151018  #######
# data = WatSAV9
dataX <- WatSAV9
    dataX$SiteGROUP <- factor(dataX$SiteGROUP, levels=c("Eastern Margin", "WS-2 to WS-4", 
                            "Mid-Spur", "WS-7 to WS-9", "Western Margin"));
## PLOTTING ##
v0 <- ggplot(data=dataX, aes(x=Month, col=factor(Year))) + theme_bw() + 
    labs(col="Sampling Year") + 
    facet_wrap(~SiteGROUP, scales="fixed", ncol=5);
v1 <- v0 + geom_point(aes(y=SAV.biovol), na.rm=T) + 
    scale_y_continuous(limits=c(NA,NA), labels=comma)  +
    scale_x_continuous(limits=c(3,12), breaks=seq(3, 12, 1), labels=c("Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
v2 <- v1 + geom_hline(yintercept=0, lty=2, col="black");
v3 <- v2 + xlab(NULL) +  ylab(paste(" SAV Biovolume (Cover x Height)")) + 
    ggtitle("Mean SAV Biovolume: Willard Spur (2011-13)");
# geom_smooth(aes(y=SAVcov.avg), na.rm=T, se=F)  + 
####  Recalculate the Mean/Median (month) for each year, by the SiteGROUP classes ####
    gm_mean <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
    cnt <- function(x) count(x);
    nmissing <- function(x) sum(is.na(x));
    nDiff <- function(x) (length(x)-nmissing(x));
    gm_mean2 <- function(x, na.rm=T, zero.propagate=FALSE) 
        { if(any(x<0, na.rm=T)) {   return(NaN) }
        if(zero.propagate) { if(any(x==0, na.rm=T)){ return(1) }
        exp(mean(log(x), na.rm=na.rm))  }   
        else { exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))   }   }       ###########
####### GROUP Wise CALCS ###  ## #######
    z1 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(gm_mean, na.rm=T));
    z2x <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(gm_mean2, na.rm=T));
    z3 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(nmissing));
    z4 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(length));
    z5 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(mean, na.rm=T));
    z6 <- ddply(dataX, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));
    dat.z6 <- z6[,c(1:3, 11, 55:58, 64:74)]     ## ##################
v5 <- v3 + geom_point(data=z6, aes(x=Month, y=SAV.biovol , col=factor(Year)), shape=19, size=3,
                      na.rm=T) + geom_line(data=z6, aes(x=Month, y=SAV.biovol , col=factor(Year)));
v6 <- v5 + theme(axis.text.x=element_text(size=rel(0.65)), legend.text=element_text(size=rel(0.65)),
                 legend.position=c(0.92,0.82), legend.background=element_rect(fill="transparent"));
v6
######
    ggsave(file=paste("WS15-SAVbioVOL_Sites_x5_v7.jpeg", sep=""), width=10, height=3.5, dpi=500);











