#  WS15_code_150923
# data = data_OW4_x
# data.nm = OW4.nm
    # set 2
    # data = data_OW4_2X        :: includes SiteNameX (& broader KMS)
    # data.nm = OW4.2x.nm2
write.csv(data_OW4_2X, file="data_OW4.2X.csv", row.names=F)
write.csv(OW4.2x.nm2, file="OW4.2x.nm2.csv", row.names=F)
# dataset 3
# OW5.dat <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/Willard Spur/WSpur2015.R/OW5.dat_150923.csv")
# OW5.nm <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/Willard Spur/WSpur2015.R/OW5.nm.csv")
# Metals.names <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/Willard Spur/WSpur2015.R/Metals.names.csv")
        OW5.dat$Year.FX <- as.factor(OW5.dat$Year)

#####################
## Metals Boxplots w/ numeric criteria embedded
# data:         Metal5.dat <- OW5.dat
# data.names:   Metal5.nm <- OW5.nm
Met.dat <- Metal5.dat
Met.dat <- Met.dat[,c(2,3,4:13,30:34,36:40)] ; # data: 13-22
Met.melt <- melt(Met.dat, id=1:12, value.name="Conc", variable.name="Metals")
Met.melt <- Met.melt[with(Met.melt, order(Metals, Year, DOY)),]
levels(Met.melt$MetName) <- c("Cadmium", "Lead", "Chromium", "Copper", "Arsenic", "Nickel", 
                              "Iron","Aluminum", "Boron", "Barium")
# need to map criteria to values in Met.melt...
write.csv(Met.melt, "Met.melt0.csv", row.names=F)
Met.melt0 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/Willard Spur/WSpur2015.R/Met.melt0.csv")
Met.melt1 <- Met.melt0[with(Met.melt0, order(MetName)),]
Met.melt0 <- Met.melt1
###############
## Data to Use:   Met.melt0
v0 <- ggplot(data=Met.melt0) + theme_bw() + facet_wrap(~MetName, scales="free", ncol=5) + 
    scale_colour_tableau();
v1 <- v0 + geom_boxplot(aes(x=Met.melt0$TYPE, y=Met.melt0$Conc), na.rm=T, outlier.shape=NA) + 
        geom_jitter(aes(x=Met.melt0$TYPE, y=Met.melt0$Conc), col="darkgreen", na.rm=T, 
                    width=0.05, height=0.1) + scale_y_continuous(limits=c(0,NA)) +
    geom_hline(data = Met.melt0, aes(yintercept=Met.melt0$Met.Crit.10), lty=2, col="black") +
    geom_hline(data = Met.melt0, aes(yintercept=Met.melt0$Met.Crit.90), lty=2, col="red");
v2 <- v1 + ylab(expression(paste("Metal Concentrations (",mu,"g/L)"))) + 
    scale_x_discrete(breaks=NULL) + xlab(NULL) + ggtitle("Dissolved Metal Concentrations");

ggsave(file=paste("WS15-Metals_NumCrit6.jpeg", sep=""), 
           width=10, height=4, dpi=500);

## VERSION #2 -- long       ###################
v0 <- ggplot(data=Met.melt0) + theme_bw() + facet_wrap(~MetName, scales="free", ncol=2) + 
    scale_colour_tableau();
v1 <- v0 + geom_boxplot(aes(x=Met.melt0$TYPE, y=Met.melt0$Conc), na.rm=T, outlier.shape=NA) + 
        geom_jitter(aes(x=Met.melt0$TYPE, y=Met.melt0$Conc), col="darkgreen", na.rm=T, 
                    width=0.05, height=0.1) + scale_y_continuous(limits=c(0,NA)) +
    geom_hline(data = Met.melt0, aes(yintercept=Met.melt0$Met.Crit.10), lty=2, col="black") +
    geom_hline(data = Met.melt0, aes(yintercept=Met.melt0$Met.Crit.90), lty=2, col="red");
v2 <- v1 + ylab(expression(paste("Metal Concentrations (",mu,"g/L)"))) + 
    scale_x_discrete(breaks=NULL) + xlab(NULL) + ggtitle("Dissolved Metal Concentrations");

ggsave(file=paste("WS15-Metals_NumCrit6-x2.jpeg", sep=""), 
           width=6, height=8, dpi=500);
## this one works.. (not sure about the "WIDE" one)

## VERSION #3 -- 3 x 3          #########################
## DROP Iron (Fe)
Met.melt1 <- Met.melt0
Met.melt1 <- subset(Met.melt0, Metals != "D_FE", drop=T)
Met.melt1 <- droplevels(Met.melt1)

v0 <- ggplot(data=Met.melt1) + theme_bw() + facet_wrap(~MetName, scales="free", ncol=3) + 
    scale_colour_tableau();
v1 <- v0 + geom_boxplot(aes(x=Met.melt1$TYPE, y=Met.melt1$Conc), na.rm=T, outlier.shape=NA) + 
        geom_jitter(aes(x=Met.melt1$TYPE, y=Met.melt1$Conc), col="darkgreen", na.rm=T, 
                    width=0.05, height=0.1) + scale_y_continuous(limits=c(0,NA)) +
    geom_hline(data = Met.melt1, aes(yintercept=Met.melt1$Met.Crit.10), lty=2, col="black") +
    geom_hline(data = Met.melt1, aes(yintercept=Met.melt1$Met.Crit.90), lty=2, col="red");
v2 <- v1 + ylab(expression(paste("Metal Concentrations (",mu,"g/L)"))) + 
    scale_x_discrete(breaks=NULL) + xlab(NULL) + ggtitle("Dissolved Metal Concentrations");
v2
ggsave(file=paste("WS15-Metals_NumCrit-7_3x3.jpeg", sep=""), 
           width=6, height=6, dpi=500);
## this one works.. Nice !!
## VERSION #3.3 -- 3 x 3   // set ymax @200% [data]  XX-doesn't work-XX ################
## DROP Iron (Fe)
Met.melt1 <- Met.melt0
Met.melt1 <- subset(Met.melt0, Metals != "D_FE", drop=T)
Met.melt1 <- droplevels(Met.melt1)

v0 <- ggplot(data=Met.melt1) + theme_bw() + facet_wrap(~MetName, scales="free", ncol=3) + 
    scale_colour_tableau();
v1 <- v0 + geom_boxplot(aes(x=Met.melt1$TYPE, y=Met.melt1$Conc), na.rm=T, outlier.shape=NA) + 
        geom_jitter(aes(x=Met.melt1$TYPE, y=Met.melt1$Conc), size=2.0, col="darkgreen", na.rm=F, 
                    width=0.05, height=0.1) + 
    scale_y_continuous(limits=c(0,(2*max(Met.melt1$Conc)))) +
    geom_hline(data = Met.melt1, aes(yintercept=Met.melt1$Met.Crit.10), lty=2, col="black") +
    geom_hline(data = Met.melt1, aes(yintercept=Met.melt1$Met.Crit.90), lty=2, col="red");
v2 <- v1 + ylab(expression(paste("Metal Concentrations (",mu,"g/L)"))) + 
    scale_x_discrete(breaks=NULL) + xlab(NULL) + ggtitle("Dissolved Metal Concentrations");
v2
ggsave(file=paste("WS15-Metals_NumCrit-7_3x3.jpeg", sep=""), 
           width=6, height=6, dpi=500);
## this will not work using Facets ... need grid_arrange (or something).

## VERSION #4 -- 3 x 3 // Drop the Criteria         #########################
## DROP Iron (Fe)
Met.melt1 <- Met.melt0
Met.melt1 <- subset(Met.melt0, Metals != "D_FE", drop=T)
Met.melt1 <- droplevels(Met.melt1)

v0 <- ggplot(data=Met.melt1) + theme_bw() + facet_wrap(~MetName, scales="free", ncol=3) + 
    scale_colour_tableau();
v1 <- v0 + geom_boxplot(aes(x=Met.melt1$TYPE, y=Met.melt1$Conc), na.rm=T, outlier.shape=NA) + 
        geom_jitter(aes(x=Met.melt1$TYPE, y=Met.melt1$Conc), col="darkgreen", na.rm=T, 
                    width=0.05, height=0.1) + scale_y_continuous(limits=c(0,NA))  ;
v2 <- v1 + ylab(expression(paste("Metal Concentrations (",mu,"g/L)"))) + 
    scale_x_discrete(breaks=NULL) + xlab(NULL) + ggtitle("Dissolved Metal Concentrations");
v2
ggsave(file=paste("WS15-Metals_NumCrit-7_3x3_xx.jpeg", sep=""), 
           width=6, height=6, dpi=500);
## this one works.. Nice !!

#######################################################
#######################################################
##### N U T R I E N T S ###############################
#######################################################
# data:         OW5.dat
# data.names:   OW5.nm
OW5.dat$TIN <- ifelse(is.na(OW5.dat$NO3.T),
       OW5.dat$TIN <- rowSums(cbind(OW5.dat$NH4.T, OW5.dat$NO3.D), na.rm=T),
       OW5.dat$TIN <- rowSums(cbind(OW5.dat$NH4.T, OW5.dat$NO3.T), na.rm=T))
## Need to re-select cases for the KMS...
nutr1 <- subset(OW5.dat, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                                SiteName == "WS-6"   ));
nutr2 <- subset(OW5.dat, (SiteNameX == "OUTFALL-CNFL" | SiteNameX == "WS-3" | SiteNameX == "WS-8" | 
                                SiteNameX == "WS-6"   ));
nutr1 <- droplevels(nutr1)  ## <<--  use this one
###   TIN   150923                      i = 66  #######################
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=nutr1$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("Total Inorganic N (mg/L)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Total Inorganic N")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites4.jpeg", sep=""), 
           width=10, height=4, dpi=500);
#####
# imported N species as OW6.dat  (Note that SiteNameX is not included!!)
# TN2 [47]  /   TON [50]    /   NH4.T [43]  /   NO3.T [44]  /   TIN [49]
# TP [40]   /   TN2.TP [48]
nutr1 <- subset(OW6.dat, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                                SiteName == "WS-6"   ));
nutr1 <- nutr1[with(nutr1, order(SiteName, Year, DOY)),]
###   TN2   150923           ###############           
i = 47
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=factor(nutr1$Year))) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("Total Nitrogen (mg/L)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Total Nitrogen")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites5.jpeg", sep=""), 
           width=10, height=4, dpi=500);

###   TIN   150923          ####################            
i = 49
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=factor(nutr1$Year))) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("Total Inorganic Nitrogen (mg/L)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Total Inorganic Nitrogen")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites5.jpeg", sep=""), 
           width=10, height=4, dpi=500);

###   TON   150923        ##########################              
i = 50
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=factor(nutr1$Year))) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("Total Organic Nitrogen (mg/L)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Total Organic Nitrogen")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites5.jpeg", sep=""), 
           width=10, height=4, dpi=500);

###  TN2.TP [48]        ##############################
i = 48
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=factor(nutr1$Year))) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("TN to TP ratio");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Total N to Total P Ratio")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites5.jpeg", sep=""), 
           width=10, height=4, dpi=500);

###  TON % of TN2       #####################
OW6.dat <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/Willard Spur/WSpur.2015/OW6.dat.csv")
OW6.dat$TON.TN2 <- (OW6.dat$TON / OW6.dat$TN2)
nutr1 <- subset(OW6.dat, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                                SiteName == "WS-6"   ));
nutr1 <- nutr1[with(nutr1, order(SiteName, Year, DOY)),]
i = 67
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=factor(nutr1$Year))) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("TON as % of TN");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA),labels=percent)
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Fraction of Total N as Organic N")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites5.jpeg", sep=""), 
           width=10, height=4, dpi=500);
#########   expanded KMS   ##################
## redo try w/ expanded KMS.
data_OW4_2 <- subset(OW6.dat, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                SiteName == "WS-6" | SiteName == "OUT-WB-TAILRACE" | SiteName == "WS-2" | 
                SiteName == "OUT-HC-WMA-DD"  ))
data_OW4_2 <- droplevels(data_OW4_2)
# 
data_OW4_2.x <- as.character(rep("",nrow(data_OW4_2)));
data_OW4_2.x[data_OW4_2[,'SiteName']=="OUT-WB-TAILRACE"] <- "OUTFALL-CNFL"
data_OW4_2.x[data_OW4_2[,'SiteName']=="OUTFALL-CNFL"] <- "OUTFALL-CNFL"
data_OW4_2.x[data_OW4_2[,'SiteName']=="OUT-HC-WMA-DD"] <- "WS-6"
data_OW4_2.x[data_OW4_2[,'SiteName']=="WS-6"] <- "WS-6"
data_OW4_2.x[data_OW4_2[,'SiteName']=="WS-2"] <- "WS-3"
data_OW4_2.x[data_OW4_2[,'SiteName']=="WS-3"] <- "WS-3"
data_OW4_2.x[data_OW4_2[,'SiteName']=="WS-8"] <- "WS-8"
as.data.frame(data_OW4_2.x)
    data_OW4_2X <- data.frame(data_OW4_2, SiteNameX=data_OW4_2.x) ## combine vector into DF
data_OW4_2X <- data_OW4_2X[,c(1:3, 68,4,5,6:(length(names(data_OW4_2X))-1))] # reorder
data_OW4_2X <- data_OW4_2X[with(data_OW4_2X, order(SiteNameX, Year, DOY)),]
nutr11 <- data_OW4_2X
#
###  TON % of TN2  [redo w/ expanded KMS: SiteNameX]  #########################
i = 68
p0 <- ggplot(data=nutr11, aes(x=nutr11$DOY, col=factor(nutr11$Year))) + 
    theme_bw() + facet_wrap(~ SiteNameX, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr11[,i])) + geom_point(aes(y=nutr11[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("TON as % of TN");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA),labels=percent)
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Fraction of Total N as Organic N")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr11[i]),"_sites5x.jpeg", sep=""), 
           width=10, height=4, dpi=500);
#############

###  DN  [46]        ##############################
i = 46
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=factor(nutr1$Year))) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("Dissolved Nitrogen");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Dissolved (total) Nitrogen")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites5.jpeg", sep=""), 
           width=10, height=4, dpi=500);

#######  Inorganic N pulses (all sites; by E-W distance)  ######
# data = OW6.dat
# xvar == SiteORDER [5]
# Yvars:    TIN [49]    /   NH4.T [43]  /   NO3.T [44]  /   TON [50]
options(digits=2)
c0 <- ggplot(data=OW6.dat, aes(x=SiteORDER, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3));
c1 <- c0 + geom_jitter(aes(y=TIN, col=factor(Year)), na.rm=T, position=position_jitter(width=0.1)) + 
        scale_y_continuous(limits=c(0.01,NA), breaks=c(0.1,0.5,1,5,10,20)) + coord_trans(y="log2") +
        scale_x_continuous(breaks=c(0.5,1:12), labels=c("East", "Side", "WS-2", "WS-3", "WS-4", "HC inflow", "WS-6", "WS-7", "WS-8", "WS-9", " WestSide", "Margin", "Outlet"));
c2 <- c1 + ylab("Total Inorganic N (mg/L)") + xlab("Relative location in Willard Spur (East to West)") +
    ggtitle("Total Inorganic N spikes in Willard Spur, 2011-2013") + labs(col="Sampling Year");
c2
ggsave(file=paste("WS_TINxSite_3.jpeg", sep=""), width=10, height=4, dpi=500);

## extra stuff:  limits=c(0.1,NA)       //  + coord_trans(y="log2")  ###########
# scale_colour_tableau()
# scale_color_brewer(palette=1)
# scale_colour_manual(values=rainbow(3))    //  

## TIN pulses, all sites x N-form   //  #################
c0 <- ggplot(data=OW6.dat, aes(x=SiteORDER), col="black") + theme_bw() ;
c1 <- c0 + geom_jitter(aes(y=NH4.T, shape="NH4"), na.rm=T, 
                    position=position_jitter(width=0.1)) +
        geom_jitter(aes(y=NO3.T, shape="NO3"), na.rm=T, 
                    position=position_jitter(width=0.1)) + 
    scale_shape_manual(name="Nitrogen Form", values=c(19, 2),breaks=c("NH4", "NO3"), 
                       labels=c("NH4", "NO3"));
c2 <- c1 + scale_y_continuous(limits=c(0.01,NA), breaks=c(0.1,0.5,1,5,10,20)) + 
    coord_trans(y="log2") +
        scale_x_continuous(breaks=c(0.5,1:12), labels=c("East", "Side", "WS-2", "WS-3", "WS-4", 
                "HC inflow", "WS-6", "WS-7", "WS-8", "WS-9", " WS-10", "West Side", "Outlet")) ;
c3 <- c2 + ylab("Total Inorganic N (mg/L)") + xlab("Relative location in Willard Spur (East to West)") +
    ggtitle("Total Inorganic N spikes in Willard Spur, 2011-2013") + labs(shape="Nitrogen Form");
c3

ggsave(file=paste("WS_NH4xNO3_spikesxSite_4xx.jpeg", sep=""), width=10, height=4, dpi=500);
####  This one is really sweet...   ############

######################################
#  TN:TP ratios         ## Try it w/ SiteX
## Data = nutr11        //      # Var = TN2.TP  [49]
i = 49
nutr12 <- nutr11;           ## work on reordering the factor levels
nutr12 <- transform(nutr12, SiteName=factor(SiteName, levels=c("OUTFALL-CNFL", "OUT-WB-TAILRACE",
                "WS-2", "WS-3", "OUT-HC-WMA-DD", "WS-6", "WS-8")))
nutr12 <- nutr12[with(nutr12, order(SiteName, Year, DOY)),]


p0 <- ggplot(data=nutr12, aes(x=nutr12$DOY, col=factor(nutr12$Year))) + 
    theme_bw() + facet_wrap(~ SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr12[,i])) + geom_point(aes(y=nutr12[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("TN:TP Ratio");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Total N to Total P Ratio")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr12[i]),"_sites5xx.jpeg", sep=""), 
           width=10, height=7, dpi=500);

######
## DN to DP  (using SiteName only)
i = 51
p0 <- ggplot(data=nutr1, aes(x=nutr1$DOY, col=factor(nutr1$Year))) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=nutr1[,i])) + geom_point(aes(y=nutr1[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab("DN to DP ratio");
p3 <- p2 + labs(col="Sampling Year") + scale_y_continuous(limits=c(5,NA),
            breaks=seq(from=0, to=100, by=20)) ;
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Dissolved Nitrogen to Dissolved P ratio")));
p4
    ggsave(file=paste("WS15-OW_",names(nutr1[i]),"_sites5x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

#breaks=c(1,5,10,20,40,80,120)) + coord_trans(y="log2")


















