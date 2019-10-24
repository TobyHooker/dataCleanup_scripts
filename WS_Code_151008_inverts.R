### WS15 Master data "WS15_MASTERDATA_9.xlsx // All used[usable] data files for Waters

# Inv3Avg9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/Inv3Avg.9.csv")
# WatSAV9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/WatSAV.9.csv")
# OWSAV729 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/OWSAV72.9.csv")
# OW6dat9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/OW6dat.9.csv")
# WChem9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/WChem.9.csv")
# WatZ9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/WaterZ.9.csv")

######################   A N A L Y S E S  ###############
# [1]  Generate Faceted plots for Key Monitoring Sites for Invertebrate Metrics (PMI, SI)
Invdat <- Inv3Avg9
Inv.nam <- as.data.frame(names(Invdat))
levels(Invdat$Site)  
levels(factor(Invdat$MLID))
## Need to trim Sites to KMS...  #########
KeyMon <- as.data.frame(rbind("OUTFALL-CNFL", "WS-3", "WS-8", "WS-6", "OUT-WB-TAILRACE", "WS-2",
                       "OUT-HC-WMA-DD"))
names(KeyMon)[names(KeyMon) == "V1"] <- "Site"
    inv <- inv[with(inv, order(MLID, Site)),]
KeyMon$MLID <- c("5984640", "5984670", "5984720", "5984700", "5984645", "5984660", "5984695")
KeyMon$SiteX <- c("OUTFALL-CNFL", "WS-3", "WS-8", "WS-6", "OUTFALL-CNFL", "WS-3", "WS-6")
KeyMon$SiteXX <- c("OUTFALL-CNFL", "WS-3", "WS-8", "WS-6", NA, NA, NA)

KMS <- (Invdat$MLID %in% KeyMon$MLID)
dat.inv <- Invdat[KMS,]
dat.inv <- dat.inv[with(dat.inv, order(MLID)),]
    dat.inv2 <- merge(dat.inv, KeyMon, by="MLID", all.x=TRUE)
dat.inv2 <- dat.inv2[,c(1,2,50,51,3:48)]
Invert <- dat.inv2
Invert.name <- as.data.frame(names(Invert))
        rm(dat.inv, dat.inv2)
##

#### More fixing of Factor Levels ...  #######
# Extract MLID, SiteName, SiteSAV, SiteName2, SiteNameX,
#               SiteGROUP, and SiteORDER from:  WATSAV9 [df]
Site.dat <- WatSAV9[,c(2:9)]
Site.dat <- unique(Site.dat)
Site.dat <- Site.dat[with(Site.dat, order(SiteORDER)),]
###  Use this one in lieu of KeyMon !! [maybe]  ###

###### PLotting  #######
#  [#1]  PMI ==> zPMI [46]         //  DATA == Invert  // Facet = SiteXX   ##########
Invert2 <- Invert
Invert.dat <- Invert
Invert$SiteXX <- factor(Invert$SiteXX)
Invert[Invert=='NA'] <- NA
Invert.dat <- Invert.dat[!(is.na(Invert.dat$SiteXX)),]  # finally !!
Invert <- Invert.dat
Invert <- Invert2;          
######
i = 48
p0 <- ggplot(data=Invert.dat, aes(x=Invert.dat$DOY, col=factor(Invert.dat$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert.dat[,i]), na.rm=T) + geom_point(aes(y=Invert.dat[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab("% PMI (counts)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: PMI Index \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert.dat[i]),"_sites9b.jpeg", sep=""), 
           width=10, height=4, dpi=500);
# ,labels=percent

p0 <- ggplot(data=Invert, aes(x=Invert$DOY, col=factor(Invert$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert[,i]), na.rm=T) + geom_point(aes(y=Invert[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab("% PMI (counts)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: PMI Index \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert[i]),"_sites9xb.jpeg", sep=""), 
           width=10, height=4, dpi=500);

##  [#1.5] PMI x Site[Order] x [Year]      ##########
options(digits=2)
# data = Invdat
Inv.Sites <- merge(Invdat, Site.dat, by="MLID", all.x=TRUE)
Inv.Sites <- Inv.Sites[!(is.na(Inv.Sites$SiteORDER)),]
Inv.Sites <- Inv.Sites[with(Inv.Sites,order(SiteORDER, Year, MLID)),]

c0 <- ggplot(data=Inv.Sites, aes(x=SiteORDER, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3));
c1 <- c0 + geom_jitter(aes(y=zPMI, col=factor(Year)), na.rm=T, position=position_jitter(width=0.1)) + 
        scale_y_continuous(limits=c(0.01,NA)) + 
        scale_x_continuous(breaks=c(0.5,1:12), labels=c("East", "Side", "WS-2", "WS-3", "WS-4", " HC inflow", "WS-6", "WS-7", "WS-8", "WS-9", "WS-10", "WestSide", "Outlet"));
c2 <- c1 + ylab("% PMI") + xlab("Relative location in Willard Spur (East to West)") +
    ggtitle("PMI Index: Willard Spur, 2011-2013\n") + labs(col="Sampling Year");
c2
ggsave(file=paste("WS_PMIxSite_9.jpeg", sep=""), width=10, height=4, dpi=500);

#####

#  [#2]  Simpson's Diversity Index ==> zSI.diff [49] //  DATA == Invert  // Facet = SiteXX   #####
######
i = 49
p0 <- ggplot(data=Invert.dat, aes(x=Invert.dat$DOY, col=factor(Invert.dat$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert.dat[,i]), na.rm=T) + geom_point(aes(y=Invert.dat[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab("SI values (0 to 1)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Simpson's Diversity Index (1-D) Index \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert.dat[i]),"_sites9x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

# 
p0 <- ggplot(data=Invert, aes(x=Invert$DOY, col=factor(Invert$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert[,i]), na.rm=T) + geom_point(aes(y=Invert[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab("SI values (0 to 1)");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA))
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Simpson's Diversity Index (1-D) Index \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert[i]),"_sites9.jpeg", sep=""), 
           width=10, height=4, dpi=500);
##  [#2.5] PMI x Site[Order] x [Year]      ##########
    options(digits=2)
# data = Invdat
Inv.Sites <- merge(Invdat, Site.dat, by="MLID", all.x=TRUE)
Inv.Sites <- Inv.Sites[!(is.na(Inv.Sites$SiteORDER)),]
Inv.Sites <- Inv.Sites[with(Inv.Sites,order(SiteORDER, Year, MLID)),]

c0 <- ggplot(data=Inv.Sites, aes(x=SiteORDER, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3));
c1 <- c0 + geom_jitter(aes(y=zSI.diff, col=factor(Year)), na.rm=T, position=position_jitter(width=0.1)) + 
        scale_y_continuous(limits=c(0.01,NA)) + 
        scale_x_continuous(breaks=c(0.5,1:12), labels=c("East", "Side", "WS-2", "WS-3", "WS-4", " HC inflow", "WS-6", "WS-7", "WS-8", "WS-9", "WS-10", "WestSide", "Outlet"));
c2 <- c1 + ylab("SI Value") + xlab("Relative location in Willard Spur (East to West)") +
    ggtitle("Simpson's Diversity Index: Willard Spur, 2011-2013\n") + labs(col="Sampling Year");
c2
ggsave(file=paste("WS_SIdiffxSite_9.jpeg", sep=""), width=10, height=4, dpi=500);

#####
#  [#3]  # Taxa (Richness) ==> zTaxa [50] //  DATA == Invert.dat  // Facet = SiteX   #####
######
i = 50
p0 <- ggplot(data=Invert.dat, aes(x=Invert.dat$DOY, col=factor(Invert.dat$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert.dat[,i]), na.rm=T) + geom_point(aes(y=Invert.dat[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab("Number of Taxa") + geom_hline(yintercept=0, lty=2, col="black");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA), 
                                    breaks=seq(0,max(Invert.dat[,i], na.rm=T)+1,5));
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Macroinvertebrate Taxa Richness \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert.dat[i]),"_sites9x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

#  DATA == Invert   acet = SiteXX 
p0 <- ggplot(data=Invert, aes(x=Invert$DOY, col=factor(Invert$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert[,i]), na.rm=T) + geom_point(aes(y=Invert[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab("Number of Taxa") + geom_hline(yintercept=0, lty=2, col="black");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA), 
                            breaks=seq(0,max(Invert[,i], na.rm=T)+1,5));
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Macroinvertebrate Taxa Richness \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert[i]),"_sites9.jpeg", sep=""), 
           width=10, height=4, dpi=500);
##  [#3.5] Richness x Site[Order] x [Year]      ##########
    options(digits=2)
# data = Invdat

c0 <- ggplot(data=Inv.Sites, aes(x=SiteORDER, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3));
c1 <- c0 + geom_jitter(aes(y=zTaxa, col=factor(Year)), na.rm=T, position=position_jitter(width=0.1)) + 
        scale_y_continuous(limits=c(0.01,NA), breaks=seq(0,max(Invert[,i], na.rm=T)+1,5)) + 
        scale_x_continuous(breaks=c(0.5,1:12), labels=c("East", "Side", "WS-2", "WS-3", "WS-4", " HC inflow", "WS-6", "WS-7", "WS-8", "WS-9", "WS-10", "WestSide", "Outlet"));
c2 <- c1 + ylab("Number of Taxa") + xlab("Relative location in Willard Spur (East to West)") +
    ggtitle("Macroinvertebrate Taxa Richness: Willard Spur, 2011-2013\n") + labs(col="Sampling Year");
c2
ggsave(file=paste("WS_TaxaRichxSite_9.jpeg", sep=""), width=10, height=4, dpi=500);

#####

#  [#4]  # Abundance (counts) ==> zCount [47] //  DATA == Invert.dat  // Facet = SiteX   #####
######
i = 47
p0 <- ggplot(data=Invert.dat, aes(x=Invert.dat$DOY, col=factor(Invert.dat$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert.dat[,i]), na.rm=T) + geom_point(aes(y=Invert.dat[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab(bquote("# Organisms / 1.5" ~m^2~ "(composited)")) + 
    geom_hline(yintercept=0, lty=2, col="black");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA), labels=comma);
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Macroinvertebrate Abundance \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert.dat[i]),"_sites9x.jpeg", sep=""), 
           width=10, height=4, dpi=500);
# , breaks=seq(0,max(Invert.dat[,i], na.rm=T)+1,5)
#  DATA == Invert   Facet = SiteXX 
p0 <- ggplot(data=Invert, aes(x=Invert$DOY, col=factor(Invert$Year))) + 
    theme_bw() + facet_wrap(~SiteX, ncol=4, drop=TRUE)
p1 <- p0 + geom_line(aes(y=Invert[,i]), na.rm=T) + geom_point(aes(y=Invert[,i]), na.rm=T) ;
p2 <- p1 + xlab("Day of year") + ylab(bquote("# Organisms / 1.5" ~m^2~ "(composited)")) + 
    geom_hline(yintercept=0, lty=2, col="black");
p3 <- p2 +     labs(col="Sampling Year") + scale_y_continuous(limits=c(0,NA), labels=comma);
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: Macroinvertebrate Abundance \n")));
p4
    ggsave(file=paste("WS15Invert_",names(Invert[i]),"_sites9.jpeg", sep=""), 
           width=10, height=4, dpi=500);
##  [#4.5] Abundance (counts) x Site[Order] x [Year]      ##########
    options(digits=2)   # data = Invdat

c0 <- ggplot(data=Inv.Sites, aes(x=SiteORDER, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3));
c1 <- c0 + geom_jitter(aes(y=zCount, col=factor(Year)), na.rm=T, position=position_jitter(width=0.1)) + 
        scale_y_continuous(limits=c(1,NA), labels=comma, breaks=seq(0,max(Invert.dat[,i], na.rm=T)*1.05,
            4000)) + scale_x_continuous(breaks=c(0.5,1:12), labels=c("East", "Side", "WS-2", "WS-3", "WS-4", " HC inflow", "WS-6", "WS-7", "WS-8", "WS-9", "WS-10", "WestSide", "Outlet"));
c2 <- c1 + ylab(bquote("# Organisms / 1.5" ~m^2~ "(composited)")) + 
    xlab("Relative location in Willard Spur (East to West)") +
    ggtitle("Macroinvertebrate Abundance: Willard Spur, 2011-2013\n") + labs(col="Sampling Year");
c2
ggsave(file=paste("WS_AbundxSite_9.jpeg", sep=""), width=10, height=4, dpi=500);
#  
#######################   Inverts Above ^^  #########



### Median summary of Invert data...

    z6.invert <- ddply(Inv.Sites, .(SiteGROUP, Year, Month), numcolwise(median, na.rm=T));

    z6.all <- ddply(WATSAVINV2, .(SiteGROUP2, Year, HydroConn), numcolwise(median, na.rm=T));

write.csv(z6.all, file="z6_HydConn_medians_all_151019.csv", row.names=F)

    HydroConn.all <- ddply(WATSAVINV2, .(HydroConn, Year), numcolwise(median, na.rm=T));

write.csv(HydroConn.all, file="HydConnall_medians_all_151019_2.csv", row.names=F)

    HydroConn.1 <- ddply(WATSAVINV2, .(HydroConn), numcolwise(median, na.rm=T))

write.csv(HydroConn.1, file="HydConn-1_medians_all_151019_3.csv", row.names=F)






