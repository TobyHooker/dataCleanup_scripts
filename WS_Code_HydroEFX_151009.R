## Effects of Hydrologic Isolation  -- 151009    #############
## Data Sources:
# Inv3Avg9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/Inv3Avg.9.csv") [n=219]
# WatSAV9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/WatSAV.9.csv") [n=212]
# OWSAV729 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/OWSAV72.9.csv") [n=275]
# OW6dat9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/OW6dat.9.csv") [n=211]
# WChem9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/WChem.9.csv") [n=402]
# WatZ9 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/WaterZ.9.csv") [n=238]
## ## ## ## Dataset Prep:
Inv.nam <- as.data.frame(names(Invdat))
KeyMon       # list of "Key Monitoring Sites" // SiteX and SiteXX  [n=7]
Invert       ## Where SiteX <> NA  (!=)  [n=88]
Site.dat     # additional listing / group data for Sites (by MLID) [n=17]
Invert.dat   # For SiteXX <> NA (this is the larger KMS set)  [n=63]
Inv.Sites    # For organizing sites along flow transect...  [152]
Inv.codes    # Inv.codes <- as.data.frame(unique(Inv.data3[,c(8,9)])) [n=47]
    ## Could add more autecology info here for taxa functional analysis...
Inv.codes2 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur2015.R/Inv.codes2.csv")
    ## w/ FFGs and some more common name/groupings [n=47]
## Identify time-periods of Connected ("Connctd") vs. Isolated ("Isolated") Hydrology    
    ow.name <- as.data.frame(names(OWSAV729))
    Site.hydro <- as.data.frame(unique(OWSAV729[,c(10,11,14,74,75)]))
    Site.hydro.2 <- unique(Site.hydro[,c(2,4)])
    Site.hydro.2$DOY <- c(NA, 166, 310, 133, 321)
#####  Hydrologic Connectivity Effects    #####
## [1] # data == WChem9     //  Physical Parameters ####
    WChem <- WChem9
    WChem.nam <- as.data.frame(names(WChem2))
dat.hyd <- as.character(rep("",nrow(WChem)))
    dat.hyd[WChem[,"Year"]==2011] <- "Connected"
    dat.hyd[WChem[,"Year"]==2012 & WChem[,"DOY"] <= 166] <- "Connected"
    dat.hyd[WChem[,"Year"]==2012 & WChem[,"DOY"] > 166] <- "Isolated"
    dat.hyd[WChem[,"Year"]==2012 & WChem[,"DOY"] > 310] <- "Connected"
    dat.hyd[WChem[,"Year"]==2013 & WChem[,"DOY"] <= 133] <- "Connected"
    dat.hyd[WChem[,"Year"]==2013 & WChem[,"DOY"] > 133] <- "Isolated"
    dat.hyd[WChem[,"Year"]==2013 & WChem[,"DOY"] > 321] <- "Connected"
    levels(as.factor(dat.hyd))
    as.data.frame(dat.hyd)
WChem2 <- data.frame(WChem, HydroConn = dat.hyd)  ## looks good to go !!
#####
######  Hydro Effect Plots #####
## T.ALK [9] ####
    i=9
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) +
        xlab("Hydrologic Connectivity")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WChem2[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=0, lty=2, col="black") +
        ylab(paste("Total Alkalinity (mg/L)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);

## Temp [52]
    i=52
# WChem2$Season <- WChem9$Season
# WChem2$Season <- factor(WChem2$Season, levels=c("Spring", "Summer", "Autumn"));
h0 <- ggplot(data=WChem2, aes(x=HydroConn, 
                              col=factor(WChem2$Season, levels=c("Spring", "Summer", "Autumn")))) + 
    theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8) +
                 scale_y_continuous(limits=c(0,NA));
h2 <- h1 + geom_hline(yintercept=0, lty=2, col="black") + 
            geom_hline(yintercept=27, lty=2, col="black") +
    ylab(paste("Water  ", names(WChem2[i]),sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
##
# 
## pH [53]
    i=53
h0 <- ggplot(data=WChem2, aes(x=HydroConn, 
                              col=factor(WChem2$Season, levels=c("Spring", "Summer", "Autumn")))) + 
    theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.65)) +
                 scale_y_continuous(limits=c(6.5,NA));
h2 <- h1 + geom_hline(yintercept=6.5, lty=2, col="black") + 
            geom_hline(yintercept=9, lty=2, col="black") +
    ylab(paste("Water  ", names(WChem2[i]),sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
##


# geom_jitter(aes(y=WChem2[,i]), na.rm=T, position = position_jitter(width=0.2)) +
## CHLA [10]  ####
    i=10
h0 <- ggplot(data=WChem2, aes(x=HydroConn, 
            col=factor(WChem2$Season, levels=c("Spring", "Summer", "Autumn")))) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8) +
            scale_y_continuous(limits=c(0,NA), breaks=seq(0,max(WChem2[,i], na.rm=T)*1.05,50)) +
    coord_trans(y="sqrt");
h2 <- h1 + geom_hline(yintercept=0, lty=2, col="black") + 
    ylab(paste("  ", names(WChem2[i])," Concentration",sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_aSQRT.jpeg", sep=""), width=5, height=4, dpi=500);
## + coord_trans(y="log2")
## TVS.TSS [14]  ####
    i=14
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8) +
    geom_jitter(aes(y=WChem2[,i]), na.rm=T, position = position_jitter(width=0.15)) +
            scale_y_continuous(limits=c(0,NA), label=percent);
h2 <- h1 + geom_hline(yintercept=0, lty=2, col="black") + 
    ylab(paste("  ", names(WChem2[i])," Ratio",sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);

## col=Season
# col=factor(WChem2$Season, levels=c("Spring", "Summer", "Autumn"))
## + coord_trans(y="log2")   +    coord_trans(y="sqrt")
# , breaks=seq(0,max(WChem2[,i], na.rm=T)*1.05,50)
## DO.con [56]  #### remove season ####
    i=56
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WChem2[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(0,NA), breaks=seq(0,30,5)) ;
h2 <- h1 + geom_hline(yintercept=3.0, lty=2, col="black") + 
    ylab(paste(" DO Concentration",sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);
# + coord_trans(y="sqrt")
# , breaks=seq(0,max(WChem2[,i], na.rm=T)*1.05,1)
## DO.perc [57]  #### remove season ####
    i=57
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WChem2[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(0,NA)) ;
h2 <- h1 + geom_hline(yintercept=100, lty=2, col="black") + 
    ylab(paste(" DO Concentration (% of saturation)",sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);
# 
## TDS [23]  #### remove season ####
    i=23
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WChem2[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(50,NA), breaks=c(100,500,1200,5000,12000,25000)) + coord_trans(y="log10") ;
h2 <- h1 + geom_hline(yintercept=1200, lty=2, col="black") + 
    ylab(paste(" Total Dissolved Solids (mg/L)",sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);
# 
## EC25 [24]  #### remove season ####
    i=24
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WChem2[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(25,NA), breaks=c(50,100,500,1000,5000,10000)) + coord_trans(y="log10") ;
h2 <- h1 + geom_hline(yintercept=NULL, lty=2, col="black") + 
    ylab(paste(" Specific Conductivity (",OW7.name2[64,3],")",sep=""));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);
# 
## Water Depth [51]  #### remove season ####
    i=51
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WChem2[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(0,NA), breaks=c(0,25,50,75,100,125)) ;
h2 <- h1 + geom_hline(yintercept=20, lty=2, col="black") + 
    ylab(paste(" Water Depth (cm)"));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);

# , breaks=c(50,100,500,1000,5000,10000)
# + coord_trans(y="log10") 
## Water Depth [51]  #### x season ####
    i=51
h0 <- ggplot(data=WChem2, aes(x=HydroConn, col=factor(WChem2$Season, 
                                                      levels=c("Spring", "Summer", "Autumn")))) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
            scale_y_continuous(limits=c(0,NA), breaks=c(0,25,50,75,100,125)) ;
h2 <- h1 + geom_hline(yintercept=20, lty=2, col="black") + 
    ylab(paste(" Water Depth (cm)"));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") +
    theme(axis.text.x=element_text(size=rel(0.8)), legend.position=c(0.85,0.85))
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydroxSeason_.jpeg", sep=""), width=5, height=4, dpi=500);

#
## TIN [45]  #### remove season ####
    i=45
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WChem2[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(0.01,NA), breaks=c(0.1,1,2,5,10,20), trans=log2_trans()) ;
h2 <- h1 + geom_hline(yintercept=10, lty=2, col="black") + 
    ylab(paste(" TIN concentration (mg N / L)"));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);
#
## TN [43]  #### remove season ####
    i=43
h0 <- ggplot(data=WChem2, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WChem2[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WChem2[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(0.05,NA), breaks=c(0.1,1,2,5,10,25), trans=log2_trans()) ;
h2 <- h1 + geom_hline(yintercept=10, lty=2, col="black") + 
    ylab(paste(" Total N concentration (mg N / L)"));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WChem2[i]),"xHydro_.jpeg", sep=""), width=5, height=4, dpi=500);
#
## Water Temp (52) x Water Depth (51) x HydroCONN ######
v0 <- ggplot(data=WChem2, aes(x=Depth.cm, y=Temp, col=HydroConn)) + theme_bw() + 
        scale_colour_manual(values=rainbow(3)) + labs(col="Hydrologic\n Connectivity");
v1 <- v0 + geom_point(na.rm=T) + geom_vline(xintercept=20,lty=2, col="black") +
        geom_hline(yintercept=27, lty=2, col="black");
# 
## DO conc. (56) x Water Depth (51) x HydroCONN ######
v0 <- ggplot(data=WChem2, aes(x=Depth.cm, y=DO.con, col=HydroConn)) + theme_bw() + 
        scale_colour_manual(values=rainbow(3)) + labs(col="Hydrologic\n Connectivity");
v1 <- v0 + geom_point(na.rm=T) + geom_vline(xintercept=20,lty=2, col="black") +
        geom_hline(yintercept=3, lty=2, col="black");
v1
#

### Building / Extracting the KMS sites...  ######
## Need to trim Sites to KMS...??  #########
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
###
### Working w/ WatZ9 (water depths) #####
WatZ9
names(WatZ9)
WatZ9 <- WatZ9[,c(1:8)]
levels(WatZ9$SiteName)
# add HydroConn to WatZ9   ####
WatZ9.2 <- WatZ9
dat.hyd <- as.character(rep("",nrow(WatZ9.2)))
    dat.hyd[WatZ9.2[,"Year"]==2011] <- "Connected"
    dat.hyd[WatZ9.2[,"Year"]==2012 & WatZ9.2[,"DOY"] <= 166] <- "Connected"
    dat.hyd[WatZ9.2[,"Year"]==2012 & WatZ9.2[,"DOY"] > 166] <- "Isolated"
    dat.hyd[WatZ9.2[,"Year"]==2012 & WatZ9.2[,"DOY"] > 310] <- "Connected"
    dat.hyd[WatZ9.2[,"Year"]==2013 & WatZ9.2[,"DOY"] <= 133] <- "Connected"
    dat.hyd[WatZ9.2[,"Year"]==2013 & WatZ9.2[,"DOY"] > 133] <- "Isolated"
    dat.hyd[WatZ9.2[,"Year"]==2013 & WatZ9.2[,"DOY"] > 321] <- "Connected"
    levels(as.factor(dat.hyd))
    as.data.frame(dat.hyd)
WatZ9.2hydro <- data.frame(WatZ9.2, HydroConn = dat.hyd)  
WatZ9.2 <- WatZ9.2hydro ## Looks Good !!
WaterZ <- WatZ9.2
### plot Water Depth (8)  @WaterZ ####
    i=8
h0 <- ggplot(data=WaterZ, aes(x=HydroConn)) + 
        theme_bw() + scale_colour_manual(values=rainbow(3)) +
        xlab(NULL) + labs(col="Season")
h1 <- h0 + geom_boxplot(aes(y=WaterZ[,i]), na.rm=T, width=0.8, position=position_dodge(0.72)) +
        geom_jitter(aes(y=WaterZ[,i]),position=position_jitter(width=0.15), na.rm=T) +
            scale_y_continuous(limits=c(0,125), breaks=seq(0,130,25)) ;
h2 <- h1 + geom_hline(yintercept=20, lty=2, col="black") + 
    ylab(paste(" Water Depth (cm)"));
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n")
h3
    ggsave(file=paste("WS_",names(WaterZ[i]),"xHydro2_.jpeg", sep=""), width=5, height=4, dpi=500);
######  data == OWSAV729  ######  151010  #######
Data9 <- OWSAV729
names(Data9)
with(Data9, table(SiteSAV, Year))
with(Data9, table(SiteNameX, Year))
## Better to use WATSAV9 [full dataset]
#### think about this and make a plan - then do it...

####### data == WatSAV9  ######
WATSAV <- WatSAV9
WatSAV9.nam <- as.data.frame(names(WatSAV9))
#### [#1] T.ALK [17] ####
    i=17
## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
        geom_hline(yintercept=0, lty=2, col="black") +
        ylab(paste("Total Alkalinity (mg/L)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=0, lty=2, col="black") +
        ylab(paste("Total Alkalinity (mg/L)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA));
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + geom_hline(yintercept=0, lty=2, col="black") ;
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("Total Alkalinity (mg/L)"));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
###
    ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL);
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + 
    scale_y_continuous(limits=c(00,NA)) + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=0, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("Total Alkalinity (mg/L)"));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d.jpeg", sep=""), width=5, height=4, dpi=500);
###
#### [#2] water Depth (Depth.cm) [62] ####
    i=62
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
        geom_hline(yintercept=20, lty=2, col="black") +
        ylab(paste("Water Depth (cm)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=20, lty=2, col="black") +
        ylab(paste("Water Depth (cm)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA));
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + geom_hline(yintercept=20, lty=2, col="black") ;
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("Water Depth (cm)"));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA), breaks=seq(0,130, 30)) + 
    geom_jitter(aes(y=WATSAV[,i], color=DOY), na.rm=T, 
                position = position_jitter(width=0.15)) + 
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") +
    geom_hline(yintercept=20, lty=2, col="black") +
    ylab(paste("Water Depth (cm)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d2.jpeg", sep=""), width=5.5, height=4, dpi=500);
### 

#### [#3] Water Temp (Temp) [63] ####
    i=63
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
        geom_hline(yintercept=27, lty=2, col="black") +
        ylab(paste("Water Temperatre (C)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=27, lty=2, col="black") +
        ylab(paste("Water Temperatre (C)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA));
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + geom_hline(yintercept=27, lty=2, col="black") ;
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("Water Temperatre (C)"));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i], color=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient(low="grey",high = "darkred", name = "Day of\n  Year") + 
    geom_hline(yintercept=27, lty=2, col="black") +
        ylab(paste("Water Temperatre (C)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d2.jpeg", sep=""), width=5, height=4, dpi=500);
   ###   scale_colour_gradient2(low="darkgreen", high="darkred", midpoint=210,name = "Day of\n  Year") +
#  scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") +
#  scale_colour_gradientn(colours=rainbow(9), name = "Day of\n  Year") + 

#### [#4] pH [64] ####
    i=64
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(5,NA)) + 
        geom_hline(yintercept=6.5, lty=2, col="darkblue") +
    geom_hline(yintercept=9, lty=2, col="red") +
        ylab(paste("Water pH"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(5.5,NA)) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=6.5, lty=2, col="darkblue") +
    geom_hline(yintercept=9, lty=2, col="red") +
        ylab(paste("Water pH"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(5,NA));
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + 
            geom_hline(yintercept=6.5, lty=2, col="darkblue") +
            geom_hline(yintercept=9, lty=2, col="red");
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("Water pH"));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + scale_y_continuous(limits=c(5.5,NA)) + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
    geom_hline(yintercept=6.5, lty=2, col="darkblue") +
    geom_hline(yintercept=9, lty=2, col="red") ;
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("Water pH"));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d.jpeg", sep=""), width=5, height=4, dpi=500);
###
#### [#5] TVS:TSS Ratio [22] #####
    i=22
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA), label=percent) + 
        geom_hline(yintercept=0, lty=2, col="black") +
        ylab(paste("TVS:TSS ratio"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA), label=percent) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=0, lty=2, col="black") +
        ylab(paste("TVS:TSS ratio"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA), label=percent);
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + 
            geom_hline(yintercept=0, lty=2, col="black");
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("TVS:TSS ratio"));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
###
#### [#6] ChlA [18] #####
    i=18
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
        geom_hline(yintercept=0, lty=2, col="black") +
        ylab(paste("Chlorophyll-a  (", OW7.name2[(i-1),3],")",sep=""));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=0, lty=2, col="black") +
        ylab(paste("Chlorophyll-a  (", OW7.name2[(i-1),3],")",sep=""));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() +
        scale_colour_manual(values=rainbow(3)) + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) ;
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + 
            geom_hline(yintercept=0, lty=2, col="black");
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") +         
        ylab(paste("Chlorophyll-a  (", OW7.name2[(i-1),3],")",sep=""));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
###
 ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL);
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + 
    scale_y_continuous(limits=c(NA,NA), breaks=c(0,10,50,100,200)) + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + coord_trans(y="sqrt") + 
        geom_hline(yintercept=0, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + 
    ylab(paste("Chlorophyll-a  (", OW7.name2[(i-1),3],")",sep=""));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d.jpeg", sep=""), width=5, height=4, dpi=500);
# 
 ## With Jitter + fade x DOY  #2
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL);
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + 
    scale_y_continuous(limits=c(0,NA), breaks=seq(0,max(WATSAV[,i], na.rm=T)*1.05,50)) + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + coord_trans(y="sqrt") + 
        geom_hline(yintercept=0, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + 
    ylab(paste("Chlorophyll-a  (", OW7.name2[(i-1),3],")",sep=""));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d2.jpeg", sep=""), width=5, height=4, dpi=500);
### coord_trans(y="sqrt") + 
#### [#7] DO.c [67] #####
    i=67
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
        geom_hline(yintercept=3, lty=2, col="darkblue") +
        ylab(paste("DO conc (mg/L)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=3, lty=2, col="darkblue") +
        ylab(paste("DO conc (mg/L)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA));
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + 
            geom_hline(yintercept=3, lty=2, col="darkblue");
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("DO conc (mg/L)"));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL);
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
    geom_hline(yintercept=3, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("DO conc (mg/L)"));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d.jpeg", sep=""), width=5, height=4, dpi=500);
###
#### [#8] DO.p [68] #####
    i=68
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
        geom_hline(yintercept=100, lty=2, col="darkblue") +
        ylab(paste("DO (% sat)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=100, lty=2, col="darkblue") +
        ylab(paste("DO (% sat)"));
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(0,NA));
h2 <- h1 + geom_dotplot(aes(y=WATSAV[,i], shape=factor(Year)), binaxis="y", stackdir="center", 
            na.rm=TRUE, dotsize=0.5, fill="darkgreen") + 
        geom_hline(yintercept=100, lty=2, col="darkblue");
h3 <- h2 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("DO (% sat)"));
h3
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_c.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL);
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + scale_y_continuous(limits=c(0,NA)) + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=100, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("DO (% sat)"));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d.jpeg", sep=""), width=5, height=4, dpi=500);
###
#### [#9] TDS  [31] #####
    i=31
    ## Without Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(50,NA), breaks=c(100,500,1200,5000,12000,25000)) + 
    coord_trans(y="log10") +  geom_hline(yintercept=1200, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n")  + ylab(paste("TDS (mg/L)"));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_a.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With Jitter
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL)
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) +
        scale_y_continuous(limits=c(50,NA), breaks=c(100,500,1200,5000,12000,25000)) + 
    coord_trans(y="log10") + 
    geom_jitter(aes(y=WATSAV[,i]), na.rm=T, position = position_jitter(width=0.15)) +
        geom_hline(yintercept=1200, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("TDS (mg/L)"))
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_b.jpeg", sep=""), width=5, height=4, dpi=500);
    ## With DotPlot
##        ### No Good W/ DOTPLOTS (log-axes...) ##

    ## With Jitter + fade x DOY
h0 <- ggplot(data=WATSAV, aes(x=HydroConn)) + theme_bw() + xlab(NULL);
h1 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + 
    scale_y_continuous(limits=c(50,NA), breaks=c(100,500,1200,5000,12000,25000)) + 
    coord_trans(y="log10") + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=1200, lty=2, col="darkblue");
h2 <- h1 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("TDS (mg/L)"));
h2
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d.jpeg", sep=""), width=5, height=4, dpi=500);
###
h4 <- h0 + geom_boxplot(aes(y=WATSAV[,i]), na.rm=T, width=0.8) + 
    scale_y_continuous(limits=c(0,NA), breaks=c(500,1000,5000,10000,15000), labels=comma) + 
    geom_jitter(aes(y=WATSAV[,i], colour=DOY), na.rm=T, position = position_jitter(width=0.15)) +
    scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=1200, lty=2, col="darkblue");
h5 <- h4 + ggtitle("Effect of Hydrologic Isolation\n") + ylab(paste("TDS (mg/L)"));
h5
    ggsave(file=paste("WSav_",names(WATSAV[i]),"xHydro_d2.jpeg", sep=""), width=5, height=4, dpi=500);
#### ### ####

####  Some Responses (WATSAV)  151011  ######
data = WATSAV
response <- c(18,20, 23,22,25,44,53,54,55,51,52,78,79,86,88,96,97,98)
k=20
for (k in unique(response)) {
    z0 <- ggplot(data=WATSAV, aes(x=HydroConn, y=WATSAV[,k])) + theme_bw() + xlab(NULL);
    z1 <- z0 + 
        geom_boxplot(na.rm=T, width=0.8) + scale_y_continuous(limits=c(0,NA), oob=squish) +
        geom_jitter(aes(colour=DOY), na.rm=T, position=position_jitter(width=0.15)) +
        coord_trans(y="sqrt") +
        scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=0, lty=2, col="darkblue");
    z2 <- z1 + ggtitle(paste("Effect of Hydrologic Isolation: ", names(WATSAV[k])," \n", sep="")) +
        ylab(paste(OW7.name2[(k-1),2]," (", OW7.name2[(k-1),3],")",sep=""));
    z2

ggsave(file=paste("WSRespnse_",names(WATSAV[k]),"xHydroXX_d2.jpeg", sep=""), width=5, height=4, dpi=500);
}   ;
############## Bring in Invert Metrics to WATSAV ######
data1 = WATSAV
data2 = Inv.Sites
WATSAVINV <- merge(WATSAV, Inv.Sites, by=c("MLID", "Year", "Month"), all.x=TRUE, sort=T)
    WATSAVINV.nam <- as.data.frame(names(WATSAVINV))
WATSAVINV.nam$VarNAME <- NA
    WATSAVINV.nam$VarNAME[112] <- "Chironminae"
Inv.codes3 <- Inv.codes2[!duplicated(Inv.codes2[c(1,2)] ), ]
invert.resp <- c(112,125,139:143)
### ADDING INVERT Responses PLots ######
k=112
for (k in unique(invert.resp)) {
    z0 <- ggplot(data=WATSAVINV, aes(x=HydroConn, y=WATSAVINV[,k])) + theme_bw() + xlab(NULL);
    z1 <- z0 + geom_boxplot(na.rm=T, width=0.8) + scale_y_continuous(limits=c(0,NA), oob=squish) +
        geom_jitter(aes(colour=DOY.x), na.rm=T, position=position_jitter(width=0.15)) +
        # coord_trans(y="sqrt") +
        scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=0, lty=2, col="darkblue");
    z2 <- z1 + ggtitle(paste("Effect of Hydrologic Isolation: ", names(WATSAVINV[k])," \n", sep="")) +
        ylab(paste(Inv.codes3[Inv.codes3$TaxaID==names(WATSAVINV[k]),3],sep=""));
    z2

ggsave(file=paste("WSInvRespnse_",names(WATSAVINV[k]),"xHydroEfx_d3.jpeg", sep=""), 
       width=5, height=4, dpi=500);
}   ;
# " (", Inv.codes3[Inv.codes3$TaxaID==names(WATSAVINV[k]),5], ")"

#### Fixing Chironomini Figure ... #####
k=112
z0 <- ggplot(data=WATSAVINV, aes(x=HydroConn, y=WATSAVINV[,k])) + theme_bw() + xlab(NULL);
    z1 <- z0 + geom_boxplot(na.rm=T, width=0.8) + 
        scale_y_continuous(limits=c(0,1800), oob=squish, breaks=c(0,100,300,500,1000,2000)) +
        geom_jitter(aes(colour=DOY.x), na.rm=T, position=position_jitter(width=0.15)) +
        coord_trans(y="sqrt") +
        scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=0, lty=2, col="darkblue");
    z2 <- z1 + ggtitle(paste("Effect of Hydrologic Isolation: ", names(WATSAVINV[k])," \n", sep="")) +
        ylab(paste("Counts of ",Inv.codes3[Inv.codes3$TaxaID==names(WATSAVINV[k]),3],"(", 
                   Inv.codes3[Inv.codes3$TaxaID==names(WATSAVINV[k]),5], ")",sep=""));
    z2

ggsave(file=paste("WSInvRespnse_",names(WATSAVINV[k]),"xHydroEfx_d2x.jpeg", sep=""), 
       width=5, height=4, dpi=500);
#
#### Fixing Hyallela Figure ... #####
k=125
z0 <- ggplot(data=WATSAVINV, aes(x=HydroConn, y=WATSAVINV[,k])) + theme_bw() + xlab(NULL);
    z1 <- z0 + geom_boxplot(na.rm=T, width=0.8) + 
        scale_y_continuous(limits=c(0,350), oob=squish, breaks=c(0,50,100,200,300,1000,2000)) +
        geom_jitter(aes(colour=DOY.x), na.rm=T, position=position_jitter(width=0.15)) +
        coord_trans(y="sqrt") +
        scale_colour_gradient2(high = "darkred",name = "Day of\n  Year") + 
        geom_hline(yintercept=0, lty=2, col="darkblue");
    z2 <- z1 + ggtitle(paste("Effect of Hydrologic Isolation: ", names(WATSAVINV[k])," \n", sep="")) +
        ylab(paste("Counts of ",Inv.codes3[Inv.codes3$TaxaID==names(WATSAVINV[k]),3],"(", 
                   Inv.codes3[Inv.codes3$TaxaID==names(WATSAVINV[k]),5], ")",sep=""));
    z2

ggsave(file=paste("WSInvRespnse_",names(WATSAVINV[k]),"xHydroEfx_d2x.jpeg", sep=""), 
       width=5, height=4, dpi=500);















