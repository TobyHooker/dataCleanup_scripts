# Willard Spur -- OW summary data analysis      14 Sept 2015  ##############

OW4.dat <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur.2015/OW4.dat2_150914.csv")
OW4.nm <- data.frame(names(OW4.dat))
OW4.dat3 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur.2015/OW4.dat3_150914.csv")

OW4.dat <- OW4.dat3
OW4.dat$Year.FX <- as.factor(OW4.dat$Year)

levels(OW4.dat$SiteName)
OW4.dat$SiteName == "WS-6"
OW4.dat$SiteName == "WS-3"

OW4.dat[OW4.dat$SiteName == "WS-6",]

for (i in 1:length(names(OW4.dat))) {
    a <- class(OW4.dat[,i])
 print(a)   }  # get classes for all Vars...

#p1 <- p0 + geom_point(aes(y=CHLA)) + geom_smooth(aes(y=CHLA), se=F, method=loess)
#p2 <- p0 + geom_point(aes(y=CHLA)) + geom_line(aes(y=CHLA), se=F)

p0 <- ggplot(data=OW4.dat[OW4.dat$SiteName == "WS-6",], aes(x=DOY, col=Year.FX)) + theme_bw()
p3 <- p0 + geom_line(aes(y=CHLA)) + geom_point(aes(y=CHLA), size=3.5) +
    geom_ribbon(aes(y=CHLA, ymin=CHLA*0.9, ymax=CHLA*1.1, linetype=NA, fill=Year.FX), alpha=0.25) +
    labs(col="Sampling Year", fill="Sampling Year")
p4 <- p3 + xlab("Day of year") + ylab(expression(paste("Chlorophyll-a concentration (",mu,"g/L)"))) +
    ggtitle(paste("Willard Spur: ","WS-6", " - ", "CHLA")) + theme(plot.title=element_text(face="bold"))
    
## scan through all VARs:  Start at Col=13 to 55, Add:  61 and 63.

{OW4.nm$Name <- "text"
OW4.nm$Name <- c("text","text","Site Name","Site Type", "Relative Site Order", "Season", "Month of Year",
                 "Year", "Decimal Year", "Decimal Date", "Day of Year", "Date", "Total Alkalinity", 
                 "Chlorophyll-a",
                 "Biochemical Oxygen Demand [5-day]", "Total Suspended Solids", "Total Volatile Solids",
                 "TVS to TSS Ratio", "Turbidity [Lab]",
                 "Dissolved Potassium", "Total Sulfate", "Dissolved Calcium", "Dissolved Magnesium", 
                 "Hardness [Ca & Mg]", "Dissolved Sodium", "Total Chloride", "Total Dissolved Solutes",
                 "Specific Conductivity (25C) [Lab]", "Dissolved Cadmium", "Dissolved Lead",
                 "Dissolved Chromium", "Dissolved Copper", "Dissolved Arsenic", "Dissolved Manganese",
                 "Dissolved Nickel", "Dissolved Iron", "Dissolved Aluminum", "Dissolved Boron", 
                 "Dissolved Barium", "Total Phosphorus", "Dissolved Phosphorus", "Total Kjeldahl N", 
                 "Total Ammonium", "Total Nitrate", "Dissolved Nitrate", "Dissolved Nitrogen", 
                 "Total Nitrogen", "Total N : Total P ratio", "Water Depth", "Water Temperature",
                 "Water Column pH", "Specific Conductivity (25C)", "Salinity",
                 "Dissolved Oxygen Concentration", "Dissolved Oxygen [% of Saturation]",
                 "Ecological.phase", "Ecophase2", "Clarity", "EPhase3", "EPhase4", "EPhase4.num",
                 "Hydrologic Connectivity", "Hydro Conn.Num", "YearFx")
write.csv(OW4.nm,"OW4.nm.csv", row.names=F)     }

OW4.2x.nm <- OW4.nm
    names(OW4.2x.nm2)
    names(OW4.nm)
OW4.2x.nm2 <- rbind(OW4.2x.nm, OW4.nm[3,])
OW4.2x.nm2 <- OW4.2x.nm2[c(1:3,65,4:(length(OW4.2x.nm2$names.OW4.dat.)-1)),]
names(OW4.2x.nm2)[names(OW4.2x.nm2) == "names.OW4.dat."] <- "Var.Name"
levels(OW4.2x.nm2$Var.Name) <- c(levels(OW4.2x.nm2$Var.Name), "SiteNameX")
OW4.2x.nm2[4,1] <- "SiteNameX"

###  Fix Labels for Variables ### [OW4.dat]

#####################################
##  Building the KMS x time PLOTS .....
i = 63
names(OW4.dat[i])
col=Year.FX
### WS-6 ##
for (i in 13:55) {
data_i <- OW4.dat[OW4.dat$SiteName == "WS-6",]
    
p0 <- ggplot(data=data_i, aes(x=data_i$DOY, col=data_i$Year.FX)) + theme_bw()
p3 <- p0 + geom_line(aes(y=data_i[,i])) + geom_point(aes(y=data_i[,i])) +
    geom_ribbon(aes(y=data_i[,i], ymin=data_i[,i]*0.9, ymax=data_i[,i]*1.1, linetype=NA, 
                    fill=Year.FX), alpha=0.25) + labs(col="Sampling Year", fill="Sampling Year");
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")")) +
    ggtitle(paste("Willard Spur: ","WS-6", " - ", OW4.nm[i,2])) + 
    theme(plot.title=element_text(face="bold"))
p4
    ggsave(file=paste("WS15-OW_",names(data_i[i]),"_WS-6.jpeg", sep=""), width=6, height=4, dpi=500);
}

### WS-8 ##
i = 61
i = 63
#
for (i in 13:55) {
data_i <- OW4.dat[OW4.dat$SiteName == "WS-8",]
    
p0 <- ggplot(data=data_i, aes(x=data_i$DOY, col=data_i$Year.FX)) + theme_bw()

p3 <- p0 + geom_line(aes(y=data_i[,i])) + geom_point(aes(y=data_i[,i])) +
    geom_ribbon(aes(y=data_i[,i], ymin=data_i[,i]*0.9, ymax=data_i[,i]*1.1, linetype=NA, 
                    fill=Year.FX), alpha=0.25) + labs(col="Sampling Year", fill="Sampling Year");

p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")")) +
    ggtitle(paste("Willard Spur: ","WS-8", " - ", OW4.nm[i,2])) + 
    theme(plot.title=element_text(face="bold"))
p4
    ggsave(file=paste("WS15-OW_",names(data_i[i]),"_WS-8.jpeg", sep=""), width=6, height=4, dpi=500);

}

### WS-3 ##
i = 61
i = 63
#
for (i in 13:55) {
data_i <- OW4.dat[OW4.dat$SiteName == "WS-3",]
    
p0 <- ggplot(data=data_i, aes(x=data_i$DOY, col=data_i$Year.FX)) + theme_bw()

p3 <- p0 + geom_line(aes(y=data_i[,i])) + geom_point(aes(y=data_i[,i])) +
    geom_ribbon(aes(y=data_i[,i], ymin=data_i[,i]*0.9, ymax=data_i[,i]*1.1, linetype=NA, 
                    fill=Year.FX), alpha=0.25) + labs(col="Sampling Year", fill="Sampling Year");

p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")")) +
    ggtitle(paste("Willard Spur: ","WS-3", " - ", OW4.nm[i,2])) + 
    theme(plot.title=element_text(face="bold"))
p4
    ggsave(file=paste("WS15-OW_",names(data_i[i]),"_WS-3.jpeg", sep=""), width=6, height=4, dpi=500);

}

### OUTFALL-CNFL  ##
i = 61
i = 63
#
for (i in 13:55) {
data_i <- OW4.dat[OW4.dat$SiteName == "OUTFALL-CNFL",]
    
p0 <- ggplot(data=data_i, aes(x=data_i$DOY, col=data_i$Year.FX)) + theme_bw()

p3 <- p0 + geom_line(aes(y=data_i[,i])) + geom_point(aes(y=data_i[,i])) +
    geom_ribbon(aes(y=data_i[,i], ymin=data_i[,i]*0.9, ymax=data_i[,i]*1.1, linetype=NA, 
                    fill=Year.FX), alpha=0.25) + labs(col="Sampling Year", fill="Sampling Year");

p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")")) +
    ggtitle(paste("Willard Spur: ","OUTFALL-CNFL", " - ", OW4.nm[i,2])) + 
    theme(plot.title=element_text(face="bold"))
p4
    ggsave(file=paste("WS15-OW_",names(data_i[i]),"_OUTFALL-CNFL.jpeg", sep=""), 
           width=6, height=4, dpi=500);

}

### OUTFALL-CNFL  ##
i = 61
i = 63
#
for (i in 13:55) {
data_i <- OW4.dat[OW4.dat$SiteName == "OUT-WB-TAILRACE",]
    
p0 <- ggplot(data=data_i, aes(x=data_i$DOY, col=data_i$Year.FX)) + theme_bw()

p3 <- p0 + geom_line(aes(y=data_i[,i])) + geom_point(aes(y=data_i[,i])) +
    geom_ribbon(aes(y=data_i[,i], ymin=data_i[,i]*0.9, ymax=data_i[,i]*1.1, linetype=NA, 
                    fill=Year.FX), alpha=0.25) + labs(col="Sampling Year", fill="Sampling Year");

p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")")) +
    ggtitle(paste("Willard Spur: ","OUT-WB-TAILRACE", " - ", OW4.nm[i,2])) + 
    theme(plot.title=element_text(face="bold"))
p4
    ggsave(file=paste("WS15-OW_",names(data_i[i]),"_OUT-WB-TAILRACE.jpeg", sep=""), 
           width=6, height=4, dpi=500);

}
#####################################################
###### Faceting by Sites:
# WS-6  # WS-8  # WS-3  # OUTFALL-CNFL
data_OW4_x <- subset(OW4.dat, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                                SiteName == "WS-6"   ))

## Expand set of Key Monitoring Sites to adjacent sites:
# Add: 
##  unique(levels(OW4.dat$SiteName))
data_OW4_2 <- subset(OW4.dat, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                SiteName == "WS-6" | SiteName == "OUT-WB-TAILRACE" | SiteName == "WS-2" | 
                SiteName == "OUT-HC-WMA-DD"  ))
data_OW4_2 <- droplevels(data_OW4_2)
###########################################
i = 61
i = 63
i = 51
# try it wide....
for (i in 13:55) {
p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")")) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3x.jpeg", sep=""), 
           width=10, height=4, dpi=500);
}

##############  Re-doing specific plots...
### Water column pH  [redo]
i = 51

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

### Water column pH  [redo  #2]
i = 51

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) +
    geom_hline(yintercept=9.0, lty=2, col="black");
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites5.jpeg", sep=""), 
           width=10, height=4, dpi=500);

### Dissolved Ca  [redo]   150922
i = 22

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

### Dissolved T.Alk (as HCO3)  [redo]   150922
i = 13

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p1 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p2 <- p1 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")"));
p3 <- p2 +     labs(col="Sampling Year");
p4 <- p3 + ggtitle(bquote(paste("Willard Spur: ", .(OW4.nm[i,2]), " as ", HCO[3])));
p4

    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

### TVS:TSS ratio [ redo 151014 ]
i = 16

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

### Water Temp [ redo 151014 ]
i = 50

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2]," (",OW4.nm[i,3],")")) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3x2.jpeg", sep=""), 
           width=10, height=4, dpi=500);



#################
#theme(plot.title=element_text(face="bold"))
#geom_ribbon(aes(y=data_OW4_x[,i], ymin=data_OW4_x[,i]*0.9, ymax=data_OW4_x[,i]*1.1, linetype=NA, 
#fill=Year.FX), alpha=0.25) 
#labs(col="Sampling Year", fill="Sampling Year")
    data_OW4_x2 <- data_OW4_x
    data_OW4_x2 <- data_OW4_x[with(data_OW4_x, order(SiteName, Year, DOY)), ]
    data_OW4_x <- data_OW4_x2
names(data_OW4_x2)

### DO.c [mg/L]  [redo]   150925 [fixed 150930]     ###################
i = 54

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year") +
    scale_y_continuous(limits=c(0,NA)) + geom_hline(yintercept=3, lty=2, col="black");
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3xx.jpeg", sep=""), 
           width=10, height=4, dpi=500);

### DO.p [%.sat]  [redo]   150925      ###################
i = 55

p0 <- ggplot(data=data_OW4_x, aes(x=data_OW4_x$DOY, col=data_OW4_x$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)
p3 <- p0 + geom_line(aes(y=data_OW4_x[,i])) + geom_point(aes(y=data_OW4_x[,i])) ;
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year") +
    scale_y_continuous(limits=c(50,NA), breaks=seq(from=50, to=300, by=50)) + 
    geom_hline(yintercept=100, lty=2, col="black");
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_x[i]),"_sites3x.jpeg", sep=""), 
           width=10, height=4, dpi=500);

+ geom_hline(yintercept=5, lty=2, col="black")
breaks=seq(from=0, to=100, by=20)

################################
########     150918
### More Re-dos:   TVS:TSS w/ expanded KMSs
data_OW4_2 <- subset(OW4.dat, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                SiteName == "WS-6" | SiteName == "OUT-WB-TAILRACE" | SiteName == "WS-2" | 
                SiteName == "OUT-HC-WMA-DD"  ))
data_OW4_2 <- droplevels(data_OW4_2)
# data = data_OW4_2
#  data_OW4_2 <- data_OW4_2[with(data_OW4_2, order(SiteORDER, Year, DOY)),]
# unique(levels(data_OW4_2$SiteName))

{KMS.name <- list(
    "OUT-HC-WMA-DD" = "WS-6", 
    "WS-6" = "WS-6", 
    "OUT-WB-TAILRACE" = "OUTFALL-CNFL", 
    "OUTFALL-CNFL" = "OUTFALL-CNFL", 
    "WS-2" = "WS-3", 
    "WS-3" = "WS-3", 
    "WS-8" = "WS-8"
    )   }  # list of names...

{data_OW4_2$SiteName2 <- data_OW4_2$SiteName
data_OW4_2 <- data_OW4_2[,c(1:4, 65, 5:(length(names(data_OW4_2))-1))] # reordered vars
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
data_OW4_2X <- data_OW4_2X[,c(1:3, 66,4,6:(length(names(data_OW4_2X))-1))] # reorder
data_2x.nm <- as.data.frame(names(data_OW4_2X))
OW4.2x.nm <- OW4.nm
    names(OW4.2x.nm2)
    names(OW4.nm)
OW4.2x.nm2 <- rbind(OW4.2x.nm, OW4.nm[3,])
OW4.2x.nm2 <- OW4.2x.nm2[c(1:3,65,4:(length(OW4.2x.nm2$names.OW4.dat.)-1)),]
names(OW4.2x.nm2)[names(OW4.2x.nm2) == "names.OW4.dat."] <- "Var.Name"
levels(OW4.2x.nm2$Var.Name) <- c(levels(OW4.2x.nm2$Var.Name), "SiteNameX")
OW4.2x.nm2[4,1] <- "SiteNameX"
}## all fixed !!

# data = data_OW4_2X
data_OW4_2X <- data_OW4_2X[with(data_OW4_2X, order(SiteNameX, Year, DOY)),]

i = 18
p0 <- ggplot(data=data_OW4_2X, aes(x=data_OW4_2X$DOY, col=data_OW4_2X$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteNameX, ncol=4)
p3 <- p0 + geom_jitter(aes(y=data_OW4_2X[,i]), na.rm=T);
p4 <- p3 + xlab("Day of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_2X[i]),"_sites4.jpeg", sep=""), 
           width=10, height=4, dpi=500);

# try a boxplot v Month
{i = 19
p0 <- ggplot(data=data_OW4_2X, aes(x=data_OW4_2X$Month, col=data_OW4_2X$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteNameX, ncol=4)
p3 <- p0 + 
    geom_jitter(aes(x=factor(data_OW4_2X$Month),y=data_OW4_2X[,i]), na.rm=T);
p4 <- p3 + xlab("Month of year") + ylab(paste(OW4.nm[i,2])) +
    ggtitle(paste("Willard Spur: ", OW4.nm[i,2])) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_OW4_2X[i]),"_sites4.jpeg", sep=""), 
           width=10, height=4, dpi=500);

#geom_boxplot(aes(x=factor(data_OW4_2X$Month),y=data_OW4_2X[,i]), na.rm=T) + 
}
#############
# try a boxplot v Season  ## This one is better  ### 150918
## Fixing this 150922
i = 19
p0 <- ggplot(data=data_OW4_2X, aes(col=data_OW4_2X$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteNameX, ncol=4)
p3 <- p0 + geom_boxplot(aes(x=data_OW4_2X$Season,y=data_OW4_2X[,i]), na.rm=T, outlier.shape=NA) + 
    geom_jitter(aes(x=data_OW4_2X$Season,y=data_OW4_2X[,i]), na.rm=T);
p4 <- p3 + ylab(paste(OW4.2x.nm2[i,2])) + xlab(NULL) +
    scale_y_continuous(labels=percent) +
    scale_x_discrete(limits=c("Spring", "Summer", "Autumn")) +
    ggtitle(paste("Willard Spur: ", OW4.2x.nm2[i,2])) + labs(col="Sampling Year")
p4

ggsave(file=paste("WS15-OW_",names(data_OW4_2X[i]),"_KEysites5xx2.jpeg", sep=""), 
           width=10, height=4, dpi=500);

## 150916
############################################################
##
##  W A T E R  D E P T H (all data)
{### Water Depth (cm) from Suzies Data
# WaterDepth.dat1 <- read.csv("C:/Users/tobyhooker/Dropbox/Willard Spur/WSpur.2015/WaterDepth.dat1.csv")
# data = WaterDepth.dat1
    names(WaterDepth.dat1)
    WaterDepth.dat1$Year.FX <- as.factor(WaterDepth.dat1$Year)
data_all <- WaterDepth.dat1
data_i2 <- subset(data_i, (SiteName == "OUTFALL-CNFL" | SiteName == "WS-3" | SiteName == "WS-8" | 
                                SiteName == "WS-6"   ));
data_i <- data_i2
data_all2 <- data_all[with(data_all, order(SiteName, Year, DOY)), ]
data_i2 <- data_i[with(data_i, order(SiteName, Year, DOY)), ]
data_i <- data_i2

    names(data_i[7])
    names(data_i)
} }
################
i = 7
## data for Key Monitoring Sites...    
p0 <- ggplot(data=data_i, aes(x=data_i$DOY, col=data_i$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=4)

p3 <- p0 + geom_line(aes(y=data_i[,i])) + geom_point(aes(y=data_i[,i]))  + 
    geom_hline(yintercept=0,lty=2, col="black");

p4 <- p3 + xlab("Day of year") + ylab("Water Depth (cm)") +
    ggtitle(paste("Willard Spur: Water Depth (cm)")) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_",names(data_i[i]),"_sites4_Key2.jpeg", sep=""), 
           width=10, height=4, dpi=500);

## bigger one for all siteS:  [need to pull back all data...]
#data = data_all2
##  data_i <- data_all2
p0 <- ggplot(data=data_all2, aes(x=data_all2$DOY, col=data_all2$Year.FX)) + 
    theme_bw() + facet_wrap(~SiteName, ncol=5)
p3 <- p0 + geom_line(aes(y=data_all2[,i])) + geom_point(aes(y=data_all2[,i])) + 
    geom_hline(yintercept=0,lty=2, col="black");
p4 <- p3 + xlab("Day of year") + ylab("Water Depth (cm)") +
    ggtitle(paste("Willard Spur: Water Depth (cm)")) + labs(col="Sampling Year")
p4
ggsave(file=paste("WS15-OW_",names(data_all2[i]),"_sites4_all2.jpeg", sep=""), 
           width=10, height=6, dpi=500);
}  }
#############################  
### SALINITY PLOT  (TDS [y] vs. EC25 [x])
# x-var == EC25.f  [52]
# y-var == TDS [27]
v0 <- ggplot(data = OW4.dat, aes(x=EC25.f, y=TDS )) + theme_bw() + ggtitle("Total Dissolved Solutes vs. Specific Conductivity, \nWillard Spur (2011-2013)")
v1 <- v0 + geom_point(na.rm=T) + xlab(paste(OW4.nm[52,2], " (",OW4.nm[52,3],")")) + 
    ylab(paste(OW4.nm[27,2], " (", OW4.nm[27,3],")"));
v2 <- v1 + scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma);
v3 <- v2 + geom_smooth(col="black", size=0.6, method=lm, se=F, na.rm=T);
    mod <- lm(TDS ~ EC25.f -1, data=OW4.dat);
    b1 <- mod$coeff[1]  #  slope
    r2 <- summary(mod)$adj.r.squared;
v4 <- v3 + annotate("text", x=1000, y=13500, hjust=0, vjust=0, label="TDS = b1 * (Spec. Cond.)");
v5 <- v4 + annotate("text", x=1200, y=12000, hjust=0, vjust=0, label=paste("b1 = ", format(b1, nsmall=2)));
v6 <- v5 + annotate("text", x=1200, y=10500, hjust=0, vjust=0, 
                    label=paste("r^2 == ", format(r2, digits=3)),parse=T)
ggsave(file=paste("WSpur_TDS-x-EC25_fig1.jpeg", sep=""), width=5, height=4, dpi=500);

######################
# EC25 [52] vs. Water Depth [49]
x0 <- ggplot(data=OW4.dat, aes(x=Depth.cm, y=EC25.f )) + theme_bw();
x1 <- x0 + geom_point(na.rm=T) + stat_smooth(method="loess", na.rm=T) + scale_y_continuous(labels=comma, 
                                limits=c(0,max(OW4.dat$EC25.f, na.rm=T)));
x2 <- x1 + ggtitle("Inverse relationship between Specific Conductivity \nand Water Depth in Willard Spur (2011-2013)");
x3 <- x2 + xlab(paste(OW4.nm[49,2], " (",OW4.nm[49,3],")")) + 
    ylab(paste(OW4.nm[52,2], " (", OW4.nm[52,3],")"));
ggsave(file="WSpur_EC-x-WaterZ_fig1.jpeg", width=5, height=4, dpi=500);

##########################
# Water Temp [50] vs. Depth [49]
x0 <- ggplot(data=OW4.dat, aes(x=Depth.cm, y=Temp ), col=Year.FX) + theme_bw();
x1 <- x0 + geom_point(na.rm=T, aes(col=Year.FX)) + 
    scale_y_continuous(limits=c(0,max(OW4.dat$Temp, na.rm=T))) +
    stat_smooth(aes(col=Year.FX),na.rm=T, method="loess",  se=F);

x2 <- x1 + ggtitle("Changes in Water Temperature vs \nWater Depth in Willard Spur (2011-2013)");
x3 <- x2 + xlab(paste(OW4.nm[49,2], " (",OW4.nm[49,3],")")) + 
    ylab(paste(OW4.nm[50,2], " (", OW4.nm[50,3],")"));
ggsave(file="WSpur_Temp-x-WaterZ_fig1.jpeg", width=5, height=4, dpi=500);

##[re-try]
v0 <- ggplot(data=OW4.dat, aes(x=factor(Month), y=Temp, col=Year.FX)) + theme_bw();
v1 <- v0 + geom_boxplot(na.rm=T, outlier.shape=NA) + 
    scale_colour_tableau() +
    labs(col="Sampling Year") + 
    geom_jitter( position=position_jitter(width=0.25), na.rm=T);
v2 <- v1 + ggtitle("Changes in Water Temperature by Month \nin Willard Spur (2011-2013)");
v3 <- v2 + xlab("Calendar Month") + ylab(paste(OW4.nm[50,2], " (", OW4.nm[50,3],")"));
ggsave(file="WSpur_Temp-x-Month_fig2.jpeg", width=5, height=4, dpi=500);

##[re-try]
## Use:  KMS4.dat <- data_OW4_x
v0 <- ggplot(data=KMS4.dat, aes(x=factor(Month), y=Temp, col=Year.FX)) + theme_bw();
v1 <- v0 + geom_boxplot(na.rm=T, outlier.shape=NA) + 
    scale_colour_tableau() +
    labs(col="Sampling Year") + 
    geom_jitter( position=position_jitter(width=0.25), na.rm=T);
v2 <- v1 + ggtitle("Changes in Water Temperature of Key Monitoring Sites\n by Month in Willard Spur (2011-2013)");
v3 <- v2 + xlab("Calendar Month") + ylab(paste(OW4.nm[50,2], " (", OW4.nm[50,3],")"));
ggsave(file="WSpur_Temp-x-Month_fig3.jpeg", width=6, height=4, dpi=500);
## I like the full dataset figure better ("fig2") above.


# Chl-A [14] vs. Depth [49] #3 All data
x0 <- ggplot(data=OW4.dat, aes(x=Depth.cm, y=CHLA ), col=factor(Year)) + 
    theme_bw() + labs(col="Sampling Year");
x1 <- x0 + geom_point(na.rm=T, aes(col=factor(Year))) + 
    scale_y_continuous(limits=c(0,max(OW4.dat$CHLA, na.rm=T))) +
    stat_smooth(aes(col=Year.FX),na.rm=T, method="loess",  se=F);

#x2 <- x1 + ggtitle("Changes in Chlorophyll-A vs \nWater Depth in Willard Spur (2011-2013)");
#x3 <- x2 + xlab(paste(OW4.nm[49,2], " (",OW4.nm[49,3],")")) + 
#    ylab(paste(OW4.nm[14,2], " (", OW4.nm[140,3],")"));
# ggsave(file="WSpur_Temp-x-WaterZ_fig1.jpeg", width=5, height=4, dpi=500);

# Chl-A [14] vs. Temp [49] #3 All data
x0 <- ggplot(data=OW4.dat, aes(x=Temp, y=CHLA ), col=factor(Year)) + 
    theme_bw() + labs(col="Sampling Year");
x1 <- x0 + geom_point(na.rm=T, aes(col=factor(Year))) + 
    scale_y_continuous(limits=c(0,max(OW4.dat$CHLA, na.rm=T)));

    stat_smooth(aes(col=Year.FX),na.rm=T, method="loess",  se=F);

# Chl-A [14] vs. EC25.f [52] #3 All data
x0 <- ggplot(data=OW4.dat, aes(x=EC25.f, y=CHLA ), col=factor(Year)) + 
    theme_bw() + labs(col="Sampling Year");
x1 <- x0 + geom_point(na.rm=T, aes(col=factor(Year))) +
    scale_y_continuous(limits=c(0.1,max(OW4.dat$CHLA, na.rm=T)), trans="sqrt") +
    scale_x_log10(labels=comma) + stat_smooth(aes(col=Year.FX),na.rm=T, method="lm");
x1

## CHLA [14] x Month
v0 <- ggplot(data=OW4.dat, aes(x=factor(Month), y=CHLA, col=Year.FX)) + theme_bw();
v1 <- v0 + geom_boxplot(na.rm=T, outlier.shape=NA) + 
    scale_colour_tableau() +
    labs(col="Sampling Year") + 
    geom_jitter(position=position_jitter(width=0.25), na.rm=T);
v2 <- v1 + ggtitle("Changes in Chlorophyll-a by Month \nin Willard Spur (2011-2013)");
v3 <- v2 + xlab("Calendar Month") + ylab(paste(OW4.nm[14,2], " (", OW4.nm[14,3],")"));

ggsave(file="WSpur_CHLA-x-Month_fig2.jpeg", width=5, height=4, dpi=500);


##  Chl-A [14] vs. DOY [11] #3 All data
x0 <- ggplot(data=OW4.dat, aes(x=factor(Month), y=CHLA ), col=factor(Year)) + 
    theme_bw() + labs(col="Sampling Year");
x1 <- x0 + geom_jitter(na.rm=T, aes(col=factor(Year)), width=0.1) + 
    scale_y_continuous(limits=c(0,max(OW4.dat$CHLA, na.rm=T)));

    scale_x_continuous(breaks=c(100,150,200,250,300));
x2 <- x1 + stat_smooth(aes(col=Year.FX),na.rm=T, method="loess");


### SAV.Cover [AVG]  [*** NEW PLOT 151018 ***]  ######
# data = WatSAV9
# 
# y = SAVcov.avg  [88]
i = 88
# subset(OW6.Exceed, NH3_excd != ""
dataW <- subset(WatSAV9, SiteName2 != "")
dataW <- subset(WatSAV9, SiteNameX != "")
dataW <- droplevels(dataW)
dataW <- dataW[with(dataW, order(SiteName, Year, DOY)),]

p0 <- ggplot(data=dataW, aes(x=dataW$DOY, col=factor(Year)), na.rm=T) + 
    theme_bw() + facet_wrap(~SiteNameX, ncol=4);
p3 <- p0 + geom_line(aes(y=dataW[,i]), na.rm=T) + geom_point(aes(y=dataW[,i]), na.rm=T) ;
p4 <- p3 + xlab("Day of year") + ylab("Average SAV cover") +
    ggtitle(paste("Willard Spur: Average SAV cover")) + labs(col="Sampling Year")
p4
    ggsave(file=paste("WS15-OW_AVGSAV.cov_sites3x2_v3.jpeg", sep=""), 
           width=10, height=4, dpi=500);
###








