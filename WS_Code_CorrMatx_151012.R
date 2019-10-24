### Correlation Matrices ### Willard Spur 2015 -- Analyses 151012  #####
# data == WATSAVINV
WATSAVINV2.nam <- as.data.frame(names(WATSAVINV2))
Inv.codes3 & [2]
##### trim Var's for data frame
#invert.resp <- c(112,125,139:143) # check and combine into RESP
#response <- c(18,20, 23,22,25,44,53,54,55,51,52,78,79,86,88,96,97,98) # from WATSAV, check these
RESP <- c(18,20,23,22,25,44,53,54,55,51,52,78,79,86,88,96,97,98,112,125,139,140,141,142,143)
DRIV <- c(62,63,64,17,67,68,31)
VARS <- c(RESP,DRIV)
######
WATSAVINV2 <- WATSAVINV
WATSAVINV2 <- rename(WATSAVINV2, c("zBiomass"="Biomass", "zCount"="Count","zPMI"="PMI", "zSI.diff"="SI",
                         "zTaxa"="Taxa", "DOY.x"="DOY", "SiteORDER.x"="SiteORDER", "Depth.cm"="Depth"))
WATSAVINV2.nam <- as.data.frame(names(WATSAVINV2))
## These data have been fixed (outliers from TIN, TON, and TON/TN revised) [151013]  #####
#               [See WS_Code_RanFor_151012.R]   ###

MSI.all <- WATSAVINV2[,c(62,63,64,17,67,68,31,
                         18,20,23,22,25,44,53,54,55,51,52,
                         78,79,86,88,96,97,98,
                         112,125,139,140,141,142,143)]
######   CORRELATION ANALYSES #####
# data = MSI.all
COR.1 <- rcorr(as.matrix(MSI.all), type="pearson")
    signif(COR.1$r,2)
    signif(COR.1$P,2)
## flatten the list (corr) into 4C tables
flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row=rownames(cormat)[row(cormat)[ut]],
        column=rownames(cormat)[col(cormat)[ut]],
        cor=(cormat)[ut],
        p=pmat[ut]      )   }   ;
##
COR1.flat <- flattenCorrMatrix(COR.1$r, COR.1$P)

## Number of correlations performed:
x <- length(names(MSI.all))
Num.Corrs <- (x*(x-1))/2

## [1] try the PerfAnalytx figure 
MSI.all.mat <- as.matrix(MSI.all)
chart.Correlation(MSI.all.mat, histogram=TRUE, method="pearson")
## this data has too many variables...

## [2] something simpler...#######
library(corrplot);
cor.dat <- COR.1$r
sig.dat <- COR.1$P
#
write.csv(cor.dat, file="MSIall.corrdat2.csv", row.names=FALSE)
write.csv(sig.dat, file="MSIall.corr-sig-dat2.csv", row.names=FALSE)
write.csv(COR.1$n, file="MSIall.corrdat-SampSz2.csv", row.names=FALSE)
#
corrplot(cor.dat, method="circle", type="upper", diag=FALSE)
corrplot(cor.dat, method="number", type="lower", diag=FALSE)
corrplot(cor.dat, method="number", type="lower", diag=FALSE, order="hclust")
corrplot(cor.dat, method="number", type="lower", diag=FALSE, order="AOE")
corrplot(cor.dat, method="number", type="lower", diag=FALSE, order="alphabet")
corrplot(cor.dat, method="number", type="lower", diag=FALSE, order="FPC")
corrplot(cor.dat, method="circle", type="upper", diag=FALSE, tl.col="black", tl.srt=45)
corrplot(cor.dat, method="circle", type="upper", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.1)
corrplot(cor.dat, method="circle", type="upper", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.05)
corrplot(cor.dat, method="circle", type="upper", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.01)
corrplot(cor.dat, method="ellipse", type="upper", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.1, insig="blank")
corrplot(cor.dat, method="color", type="lower", order="hclust",diag=FALSE, 
         tl.col="black", tl.srt=45, addCoef.col="black", cl.cex=0.05,
         p.mat=sig.dat, sig.level=0.05, insig="blank")

# output and save ... [set]
jpeg(file="Corrplots_-10p-1c.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.05, insig="blank", tl.cex=0.5,order="original")
dev.off()
##
#### [3] split into Dataset Groups   #####

MSI.all.mat <- as.matrix(MSI.all)
chart.Correlation(MSI.all.mat, histogram=TRUE, method="pearson")
#
MSI.nam <- as.data.frame(names(MSI.all))
#
MSI.all <- WATSAVINV2[,c(62,63,64,17,67,68,31,
                         18,20,23,22,25,44,53,54,55,51,52,78,79,86,88,96,97,98,112,125,139,140,141,142,143)]
## Fixify some Var names...
names(MSI.all)[names(MSI.all) == "T_ALK"] <- "Alk.T"
names(MSI.all)[names(MSI.all) == "T_SO4"] <- "SO4.T"
names(MSI.all)[names(MSI.all) == "TN2"] <- "TotalN"
names(MSI.all)[names(MSI.all) == "Algmat.avg"] <- "AlgalMat"
names(MSI.all)[names(MSI.all) == "SAV.COND.avg"] <- "SAV.COND"
names(MSI.all)[names(MSI.all) == "SAVcov.avg"] <- "SAV.cover"

###############
## MSI.chem <- MSI.all[,c(1:18)]
# [1]
jpeg(file="Corr_chem_1.jpg", width=7, height=6, units="in", quality=80, res=500)
chart.Correlation(as.matrix(MSI.chem), histogram=TRUE, method="pearson")
dev.off()

# [1B]
COR.1 <- rcorr(as.matrix(MSI.chem), type="pearson")
cor.dat <- COR.1$r
sig.dat <- COR.1$P
jpeg(file="Corrplots_Chem_v2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5,order="original")
dev.off()

## MSI.SAV <- MSI.all[,c(1:18,19:25)]
# [2] jpeg(file="Corr_SAV_1.jpg", width=7, height=6, units="in", quality=80, res=500)
chart.Correlation(as.matrix(MSI.SAV), histogram=TRUE, method="pearson")
dev.off()

# [2B]
COR.1 <- rcorr(as.matrix(MSI.SAV), type="pearson")
cor.dat <- COR.1$r
sig.dat <- COR.1$P
jpeg(file="Corrplots_SAV_v2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5,order="original")
dev.off()

## MSI.INV <- MSI.all[,c(1:18,26:32)]
# [3] jpeg(file="Corr_INV_1.jpg", width=7, height=6, units="in", quality=80, res=500)
chart.Correlation(as.matrix(MSI.INV), histogram=TRUE, method="pearson")
dev.off()

# [3B]
COR.1 <- rcorr(as.matrix(MSI.INV), type="pearson")
cor.dat <- COR.1$r
sig.dat <- COR.1$P
jpeg(file="Corrplots_INV_v2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5,order="original")
dev.off()

## MSI.SAVINV <- MSI.all[,c(19:32)]
# [4] jpeg(file="Corr_SAVxINV_1.jpg", width=7, height=6, units="in", quality=80, res=500)
chart.Correlation(as.matrix(MSI.SAVINV), histogram=TRUE, method="pearson")
dev.off()

# [4B]
COR.1 <- rcorr(as.matrix(MSI.SAVINV), type="pearson")
cor.dat <- COR.1$r
sig.dat <- COR.1$P
jpeg(file="Corrplots_SAVxINV_v2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5,order="original")
dev.off()

#####  [4] Effects of Water Depth on System-level responses... #######
# data = WATSAVINV2
levels(WATSAVINV2$HydroConn) <- c("Connected", "Isolated")
data <- WATSAVINV2
# also try the limited group (WATSAVINV2), where SiteNameX.x != ""
data.lim <- subset(WATSAVINV2, SiteNameX.x != "")
data.lim <- droplevels(data.lim)
    levels(data.lim$SiteNameX.x)
data <- data.lim
data$HydroConn <- factor(data$HydroConn, levels=c("Connected", "Isolated"))
levels(data$HydroConn) <- c("Connected", "Isolated")


# [plot1  Temp vs Depth]
# limited dataset (expanded KMS, SiteNameX.x != "")       data <- data.lim
a0 <- ggplot(data=subset(data, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=Temp), na.rm=T, size=2)
a2 <- a1 + geom_smooth(aes(x=Depth, y=Temp), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black") + geom_hline(yintercept=27, lty=2, col="black");
a3 <- a2 + xlab("Water Depth (cm)") + scale_y_continuous(limits=c(0,NA), name="Water Temperature (C)") +
        scale_x_continuous(limits=c(0,NA)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x Temperature\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_Temp_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***
# larger dataset (SiteSAV != "")        data <- WATSAVINV2
a0 <- ggplot(data=subset(data, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=Temp), na.rm=T, size=2)
a2 <- a1 + geom_smooth(aes(x=Depth, y=Temp), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black") + geom_hline(yintercept=27, lty=2, col="black");
a3 <- a2 + xlab("Water Depth (cm)") + scale_x_continuous(limits=c(0,NA),breaks=seq(0,150,30)) +
        scale_y_continuous(limits=c(0,NA), name="Water Temperature (C)") +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x Temperature\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_Temp_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500);

x

## [plot2: SAV cover [88] vs Depth]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SAVcov.avg), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=SAVcov.avg), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + scale_y_continuous(limits=c(0,100), name="SAV Cover") +
        scale_x_continuous(limits=c(0,NA)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Cover\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVcov_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500);

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=SAVcov.avg), na.rm=T, size=2)
b2 <- b1 + geom_smooth(aes(x=Depth, y=SAVcov.avg), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
        scale_x_continuous(limits=c(0,NA),breaks=seq(0,150,30)) +
        scale_y_continuous(limits=c(0,100), name="SAV Cover") +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Cover\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_SAVcov_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500); # ***




## [plot3: SAV COND [86] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SAV.COND.avg), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=SAV.COND.avg), na.rm=T, method=lm, se=T) +
            geom_vline(xintercept=20, lty=2, color="black")
### Not interesting (no diff)

## [plot4: SAV.biovol [96] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SAV.biovol), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=SAV.biovol), na.rm=T, method=lm, se=F) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SAV.biovol), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=SAV.biovol), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SAV Biovolume (cover x canopy height (cm))", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$SAV.biovol, na.rm=T)*0.05),
            max(data$SAV.biovol, na.rm=T)*1.1), xlim=c(0,125)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Biovolume\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVBioV_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500);

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=SAV.biovol), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=SAV.biovol), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SAV Biovolume (cover x canopy height (cm))", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$SAV.biovol, na.rm=T)*0.05),
            max(data$SAV.biovol, na.rm=T)*1.1), xlim=c(0,125)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Biovolume\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_SAVBioV_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500); # ***

## [plot5: SAV.vigor [98] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SAV.Vigor), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=SAV.Vigor), na.rm=T, method=lm, se=F) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SAV.Vigor), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=SAV.Vigor), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SAV Vigor (cover x can. ht. x condition)", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$SAV.Vigor, na.rm=T)*0.05),
            max(data$SAV.Vigor, na.rm=T)*1.1), xlim=c(0,125)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Vigor\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVVig_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500);

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=SAV.Vigor), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=SAV.Vigor), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SAV Vigor (cover x can. ht. x condition)", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$SAV.Vigor, na.rm=T)*0.05),
            max(data$SAV.Vigor, na.rm=T)*1.1), xlim=c(0,125)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Vigor\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_SAVVig_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500); # ***


## [plot6: PMI [141] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=PMI), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=PMI), na.rm=T, method=lm, se=F) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=PMI), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=PMI), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="PMI metric", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$PMI, na.rm=T)*0.05),
            max(data$PMI, na.rm=T)*1.1), xlim=c(-5,125)) +
        ggtitle("Effects of Hydrologic Isolation on PMI metric\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_PMI_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500); 

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=PMI), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=PMI), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="PMI metric", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$PMI, na.rm=T)*0.05),
            max(data$PMI, na.rm=T)*1.1), xlim=c(-5,125)) +
        ggtitle("Effects of Hydrologic Isolation on PMI metric\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_PMI_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***


## [plot6: SI [142] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SI), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=SI), na.rm=T, method=lm, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

a2.b <- a1 + geom_smooth(aes(x=Depth, y=SI), na.rm=T, method=loess, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SI), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=SI), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SI metric", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$SI, na.rm=T)*0.05),
            max(data$SI, na.rm=T)*1.1), xlim=c(-5,125)) +
        ggtitle("Effects of Hydrologic Isolation on SI metric\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SI_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=SI), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=SI), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SI metric", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$SI, na.rm=T)*0.05),
            max(data$SI, na.rm=T)*1.1), xlim=c(-5,125)) +
        ggtitle("Effects of Hydrologic Isolation on Simpson's Diversity Index (SI) metric\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_SI_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500); 

## [plot7 Taxa [143] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=Taxa), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=Taxa), na.rm=T, method=lm, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

a2.b <- a1 + geom_smooth(aes(x=Depth, y=Taxa), na.rm=T, method=loess, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")
    #### < skip this one > ###

## [plot8 TDS [31] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=TDS), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=TDS), na.rm=T, method=lm, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

a2.b <- a1 + geom_smooth(aes(x=Depth, y=TDS), na.rm=T, method=loess, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=TDS), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=TDS), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black") +
            geom_hline(yintercept=1200, lty=2, col="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="TDS (mg/L)", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$TDS, na.rm=T)*0.05),
            max(data$TDS, na.rm=T)*1.1), xlim=c(-5,125)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x TDS\n") +
        theme(legend.position=c(0.9,0.8), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_TDS_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500); 


## [plot9 T_SO4 [25] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=T_SO4), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=T_SO4), na.rm=T, method=lm, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

a2.b <- a1 + geom_smooth(aes(x=Depth, y=T_SO4), na.rm=T, method=loess, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")
#  #######  ####   ##  SKIP IT


## [plot10 TN2 [51] vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=TN2), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=TN2), na.rm=T, method=lm, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

a2.b <- a1 + geom_smooth(aes(x=Depth, y=TN2), na.rm=T, method=loess, se=T) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=TN2), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=TN2), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="Total N (mg/L)", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$TN2, na.rm=T)*0.05),
            max(data$TN2, na.rm=T)*1.1), xlim=c(-5,125)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x Total N\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_TotalN_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

## [plot10.5 TP [44] vs Depth] ******

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=TP), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=TP), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="Total P (mg/L)", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$TP, na.rm=T)*0.05),
            max(data$TP, na.rm=T)*1.1), xlim=c(-5,125)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x Total P\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_TP_x_Depth_dataLimX_v2.jpeg", width=6, height=5, dpi=500); # ***


## [plot11 TP [44] vs TSS [20]]  ## updated 10/20/15
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TSS, y=TP), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=TSS, y=TP), na.rm=T, method=lm, se=T) + ylim(0,NA) 

a2.b <- a1 + geom_smooth(aes(x=TSS, y=TP), na.rm=T, method=loess, se=T) + ylim(0,NA) 

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=TSS, y=TP), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=TSS, y=TP), na.rm=T, method=lm, se=T, alpha=0.15);
b3 <- b2 + xlab("Total Suspended Solids (mg/L)") + 
    scale_y_continuous(name="Total P (mg/L)", labels=comma) +
        scale_x_continuous(oob=squish, limits=c((-max(data$TSS, na.rm=T)*0.05),250)) + 
        coord_cartesian(ylim=c((-max(data$TP, na.rm=T)*0.05),
            max(data$TP, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on TSS x Total P\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_TP_x_TSS_dataOW_v2.jpeg", width=6, height=5, dpi=500); 
# 
xlim=c((-max(data$TSS, na.rm=T)*0.05),
            max(data$TSS, na.rm=T)*1.1)
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TSS, y=TP), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TSS, y=TP), na.rm=T, method=lm, se=T, alpha=0.15);
a3 <- a2 + xlab("Total Suspended Solids (mg/L)") + 
    scale_y_continuous(name="Total P (mg/L)", labels=comma) +
        scale_x_continuous(limits=c(NA,250), oob=squish) + 
        coord_cartesian(ylim=c((-max(data$TP, na.rm=T)*0.05),
            max(data$TP, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water TSS x Total P\n") +
        theme(legend.position=c(0.85,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_TP_x_TSS_dataLimX_v3.jpeg", width=6, height=5, dpi=500); # ***


## [plot12 SAV.SCR [44] vs TN2 [51]]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAV.SCR), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAV.SCR), na.rm=T, method=lm, se=T) + ylim(0,NA) 
a2.b <- a1 + geom_smooth(aes(x=TN2, y=SAV.SCR), na.rm=T, method=loess, se=T) + ylim(0,NA)

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAV.SCR), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAV.SCR), na.rm=T, method=lm, se=T, alpha=0.15);
a3 <- a2 + xlab("Total N (mg/L)") + 
    scale_y_continuous(name="SAV Score (cover x cond. class)", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$SAV.SCR, na.rm=T)*0.05),
            max(data$SAV.SCR, na.rm=T)*1.1),
            xlim=c((-max(data$TN2, na.rm=T)*0.05),
            max(data$TN2, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total N x SAV Score\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVSCR_x_TN_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

### <plot 12.5)  SAV.cover x TN ### 
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAVcov.avg), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAVcov.avg), na.rm=T, method=lm, se=T, alpha=0.15);
a3 <- a2 + xlab("Total N (mg/L)") + 
    scale_y_continuous(name="SAV Cover", labels=comma) +
        scale_x_continuous(limits=c(NA,NA)) + 
        coord_cartesian(ylim=c((-max(data$SAVcov.avg, na.rm=T)*0.05),
            max(data$SAVcov.avg, na.rm=T)*1.1),
            xlim=c((-max(data$TN2, na.rm=T)*0.05),
            max(data$TN2, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total N x SAV Cover\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVCov_x_TN_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***


## [plot13 SAV.SCR [44] vs TP [44]]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAV.SCR), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=TP, y=SAV.SCR), na.rm=T, method=lm, se=T) + ylim(0,NA) 
a2.b <- a1 + geom_smooth(aes(x=TP, y=SAV.SCR), na.rm=T, method=loess, se=T) + ylim(0,NA)

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAV.SCR), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TP, y=SAV.SCR), na.rm=T, method=lm, se=T, alpha=0.15);
a3 <- a2 + xlab("Total P (mg/L)") + 
    scale_y_continuous(name="SAV Score", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$SAV.SCR, na.rm=T)*0.05),
            max(data$SAV.SCR, na.rm=T)*1.1),
            xlim=c((-max(data$TP, na.rm=T)*0.05),
            max(data$TP, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total P x SAV Score\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVSCR_x_TP_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # *****

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   <Uses SAVCov>>
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAVcov.avg), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TP, y=SAVcov.avg), na.rm=T, method=lm, se=T, alpha=0.15);
a3 <- a2 + xlab("Total P (mg/L)") + 
    scale_y_continuous(name="SAV Cover", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$SAVcov.avg, na.rm=T)*0.05),
            max(data$SAVcov.avg, na.rm=T)*1.1),
            xlim=c((-max(data$TP, na.rm=T)*0.05),
            max(data$TP, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total P x SAV Cover\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVCov_x_TP_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***


## [plot14 TP [44] vs TN2 [51]]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=TP), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=TN2, y=TP), na.rm=T, method=lm, se=F) + ylim(0,NA) 
a2.b <- a1 + geom_smooth(aes(x=TN2, y=TP), na.rm=T, method=loess, se=F) + ylim(0,NA)

## [plot15 SAV.Vigor [98] vs TN2 [51]]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAV.Vigor), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAV.Vigor), na.rm=T, method=lm, se=F) + ylim(0,NA) 
a2.b <- a1 + geom_smooth(aes(x=TN2, y=SAV.Vigor), na.rm=T, method=loess, se=F) + ylim(0,NA)

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   <Uses SAVCov>>
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAV.Vigor), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAV.Vigor), na.rm=T, method=lm, se=T, alpha=0.15);
a3 <- a2 + xlab("Total N (mg/L)") + 
    scale_y_continuous(name="SAV Vigor", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$SAV.Vigor, na.rm=T)*0.05),
            max(data$SAV.Vigor, na.rm=T)*1.1),
            xlim=c((-max(data$TN2, na.rm=T)*0.05),
            max(data$TN2, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total N x SAV Vigor\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVVig_x_TN_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

## [plot16 SAV.Vigor [98] vs Tp [51]]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAV.Vigor), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=TP, y=SAV.Vigor), na.rm=T, method=lm, se=F) + ylim(0,NA) 
a2.b <- a1 + geom_smooth(aes(x=TP, y=SAV.Vigor), na.rm=T, method=loess, se=F) + ylim(0,NA)

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   <Uses SAVCov>>
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAV.Vigor), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TP, y=SAV.Vigor), na.rm=T, method=lm, se=T, alpha=0.15);
a3 <- a2 + xlab("Total P (mg/L)") + 
    scale_y_continuous(name="SAV Vigor", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$SAV.Vigor, na.rm=T)*0.05),
            max(data$SAV.Vigor, na.rm=T)*1.1),
            xlim=c((-max(data$TP, na.rm=T)*0.05),
            max(data$TP, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total P x SAV Vigor\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVVig_x_TP_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

#################################################################
##### T.ALK x pH ###
data <- WATSAVINV2
data <- data.lim

q0 <- ggplot(data=WATSAVINV2, aes(x=pH, y=T_ALK, col=factor(Year))) + theme_bw() + 
    scale_colour_manual(values=rainbow(3));
q1 <- q0 + geom_point(na.rm=T) + geom_smooth(method="lm", se=F)

q0 <- ggplot(data=WATSAVINV2, aes(x=pH, y=T_ALK)) + theme_bw() + 
    scale_colour_manual(values=rainbow(3));
q1 <- q0 + geom_point(na.rm=T) + geom_smooth(method="lm", se=F) +
    xlim(7,11) + ylim(0,NA);
q1
ggsave(file="WS_pH_x_TALK_v1.jpeg", width=6, height=5, dpi=500);
#####

## [plot17: inv.count vs Depth]
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=Count), na.rm=T)
a2 <- a1 + geom_smooth(aes(x=Depth, y=Count), na.rm=T, method=lm, se=F) + ylim(0,NA) +
            geom_vline(xintercept=20, lty=2, color="black")

# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=Count), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=Count), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
        scale_y_continuous(name="Invertebrate Abundance", labels=comma) +
        coord_cartesian(ylim=c((-max(data$Count, na.rm=T)*0.05),
            max(data$Count, na.rm=T)*1.1), xlim=c(-5,125)) +
        scale_x_continuous() + 
        ggtitle("Effects of Hydrologic Isolation on Count metric\n") +
        theme(legend.position=c(0.9,0.8), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_Count_x_Depth_dataOW_v2.jpeg", width=6, height=5, dpi=500); 

# coord_cartesian(ylim=c((-max(data$Count, na.rm=T)*0.05),
            max(data$Count, na.rm=T)*1.1), xlim=c(-5,125)) +

# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=Count), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=Count), na.rm=T, method=lm, se=T, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name=bquote("# Organisms / m"^2), labels=comma, trans="log2",
                       breaks=c(10,50,100,200,500,1000,5000), limits=c(10,NA)) +
        scale_x_continuous(limits=c(NA,NA)) + 
                ggtitle("Effects of Hydrologic Isolation on Invertebrate Abundance\n") +
        theme(legend.position=c(0.9,0.8), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_Count_x_Depth_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

#####

## [plot18 SAV.biovol [96] vs TN2 [51]]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAV.biovol), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAV.biovol), na.rm=T, method=lm, se=F, alpha=0.15);
a3 <- a2 + xlab("Total N (mg/L)") + 
    scale_y_continuous(name="SAV Biovolume (Cover x Height (cm))", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$SAV.biovol, na.rm=T)*0.05),
            max(data$SAV.biovol, na.rm=T)*1.1),
            xlim=c((-max(data$TN2, na.rm=T)*0.05),
            max(data$TN2, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total N x SAV Biovolume\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAV.biovol_x_TN_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

## [plot19 SAV.biovol [96] vs TP [51]]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   <Uses SAVCov>>
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAV.biovol), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TP, y=SAV.biovol), na.rm=T, method=lm, se=F, alpha=0.15);
a3 <- a2 + xlab("Total P (mg/L)") + 
    scale_y_continuous(name="SAV Biovolume (Cover x Height (cm))", labels=comma) +
        scale_x_continuous() + 
        coord_cartesian(ylim=c((-max(data$SAV.biovol, na.rm=T)*0.05),
            max(data$SAV.biovol, na.rm=T)*1.1),
            xlim=c((-max(data$TP, na.rm=T)*0.05),
            max(data$TP, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Total P x SAV Biovolume\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_SAVbiovol_x_TP_dataLimX_v1.jpeg", width=6, height=5, dpi=500); # ***

##


























# spare parts
data.lim <- data
 data[(data$Count==1 & !(is.na(data$Count))),140] <- NA  ### FIXING ZEROS in INVERT ABund 

