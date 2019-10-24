### Bivariate Parameter x Depth (or Nutrient) vs. Hydrologic Connectivity -- 151020 PLOTS  #####
# data == WATSAVINV
WATSAVINV2.nam <- as.data.frame(names(WATSAVINV2))
Inv.codes3 & [2]
WATSAVINV2 <- rename(WATSAVINV2, c("zBiomass"="Biomass", "zCount"="Count","zPMI"="PMI", "zSI.diff"="SI",
                         "zTaxa"="Taxa", "DOY.x"="DOY", "SiteORDER.x"="SiteORDER", "Depth.cm"="Depth"))
WATSAVINV2.nam <- as.data.frame(names(WATSAVINV2))
## These data have been fixed (outliers from TIN, TON, and TON/TN revised) [151013]  #####

#####  Effects of Water Depth on System-level responses... #######
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
######  This version has all SE's removed from Smoother plots #######

# [plot1  Temp vs Depth]  ##########
# limited dataset (expanded KMS, SiteNameX.x != "")       data <- data.lim
a0 <- ggplot(data=subset(data, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=Temp), na.rm=T, size=2)
a2 <- a1 + geom_smooth(aes(x=Depth, y=Temp), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black") + geom_hline(yintercept=27, lty=2, col="black");
a3 <- a2 + xlab("Water Depth (cm)") + scale_y_continuous(name="Water Temperature (C)") +
        scale_x_continuous(limits=c(0,NA)) +
        coord_cartesian(ylim=c((-max(data$Temp, na.rm=T)*0.05),
                                max(data$Temp, na.rm=T)*1.1), 
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x Temperature\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_Temp_x_Depth_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***


## [plot2: SAV cover [88] vs Depth]
# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=SAVcov.avg), na.rm=T, size=2)
b2 <- b1 + geom_smooth(aes(x=Depth, y=SAVcov.avg), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
        scale_x_continuous(breaks=seq(0,150,30)) +
        scale_y_continuous( name="SAV Cover") +
    coord_cartesian(ylim=c((-max(data$SAVcov.avg, na.rm=T)*0.05),
                                max(data$SAVcov.avg, na.rm=T)*1.1), 
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Cover\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_SAVcov_x_Depth_dataOW_v2X.jpeg", width=6, height=5, dpi=500); # ***


## [plot3: SAV COND [86] vs Depth]
    ### Not interesting (no diff)

## [plot4: SAV.biovol [96] vs Depth]
# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=SAV.biovol), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=SAV.biovol), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SAV Biovolume (cover x canopy height (cm))", labels=comma) +
        scale_x_continuous(breaks=seq(0,150,30)) + 
        coord_cartesian(ylim=c((-max(data$SAV.biovol, na.rm=T)*0.05),
                                max(data$SAV.biovol, na.rm=T)*1.1), 
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Biovolume\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_SAVBioV_x_Depth_dataOW_v2X.jpeg", width=6, height=5, dpi=500); # ***

## [plot5: SAV.vigor [98] vs Depth]
# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2, Month < 10), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=SAV.Vigor), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=SAV.Vigor), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="SAV Vigor (cover x can. ht. x condition)", labels=comma) +
        scale_x_continuous(breaks=seq(0,150,30)) + 
        coord_cartesian(ylim=c((-max(data$SAV.Vigor, na.rm=T)*0.05),
                                max(data$SAV.Vigor, na.rm=T)*1.1),
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x SAV Vigor\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_SAVVig_x_Depth_dataOW_v2X.jpeg", width=6, height=5, dpi=500); # ***


## [plot6: PMI [141] vs Depth]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=PMI), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=PMI), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="PMI metric", labels=comma) +
        scale_x_continuous(breaks=seq(0,150,30)) + 
        coord_cartesian(ylim=c((-max(data$PMI, na.rm=T)*0.05),
                                max(data$PMI, na.rm=T)*1.1),
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on PMI metric\n") +
        theme(legend.position=c(0.9,0.17), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_PMI_x_Depth_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***


## [plot6.5: SI [142] vs Depth]    #### SKIP IT -- >>>
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=SI), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=SI), na.rm=T, method=lm, se=F, alpha=0.15) +
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
    ggsave(file="WS_SI_x_Depth_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***


## [plot7 Taxa [143] vs Depth]
    #### < skip this one > ###

## [plot8 TDS [31] vs Depth]
# larger dataset (SiteSAV != "")        data2 <- WATSAVINV2
b0 <- ggplot(data=subset(data2), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
b1 <- b0 + geom_point(aes(x=Depth, y=TDS), na.rm=T, size=2);
b2 <- b1 + geom_smooth(aes(x=Depth, y=TDS), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black") +
            geom_hline(yintercept=1200, lty=2, col="black");
b3 <- b2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="TDS (mg/L)", labels=comma) +
        scale_x_continuous(breaks=seq(0,150,30)) + 
        coord_cartesian(ylim=c((-max(data$TDS, na.rm=T)*0.05),
                                max(data$TDS, na.rm=T)*1.1), 
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x TDS\n") +
        theme(legend.position=c(0.9,0.8), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
b3
    ggsave(file="WS_TDS_x_Depth_dataOW_v2X.jpeg", width=6, height=5, dpi=500); # *****


## [plot9 T_SO4 [25] vs Depth]
#  #######  ####   ##  SKIP IT


## [plot10 TN2 [51] vs Depth]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=TN2), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=TN2), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="Total N (mg/L)", labels=comma) +
        scale_x_continuous(breaks=seq(0,150,30)) + 
        coord_cartesian(ylim=c((-max(data$TN2, na.rm=T)*0.05),
                                max(data$TN2, na.rm=T)*1.1), 
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x Total N\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_TotalN_x_Depth_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***

## [plot10.5 TP [44] vs Depth] ******
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=TP), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=TP), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
    scale_y_continuous(name="Total P (mg/L)", labels=comma) +
        scale_x_continuous(breaks=seq(0,150,30)) + 
        coord_cartesian(ylim=c((-max(data$TP, na.rm=T)*0.05),
                                max(data$TP, na.rm=T)*1.1), 
                        xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
        ggtitle("Effects of Hydrologic Isolation on Water Depth x Total P\n") +
        theme(legend.position=c(0.9,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_TP_x_Depth_dataLimX_v2X.jpeg", width=6, height=5, dpi=500); # ***


## [plot11 TP [44] vs TSS [20]]  ## updated 10/20/15
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TSS, y=TP), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TSS, y=TP), na.rm=T, method=lm, se=F, alpha=0.15);
a3 <- a2 + xlab("Total Suspended Solids (mg/L)") + 
    scale_y_continuous(name="Total P (mg/L)", labels=comma) +
        coord_cartesian(ylim=c((-max(data$TP, na.rm=T)*0.05),
                                max(data$TP, na.rm=T)*1.1)) +
        scale_x_continuous(limits=c(NA,250), oob=squish) + 
        ggtitle("Effects of Hydrologic Isolation on Water TSS x Total P\n") +
        theme(legend.position=c(0.85,0.82), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_TP_x_TSS_dataLimX_v3X.jpeg", width=6, height=5, dpi=500); # ***


## [plot12 SAV.SCR [44] vs TN2 [51]]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAV.SCR), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAV.SCR), na.rm=T, method=lm, se=F, alpha=0.15);
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
    ggsave(file="WS_SAVSCR_x_TN_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***

### <plot 12.5)  SAV.cover x TN ### 
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAVcov.avg), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAVcov.avg), na.rm=T, method=lm, se=F, alpha=0.15);
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
    ggsave(file="WS_SAVCov_x_TN_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***


## [plot13 SAV.SCR [44] vs TP [44]]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAV.SCR), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TP, y=SAV.SCR), na.rm=T, method=lm, se=F, alpha=0.15);
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
    ggsave(file="WS_SAVSCR_x_TP_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # *****

## [plot13.5 SAV.Cov [44] vs TP [44]]  **UPDATED**
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   <Uses SAVCov>>
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAVcov.avg), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TP, y=SAVcov.avg), na.rm=T, method=lm, se=F, alpha=0.15);
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
    ggsave(file="WS_SAVCov_x_TP_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***


## [plot14 TP [44] vs TN2 [51]]

## [plot15 SAV.Vigor [98] vs TN2 [51]]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   <Uses SAVCov>>
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TN2, y=SAV.Vigor), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TN2, y=SAV.Vigor), na.rm=T, method=lm, se=F, alpha=0.15);
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
    ggsave(file="WS_SAVVig_x_TN_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***

## [plot16 SAV.Vigor [98] vs Tp [51]]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim   <Uses SAVCov>>
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=TP, y=SAV.Vigor), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=TP, y=SAV.Vigor), na.rm=T, method=lm, se=F, alpha=0.15);
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
    ggsave(file="WS_SAVVig_x_TP_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***

#################################################################
##### T.ALK x pH ########
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
ggsave(file="WS_pH_x_TALK_v1X.jpeg", width=6, height=5, dpi=500);
#####

## [plot17: inv.count vs Depth]
# limited dataset (expanded KMS, SiteNameX.x != "")   data <- data.lim
a0 <- ggplot(data=subset(data), aes(colour=HydroConn),na.rm=TRUE) + theme_bw() + 
        scale_colour_manual(values=rainbow(3));
a1 <- a0 + geom_point(aes(x=Depth, y=Count), na.rm=T, size=2);
a2 <- a1 + geom_smooth(aes(x=Depth, y=Count), na.rm=T, method=lm, se=F, alpha=0.15) +
            geom_vline(xintercept=20, lty=2, color="black");
a3 <- a2 + xlab("Water Depth (cm)") + 
        scale_x_continuous(breaks=seq(0,150,30)) +
        coord_cartesian(xlim=c((-max(data$Depth, na.rm=T)*0.05),
                                max(data$Depth, na.rm=T)*1.1)) +
            scale_y_continuous(limits=c(0,5000),oob=squish) +
                ggtitle("Effects of Hydrologic Isolation on Invertebrate Abundance\n") +
        theme(legend.position=c(0.9,0.8), legend.title=element_text(size=8),
              legend.text=element_text(size=8), plot.title=element_text(size=13)) + 
        labs(col="Hydrologic\n Connectivity");
a3
    ggsave(file="WS_Count_x_Depth_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***

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
    ggsave(file="WS_SAV.biovol_x_TN_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***

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
    ggsave(file="WS_SAVbiovol_x_TP_dataLimX_v1X.jpeg", width=6, height=5, dpi=500); # ***

##























