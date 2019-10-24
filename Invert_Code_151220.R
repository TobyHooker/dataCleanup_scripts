### IW Benthic Macroinvertebrates -- 151220 ##############################
####
####
Inverts_IWAll <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/REFSTD_R/Inverts_IWAll_151202.csv")
INV0 <- Inverts_IWAll
### Data fixup  #####
### Step 1:  Data scan for VAR.classes  ######
INV1.var <- data.frame()
for (i in 1:length(names(INV1))){
  class_i <- class(INV1[,i])
  name_i <- names(INV1[i])
  num.NAs <- sum(is.na(INV1[,i]))
  INV1.var <- rbind(INV1.var, data.frame(i, name_i, class_i, num.NAs))  
#  WChem4[,i] <- as.numeric(WChem4[,i])
}   ### Updated for INV1... {151221}
### Step 2:  Update SET.names for labeling ######
levels(INV0$Set)
INV0$SET.name <- INV0$Set
levels(INV0$SET.name) <- c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
                           "Willard Spur (2011-3)")

### Step 3:  FIX Dates  ######
  class(INV0$Date)
INV0$Date2 <- as.Date(INV0$Date, format="%m/%d/%Y")
INV0$Year <- as.numeric(format(INV0$Date2, "%Y"))
INV0$Month <- as.numeric(format(INV0$Date2, "%m"))
INV0$DOY <- yday(INV0$Date2)

#INV0 <- INV0[,(-90)]
### Step 4:  Add row.names (IDvar)   #####
INV0$iden <- interaction(INV0$STORET, as.numeric(INV0$Date2), sep=".")
write.csv(INV0, "INV0_151221_v0.csv", row.names=F) ## fixed <iden> in Excel...
INV1 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/INV0_151221_v0.csv")
## Note:  Still have some Dups in <iden>, due to Samp.Type issues... #########

# STEP 5:  Index Periods (IPs)  ######
INV1$IP_calc <- ifelse(INV1$DOY<166,NA,
                    ifelse(INV1$DOY<228,1,
                    ifelse(INV1$DOY>274,NA,
                    ifelse(INV1$DOY>229,2,NA))));

##############################################################################
##############################################################################
levels(INV1$Samp.Type)
####  PLot 1  ::  Invert Abundance  /  zAbund  [85]    ######
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zAbund))
data <- data[with(data, order(SET.name, Month, Year)),]
##
w0 <- ggplot(data, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F")) +
      facet_wrap(~SET.name, scales="fixed", ncol=1);
w1 <- w0 + geom_jitter(aes(y=(zAbund/1.5)), shape=20, na.rm=T, size=2,
      position=position_jitter(w=0.15)) +
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,4200), oob=squish, breaks=c(0,1000,2000,3000,4000)) +
      coord_trans() +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov"));
w2 <- w1 + xlab(NULL) + labs(col="Sampling Year") + 
      ylab(bquote('Total Macroinvertebrate Abundance (individuals / ' ~m^2*' )')) + 
      ggtitle("Seasonal patterns in Macroinvertebrate Abundance\n");
w2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
   Summ.Geomean2 <- ddply(data, .(SET.name, Month), numcolwise(gm_mean1, na.rm=T));
w3 <- w2 + geom_point(data=Summ.Geomean2, aes(x=factor(Month), y=zAbund, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
w3

ggsave(file="AllIW_Inv-Abund_jitter_1.jpeg", width=7, height=7, dpi=500)     #######

#### PLot 1B :: Log y-axis     ######
w0 <- ggplot(data, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F")) +
      facet_wrap(~SET.name, scales="fixed", ncol=1);
w1 <- w0 + geom_jitter(aes(y=log(zAbund/1.5)), shape=20, na.rm=T, size=2,
      position=position_jitter(w=0.15)) +
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(1,6.5), oob=squish, breaks=c(2,3,4,5,6)) + 
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov"));
w2 <- w1 + xlab(NULL) + labs(col="Sampling Year") + 
      ylab(bquote('Total Abundance [log-scale] (individuals / ' ~m^2*' )')) + 
      ggtitle("Seasonal patterns in Macroinvertebrate Abundance\n");
w2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
   Summ.Geomean2 <- ddply(data, .(SET.name, Month), numcolwise(gm_mean1, na.rm=T));
w3 <- w2 + geom_point(data=Summ.Geomean2, aes(x=factor(Month), y=log(zAbund/1.5), col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
w3
ggsave(file="AllIW_Inv-Abund_jitter_1B.jpeg", width=7, height=7, dpi=500)     #####
## Not too sure about this one (1B)... ###

#### Plot 2  ::  Invert Abundance  :: Box + Margins    #####
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zAbund))
data <- data[with(data, order(SET.name, Month, Year)),]
data2 <- subset(data, !is.na(IP_calc))

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=(zAbund/1.5)), outlier.shape=NA, na.rm=T)
  w2 <- w1 + geom_jitter(aes(y=(zAbund/1.5), col=factor(Year)), 
                         position=position_jitter(width=0.15, height=0.02), na.rm=T);
  w3 <- w2 + xlab(NULL) + 
      ylab(bquote('Total Macroinvertebrate Abundance (individuals / ' ~m^2*' )')) + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,4550), oob=squish, breaks=c(0,1500,3000,4500),
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=(zAbund/1.5), col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,4550), oob=squish, breaks=c(0,1500,3000,4500),
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
jpeg(filename="INV_zAbund_Box+Margin_v1.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Macroinvertebrate Abundance during Index Periods", vjust=1),legendX)
dev.off();    ################

#### Plot 3 ::  Taxa Richness  :: Box + Margins   #####
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zTaxa.rich))
data <- data[with(data, order(SET.name, Month, Year)),]
data2 <- subset(data, !is.na(IP_calc))

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=(zTaxa.rich)), outlier.shape=NA, na.rm=T)
  w2 <- w1 + geom_jitter(aes(y=(zTaxa.rich), col=factor(Year)), 
                         position=position_jitter(width=0.15, height=0.1), na.rm=T);
  w3 <- w2 + xlab(NULL) + 
      ylab(bquote('Macroinvertebrate Taxa Richness')) + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,17), oob=squish, breaks=c(0,5,10,15),
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=(zTaxa.rich), col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,17), oob=squish, breaks=c(0,5,10,15),
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
jpeg(filename="INV_zTaxaRich_Box+Margin_v2.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Macroinvertebrate Taxa Richness during Index Periods", vjust=1),legendX)
dev.off();    ################

#######################  PLOT 3B ::  Taxa Richness  ::  Scatter by Month(Year)  ######
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zTaxa.rich))
data <- data[with(data, order(SET.name, Year, Month)),]
##
w0 <- ggplot(data, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F")) +
      facet_wrap(~SET.name, scales="fixed", ncol=1);
w1 <- w0 + geom_jitter(aes(y=(zTaxa.rich)), shape=20, na.rm=T, size=2,
      position=position_jitter(w=0.15)) +
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,17), oob=squish, breaks=c(0,5,10,15)) +
      coord_trans() +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov"));
w2 <- w1 + xlab(NULL) + labs(col="Sampling Year") + 
      ylab(bquote('Macroinvertebrate Taxa Richness')) + 
      ggtitle("Seasonal patterns in Macroinvertebrate Taxa Richness\n");
w2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
   Summ.Geomean2 <- ddply(data, .(SET.name, Month), numcolwise(gm_mean1, na.rm=T));
w3 <- w2 + geom_point(data=Summ.Geomean2, aes(x=factor(Month), y=zTaxa.rich, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
w3

ggsave(file="AllIW_TaxaRich_jitter_1.jpeg", width=7, height=7, dpi=500)     #######


#### Plot 4  ::  Simpson's Diversity Index (1-D) //  zSI.comp [88] {CDF + margin}  ######
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zSI.comp))
data <- data[with(data, order(SET.name, Month, Year)),]
data2 <- subset(data, !is.na(IP_calc))
      # VAR x CDF w/ marginal plots...[****]  
x0 <- ggplot(data=data2, aes(x=zSI.comp, col=SET.name), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=zSI.comp, col=SET.name), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,1), oob=squish) + 
      xlab("Simpson's Diversity (1-D)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot ########
margin.x3 <- ggplot(data=data2, aes(x=zSI.comp, col=SET.name)) + 
  stat_density(aes(y=(..density../100)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.9),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,1), oob=squish) + 
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
jpeg(filename="INV_zSI_CDF+MARG_1.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
      main=textGrob("Distribution Simpson's Diversity over Summer Index Periods", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
######
#######################  PLOT 4B ::  Simpson  ::  Scatter by Month(Year)  ######
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zSI.comp))
data <- data[with(data, order(SET.name, Year, Month)),]
##
w0 <- ggplot(data, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F")) +
      facet_wrap(~SET.name, scales="fixed", ncol=1);
w1 <- w0 + geom_jitter(aes(y=(zSI.comp)), shape=20, na.rm=T, size=2,
      position=position_jitter(w=0.15)) +
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,1), oob=squish) +
      coord_trans() +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov"));
w2 <- w1 + xlab(NULL) + labs(col="Sampling Year") + 
      ylab(bquote("Simpson's Diversity (1-D)")) + 
      ggtitle("Seasonal patterns in Simpson's Diversity Index (1-D)\n");
w2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
   Summ.Geomean2 <- ddply(data, .(SET.name, Month), numcolwise(gm_mean1, na.rm=T));
w3 <- w2 + geom_point(data=Summ.Geomean2, aes(x=factor(Month), y=zSI.comp, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
w3

ggsave(file="AllIW_Simpson_jitter_1.jpeg", width=7, height=7, dpi=500)     #######

#### Plot 5  ::  PMI //  zSI.comp [88] {CDF + margin}  ######
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zPMI.f))
data <- data[with(data, order(SET.name, Month, Year)),]
data2 <- subset(data, !is.na(IP_calc))
      # VAR x CDF w/ marginal plots...[****]
x0 <- ggplot(data=data2, aes(x=zPMI.f, col=SET.name), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=zPMI.f, col=SET.name), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,1), oob=squish) + 
      xlab("Plant-Associated Macroinvertebrate Index Score") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot ########
margin.x3 <- ggplot(data=data2, aes(x=zPMI.f, col=SET.name)) + 
  stat_density(aes(y=(..density../100)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.6),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,1), oob=squish) + 
  scale_y_continuous(labels=percent) +
  scale_color_tableau(breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
  labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
          "Willard Spur (2011-3)"));    ################
margin.x3  ## this is the density plot(s)   + theme( axis.text.y = element_blank())   [marg [L]=1.4
# Use blank placeholder plot :  #######
blankPlot <- ggplot()+geom_blank(aes(1,1))+theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
   panel.border = element_blank(),panel.background = element_blank(),
   axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), 
   axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line = element_blank()   );
#####  Combine the PLots and save to JPEG file...   ######
jpeg(filename="INV_zPMI_CDF+MARG_1.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
      main=textGrob("Distribution PMI over Summer Index Periods", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
######

#### Plot 6  ::  PMI  // Box + MArgin #####
data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zPMI.f))
data <- data[with(data, order(SET.name, Month, Year)),]
data2 <- subset(data, !is.na(IP_calc))

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=(zPMI.f)), outlier.shape=NA, na.rm=T)
  w2 <- w1 + geom_jitter(aes(y=(zPMI.f), col=factor(Year)), 
                         position=position_jitter(width=0.15, height=0.02), na.rm=T);
  w3 <- w2 + xlab(NULL) + 
      ylab(bquote('Plant-Associated Macroinverebrate Index Score')) + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,1), oob=squish,
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=(zPMI.f), col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,1), oob=squish,
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
jpeg(filename="INV_zPMI_Box+Margin_v1.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Macroinvertebrate PMI Score during Index Periods", vjust=1),legendX)
dev.off();    ################


#######  PLot 7 :: Abund vs Richness  :: Scatterplot  ######

data = subset(INV1, (Samp.Type == "OWDN" | Samp.Type == "sweep" | Samp.Type == "sweep5") & 
              !is.na(zTaxa.rich) & !is.na(zAbund))
data <- data[with(data, order(SET.name, Year, Month)),]
data2 <- subset(data, !is.na(IP_calc))

##
w0 <- ggplot(data2, aes(x=zTaxa.rich, col=SET.name)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
w1 <- w0 + geom_point(aes(y=(zAbund)), shape=20, na.rm=T, size=2, 
                      position=position_jitter(width=0.15)) +
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,8100), oob=squish, 
                         breaks=c(0,500,2500,5000,8000),label=comma) +
      coord_trans() +
      scale_x_continuous(limits=c(1,NA), oob=squish);
w2 <- w1 + xlab(bquote('Macroinvertebrate Taxa Richness')) + labs(col="Dataset") + 
      ylab(bquote('Total Macroinvertebrate Abundance (individuals / ' ~m^2*' )')) + 
      ggtitle("Macroinvertebrte abundance as function of taxa richness\n");
w2
   
ggsave(file="AllIW_TaxaRich-x-Abundance_Scatter_1.jpeg", width=7, height=7, dpi=500)     #######



#  + facet_wrap(~SET.name, scales="fixed", ncol=1)









































