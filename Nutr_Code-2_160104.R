## WChem Nutrients:

# data ==>  WChem7
data4 <- WChem7
summary(WChem7$TIN)
summary(WChem7$NH4_T)
summary(WChem7$NO3_T)
#  data4 <- data4[with(data4, order(SET.name, Date2)),]
q0 <- ggplot(data4, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=TIN, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,10), oob=squish) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total inorganic N (mg/L)") + 
      ggtitle("Seasonal patterns in Total Inorganic N over the growing season\n");
q2
      ggsave(file="AllIW_TIN-x-DOY_1.jpeg", width=7, height=7, dpi=500)   ####

## Plot 2:  NH4 vs. NO3 
q0 <- ggplot(data4, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=NH4_T, col=factor(Year)), shape=5, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      geom_jitter(aes(y=NO3_T, col=factor(Year)), shape=3, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) +   
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,10), oob=squish) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total inorganic N (mg/L)") + 
      ggtitle("Seasonal patterns in Total Inorganic N species over the growing season\n");
q2
      ggsave(file="AllIW_TIN-x-DOY_2.jpeg", width=7, height=7, dpi=500)   ####

## Plot 3:  NH4 vs. NO3  - w/o Factor(Year) [shape 5 = diamond [NH4] / shape 3 = plus [NO3]]
q0 <- ggplot(data4, aes(x=factor(Month))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="free_y", ncol=1);
q1 <- q0 + geom_jitter(aes(y=NH4_T), shape=5, na.rm=T, size=2, 
            position=position_jitter(w=0.15)) + 
      geom_jitter(aes(y=NO3_T), shape=3, na.rm=T, size=2, 
                  position=position_jitter(w=0.15)) +   
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous() +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total inorganic N (mg/L)") + 
      ggtitle("Seasonal patterns in Total Inorganic N species over the growing season\n");
q2
      ggsave(file="AllIW_TIN-x-DOY_3.jpeg", width=7, height=7, dpi=500)   ####
# limits=c(0,10), oob=squish

## Plot 4:  Melt: NH4 vs. NO3  - w/o Factor(Year) [shape 5 = diamond [NH4] / shape 3 = plus [NO3]]
TIN.dat <- WChem7[,c(80,77,3,70,73,72,74,75,36,38)]
TIN.dat <- TIN.dat[with(TIN.dat, order(SET.name, Month, MLID)),]
TIN.melt <- melt(TIN.dat, id=1:8, value.name="Conc", variable.nam="Nform")
data5 <- TIN.melt

q0 <- ggplot(data5, aes(x=factor(Month), shape=Nform)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1)+
      scale_shape_manual(values=c(5,3), labels=c("NH4", "NO3"));
q1 <- q0 + geom_jitter(aes(y=Conc, shape=Nform), na.rm=T, size=2, 
            position=position_jitter(w=0.18)) + 
            theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,10), oob=squish) +
      scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(shape="Inorganic \n  N form") + ylab("Total inorganic N (mg/L)") + 
      ggtitle("Seasonal patterns in Total Inorganic N species over the growing season\n");
q2
      ggsave(file="AllIW_NH4-NO3-x-DOY_4.jpeg", width=7, height=7, dpi=500)   ## XX ####

## Plot 5:  TN x Dataset  (Bar w/ marginal density)  ######
data4 <- WChem7
data2 = subset(data4, !is.na(TN) & !is.na(IP_calc))

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=TN), na.rm=T, outlier.shape=NA)
  w2 <- w1 + geom_jitter(aes(y=TN, col=factor(Year)), 
                         position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("Total Nitrogen (mg/L)") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,9), oob=squish, breaks=c(0,0.5,1,2,4,6,8,10),
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=TN, col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,9), oob=squish, breaks=c(0,0.5,1,2,4,6,8,10), 
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
jpeg(filename="TN_Box+Margin_v2X.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Total N concentration in Water Column", vjust=1),legendX)
dev.off()


## Plot 6 TP x Dataset  (Bar w/ marginal density)   #####
data4 <- WChem7
data2 = subset(data4, !is.na(TP) & !is.na(IP_calc))

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=TP), na.rm=T, outlier.shape=NA)
  w2 <- w1 + geom_jitter(aes(y=TP, col=factor(Year)), 
                         position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("Total Phosphorus (mg/L)") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,1.6), oob=squish, breaks=c(0,0.2,0.5,1.0,1.5),
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=TP, col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,1.6), oob=squish, breaks=c(0,0.2,0.5,1.0,1.5),
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
jpeg(filename="TP_Box+Margin_v2X.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Total P concentration in Water Column", vjust=1),legendX)
dev.off();    ################


## PLot 7 TN:TP Ratios... ** 7A ** N:P x month [jitter] ######
data4 <- WChem7
data2 = subset(data4, !is.na(TN.TP))
data4 <- data2
data4 <- data4[with(data4, order(SET.name, Month, Year)),]
      x=factor(data4$Month)
      y=data4$TN.TP
q0 <- ggplot(data4, aes(x=x, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=y, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,43), oob=squish, breaks=c(0,10,20,30,40)) +
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total N to Total P ratio") + 
      ggtitle("Seasonal patterns in TN:TP ratios over the growing season\n");
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=TN.TP, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_TNTP_jitter_1.jpeg", width=7, height=7, dpi=500)   #######

## Plot 7 TN:TP Ratios...  **  7B  **  N:P Box w/ margins..#####
data4 <- WChem7
data2 = subset(data4, (!is.na(TN.TP) & !is.na(IP_calc)))
data2 <- data2[with(data2, order(SET.name, Month, Year)),]

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=TN.TP), na.rm=T, outlier.shape=NA)
  w2 <- w1 + geom_jitter(aes(y=TN.TP, col=factor(Year)), 
                         position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("TN:TP ratio (g/g)") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,100), oob=squish, 
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=TN.TP, col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,100), oob=squish, 
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
jpeg(filename="TNTP_Box+Margin_v2.jpeg", width=7.5, height=6, units="in", res=500)

grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("TN:TP ratio in Water Column", vjust=1),legendX)
dev.off();    ################

# Aq. Metab --------------------------------------------------------------
##################################################################################
###### I N D I C A T O R S  O F  A Q U A T I C  M E T A B O L I S M  #############
##################################################################################
## Plot 8 :: CHLA  Box w/ Margins  ########
data4 <- WChem7
data2 = subset(data4, !is.na(CHLa_X) & !is.na(IP_calc))
data2 <- data2[with(data2, order(SET.name, Month, Year)),]

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=CHLa_X), na.rm=T, outlier.shape=NA)
  w2 <- w1 + geom_jitter(aes(y=CHLa_X, col=factor(Year)), 
                         position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("Chlorophyll-a conc. (ug/L)") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0.5,102), oob=squish, breaks=c(0,10,25,50,75,100), 
                         labels=comma) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=CHLa_X, col=SET.name)) + 
    stat_density(aes(y=(..count..)), position="identity", geom="line",na.rm=T) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),  
                        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0.5,102), oob=squish, breaks=c(0,10,25,50,75,100), 
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
jpeg(filename="CHLa_Box+Margin_v2.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Chlorophyll-a in Water Column", vjust=1),legendX)
dev.off();    ################

## Plot 9 :: CHLA - Jitter by Month   ######
data4 <- WChem7
data4 = subset(data4, !is.na(TN.TP))
data4 <- data4[with(data4, order(SET.name, Month, Year)),]
      x=factor(data4$Month)
      y=data4$CHLa_X
q0 <- ggplot(data4, aes(x=x, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=y, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0.4,150), oob=squish, breaks=c(0.5,5,20,50,120)) + 
      coord_trans(y="log10") +
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Chlorophyll-a Concentration (ug/L)") + 
      ggtitle("Seasonal patterns in CHL-a choncentration over the growing season\n");
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=CHLa_X, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_CHLA_jitter_1.jpeg", width=7, height=7, dpi=500)   #######

## Plot 10  ::  DO -- Jitter by Month    ######
## fix data
WChem7[(WChem7$DO.c > 50  & !is.na(WChem7$DO.c)),c(66,67)]
WChem7[(WChem7$DO.c > 50  & !is.na(WChem7$DO.c)),66] <- c(11.06,11.95,10.05)
WChem7[(WChem7$DO.c > 50  & !is.na(WChem7$DO.c)),67] <- c(143.6,155.2,133.4)
WChem7[(WChem7$DO.p < 5  & !is.na(WChem7$DO.p)),c(66,67)]
WChem7[(WChem7$DO.c < 0.1  & !is.na(WChem7$DO.c)),c(66,67)] <- NA
#########
data4 <- WChem7
data4 = subset(data4, !is.na(DO.c) )
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
      x=factor(data4$Month)
      y=data4$DO.c
q0 <- ggplot(data4, aes(x=x, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=y, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,22), oob=squish, breaks=c(0,5,10,15,20)) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Dissolved Oxygen Concentration (mg/L)") + 
      ggtitle("Seasonal patterns in DO concentration over the growing season\n")+
      geom_hline(yintercept=5,lty=2, col="black") + geom_hline(yintercept=3, lty=2, col="red");
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=DO.c, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_DO.c_jitter_2.jpeg", width=7, height=7, dpi=500)   #######

## PLot 11 :: DO.p (as above)    ######
data4 <- WChem7
data4 = subset(data4, !is.na(DO.p))
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
      x=factor(data4$Month)
      y=data4$DO.p
q0 <- ggplot(data4, aes(x=x, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=y, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,220), oob=squish, breaks=seq(0,200,50)) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Dissolved Oxygen (% saturation)") + 
      ggtitle("Seasonal patterns in DO (% sat) over the growing season\n") +
      geom_hline(yintercept=100,lty=2, col="black");
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=DO.p, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_DO.p_jitter_1B.jpeg", width=7, height=7, dpi=500)   #######

## PLot 12 :: pH (as above)    ######
data4 <- WChem7
## fix data
WChem7$pH.fld[WChem7$pH.fld > 11 & !is.na(WChem7$pH.fld)] <- NA
##
data4 = subset(data4, !is.na(pH.fld))
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
      x=factor(data4$Month)
      y=data4$pH.fld
q0 <- ggplot(data4, aes(x=x, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=y, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(NA,NA), oob=squish) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Water Column pH") + 
      ggtitle("Seasonal patterns in pH over the growing season\n") +
      geom_hline(yintercept=9,lty=2, col="black");
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=pH.fld, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_pH_jitter_1.jpeg", width=7, height=7, dpi=500)   #######


# Jan 2016 -- Newer Plots ------------------------------------------------

############################################  160102   #######
####  160102 ## TIN:TN ##   ####
data4 <- WChem3
data2 = subset(data4, !is.na(TIN.TN))
data2 <- data2[with(data2, order(SET.name, Month, Year)),]

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_manual(values=c("#1F77B4",
        "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22",
        "#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
  w1 <- w0 + geom_boxplot(aes(y=TIN.TN), na.rm=T,outlier.shape=NA)
  w2 <- w1 + geom_jitter(aes(y=TIN.TN, col=factor(Year)), 
                         position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("Inorganic fraction of Total N") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,0.8), oob=squish,  
                         labels=percent) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  #####
  margin.x2 <- ggplot(data=data2, aes(x=TIN.TN, col=SET.name)) + 
    stat_density(aes(y=(..count..)), position="identity", geom="line",na.rm=T) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),  
                        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
      scale_x_continuous(limits=c(0,0.8), oob=squish,  
                         labels=percent) + 
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
jpeg(filename="TIN-TN_Box+Margin_v1.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, 
             widths=c(5,1.4), heights=c(5,0.2), 
             main=textGrob("Total Inorganic N as fraction of Total N", vjust=1),legendX)
dev.off();    ################

######  TIN:TN x Month (jitter ) ######
data4 = subset(data4, !is.na(TIN.TN))
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
q0 <- ggplot(data4, aes(x=factor(Month), y=TIN.TN, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=TIN.TN, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,0.76), oob=squish, breaks=seq(0,0.75,0.25), labels=percent) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Inorganic fraction of Total N") + 
      ggtitle("Total Inorganic N as fraction of Total N\n") ;
#+      geom_hline(yintercept=0.5,lty=2, col="black");
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=TIN.TN, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_TINTN_jitter_1.jpeg", width=7, height=7, dpi=500)   #######


#######  Scatter Plot :: TIN vs. TN #######
data6 <- subset(WChem3, !is.na(TN) & !is.na(TIN) & !is.na(IP_calc))
v0 <- ggplot(data6, aes(x=TN, y=TIN.TN)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
v1 <- v0 + geom_point(na.rm=T)
v1

v2 <- v0 + geom_point(aes(col=SET.name), na.rm=T) 
v2

# +  scale_x_sqrt() +  scale_y_sqrt()
data6 <- subset(WChem3, !is.na(TN) & !is.na(TIN) & !is.na(IP_calc))
v0 <- ggplot(data6, aes(x=TN, y=TIN)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
v1x <- v0 + geom_point(na.rm=T)
v1x

v2x <- v0 + geom_point(aes(col=SET.name), na.rm=T) + xlim(c(0,15)) + ylim(c(0,15));
v2x

v3x <- v0 + geom_point(aes(col=SET.name), na.rm=T) + xlim(c(0,10)) + ylim(c(0,10));
v3x

v4x <- v0 + geom_point(aes(col=SET.name), na.rm=T) + xlim(c(0,6)) + ylim(c(0,6));
v4x

v5x <- v0 + geom_point(aes(col=SET.name), na.rm=T) + xlim(c(0,2.5)) + ylim(c(0,2.5));
v5x

v6x <- v0 + geom_point(aes(col=SET.name), na.rm=T) + xlim(c(0,1.1)) + ylim(c(0,1.1));
v6x

#######  Scatter Plot :: TIN vs. TON #######
data6 <- subset(WChem3, !is.na(TN) & !is.na(TON) & !is.na(IP_calc))
v0 <- ggplot(data6, aes(x=TN, y=TON)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
v1 <- v0 + geom_point(na.rm=T)
v1

v2 <- v0 + geom_point(aes(col=SET.name), na.rm=T) 
v2


##############################################################################
######  SUSPENDED DETRITUS  ::  TSS and TVS   ################################
##############################################################################
data6 <- WChem3
data6 <- subset(WChem3, !is.na(T_TSS))

######  T_TSS x Month (jitter ) ######
data4 <- data6
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
q0 <- ggplot(data4, aes(x=factor(Month), y=T_TSS, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=T_TSS, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,400), oob=squish ) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total Suspended Solids (mg/L)") + 
      ggtitle("Total Suspended Solids\n") ;
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=T_TSS, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_TSS_jitter_1.jpeg", width=7, height=7, dpi=500)   #######

######  T_TVS x Month (jitter ) ######
data6 <- subset(WChem3, !is.na(T_TVS))
data4 <- data6
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
q0 <- ggplot(data4, aes(x=factor(Month), y=T_TVS, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=T_TVS, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,120), oob=squish ) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total Volatile Solids (mg/L)") + 
      ggtitle("Total Volatile Solids (Loss on Ignition)\n") ;
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=T_TVS, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_TVS_jitter_1.jpeg", width=7, height=7, dpi=500)   #######


######  TVS:TSS Ratio x Month (jitter ) ######
data6 <- subset(WChem3, !is.na(TVS.TSS))
data4 <- data6
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
q0 <- ggplot(data4, aes(x=factor(Month), y=TVS.TSS, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=TVS.TSS, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,0.85), oob=squish, labels=percent, breaks=seq(0,0.8,0.2)) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("% OM of Suspended Solids (g/g)") + 
      ggtitle("TVS:TSS Ratio:  % OM of Suspended Solids\n") ;
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=TVS.TSS, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_TVS-TSSratio_jitter_1.jpeg", width=7, height=7, dpi=500)   #######

#######  Scatter Plot :: TIN vs. TON #######
data6 <- subset(WChem3, !is.na(T_TVS) & !is.na(T_TSS))
v0 <- ggplot(data6, aes(x=T_TSS, y=T_TVS)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
v1 <- v0 + geom_point(na.rm=T)
v1

v2 <- v0 + geom_point(aes(col=SET.name), na.rm=T) 
v2
#
data6 <- subset(WChem3, !is.na(TVS.TSS) & !is.na(T_TSS))
v0 <- ggplot(data6, aes(x=T_TSS, y=TVS.TSS)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
v1 <- v0 + geom_point(na.rm=T)
v1

v2 <- v0 + geom_point(aes(col=SET.name), na.rm=T) 
v2
#####

##############  TN  ::  CDF w/ Marginal Density  ###############
# VAR x CDF w/ marginal plots...[****]
data2 = subset(WChem3, !is.na(IP_calc))
x0 <- ggplot(data=data2, aes(col=SET), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.8, 0.17), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=TN, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,13), oob=squish, breaks=c(0,1,2,4,6,8,10,12)) + 
      xlab("Total N (mg N/L)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot   ####
margin.x3 <- ggplot(data=data2, aes(x=TN, col=SET)) + 
  stat_density(aes(y=(..density../100)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.4),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,13), oob=squish, breaks=c(0,1,2,4,6,8,10,12)) + 
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
jpeg(filename="AllIW_TN_xSETgrop_CDF+MARG_6.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
      main=textGrob("Distribution of Total N During Summer Index Periods", vjust=1),
             left=textGrob("Proportion of data  below x-value", rot=90, vjust=1))
dev.off()
######
##############  TP  ::  CDF w/ Marginal Density  ###############
# VAR x CDF w/ marginal plots...[****]
data2 = subset(WChem3, !is.na(IP_calc))
x0 <- ggplot(data=data2, aes(col=SET), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.8, 0.17), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=TP, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,1.55), oob=squish, breaks=c(0,0.2,0.5,1,1.5)) + 
      xlab("Total P (mg P/L)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot   ####
margin.x3 <- ggplot(data=data2, aes(x=TP, col=SET)) + 
  stat_density(aes(y=(..density../10000)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.4),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,1.55), oob=squish, breaks=c(0,0.2,0.5,1,1.5)) + 
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
jpeg(filename="AllIW_TP_xSETgrop_CDF+MARG_6.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
      main=textGrob("Distribution of Total P During Summer Index Periods", vjust=1),
             left=textGrob("Proportion of data  below x-value", rot=90, vjust=1))
dev.off()
######


######  TN x Month (jitter ) ######
data6 <- subset(WChem3, !is.na(TN))
data4 <- data6
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
q0 <- ggplot(data4, aes(x=factor(Month), y=TN, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=TN, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,8), oob=squish) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total N (mg N/L)") + 
      ggtitle("Water Column Total N Concenration") ;
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=TN, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_TN_jitter_1B.jpeg", width=7, height=7, dpi=500)   #######

######  TP x Month (jitter ) ######
data6 <- subset(WChem3, !is.na(TP))
data4 <- data6
data4 <- data4[with(data4, order(SET.name, Date2, Month)),]
q0 <- ggplot(data4, aes(x=factor(Month), y=TP, col=factor(Year))) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
         "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"))+
      facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=TP, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,1), oob=squish) + 
      scale_x_discrete(labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total P (mg P/L)") + 
      ggtitle("Water Column Total P Concenration") ;
q2
   gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
   Summ.Geomean1 <- ddply(data4, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
v5 <- q2 +  geom_point(data=Summ.Geomean1, aes(x=factor(Month), y=TP, col=NULL), 
                       shape=5, size=2, col="black", na.rm=T);
v5
ggsave(file="AllIW_TP_jitter_1.jpeg", width=7, height=7, dpi=500)   #######

####
#######  Scatter Plot :: DP vs. TP #######
data6 <- subset(WChem3, !is.na(DP) & !is.na(TP) & !is.na(IP_calc))
v0 <- ggplot(data6, aes(x=TP, y=DP)) + theme_bw() +
      scale_color_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B",
      "#E377C2","#7F7F7F","#BCBD22","#17BECF","#B10318","#DBA13A","#309343","#32A251","#FF7F0F"));
v1 <- v0 + geom_point(na.rm=T) + geom_smooth()
v1

v2 <- v0 + geom_point(aes(col=SET.name), na.rm=T) 
v2

#####################################################
#############  CHLA ::  CDF w/ Marginal Density  ###############
# VAR x CDF w/ marginal plots...[****]
data2 = subset(WChem3, (!is.na(CHLa_X) & !is.na(IP_calc)))
x0 <- ggplot(data=data2, aes(col=SET), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.8, 0.17), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("HIST", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=CHLa_X, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + 
      scale_x_continuous(limits=c(0,122), oob=squish, breaks=c(0,5,20,50,100,75,120)) + 
      xlab("Chlorophyll-a Concentration (ug/L)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot   ####
margin.x3 <- ggplot(data=data2, aes(x=CHLa_X, col=SET)) + 
  stat_density(aes(y=(..density..)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.92),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,122), oob=squish, breaks=c(0,5,20,50,100,75,120)) + 
  scale_y_continuous() +
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
jpeg(filename="AllIW_CHLa_X_xSETgrop_CDF+MARG_6b.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
      main=textGrob("Distribution of Chlorophyll-a During Summer Index Periods", vjust=1),
             left=textGrob("Proportion of data  below x-value", rot=90, vjust=1))
dev.off()
######
