## Reference Standard Sites - Code for Figures  -- 151213

##  From SAV...code and others...



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
# [7] SAV x IP_calc [SET] Facets (Lines = geom_path)
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## IP != NA & TRT = control

q0 <- ggplot(data3, aes(x=factor(IP_calc), col=factor(Year), group=STORET)) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_path(aes(y=SAV.tot, group=STORET), na.rm=T) + geom_point(aes(y=SAV.tot),size=0.8, 
      col="black", position=position_jitter(width=0.01), na.rm=T) + scale_y_continuous(limits=c(0,100), 
      breaks=seq(0,100,25)) + scale_x_discrete(labels=c("IP #1: mid-summer", "IP #2: early-autumn")) +
      theme(legend.key.size=unit(0.7, "cm"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Total SAV cover (%)") + 
      ggtitle("Seasonal patterns in SAV cover over IW Index Periods\n");
q2
ggsave(file="AllIW_SAVxIP_7.jpeg", width=7, height=7, dpi=500)   ## Better (*now*)
############################################################

############################################################
#####   C D F's  w/  M A R G I N A L   D E N S I T Y  ######
############################################################
###  PLOT 4  ### COMBO PLOT [2] :: CDF (by SET) w/ Marginal Density Pot (x) ######
data2 <- subset(SAVdat.0, TRT != "nocarp") ## data w/o 'nocarp' treatment

x0 <- ggplot(data=data2, aes(x=SAV.tot, col=SET)) + theme_bw() + 
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

margin.x2 <- ggplot(data=data2, aes(x=SAV.tot, col=SET)) + geom_density(aes(col=SET), na.rm=T, alpha=0.5) +
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
############

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










