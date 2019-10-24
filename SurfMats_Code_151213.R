## Surface Mats Cover -- 151213

### SAV and field cover metrics -- response Vars  ########
# SAV.tot  [x]
# Pond_mat [x]
# Z.All   [doesn't look too useful] [x]
# SAV_cond
# SAV.Ht
# FAV_COV (??)
# BareCov (as complement of SAV.tot)...

data2 <- subset(data, !is.na(IP_calc))  # ok, should probably remove the nocarp Exp obs
data3 <- subset(data, TRT != "nocarp" & !is.na(IP_calc)) ## works fine [better]
data4 <- subset(data, TRT != "nocarp")  # Nocarp TRT removed...
data5X  # summary of SAV Index score calculations

data6 <- data4
data6$SurfMat <-rowSums(cbind(data6$Pond_mat, data6$FAV_COV), na.rm=T)

## save working SAV / Surface mat data files
SAVdat_dat1 <- data
SAVdat_dat2 <- data2
SAVdat_dat3 <- data3
SAVdat_dat4 <- data4
SAVdat_dat5X <- data5X
SAVdat_dat6 <- data6

# [#1: SurfMat ] Pond_mat x DOY | [SET]Facets (Lines = geom_path) [NOPE]######
  data3 <- data3[with(data3, order(SET.name, Date2)),]
q0 <- ggplot(data3, aes(x=DOY, col=factor(Year), group=STORET)) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 +  geom_path(aes(y=Pond_mat, group=STORET), na.rm=T) + 
            geom_point(aes(y=Pond_mat),size=0.8, col="black") + 
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_continuous(breaks=c(166,182,213,244,265), labels=c("mid-June", "July", "Aug", "Sept", 
      "lat-Sept")) + 
      theme(legend.key.size=unit(0.7, "cm"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Surface Mat Cover (%)") + 
      ggtitle("Seasonal patterns in Surface Mat cover during the growing season\n");
q2
ggsave(file="AllIW_SurfMatxDOY_1.jpeg", width=7, height=7, dpi=500) ## Not really enlightening

### [#2: SurfMat ] Try one w/ the Jitter instead  :::  Looks okay #####
#  data3 <- data3[with(data3, order(SET.name, Date2)),]
  data4
q0 <- ggplot(data4, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=Pond_mat, col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Surface Mat Cover (%)") + 
      ggtitle("Seasonal patterns in Surface Mat cover during the growing season\n");
q2
ggsave(file="AllIW_SurfMatxDOY_2.jpeg", width=7, height=7, dpi=500)   ####

### [#3: SurfMat ] #### BOXPLOT + MARG-DENSITY ** NO ** #####
###   data3
  w0 <- ggplot(data=data3, aes(x=SET.name)) + theme_bw() + scale_color_tableau()
  w1 <- w0 + geom_boxplot(aes(y=Pond_mat), na.rm=T)
  w2 <- w1 + geom_jitter(aes(y=Pond_mat, col=factor(Year)), position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("Surface Mat Cover (%)") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous() + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  ####
  margin.x2 <- ggplot(data=data3, aes(x=Pond_mat, col=SET.name)) + 
    stat_density(aes(y=(..density..)/100), position="identity", geom="line",na.rm=T) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + scale_x_continuous(limits=c(0,1)) + 
      scale_color_tableau(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
            "Willard Spur (2011-3)")) + coord_flip();
  margin.x2  ## this is the density plot(s)

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
jpeg(filename="SurfMay_Box+Margin_v1.jpeg", width=7.5, height=6, units="in", res=500)
  grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, widths=c(5,1.4),
    heights=c(5,0.2), main=textGrob("Surface Mat Cover", 
                                    vjust=1),legendX)
dev.off()
########

#### WATER DEPTH ####
# [#4: water depth x DOY | [SET]Facets (Lines = geom_path) [NOPE]######
q0 <- ggplot(data3, aes(x=DOY, col=factor(Year), group=STORET)) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 +  geom_path(aes(y=Z.All, group=STORET), na.rm=T) + 
            geom_point(aes(y=Z.All),size=0.8, col="black", na.rm=T) + 
      scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
      scale_x_continuous(breaks=c(166,182,213,244,265), labels=c("mid-June", "July", "Aug", "Sept", 
      "lat-Sept")) + 
      theme(legend.key.size=unit(0.7, "cm"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Water Depth (cm)") + 
      ggtitle("Seasonal patterns in Water Depth during the growing season\n");
q2
ggsave(file="AllIW_WaterZxDOY_4.jpeg", width=7, height=7, dpi=500) #
### Don't think this is that useful...(maybe try a CDF ?)

##[#5: Water Depth  CDF  (by SET) w/ Marginal Density Pot (x) ######
data3
x0 <- ggplot(data=data3, aes(x=Z.All, col=SET), na.rm=T) + theme_bw() + 
      theme(plot.margin = unit(c(0,1,0.5,0.5),"cm"), legend.position=c(0.17, 0.86), 
      legend.margin=unit(0, "cm"), legend.key.size=unit(0.5, "cm")) + 
      guides(color=guide_legend(override.aes=list(size=1.2))) +
      scale_colour_tableau(name="Dataset", breaks=c("Hist", "IW12", "REF1415", "WSpur"), 
      labels=c("Historical (2007-11)", "IW Survey (2012)",
      "West Desert IWs (2014-15)", "Willard Spur (2011-3)"));
x1 <- x0 + stat_ecdf(aes(x=Z.All, col=SET), na.rm=T) + scale_y_continuous(labels=percent) + 
      labs(y=NULL) + scale_x_continuous(limits=c(0,125), breaks=seq(0,125,25)) + 
      xlab("Water Depth (cm)") +
      geom_hline(yintercept=c(0.25, 0.5, 0.75), lty=2, col="black");
x1   ## this is the CDF plot

margin.x3 <- ggplot(data=data3, aes(x=Z.All, col=SET)) + 
  stat_density(aes(y=(..density..)),position="identity", geom="line",na.rm=T) + theme_bw() + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
        plot.margin = unit(c(0.5,1,0,0.9),"cm"), legend.position="none") + 
  labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,125), breaks=seq(0,125,25)) + 
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
jpeg(filename="AllIw_wat-z_xSETgrop_CDF+MARG_5.jpeg", width=7.5, height=6, units="in", res=500)
grid.arrange(margin.x3, x1, blankPlot, ncol=1, nrow=2, widths=4, heights=c(1.3,5),
             main=textGrob("Cumulative Distribution of Pond Water Depths", vjust=1),
             left=textGrob("Proportion of data occuring below x-value", rot=90, vjust=1))
dev.off()
####

### [#6: Pond_mat + FAV_cov ==> SurfMat] Jitter x Month #####
  data6
q0 <- ggplot(data6, aes(x=factor(Month), col=factor(Year))) + theme_bw() +
      scale_color_tableau() + facet_wrap(~SET.name, scales="fixed", ncol=1);
q1 <- q0 + geom_jitter(aes(y=(SurfMat), col=factor(Year)), shape=20, na.rm=T, size=2, 
      position=position_jitter(w=0.15)) + 
      theme(legend.key.size=unit(0.7, "cm")) +
      scale_y_continuous(limits=c(0,110), breaks=seq(0,100,25), oob=squish) +
      scale_x_discrete(labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"));
q2 <- q1 + xlab(NULL) + labs(col="Sampling Year") + ylab("Surface Algal Mat + FAV Cover (%)") + 
      ggtitle("Seasonal patterns in Surface Mat cover during the growing season\n");
q2
ggsave(file="AllIW_Pond+FAVMatxDOY_6.jpeg", width=7, height=7, dpi=500)   ####

### [#7: Pond_mat vs. FAV_cov  ] SCatterplot
z0 <- ggplot(data6, aes(x=Pond_mat, y=FAV_COV), na.rm=T) + theme_bw() + scale_color_tableau()
z1 <- z0 + geom_point(na.rm=T) + geom_smooth(method="lm", na.rm=T)
# Linear modeling:
zz <- lm(FAV_COV ~ Pond_mat, data=data6,na.rm=T)
  summary(zz)
##  model is sig (***), but low power (0.166 adjr^2); df=237...Low values co-occur, occasional high 
### values increase slopw

### [#8: SurfMat  ==  Pond_mat + FAV_cov  ] #### BOXPLOT + MARG-DENSITY #####
###   data6
  w0 <- ggplot(data=data6, aes(x=SET.name)) + theme_bw() + scale_color_tableau()
  w1 <- w0 + geom_boxplot(aes(y=SurfMat), na.rm=T)
  w2 <- w1 + geom_jitter(aes(y=SurfMat, col=factor(Year)), position=position_jitter(width=0.15), na.rm=T);
  w3 <- w2 + xlab(NULL) + ylab("Surface Mat Cover (%)") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,110), breaks=seq(0,100,25), oob=squish) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
  w3
## marginal plot  ####
  margin.x2 <- ggplot(data=data6, aes(x=SurfMat, col=SET.name)) + 
    stat_density(aes(y=(..density..)), position="identity", geom="line",na.rm=T, alpha=0.8) +
      theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                         axis.ticks.y=element_blank(), axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1.6,0),"cm")) + 
      labs(x=NULL, y=NULL, col="") + 
  scale_x_continuous(limits=c(0,110), breaks=seq(0,100,25), oob=squish) + 
      scale_color_tableau(labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
            "Willard Spur (2011-3)")) + coord_flip();
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
jpeg(filename="SurfMay_Box+Margin_v1.jpeg", width=7.5, height=6, units="in", res=500)
  grid.arrange(w3, margin.x2+theme(legend.position="none"),  blankPlot, ncol=2, nrow=2, widths=c(5,1.4),
    heights=c(5,0.2), main=textGrob("Surface Mat Cover", 
                                    vjust=1),legendX)
dev.off()
########
ggplot(data=data6, aes(x=SurfMat, col=SET.name))+stat_density(position="identity", geom="line") + 
  scale_x_continuous(limits=c(0,110), breaks=seq(0,100,25), oob=squish) +
  coord_flip();




