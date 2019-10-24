## All IW data -- Toxic Metals Code -- 151217   #####
# data from WChem_IWData-All_151217_t3.xlsx --> WChem7_dat_151217.csv

WChem7_dat1 <- read.csv(file="C:/Users/tobyhooker/Dropbox/REFSTD_R/WChem7_dat_151217.csv",
                        row.names="iden");
WChem7 <- WChem7_dat1
WChem7[!is.na(WChem7$TDS) & WChem7$TDS==204000,]$TDS <- NA ## data fix from 151218


######
names(WChem7)
WCh7.nam <- as.data.frame(names(WChem7))
KeyMetals <- c("AL_D", "AS_D", "CD_D", "CU_D", "FE_D", "MN_D", "NI_D", "PB_D", "SE_D")
KeyMetals.num <- c(4,5,8,11,12,14,15,16,17)
###############################
WChem7.met <- WChem7[,c(77,2,80,18,19:34,61,64,65,94:115,119,120)]
WChem7.metcon <- WChem7.met[,c(1:3,5:20)]

Metals.melt1 <- melt(WChem7.met, id=1:3,value.name="Conc", variable.name="Variable" )
Metals.melt1 <- Metals.melt1[with(Metals.melt1, order(Variable, SET, iden.x )),]
Metals.melt1$MetalName <- Metals.melt1$Variable

Metals.melt2 <-  melt(WChem7.metcon, id=1:3,value.name="Conc", variable.name="Variable" )
Metals.melt2 <- Metals.melt2[with(Metals.melt2, order(Variable, SET, iden.x )),]
Metals.melt2$MetalName <- Metals.melt2$Variable

levels(Metals.melt2$MetalName) <- c("Silver", "Aluminum", "Arsenic","Boron","Barium", "Cadmium",
         "Cobalt", "Chromium", "Copper", "Iron", "Mercury", "Manganese", "Nickel", "Lead", "Selenium",
         "Zinc")

Metals.Conc <- subset(Metals.melt2, (Variable != "Ag_D" & Variable != "B_D" & Variable != "BA_D" & 
            Variable != "CO_D" & Variable != "CR_D" & Variable != "HG_D" & Variable != "ZN_D"),drop=T)
Metals.Conc <- droplevels(Metals.Conc)
names(Metals.Conc)
####  FACETED BOXPLOTs (dataset x 9 Major Metals) [for earlier fig see v1 of Code.file]#####
## Extract the Criteria for each Metal / Dataset and plot w/ the Boxes...
######### Coding up Criteria ########
Metal.Std1 <- WChem7[,c(77,2,80,81:93)]
Met.Std.Summ1 <- ddply(Metal.Std1, .(SET.name), numcolwise(median, na.rm=T));
Met.Std.Summ1$AL <- c(rep(750,4))
Met.Std.Summ1$AS <- c(rep(150,4))
Met.Std.Summ1$FE <- c(rep(1000,4))
Met.Std.Summ1$MN <- c(rep(80,4))
Met.Std.Summ1$SE <- c(rep(4.6,4))
Met.Std.Summ2 <- Met.Std.Summ1[,c(1,15,16,3,7,17,18,9,11,19)]
names(Met.Std.Summ2) <- c("SET.name","Aluminum","Arsenic","Cadmium","Copper","Iron",
                          "Manganese","Nickel","Lead","Selenium")
Met.Std2.melt <- melt(Met.Std.Summ2, id=1, value.name="STD", variable.name="MetalName")
######  Metals.Conc  &&  Met.Std2.melt  #####
v0 <- ggplot(data=Metals.Conc) + theme_bw() + facet_wrap(~MetalName, scales="free", ncol=3) + 
    scale_colour_tableau();
v1 <- v0 + geom_boxplot(aes(x=Metals.Conc$SET.name, y=Metals.Conc$Conc),na.rm=T, outlier.shape=NA) + 
      geom_jitter(aes(x=Metals.Conc$SET.name, y=Metals.Conc$Conc, col=SET.name), 
      na.rm=T, position=position_jitter(w=0.15), size=1) + scale_y_continuous(limits=c(0,NA))  ;
v2 <- v1 + ylab(expression(paste("Metal Concentrations (",mu,"g/L)"))) + xlab(NULL) +
      scale_x_discrete(breaks=NULL) + ggtitle("Dissolved Metal Concentrations\n") +
      theme(panel.margin = unit(0.1, "cm"),
            legend.key.size=unit(0.3, "cm"),legend.margin=unit(0.01, "cm"), 
            legend.position="bottom",
            legend.text = element_text(size=10), legend.direction="horizontal") + 
            labs(col="Dataset") +
         guides(col = guide_legend(override.aes = list(size=2)))
v3 <- v2 + geom_errorbar(data=Met.Std2.melt,width=0.5,
      aes(y=Met.Std2.melt$STD, x=Met.Std2.melt$SET.name,ymax=Met.Std2.melt$STD,
          ymin=Met.Std2.melt$STD), lty=1, color="black");
v3
ggsave(file=paste("ALLIW_MetalsConc_v3D.jpeg", sep=""), width=7.5, height=7, dpi=500);
########

## Total Number of Exceedences (of 15 constituents, max)  #####
data <- subset(WChem7, MET_eval > 0)

w0 <- ggplot(data=data2, aes(x=SET.name)) + theme_bw() + scale_color_tableau();
w1 <- w0 + geom_boxplot(aes(y=MET_crit), na.rm=T)
w2 <- w1 + geom_jitter(aes(y=MET_crit), shape=1, 
                         position=position_jitter(width=0.15, height=0.15), na.rm=T);
w3 <- w2 + xlab(NULL) + ylab("Number of Observed Metals Exceedences") + 
      theme(legend.position="none", plot.margin=unit(c(1,0.1,1,1),"cm")) +
      scale_y_continuous(limits=c(0,NA), oob=squish) + 
      scale_x_discrete(labels=c("Historical \n(2007-11)", "IW Survey (2012)", 
                                "West Desert IWs \n(2014-15)", "Willard Spur \n(2011-3)"));
w3

## How about a density plot instead ? Not for categ. [x], try historgram
data <- data[with(data, order(SET, MET_eval)),]

q <- ggplot(data, aes(x=(MET_crit))) + theme_bw() + 
      geom_histogram(aes(y =(..density..)/100, fill=SET.name),na.rm=T, binwidth=0.5,
                     position="dodge") +
      scale_colour_tableau(name="Dataset", breaks=c("HIST","IW12","REF1415","WSpur"),
      labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
      "Willard Spur (2011-3)"));
q1 <- q + ylab("Proportion of data observed") + xlab("Number of Exceedences") + 
      scale_x_continuous(breaks=seq(0,6,1), limits=c(NA,6), oob=squish) + 
      ggtitle("Number of Observed Metals Exceendences\n") +
      theme(legend.key.size=unit(1, "cm"),legend.margin=unit(0, "cm"), legend.position=c(0.8,0.8))
q1
ggsave(file="IWAll_Metals Exceed_Density_2.jpeg", width=6, height=6, dpi=500)
## don't really like this...think about how to better display this idea...

