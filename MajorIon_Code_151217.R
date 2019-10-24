## Major Ions -- 151217 ---- ######
## data :: 

MajorIons <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/MajorIons_dat_151217.csv") # Updated !!
MajorIons2 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/MajorIons_dat_151217_v2.csv")

MajorIons <- MajorIons[,-12]
Maj.dat <- MajorIons2

Major.var <- data.frame() # reports class of all vars
for (i in 1:length(names(Maj.dat))){
  class_i <- class(Maj.dat[,i])
  name_i <- names(Maj.dat[i])
  num.NAs <- sum(is.na(Maj.dat[,i]))
  Major.var <- rbind(Major.var, data.frame(i, name_i, class_i, num.NAs))  }

## data rows:  5:11, 14:16
length(unique(Maj.dat$MLID))

### Functions ######
gm_mean1 <- function(x, na.rm=TRUE){ exp(sum(log(x[x>0]), na.rm=na.rm) / length(x))  };
cnt <- function(x) count(x);
nmissing <- function(x) sum(is.na(x));
nDiff <- function(x) (length(x)-nmissing(x));
gm_mean2 <- function(x, na.rm=T, zero.propagate=FALSE) 
      { if(any(x<0, na.rm=T)) {   return(NaN) }
      if(zero.propagate) { if(any(x==0, na.rm=T)){ return(1) }
      exp(mean(log(x), na.rm=na.rm))  }   
      else { exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))   }   }
Summ.Geomean1 <- ddply(data, .(SET.name, Year, Month), numcolwise(gm_mean1, na.rm=T));
######

## Take a look at distributions....#######
data <- Maj.dat
varlist <- c(5:8,9:11,12:16)
k = 5
for (k in varlist) {
m <- ggplot(data, aes(x=data[,k],col=SET)) + stat_ecdf(aes(col=SET)) +
      theme_bw() + scale_colour_tableau(name="Dataset", breaks=c("HIST","IW12","REF1415","WSpur"),
      labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
      "Willard Spur (2011-3)"));
m2 <- m + ylab("Proportion of data occuring below x-value") + xlab(names(data[k])) + 
    geom_hline(yintercept=c(0.25,0.50,0.75), lty=2, col="black") +
    ggtitle("CDF of Major Ions") + theme(legend.position=c(0.83, 0.18));

ggsave(file=paste("IWAll_CDF_",(names(data[k])),"-1.jpeg", sep=""),
         width=7.5, height=6, dpi=500)
print(m2)
dev.off()    }
#####
## Same as abvoe -- density plots   ##############
for (k in varlist) {
m <- ggplot(data, aes(x=data[,k],col=SET)) + theme_bw() + 
      stat_density(aes(y=(..density..),col=SET), position="identity", geom="line",na.rm=T) +
      scale_colour_tableau(name="Dataset", breaks=c("HIST","IW12","REF1415","WSpur"),
      labels=c("Historical (2007-11)", "IW Survey (2012)", "West Desert IWs (2014-15)", 
      "Willard Spur (2011-3)"));
m2 <- m + ylab("Proportion of data observed") + xlab(names(data[k])) + 
      ggtitle("CDF of Major Ions") + theme(legend.position=c(0.85, 0.8));

ggsave(file=paste("IWAll_Density_",(names(data[k])),"-2.jpeg", sep=""),
         width=6, height=6, dpi=500)
print(m2)
dev.off()    }
#####

## calculate geometric means (all data appear to have long tails; see density plots below) ####
MajIon.SiteGeoMn2 <- ddply(Maj.dat, .(SET.name, MLID), numcolwise(gm_mean1, na.rm=T));

# export data...
write.csv(MajIon.SiteGeoMn2, file="MajorIon_SiteMeans_151217_v2.csv", row.names=F)

## pull out cases w/ missing SO4... (IW12)
Maj.dat2 <- subset(Maj.dat, (!is.na(CL_T) & !is.na(SO4_T) & !is.na(T.Alk) & !is.na(NA_D)))
summary(Maj.dat2)

MajIon.SiteGeoMn3 <- ddply(Maj.dat2, .(SET.name, MLID), numcolwise(gm_mean1, na.rm=T));

write.csv(MajIon.SiteGeoMn3, file="MajorIon_SiteMeans_151230_v3.csv", row.names=F)


################ Updated to include pH.F (need to calculate CO3 contribution !!)
## data entered into GW_Chart and plotted !!
## also, developed (redneck) model to estimate CO3 conc. from HCO3 and pH.  (Kept Temp constant at 15C).

##################################################################################
##################################################################################
### S A L I N I T Y ::  T D S = EC25  ############################################
##################################################################################
#  WChem7X <- WChem7 <- WChem7X
WChem7[!is.na(WChem7$TDS) & WChem7$TDS==204000,]$TDS <- NA
data <- WChem7
data2 <- subset(data, (!is.na(TDS) & !is.na(EC25)))

## Linear estimation of TDS ! EC25 slopes...  #######
mod <- lm(TDS ~ EC25, data2)
   b0 <- mod$coeff[1]
   b1 <- mod$coeff[2]
   rsq <- summary(mod)$adj.r.squared

mod0 <- lm(TDS ~ EC25 - 1, data2)
   b0.0 <- mod0$coeff[1]
   b1.0 <- mod0$coeff[2]
   rsq.0 <- summary(mod0)$adj.r.squared

hist.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="HIST"))
IW12.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="IW12"))
WDes.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="REF1415"))
WSpur.lm0 <- lm(TDS ~ EC25, data = subset(data2, SET=="WSpur"))

   summary(mod)

hist.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="HIST"))
IW12.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="IW12"))
WDes.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="REF1415"))
WSpur.lm <- lm(TDS ~ EC25 - 1, data = subset(data2, SET=="WSpur"))

   summary(WSpur.lm)
############

## PLOTTING ##
z0 <- ggplot(data2, aes(x=EC25, y=TDS, col=SET.name), na.rm=T) + theme_bw() + 
      scale_color_manual(values=c("darkgreen", "red", "blue", "purple"))
z1 <- z0 + geom_point(na.rm=T) + stat_smooth(aes(group=1),formula = y ~ x, col="black",
                                             linetype=2, size=0.8,method=lm, se=F, na.rm=T) +
      scale_x_continuous(labels=comma, limits=c(0,75000), oob=squish) +
      scale_y_continuous(labels=comma, limits=c(0,45000), oob=squish)
z2 <- z1 + annotate("text", size=6, x=5000, y=43000, hjust=0.1, vjust=0, color = "black",
            label="Total Dissolved Solids ~ Specific Conductance", show_guide=F) +
           annotate("text", size=5, x=5000, y=41000, hjust=0, vjust=0.2, color = "black",
            label=paste("slope = ", format(b0.0, nsmall=2, digits=3)), show_guide=F) +
           annotate("text", size=5, x=5000, y=39000, hjust=0, vjust=0.4, color = "black",
            label=paste("r^2 == ", format(rsq.0, nsmall=2, digits=2)), parse=T, show_guide=F);
z3 <- z2 + xlab("Specific Conductance (uS/cm @ 25C)") + labs(col="Dataset") + 
           ylab("Total Dissolved Solids (mg/L)") +
           theme(legend.position=c(0.825, 0.13));
z3

ggsave(file="AllIW_TDS-vs-EC25_fig1.jpeg", width=7, height=6.5, dpi=500);






















