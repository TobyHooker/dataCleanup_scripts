## Hemimiarsh - plots

Hemi_1 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/HemiMarsh_SoilVeg_1.csv")

## plots x Distance X "SITE"

    Hemi_1 <- Hemi_1[with(Hemi_1, order(Site, Storet, Dist)),]

levels(Hemi_1$CLASS)
Hemi_1$CLASS <- Hemi_1$Site
Hemi_1$CLASS <- factor(Hemi_1$CLASS, levels=c("Hemi-Marsh", "Reference"));

head(Hemi_1)

Hemi.var <- data.frame()  ## prepare DF
for (i in 1:length(names(Hemi_1))) {
  class_i <- class(Hemi_1[,i])
  name_i <- names(Hemi_1[i])
  num.NAs <- sum(is.na(Hemi_1[,i]))
Hemi.var <- rbind(Hemi.var, data.frame(i, name_i, class_i, num.NAs))
}

levels(Hemi_1$Site) <- c("Hemi-Marsh", "Reference")

# Soil d15N
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=d15N, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=d15N, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + ylab("Soil d15N (o/oo)")
ggsave(file="Soild15N_1.jpeg", width=7, height=5, dpi=500)  

# Soil Olsen-P
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=OlsenP, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=OlsenP, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Soil Olsen-P (mg P/kg soil)")
ggsave(file="SoilOL-P_1.jpeg", width=7, height=5, dpi=500)  

# Soil TP
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=TP, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=TP, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Soil TP (g P/kg soil)")
ggsave(file="Soil_TP_1.jpeg", width=7, height=5, dpi=500)  


# Soil Olsen-P/TP
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=OlsenP/TP/1000, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=OlsenP/TP/1000, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Soil Olsen-P to TP ratio") + scale_y_continuous(labels=percent)
ggsave(file="Soil_OLP-TP_1.jpeg", width=7, height=5, dpi=500)  

# Soil TIN
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=TIN, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=TIN, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Soil Inorganic N (mg N/kg soil)")
ggsave(file="Soil_TIN_2.jpeg", width=7, height=5, dpi=500)  

# Soil TIN:OlsenP
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=TIN/OlsenP, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=TIN/OlsenP, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z1 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Soil TIN to Olsen-P ratio") + scale_y_continuous(limits=c(NA,12), oob=squish)
ggsave(file="Soil_TIN-OLP_1.jpeg", width=7, height=5, dpi=500)  


# Veg d15N
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=d15N.1, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=d15N.1, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Vegetation d15N (o/oo)");
ggsave(file="Veg_d15N_1.jpeg", width=7, height=5, dpi=500)  


# Veg C:N
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=Veg.CN, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=Veg.CN, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Vegetation C:N ratio");
ggsave(file="Veg_CN_1.jpeg", width=7, height=5, dpi=500)  

# Veg C:P
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=Veg.CP, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=Veg.CP, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Vegetation C:P ratio");
ggsave(file="Veg_CP_1.jpeg", width=7, height=5, dpi=500)  

# Veg N:P
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=Veg.NP, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=Veg.NP, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z1 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Vegetation N:P ratio (by mass)");
ggsave(file="Veg_NP_1.jpeg", width=7, height=5, dpi=500)  

# Veg + Algal MAt d15N
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=d15N.1, col=Site.name), position=position_jitter(width=20), na.rm=T) +
          geom_jitter(aes(y=d15N.2, col=Site.name), shape=4, size=4,
                      position=position_jitter(width=20), na.rm=T);
z2 <- z1 + geom_smooth(aes(y=d15N.1, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Vegetation d15N (o/oo)");
ggsave(file="Veg+AlgMat_d15N_1.jpeg", width=7, height=5, dpi=500)  

# Veg + Algal MAt d15N x Hemimarsh only
z0 <- ggplot(data=subset(Hemi_1,CLASS=="Hemi"), aes(x=as.numeric(Dist))) + 
               theme_bw() + scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=d15N.1, col=Site.name), position=position_jitter(width=20), na.rm=T) +
          geom_jitter(aes(y=d15N.2, col=Site.name), shape=3, size=3,
                      position=position_jitter(width=20), na.rm=T);
z2 <- z1 + geom_smooth(aes(y=d15N.1, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z1 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Vegetation d15N (o/oo)");

## Calculate Apparent Enrichment Factors
Hemi_1$E.VegSoil <- Hemi_1$d15N.1 - Hemi_1$d15N
Hemi_1$E.AlgSoil <- Hemi_1$d15N.2 - Hemi_1$d15N
Hemi_1$E.AlgVeg <- Hemi_1$d15N.2 - Hemi_1$d15N.1
###

#  Plant Apparent d15N enrichment
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=E.VegSoil, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=E.VegSoil, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z1 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Plant Apparent d15N Enrichmess vs. Soil (o/oo)");

#  Algal MAt Apparent d15N enrichment
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=E.AlgSoil, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=E.AlgSoil, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z1 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Algal Mat Apparent d15N Enrichmess vs. Soil (o/oo)");

#  Algal MAt Apparent d15N enrichment vs Plants
z0 <- ggplot(data=subset(Hemi_1,CLASS=="Hemi"), aes(x=as.numeric(Dist))) + theme_bw() +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=E.AlgVeg, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=E.AlgVeg, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z1 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Algal Mat Apparent d15N Enrichmess vs. Plants (o/oo)");


###  Veg C:N vs. d15N
z0 <- ggplot(data=Hemi_1) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_point(aes(x=Veg.CN, y=d15N.1, col=Site.name), na.rm=T);
z3 <- z1 + xlab("Vegetation C:N ratio") + labs(col="Sites") + 
          ylab("Vegetation d15N (o/oo)");
ggsave(file="VegCN_vs_d15N_1.jpeg", width=7, height=5, dpi=500)  

###  Veg C:P vs. d15N
z0 <- ggplot(data=Hemi_1) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_point(aes(x=Veg.CP, y=d15N.1, col=Site.name), na.rm=T);
z3 <- z1 + xlab("Vegetation C:P ratio") + labs(col="Sites") + 
          ylab("Vegetation d15N (o/oo)");
ggsave(file="VegCP_vs_d15N_1.jpeg", width=7, height=5, dpi=500)  

###  Veg C:N vs. C:P
z0 <- ggplot(data=Hemi_1) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_point(aes(x=Veg.CN, y=Veg.CP, col=Site.name), na.rm=T);
z3 <- z1 + xlab("Vegetation C:N ratio") + labs(col="Sites") + 
          ylab("Vegetation C:P ratio");
ggsave(file="VegCN_vs_CN_1.jpeg", width=7, height=5, dpi=500)  

###  Veg C:N vs. C:P
z0 <- ggplot(data=Hemi_1) + theme_bw() + scale_colour_tableau() ;
z1 <- z0 + geom_point(aes(x=Veg.CN, y=Veg.CP, col=Site), na.rm=T);
z3 <- z1 + xlab("Vegetation C:N ratio") + labs(col="Wetland Type") + 
          ylab("Vegetation C:P ratio");
ggsave(file="VegCP_vs_d15Nfull_2.jpeg", width=7, height=5, dpi=500)  


# Soil d15N - consistnet scale
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=d15N, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=d15N, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + ylab("Soil d15N (o/oo)") +
        scale_y_continuous(limits=c(-5,30), oob=squish) + geom_hline(yintercept=0)
ggsave(file="Soild15N_2.jpeg", width=7, height=5, dpi=500)  

# Veg d15N  consistnet scale
z0 <- ggplot(Hemi_1, aes(x=as.numeric(Dist))) + theme_bw() + facet_wrap(~ Site, ncol=2) +
        scale_colour_tableau() ;
z1 <- z0 + geom_jitter(aes(y=d15N.1, col=Site.name), position=position_jitter(width=20), na.rm=T)
z2 <- z1 + geom_smooth(aes(y=d15N.1, col=Site.name), method="lm", na.rm=T, se=F);
z3 <- z2 + xlab("Distance along Transect (m)") + labs(col="Sites") + 
          ylab("Vegetation d15N (o/oo)") + scale_y_continuous(limits=c(-5,30), oob=squish) + 
          geom_hline(yintercept=0);
ggsave(file="Veg_d15N_2.jpeg", width=7, height=5, dpi=500)  


