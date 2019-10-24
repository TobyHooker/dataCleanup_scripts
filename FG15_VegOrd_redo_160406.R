## Veg NMDS redo:  SCREE PlOT + Select Dims + Scores...160406  ######
# data == veg2.dat3
#
#
######  NEW  160406  ####  NMDS  SCREE PLOT  #######
nmds.scree <- function(x){
    plot(rep(1,10), replicate(10, metaMDS(x, k=1)$stress),
         xlim=c(1,6), ylim=c(0,0.5), xlab="# Dimensions", ylab="Stress", 
         main="NMDS Stress plot: Invertebrates")
    for (i in 1:6) {
        points(rep(i+1, 10), replicate(10, metaMDS(x, k=i+1)$stress))   }   };

jpeg("NMDS_VEG2_Scree.jpeg", width=5, height=5, units="in", quality=80, res=500)
nmds.scree(veg2.dat3)
dev.off();
#############  NMDS  ORDINATION  ################
veg2.nmds3 <- metaMDS(veg2.dat3, distance="bray", k=3)   ## This is the Ordination ###
veg3.nmds <- veg2.nmds3     ### Make a clear copy
#
########################## Exploring NMDS Results <plots> #################
## Shepard Plot
jpeg(filename="Veg2.nmds3.Stress_2.jpeg", width=7.5, height=6, units="in", res=500)
veg2.stress3 <- stressplot(veg2.nmds3)
dev.off();

plot(veg2.nmds3, choices=c(1,2))
plot(veg2.nmds3, choices=c(1,3))
plot(veg2.nmds3, choices=c(2,3))

plot(veg2.nmds3, choices=c(1,2), display="sp", type="t")
plot(veg2.nmds3, choices=c(1,3), display="sp", type="t")
plot(veg2.nmds3, choices=c(2,3), display="sp", type="t")
########################################
########## Generate NMDS Ordination Scores ###########
veg3.scores <- as.data.frame(scores(veg3.nmds)) ## these are the SITE scores...
veg3.spp.scores <- as.data.frame(scores(veg3.nmds, "species")) ## these are the SPP scores
veg3.spp.scores$SPP <- rownames(veg3.spp.scores)

### Create Factor and Vector [supplemental] datasets ####
ALL.dist0 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Veg-SiteV-WCh-INV.Dist_1.csv")
# make sure that all vars have same / consistent order (key.dist)
ALL.dist0 <- ALL.dist0[with(ALL.dist0, order(key.dist)),]  ## okay, but messy
## Summarize VEG-VAR characteristics  ######
    All0.var <- data.frame()  ## prepare DF
        for (i in 1:length(names(ALL.dist0))) {
            class_i <- class(ALL.dist0[,i])
            name_i <- names(ALL.dist0[i])
            num.NAs <- sum(is.na(ALL.dist0[,i]))
    All0.var <- rbind(All0.var, data.frame(i, name_i, class_i, num.NAs))    };

######  [1] Plant (Species) Taxa  ########
veg2.data3  ## okay. rownames have been set to key.dist

######  [2] Site Variables   #############
## Earlier ver. used:  VEG2.SCORES3.ENV2  [order looks ok]
## [Vectors]::  
#  Sum.Aq + Richness + Bare.mud + Em.Veg + Litter.cov + Water.cov + Water.z.cm + Veg.Ht.cm + RelCov.INV

## [Factors]::  See (ALL.dist0[,c(2,107,108)]) OR  VEG2.SCORES3.ENV2
# Wtrshd + Water.Src

######  [3] Water Chemistry   ############
# data == WChem.dist1  [key == ket.dist2]       ## 160406:  Update some VARS !!!
# set rownames
rownames(WChem.dist1) <- WChem.dist1$key.dist2
 WChem.var <- data.frame()  ## prepare DF to keep track of Var types
    for (i in 1:length(names(WChem.dist1))) {
        class_i <- class(WChem.dist1[,i])
        name_i <- names(WChem.dist1[i])
        num.NAs <- sum(is.na(WChem.dist1[,i]))
        WChem.var <- rbind(WChem.var, data.frame(i, name_i, class_i, num.NAs))    };
WChem.dist2 <- WChem.dist1
## vars to remove:  HCO3, CO3, DIC_mM, PIM, pH.L, H2S_T, NH3_std
WChem.dist2 <- WChem.dist2[,c(-19,-20,-26,-35,-43, -49, -52)]

######  [4] Soil Chemistry   ############
## copied Soil.MASTER2_160328.csv, updated vals...import:
Soil.ALL3xx <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Soil.MASTER3_160406.csv")
    # Now re-process (from TRANS to DIST)::
Soil.ALL3 <- ddply(Soil.ALL3xx, .(MLID, Year, Dist), numcolwise(mean, na.rm=T))
Soil.ALL3$key.dist <- paste(Soil.ALL3$MLID, ".", Soil.ALL3$Year, ".", Soil.ALL3$Dist, sep="")
Soil.ALL3 <- Soil.ALL3[,c(36,1:35)]  # move key.dist up to frint
Soil.ALL3 <- Soil.ALL3[,c(-2,-3,-4,-5,-25)]  # omit site Vars and -sed.Cr [too many NAs)]
    SoilChem.var <- data.frame()  ## prepare DF to keep track of Var types
        for (i in 1:length(names(Soil.ALL3))) {
            class_i <- class(Soil.ALL3[,i])
            name_i <- names(Soil.ALL3[i])
            num.NAs <- sum(is.na(Soil.ALL3[,i]))
            SoilChem.var <- rbind(SoilChem.var, data.frame(i, name_i, class_i, num.NAs))    };
rownames(Soil.ALL3) <- Soil.ALL3$key.dist
# data == Soil.ALL3

######  [5] Leaf CNP   #################
# data == Veg.ALL3

###########################################################
###########################################################
###########   FITTING ENVIRONMENTAL VARs  #################
###########################################################
###########################################################
## ORDINATION SCORES (Enhanced Dataset::)
veg3.scores     ## data from nmds
veg3.scores.1 <- veg3.scores
veg3.scores.1$key.dist <- rownames(veg3.scores.1)
    all.var.sub <- VEG2.SCORES3.ENV2[,c(1,4:25)]
veg3.scores.2 <- merge(veg3.scores.1, all.var.sub, by="key.dist", sort=F)
rownames(veg3.scores.2) <- veg3.scores.2$key.dist

### [1] Plant Taxa  #######
# Use data from nmds
veg3.taxa.fit <- envfit(veg3.nmds, veg2.dat3, choices=1:3, permu=999)

fit.r2 <- data.frame(vect.r2 = veg3.taxa.fit$vectors[[2]])
fit.SIG <- data.frame(vect.sig = veg3.taxa.fit$vectors[[4]])
fit.SCORES <- data.frame(vect.SCORE = veg3.taxa.fit$vectors[[1]])
    colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
veg3.taxa.fit.VAR <- data.frame(fit.r2, fit.SIG, fit.SCORES)
veg3.taxa.fit.VAR$Var <- rownames(veg3.taxa.fit.VAR)

write.csv(veg3.taxa.fit.VAR, file="Veg3nmds.Taxa.Vectors.csv", row.names=F) # extract ENVFIT values
#####
### [2] Site Vars [vector]  ####
# Use data from nmds
veg3.SiteV.fit <- envfit(veg3.nmds ~ Sum.Aq + Richness + Bare.mud + Em.Veg + Litter.cov + Water.cov + Water.z.cm + Veg.Ht.cm + RelCov.INV, VEG2.SCORES3.ENV2, choices=1:3, permu=999)

fit.r2 <- data.frame(vect.r2 = veg3.SiteV.fit$vectors[[2]])
fit.SIG <- data.frame(vect.sig = veg3.SiteV.fit$vectors[[4]])
fit.SCORES <- data.frame(vect.SCORE = veg3.SiteV.fit$vectors[[1]])
    colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
veg3.SiteV.fit.VAR <- data.frame(fit.r2, fit.SIG, fit.SCORES)
veg3.SiteV.fit.VAR$Var <- rownames(veg3.SiteV.fit.VAR)

write.csv(veg3.SiteV.fit.VAR, file="Veg3nmds.SiteVar.Vectors.csv", row.names=F) # extract ENVFIT values
#####
### [2.5] Site Vars [FACTOR]  ####
# Use data from nmds
veg3.SiteVChar.fit <- envfit(veg3.nmds ~ Wtrshd + Water.Src, VEG2.SCORES3.ENV2, choices=1:3, permu=999)

fit.r2 <- data.frame(vect.r2 = veg3.SiteVChar.fit$factors[[2]])
fit.SIG <- data.frame(vect.sig = veg3.SiteVChar.fit$factors[[4]])
fit.SCORES <- data.frame(vect.SCORE = veg3.SiteVChar.fit$factors[[1]])
    colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")

###  New plots for FACTORS  060404 *** ###########
cols <- c("Chan" = "black", "GW" = "brown", "Inter" = "darkgreen", "IW" = "blue", "UPDES" = "red")
colZ <- c("brown", "black", "darkgreen", "blue", "red")

plot(veg2.nmds3, choices=c(1,2), display="si")
ordispider(veg2.nmds3, groups = veg3.scores.2$Water.Src, choices=c(1,2), label=T)

plot(veg2.nmds3, choices=c(1,3), display="si")
ordispider(veg2.nmds3, groups = veg3.scores.2$Water.Src, choices=c(1,3), label=T)

plot(veg2.nmds3, choices=c(2,3), display="si")
ordispider(veg2.nmds3, groups = veg3.scores.2$Water.Src, choices=c(2,3), label=T)

dev.off()

plot(veg2.nmds3, choices=c(1,2), display="si")
ordispider(veg2.nmds3, groups = veg3.scores.2$Wtrshd, choices=c(1,2), label=T)

plot(veg2.nmds3, choices=c(1,3), display="si")
ordispider(veg2.nmds3, groups = veg3.scores.2$Wtrshd, choices=c(1,3), label=T)

plot(veg2.nmds3, choices=c(2,3), display="si")
ordispider(veg2.nmds3, groups = veg3.scores.2$Wtrshd, choices=c(2,3), label=T)

dev.off()

#####
### [3] Water Chemistry  ####
# Use data from nmds
veg3.WChem.fit <- envfit(veg3.nmds, WChem.dist2[,c(2:47)], choices=1:3, permu=999)

fit.r2 <- data.frame(vect.r2 = veg3.WChem.fit$vectors[[2]])
fit.SIG <- data.frame(vect.sig = veg3.WChem.fit$vectors[[4]])
fit.SCORES <- data.frame(vect.SCORE = veg3.WChem.fit$vectors[[1]])
    colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
veg3.WChem.fit.VAR <- data.frame(fit.r2, fit.SIG, fit.SCORES)
veg3.WChem.fit.VAR$Var <- rownames(veg3.WChem.fit.VAR)

write.csv(veg3.WChem.fit.VAR, file="Veg3nmds.WChem.Vectors.csv", row.names=F) # extract ENVFIT values

### [3.5] rerun w/o [Ca.Mg] ratio ####

veg3.WChem.fit2 <- envfit(veg3.nmds, WChem.dist2[,c(2:24,26:47)], choices=1:3, permu=999)

fit.r2 <- data.frame(vect.r2 = veg3.WChem.fit2$vectors[[2]])
fit.SIG <- data.frame(vect.sig = veg3.WChem.fit2$vectors[[4]])
fit.SCORES <- data.frame(vect.SCORE = veg3.WChem.fit2$vectors[[1]])
    colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
veg3.WChem.fit2.VAR <- data.frame(fit.r2, fit.SIG, fit.SCORES)
veg3.WChem.fit2.VAR$Var <- rownames(veg3.WChem.fit2.VAR)

write.csv(veg3.WChem.fit2.VAR, file="Veg3nmds.WChem.Vectors_v2.csv", row.names=F) # extract ENVFIT values

#####
### [4] Soil Chemistry  ####
# Use data from nmds
veg3.Soil.fit <- envfit(veg3.nmds, Soil.ALL3[,c(2:31)], choices=1:3, permu=999, na.rm=T)

fit.r2 <- data.frame(vect.r2 = veg3.Soil.fit$vectors[[2]])
fit.SIG <- data.frame(vect.sig = veg3.Soil.fit$vectors[[4]])
fit.SCORES <- data.frame(vect.SCORE = veg3.Soil.fit$vectors[[1]])
    colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
veg3.Soil.fit.VAR <- data.frame(fit.r2, fit.SIG, fit.SCORES)
veg3.Soil.fit.VAR$Var <- rownames(veg3.Soil.fit.VAR)

write.csv(veg3.Soil.fit.VAR, file="Veg3nmds.SoilChem.Vectors.csv", row.names=F) # extract ENVFIT values
#####
### [5] Leaf CNP   ####
# Use data from nmds
veg3.Leaf.fit <- envfit(veg3.nmds, Veg.ALL3[,c(2:9)], choices=1:3, permu=999, na.rm=T)

fit.r2 <- data.frame(vect.r2 = veg3.Leaf.fit$vectors[[2]])
fit.SIG <- data.frame(vect.sig = veg3.Leaf.fit$vectors[[4]])
fit.SCORES <- data.frame(vect.SCORE = veg3.Leaf.fit$vectors[[1]])
    colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
veg3.Leaf.fit.VAR <- data.frame(fit.r2, fit.SIG, fit.SCORES)
veg3.Leaf.fit.VAR$Var <- rownames(veg3.Leaf.fit.VAR)

write.csv(veg3.Leaf.fit.VAR, file="Veg3nmds.LeafCNP.Vectors.csv", row.names=F) # extract ENVFIT values
#####

###########   P L O T S  O F  F I T T E D  V A R S   #############
## [1] Plant Taxa Scores x NMDS Axes ####

jpeg(file="Veg3.ord_Sig-TAXA.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(4,4,1,1.5))
plot(veg3.nmds, display="spec", type="n", choices=c(1,2))
    plot(veg3.taxa.fit, p.max=0.001, choices=c(1,2), cex=0.7)

plot(veg3.nmds, display="spec", type="n", choices=c(1,3))
    plot(veg3.taxa.fit, p.max=0.001, choices=c(1,3), cex=0.7)

plot(veg3.nmds, display="spec", type="n", choices=c(2,3))
    plot(veg3.taxa.fit, p.max=0.001, choices=c(2,3), cex=0.7)

dev.off();
##########################

## [2.5]  Water Source x NMDS Axes  ######

jpeg(file="Veg3.ord_Sig-WaterSrc_2.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(4,4,1,1.5))

plot(veg3.nmds, display="spec", type="n", choices=c(1,2))
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Chan",col="brown", 
               choices=c(1,2), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="GW",col="blue", 
               choices=c(1,2), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Inter",col="green", 
               choices=c(1,2), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="IW",col="black", 
               choices=c(1,2), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="UPDES",col="red", 
               choices=c(1,2), label=T, cex=0.7)

plot(veg3.nmds, display="spec", type="n", choices=c(1,3))
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Chan",col="brown", 
               choices=c(1,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="GW",col="blue", 
               choices=c(1,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Inter",col="green", 
               choices=c(1,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="IW",col="black", 
               choices=c(1,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="UPDES",col="red", 
               choices=c(1,3), label=T, cex=0.7)

plot(veg3.nmds, display="spec", type="n", choices=c(2,3))
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Chan",col="brown", 
               choices=c(2,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="GW",col="blue", 
               choices=c(2,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Inter",col="green", 
               choices=c(2,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="IW",col="black", 
               choices=c(2,3), label=T, cex=0.7)
    ordispider(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="UPDES",col="red", 
               choices=c(2,3), label=T, cex=0.7)

dev.off();
###### [2.5]  Water Source x NMDS Axes  ######
## try it w/ hulls and polys   This one is good ######
jpeg(file="Veg3.ord_Sig-WaterSrc_3.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(4,4,0.5,0.5))

plot(veg3.nmds, display="sp", type="t", choices=c(1,2), cex=0.5)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Chan",col="brown", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="GW",col="blue", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Inter",col="green", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="IW",col="black", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="UPDES",col="red", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)

plot(veg3.nmds, display="spec", type="t", choices=c(1,3), cex=0.5)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Chan",col="brown", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="GW",col="blue", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Inter",col="green", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="IW",col="black", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="UPDES",col="red", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)

plot(veg3.nmds, display="spec", type="t", choices=c(2,3), cex=0.5)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Chan",col="brown", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="GW",col="blue", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="Inter",col="green", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="IW",col="black", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(veg3.nmds, group=veg3.scores.2$Water.Src, show.groups="UPDES",col="red", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)

dev.off();
######

### [00]  Base plotting of NMDS w/ SPP and Site Names #######
cols <- c("Chan" = "brown", "GW" = "blue", "Inter" = "darkgreen", "IW" = "black", "UPDES" = "red")

x12 <- ggplot() + theme_bw() + theme(plot.margin=unit(c(0.2,0.1,0,0.5), "cm")) + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "IW", "UPDES"),
                values=cols, guide=guide_legend(title="Water Source")) +
        geom_point(data=veg3.scores.2, aes(x=NMDS1, y=NMDS2, col=Water.Src), size=1.5) +
        geom_text(data=veg3.spp.scores, aes(x=NMDS1, y=NMDS2, label=SPP),
                                alpha=0.5, size=2, col="red");

x13 <- ggplot() + theme_bw() + theme(plot.margin=unit(c(0.2,0.1,00,0.5), "cm")) + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "IW", "UPDES"),
                            values=cols) + guides(col=FALSE) +
        geom_point(data=veg3.scores.2, aes(x=NMDS1, y=NMDS3, col=Water.Src), size=1.5) +
        geom_text(data=veg3.spp.scores, aes(x=NMDS1, y=NMDS3, label=SPP),
                                alpha=0.5, size=2, col="red");

x23 <- ggplot() + theme_bw() + theme(plot.margin=unit(c(0.2,0.1,0,0.5), "cm")) + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "IW", "UPDES"),
                            values=cols) + guides(col=FALSE) +
        geom_point(data=veg3.scores.2, aes(x=NMDS2, y=NMDS3, col=Water.Src), size=1.5) +
        geom_text(data=veg3.spp.scores, aes(x=NMDS2, y=NMDS3, label=SPP),
                                alpha=0.5, size=2, col="red");

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

    x_legend <- g_legend(x12)       #  print(x_legend)

######  Combo Plot  ##
jpeg(filename="Veg3_NMDS_plot_v1.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(x12 + theme(legend.position="none"), x13, x23, x_legend, 
        ncol=2, nrow=2, widths=4, heights=4)
dev.off()
#####
## [2] Site Var Scores x NMDS Axes ####

jpeg(file="Veg3.ord_Sig-SiteVars_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(veg3.nmds, display="spec", type="t", choices=c(1,2))
    plot(veg3.SiteV.fit, p.max=0.001, choices=c(1,2), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(1,3))
    plot(veg3.SiteV.fit, p.max=0.001, choices=c(1,3), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(2,3))
    plot(veg3.SiteV.fit, p.max=0.001, choices=c(2,3), cex=0.7)

dev.off();
#####
## [3] Water Chem  Scores x NMDS Axes ####

jpeg(file="Veg3.ord_Sig-WChem_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(veg3.nmds, display="spec", type="t", choices=c(1,2))
    plot(veg3.WChem.fit, p.max=0.005, choices=c(1,2), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(1,3))
    plot(veg3.WChem.fit, p.max=0.005, choices=c(1,3), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(2,3))
    plot(veg3.WChem.fit, p.max=0.005, choices=c(2,3), cex=0.7)

dev.off();

### try w/o Ca.Mg ratio..

jpeg(file="Veg3.ord_Sig-WChem_v2.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(veg3.nmds, display="spec", type="t", choices=c(1,2))
    plot(veg3.WChem.fit2, p.max=0.005, choices=c(1,2), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(1,3))
    plot(veg3.WChem.fit2, p.max=0.005, choices=c(1,3), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(2,3))
    plot(veg3.WChem.fit2, p.max=0.005, choices=c(2,3), cex=0.7)

dev.off();
#####
## [4] Soil Chem  Scores x NMDS Axes ####

jpeg(file="Veg3.ord_Sig-SoilChem_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(veg3.nmds, display="spec", type="t", choices=c(1,2))
    plot(veg3.Soil.fit, p.max=0.005, choices=c(1,2), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(1,3))
    plot(veg3.Soil.fit, p.max=0.005, choices=c(1,3), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(2,3))
    plot(veg3.Soil.fit, p.max=0.005, choices=c(2,3), cex=0.7)

dev.off();
#####

## [5] Leaf CNP  Scores x NMDS Axes ####

jpeg(file="Veg3.ord_Sig-LeafCNP_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(veg3.nmds, display="spec", type="t", choices=c(1,2))
    plot(veg3.Leaf.fit, p.max=0.005, choices=c(1,2), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(1,3))
    plot(veg3.Leaf.fit, p.max=0.005, choices=c(1,3), cex=0.7)

plot(veg3.nmds, display="spec", type="t", choices=c(2,3))
    plot(veg3.Leaf.fit, p.max=0.005, choices=c(2,3), cex=0.7)

dev.off();
#####

###  160406 ::  Okay for now !!


