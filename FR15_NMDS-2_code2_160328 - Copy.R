## FR15 Combo Data -- Workup and Ordinations    160316   #####
`V+S+WCh+In.dist0` <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/GSL_Fringe2015/FR15_R/Veg-SiteV-WCh-INV.Dist_1.csv")

ALL.dist0 <- `V+S+WCh+In.dist0`
ALL.dist1 <- ALL.dist0     ##### working data
## Data Groups   #######
All.dist1.nam <- as.data.frame(names(ALL.dist1))
#   Group 1   Veg Cover           2   --   83  [86 w/ totals]
#   Group 2   Site Variables      87  --   100
#   Group 3   Water Chemistry     102 --   
##    Metals                      115 --   127 // 140 // 160
##    Major Ions                  161 --   171 // 184
##    Nutrients                   185 --   199
##    Field Chem                  201 --   216
#   Group 4   Invertebrates       224 --   290
##    Metrics                     298 --   300
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  #####
#############################################################
#############################################################
####   O  R  D  I  N  A  T  I  O  N   #######################
#############################################################
######################################
#### ORDINATION  WORKFLOW  ###########
# 1) metaMDS()
# 2) summary of object
# 3) stressplot
# 4) plot / ordiplot
# 5) biplot  [??]
# 6) envfit()  (incorporate in step 4/5)
######################################

### NMDS Set #1  -- Vegetation  #######
veg.dat0 <- ALL.dist1[,c(7:86)]
    summary(veg.dat0)  ## check for NAs, or non-numeric var classes
##
veg.var <- data.frame()  ## prepare DF
for (i in 1:length(names(veg.dat0))) {
  class_i <- class(veg.dat0[,i])
  name_i <- names(veg.dat0[i])
  num.NAs <- sum(is.na(veg.dat0[,i]))
veg.var <- rbind(veg.var, data.frame(i, name_i, class_i, num.NAs))
}
##  The above ( ^^ ) gives a df of Vars, w/ the class of each variable... ##
veg.dat1 <- na.omit(veg.dat0)  ## remove all NAs

veg.nmds1 <- metaMDS(veg.dat1, distance = "bray")  # run the ordination, then look at results <obj>
veg.stress <- stressplot(veg.nmds1)  ## useful details from this plot ?
plot(veg.stress)
veg1.plot <- plot(veg.nmds1) # recall row veg1.plot is now an ordiplot-object. additional work on the ordination should be based on the ordiplot-object (veg1.plot) and not the ordination-object (veg.nmds1)
# red crosses - wtd avg centroids of SPP
#          identify(veg1.plot, what="species") ## can id specific SPP
#          identify(veg1.plot, what="sites")   ## can id specific sites (but they are unlabelled)

# can use SCORES to extract nmds axis scores for either SPP or SITES...
# xx <- scores(Ord.object, choices [ord. axes, def=all], display=("sites" or "species"), ... )

veg1.scores <- as.data.frame(scores(veg.nmds1))
# extract class-vars from initial dataset (make sure to check dim's.)
    veg1.class <- ALL.dist1[,c(1:6,7,84:86,92:100, 107, 108)]
    veg1.class <- na.omit(veg1.class)
veg1.scores2 <- data.frame(veg1.scores, veg1.class)  # this is the nmds [site] scores + class-Vars !!

veg1.spp.scores <- as.data.frame(scores(veg.nmds1, "species"))
veg1.spp.scores$SPP <- rownames(veg1.spp.scores)
## if needed, could pull in Spp-characteristics...(WetL indicator status, etc.)

## now could plot SPP and SITES on plot...(see that dims match the nmds dataset dims)
# Sites == veg1.scores2       SPP == veg1.spp.scores

z0 <- ggplot() + theme_bw() + scale_colour_tableau() ;  # + coord_equal()  ?
z1 <- z0 + geom_text(data=veg1.spp.scores, aes(x=NMDS1, y=NMDS2, label=SPP), alpha=0.5, size=4) # SPP labels
z2 <- z1 + geom_point(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, col=Wtrshd), size=3) # Sites
z3 <- z2 + geom_line(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, group=Site)) +
          geom_text(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, group=Site, label=Site), size=4, vjust=0)
## a bit cluttered, but workable
# The above (  ^^^  ) plot uses Watersheds as the Group, w/ Site Names and SPP labeled, and lines connecting the plots within the SITES...
## setup groups for convex Hulls... (Watersheds - Wtrshd)   ######
grp.1 <- veg1.scores2[veg1.scores2$Wtrshd == "Bear", ][chull(veg1.scores2[veg1.scores2$Wtrshd == "Bear", c("NMDS1", "NMDS2")]), ]
grp.2 <- veg1.scores2[veg1.scores2$Wtrshd == "Farm", ][chull(veg1.scores2[veg1.scores2$Wtrshd == "Farm", c("NMDS1", "NMDS2")]), ]
grp.3 <- veg1.scores2[veg1.scores2$Wtrshd == "Gilb", ][chull(veg1.scores2[veg1.scores2$Wtrshd == "Gilb", c("NMDS1", "NMDS2")]), ]
grp.4 <- veg1.scores2[veg1.scores2$Wtrshd == "SV", ][chull(veg1.scores2[veg1.scores2$Wtrshd == "SV", c("NMDS1", "NMDS2")]), ]
veg1.Wtrshd.hulls <- rbind(grp.1, grp.2, grp.3, grp.4)

z4 <- z0 + geom_polygon(data=veg1.Wtrshd.hulls, aes(x=NMDS1, y=NMDS2, fill=Wtrshd, group=Wtrshd), alpha=0.25)
z5 <- z4 + geom_text(data=veg1.spp.scores, aes(x=NMDS1, y=NMDS2, label=SPP), alpha=0.5, size=4) +
          geom_point(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, col=Wtrshd), size=3);
z6 <- z5 + geom_line(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, group=Site)) +
          geom_text(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, group=Site, label=Site), size=4, vjust=0);
## Work up plots x Water Source (Water.Src)
grp.1 <- veg1.scores2[veg1.scores2$Water.Src == "Chan", ][chull(veg1.scores2[veg1.scores2$Water.Src == "Chan", c("NMDS1", "NMDS2")]), ]
grp.2 <- veg1.scores2[veg1.scores2$Water.Src == "GW", ][chull(veg1.scores2[veg1.scores2$Water.Src == "GW", c("NMDS1", "NMDS2")]), ]
grp.3 <- veg1.scores2[veg1.scores2$Water.Src == "Inter", ][chull(veg1.scores2[veg1.scores2$Water.Src == "Inter", c("NMDS1", "NMDS2")]), ]
grp.4 <- veg1.scores2[veg1.scores2$Water.Src == "IW", ][chull(veg1.scores2[veg1.scores2$Water.Src == "IW", c("NMDS1", "NMDS2")]), ]
grp.5 <- veg1.scores2[veg1.scores2$Water.Src == "UPDES", ][chull(veg1.scores2[veg1.scores2$Water.Src == "UPDES", c("NMDS1", "NMDS2")]), ]
veg1.WtrSrc.hulls <- rbind(grp.1, grp.2, grp.3, grp.4, grp.5)
## new Fig --- Water Source
x0 <- ggplot() + theme_bw() + scale_colour_tableau() + scale_fill_tableau() ;  # + coord_equal()  ?
x4 <- x0 + geom_polygon(data=veg1.WtrSrc.hulls, aes(x=NMDS1, y=NMDS2, fill=Water.Src, group=Water.Src), alpha=0.25)
x5 <- x4 + geom_text(data=veg1.spp.scores, aes(x=NMDS1, y=NMDS2, label=SPP), alpha=0.5, size=4) +
          geom_point(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, col=Water.Src), size=3);
x6 <- x5 + geom_line(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, group=Site)) +
          geom_text(data=veg1.scores2, aes(x=NMDS1, y=NMDS2, group=Site, label=Site), size=4, vjust=0);
################
##  try out envfit...
# data = veg.nmds1
# Vars = veg1.scores2 (partial)
veg1.vars <- veg1.scores2[,c(5,7,8,10:23)]

veg1.fit <- envfit(veg.nmds1, veg1.vars, permu = 999)
veg1.fit2 <- envfit(veg.nmds1 ~ Sum.Em + Sum.Aq + Richness + Algae + Em.Veg + Water.cov + Water.z.cm,
                    data=veg1.vars, permu = 999)

plot(veg1.fit2)
##########################################################################
################### REDO Veg NMDS w/ Trimmed Veg Spp List (>5% rel.freq) ######
#### For Above analyses...need to go back and trim up SPP that are too rare (<5% cases)
## 160318 ##
INV.Dist.trim_2 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/INV.Dist.trim_2.csv")
Veg.Dist.trim_2 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Veg.Dist.trim_2.csv")
Veg.SPP.list_1 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Veg.SPP.list_1.csv")
## will need to combine these data w/ the other Vars...######
veg2.dat <- Veg.Dist.trim_2
veg2.dat2 <- veg2.dat[,c(6:42)]  ## Veg Cover data only
##########  Ordination  Steps  ####
# step 1:  remove all NAs
veg2.dat2 <- na.omit(veg2.dat2)  ## n = 84 x k = 37 spp
  ## fix some more SPP:  Typha = TYDO + TYLA // SARU = SARU + SAUT
veg2.dat3 <- veg2.dat2
veg2.dat3$Typha <- veg2.dat3$TYLA + veg2.dat3$TYDO
veg2.dat3 <- veg2.dat3[,c(-5, -19)]
veg2.dat3$SARUx <- veg2.dat3$SARU + veg2.dat3$SAUT
veg2.dat3 <- veg2.dat3[,c(-16, -28)]

veg2.nmds2 <- metaMDS(veg2.dat3, distance="bray")   ## This is the Ordination ###
##### NMDS Calculations #####
## plot Shepard Plot ######
jpeg(filename="Veg2.nmds.Stress_1.jpeg", width=7.5, height=6, units="in", res=500)
veg2.stress <- stressplot(veg2.nmds2)
dev.off();
#####  NMDS Ordination Plot #####
veg2.plot1 <- plot(veg2.nmds2)  # can work from ordiplot <object> or nmds <object>
# extract scores to additional data manipulation and plotting #####
    veg2.scores <- as.data.frame(scores(veg2.nmds2)) ## these are the SITE scores...
    veg2.spp.scores <- as.data.frame(scores(veg2.nmds2, "species")) ## these are the SPP scores
# add site classification vars
    site.class <- veg2.dat[,c(1:5,43:54)]
    site.class <- na.omit(site.class)  ## 84 x 17 vars
veg2.scores2 <- data.frame(veg2.scores, site.class)  ## okay !!
    site.cl2 <- ALL.dist1[,c(2, 107,108)]
    site.cl2 <- na.omit(site.cl2)
veg2.scores3 <- merge(veg2.scores2, site.cl2, by="key.dist", sort=F)
## SPP scores & vars...#####
veg2.spp.scores$SPP <- rownames(veg2.spp.scores)
vsp3 <- merge(veg2.spp.scores, Veg.SPP.list_1, by.x = "SPP", by.y = "Taxa", all.x=T, sort=F)
veg2.spp.scores2 <- vsp3
######  ORDINATION PLOTTING  ############
### Base plotting of NMDS w/ SPP and Site Names #######
z0 <- ggplot() + theme_bw() + scale_colour_tableau(labels=c("Bear R.", "Farmington Bay", 
                                          "Gilbert", "Snake Valley")) ;  # + coord_equal()  ?
z1 <- z0 + geom_text(data=veg2.spp.scores2, aes(x=NMDS1, y=NMDS2, label=SPP), 
                     alpha=0.5, size=4, col="red") # SPP
ggsave(file="Veg2.nmds.VegSpecies_1.jpeg", width=7, height=7, dpi=500) ## Veg2.nmds.VegSPP plot 

z2 <- z1 + geom_point(data=veg2.scores3, aes(x=NMDS1, y=NMDS2, col=Wtrshd), size=3);
ggsave(file="Veg2.nmds.SitesxWtrshd_1.jpeg", width=7.5, height=6, dpi=500) # Sites plot

z3 <- z2 + geom_line(data=veg2.scores3, aes(x=NMDS1, y=NMDS2, group=Site, col=Wtrshd), alpha=0.5);
z4 <- z3 + geom_text(data=subset(veg2.scores3, Dist==300), aes(x=NMDS1, y=NMDS2, group=Site, 
                label=Site), size=4, vjust=1, alpha=0.8) +
          theme(legend.position=c(0.88,0.18)) + labs(col="Watershed");
ggsave(file="Veg2.nmds.SitesxDISTxWtrshd_3.jpeg", width=7.5, height=6, dpi=500) # SitexDist plot
######
## Set up Convex Hulls (Water Source)   ##########
grp.1 <- veg2.scores3[veg2.scores3$Water.Src == "Chan", ][chull(veg2.scores3[veg2.scores3$Water.Src == "Chan", c("NMDS1", "NMDS2")]), ]
grp.2 <- veg2.scores3[veg2.scores3$Water.Src == "GW", ][chull(veg2.scores3[veg2.scores3$Water.Src == "GW", c("NMDS1", "NMDS2")]), ]
grp.3 <- veg2.scores3[veg2.scores3$Water.Src == "Inter", ][chull(veg2.scores3[veg2.scores3$Water.Src == "Inter", c("NMDS1", "NMDS2")]), ]
grp.4 <- veg2.scores3[veg2.scores3$Water.Src == "IW", ][chull(veg2.scores3[veg2.scores3$Water.Src == "IW", c("NMDS1", "NMDS2")]), ]
grp.5 <- veg2.scores3[veg2.scores3$Water.Src == "UPDES", ][chull(veg2.scores3[veg2.scores3$Water.Src == "UPDES", c("NMDS1", "NMDS2")]), ]
veg2.WtrSrc.hulls <- rbind(grp.1, grp.2, grp.3, grp.4, grp.5)
## ORD figure w/ convex hulls (water source)  #####
x0 <- ggplot() + theme_bw() + 
  scale_fill_tableau(name="Water Source", labels=c("Channel", "Groundwater", "Interior", "Impounded Wetland", "UPDES")) +
  scale_colour_tableau(name="Water Source", labels=c("Channel", "Groundwater", "Interior", "Impounded Wetland", "UPDES"))  ;
x4 <- x0 + geom_polygon(data=veg2.WtrSrc.hulls, aes(x=NMDS1, y=NMDS2, fill=Water.Src, group=Water.Src), alpha=0.25)
x5 <- x4 + geom_text(data=veg2.spp.scores2, aes(x=NMDS1, y=NMDS2, label=SPP), 
                     alpha=0.5, size=2, col="red") +
          geom_point(data=veg2.scores3, aes(x=NMDS1, y=NMDS2, col=Water.Src), size=2); # SPP
x6 <- x5 + geom_line(data=veg2.scores3, aes(x=NMDS1, y=NMDS2, group=Site, col=Water.Src), alpha=0.5) +
          geom_text(data=subset(veg2.scores3, Dist==300), 
                    aes(x=NMDS1, y=NMDS2, group=Site, label=Site), size=2.5, vjust=1, alpha=0.8) +
          theme(legend.position=c(0.88,0.15), legend.background=element_rect(fill="white"));
ggsave(file="Veg2.nmds.SitesxWaterSrc_1.jpeg", width=7.5, height=6, dpi=500) # SitexWtrSrc plot
## above okay for Fig 7[6]., a bit congested  #####
####################################  160325  #############
###########   FITTING ENVIRONMENTAL VARs  #################
veg2.vars <- veg2.scores3[,c(6:12,14:21)]
    veg2.vars$Year <- as.factor(veg2.vars$Year)
    veg2.vars$Dist <- as.factor(veg2.vars$Dist)
### Export Environmental Site & Species Scores and Add in Response Vars ######
write.csv(veg2.scores3, file="VEG2.SCORES3.ENV1.csv", row.names=F)
write.csv(veg2.spp.scores2, file="VEG2.SPP.SCORES3.ENV1.csv", row.names=F);
## 160328 ## Env data for fitting ::::  ##############
    ##  (1)  Water Chemistry:  WChem.dist_1460325 --> WChem.dist1
WChem.dist1 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/WChem.dist_160325.csv")
    ##  (2)  Invrtebrates:  Inv.dist_160325 --> Inv.dist1
Inv.dist1 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Inv.dist_160325.csv")
    ##  (3)  Soil Chemistry:  Soil.MASTER1_160328  --> Soil.All0  [needs some work]
Soil.All0 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Soil.MASTER1_160328.csv")
    ##  (4)  Plant Chem:  Veg.Master1.CNP_160328  --> Veg.All0 [needs work?]
Veg.All0 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Veg.MASTER1.CNP_160328.csv")
########
##      Look at Individual Plant Taxa vs. NMDS Axes     ######
veg2.taxa <- veg2.dat3  # just use the raw input data
    veg2.taxa.fit <- envfit(veg2.nmds2, veg2.taxa, permu = 999) ## run vs Veg taxa

veg2.taxa.fit.r2 <- data.frame(vect.r2=veg2.taxa.fit$vectors[[2]])
veg2.taxa.fit.SIG <- data.frame(vect.sig=veg2.taxa.fit$vectors[[4]])
veg2.taxa.fit.vars <- data.frame(veg2.taxa.fit.r2, veg2.taxa.fit.SIG)
veg2.taxa.fit.vars$Var <- rownames(veg2.taxa.fit.vars)
write.csv(veg2.taxa.fit.vars, file="Veg2.Taxa.Vectors.csv", row.names=F) # extract ENVFIT values
# plot refined ordination and fit vars...
jpeg(file="Veg2_ord_Sig-VegTaxa.jpg", width=7, height=7, units="in", quality=80, res=500);
plot(veg2.nmds2, display="species", type="none")
text(veg2.nmds2, "species", cex=0.5)
plot(veg2.taxa.fit, cex=0.7, p.max=0.001)
dev.off()

# pull in key.dist from data to nmds data.matrix for Corr analysis
veg2.dat4 <- data.frame(key.dist=veg2.dat[,1], veg2.dat3)
    Veg2.Scores3.Taxa <- merge(veg2.scores3, veg2.dat4, by="key.dist", sort=F) # not strictly necc.
##
CorX.dat <- Veg2.Scores3.Taxa[,c(2,3)]
CorY.dat <- Veg2.Scores3.Taxa[,c(22:length(names(Veg2.Scores3.Taxa)))]
VegTaxa.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(VegTaxa.Cor1$r,2)
    signif(VegTaxa.Cor1$P,2)
cor.dat <- VegTaxa.Cor1$r
sig.dat <- VegTaxa.Cor1$P
    write.csv(VegTaxa.Cor1$r, file="Veg2.Ord.Taxa.Cor.csv", row.names=F);
    write.csv(VegTaxa.Cor1$P, file="Veg2.Ord.Taxa.CorSIG.csv", row.names=F);
    write.csv(VegTaxa.Cor1b$r, file="Veg2.Ord.Taxa.Cor.Spear.csv", row.names=F) # unneeded
# plotting ::  This runs the whole matrix (square), not just the 2 x k matrix...######
jpeg(file="Corrplot_Veg2Ord-x-Taxa_Pearsn_1.jpg", width=7, height=7, units="in", quality=80, res=500);
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
             p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5, order="original");
dev.off();
#####################
#### Fit Site Variables #####
## import *updated* Env Vars::
VEG2.SCORES3.ENV2 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/VEG2.SCORES3.ENV2.csv")
## Use this file to specify vector / continuous vars... #####
    site.var <- data.frame()  ## prepare DF to keep track of Var types
for (i in 1:length(names(VEG2.SCORES3.ENV2))) {
  class_i <- class(VEG2.SCORES3.ENV2[,i])
  name_i <- names(VEG2.SCORES3.ENV2[i])
  num.NAs <- sum(is.na(VEG2.SCORES3.ENV2[,i]))
site.var <- rbind(site.var, data.frame(i, name_i, class_i, num.NAs))    };
#######
veg2.fit3 <- envfit(veg2.nmds2 ~ Sum.Aq + Richness + Bare.mud + Em.Veg + 
            Litter.cov + Water.cov + Water.z.cm + Veg.Ht.cm + RelCov.INV, 
            data=VEG2.SCORES3.ENV2, permu = 999) # Cont.Vars

veg2.fit3.r2 <- data.frame(vect.r2=veg2.fit3$vectors[[2]])
veg2.fit3.SIG <- data.frame(vect.sig=veg2.fit3$vectors[[4]])
veg2.fit3.vars <- data.frame(veg2.fit3.r2, veg2.fit3.SIG)
veg2.fit3.vars$Var <- rownames(veg2.fit3.vars)
write.csv(veg2.fit3.vars, file="Veg2.SiteVar.Vectors.csv", row.names=F) # extract ENVFIT values
# Water Source Efx::: ######
veg2.fit4 <- envfit(veg2.nmds2 ~ Water.Src, data = VEG2.SCORES3.ENV2, permu = 999) 

#### plot - SiteVars Biplot (vs Species)
jpeg(file="Veg2_ord_Sig_SiteVars.jpg", width=7, height=7, units="in", quality=80, res=500);
    plot(veg2.nmds2, display="sites", type="none")
    text(veg2.nmds2, "species", cex=0.5)
    plot(veg2.fit3, cex=0.7, p.max=0.001)
dev.off()
#### plot - SiteVars Biplot (vs Sites)
jpeg(file="Veg2_ord_Sig_SiteVars.Sites.jpg", width=7, height=7, units="in", quality=80, res=500);
    plot(veg2.nmds2, display="sites", type="none")
    text(veg2.nmds2, "sites", cex=0.5)
    plot(veg2.fit3, cex=0.7, p.max=0.001)
dev.off();
#### Can try to fix this figure..(Fig 6.5 ??).. ######
###### Now, Correlations b/w Axes and Vars  #####
CorX.dat <- VEG2.SCORES3.ENV2[,c(2,3)]
CorY.dat <- VEG2.SCORES3.ENV2[,c(9,10,11,12,14,18,19,17,23)]
Veg2.SiteV.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Veg2.SiteV.Cor1$r,2)
    signif(Veg2.SiteV.Cor1$P,4)
cor.dat <- Veg2.SiteV.Cor1$r
sig.dat <- Veg2.SiteV.Cor1$P
    write.csv(Veg2.SiteV.Cor1$r, file="Veg2.Ord.Taxa.Cor.csv", row.names=F);
    write.csv(Veg2.SiteV.Cor1$P, file="Veg2.Ord.Taxa.CorSIG.csv", row.names=F);
# plotting ::  This runs the whole matrix (square), not just the 2 x k matrix...##
jpeg(file="Corrplot_Veg2Ord-x-SiteVar_Pearsn_1.jpg", width=7, height=7, units="in", 
                            quality=80, res=500);
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
             p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5, order="original");
dev.off();
#####################
#### Look at Plant Taxa Characteristics...<skip> #####
VEG2.SPP.SCORES3.ENV2 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/VEG2.SPP.SCORES3.ENV2.csv")
##
veg2.spp.fit4 <- envfit(veg2.nmds2 ~ C.value, data=VEG2.SPP.SCORES3.ENV2, 
                        permu = 999, na.rm=T) # doesn't work, data ~ SPP vars
### Can't make this work right now... SKIP IT ! 
 
#### Fit Water Chem Variables #####
WChem1.dist <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/WChem.dist_160325.csv")
    WChem.var <- data.frame()  ## prepare DF to keep track of Var types
    for (i in 1:length(names(WChem1.dist))) {
        class_i <- class(WChem1.dist[,i])
        name_i <- names(WChem1.dist[i])
        num.NAs <- sum(is.na(WChem1.dist[,i]))
        WChem.var <- rbind(WChem.var, data.frame(i, name_i, class_i, num.NAs))    };
## Fit Water Chem to Veg-NMDS Ordination #######
    data1 <- subset(WChem1.dist, Dist.m != 0)
    data1 <- data1[,c(6:9, 11, 13:length(names(WChem1.dist)))] ## ok !!
veg2.Wchem.fit5 <- envfit(veg2.nmds2, data1, permu = 999) # Cont.Vars
    fit.r2 <- data.frame(vect.r2=veg2.Wchem.fit5$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=veg2.Wchem.fit5$vectors[[4]])
    fit.vars <- data.frame(fit.r2, fit.SIG)
    fit.vars$Var <- rownames(fit.vars)
write.csv(fit.vars, file="Veg2.WChem.Vectors.csv", row.names=F) # extract ENVFIT values
#### plot - SiteVars Biplot (vs Species)
jpeg(file="Veg2_ord_Sig_WChemVars.jpg", width=7, height=7, units="in", quality=80, res=500);
    plot(veg2.nmds2, display="sites", type="none")
    text(veg2.nmds2, "species", cex=0.5)
    plot(veg2.Wchem.fit5, cex=0.7, p.max=0.10)
dev.off()
#### plot - SiteVars Biplot (vs Sites) - needs to be fixed ####
jpeg(file="Veg2_ord_Sig_WChemVars.Sites.jpg", width=7, height=7, units="in", quality=80, res=500);
    plot(veg2.nmds2, display="sites", type="none")
    text(veg2.nmds2, "sites", cex=0.5)
    plot(veg2.Wchem.fit5, cex=0.7, p.max=0.01)
dev.off();
###### Now, Correlations b/w Axes and Vars  #####
Veg.Water.scores <- merge(VEG2.SCORES3.ENV2[,c(1:3)], subset(WChem1.dist, Dist.m != 0),
                          by.x="key.dist", by.y="key.dist2", all.x=T)
CorX.dat <- Veg.Water.scores[,c(2,3)]
CorY.dat <- Veg.Water.scores[,c(8:11,13,15:length(names(Veg.Water.scores)))]
Veg2.WChem.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Veg2.WChem.Cor1$r,2)
    signif(Veg2.WChem.Cor1$P,4)
cor.dat <- Veg2.WChem.Cor1$r
sig.dat <- Veg2.WChem.Cor1$P
    write.csv(Veg2.WChem.Cor1$r, file="Veg2.Ord.WChem.Cor.csv", row.names=F);
    write.csv(Veg2.WChem.Cor1$P, file="Veg2.Ord.WChem.CorSIG.csv", row.names=F);
# plotting ::  This runs the whole matrix (square), not just the 2 x k matrix...#####
jpeg(file="Corrplot_Veg2Ord-x-WChem_Pearsn_1.jpg", width=7, height=7, units="in", 
                            quality=80, res=500);
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
             p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5, order="original");
dev.off();
#####################
###  Fit Soil Chemistry Variables ######
###  Work up Soil Chem data --> from .Trans to .Dist ####
Soil.ALL1 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/GSL_Fringe2015/FR15_R/Soil.MASTER2_160328.csv")
#
SoilChem.var <- data.frame()  ## prepare DF to keep track of Var types
    for (i in 1:length(names(Soil.ALL1))) {
        class_i <- class(Soil.ALL1[,i])
        name_i <- names(Soil.ALL1[i])
        num.NAs <- sum(is.na(Soil.ALL1[,i]))
        SoilChem.var <- rbind(SoilChem.var, data.frame(i, name_i, class_i, num.NAs))    };
Soil.ALL2 <- ddply(Soil.ALL1, .(MLID, Year, Dist), numcolwise(mean, na.rm=T))
Soil.ALL2$key.dist <- paste(Soil.ALL2$MLID, ".", Soil.ALL2$Year, ".", Soil.ALL2$Dist, sep="")
Soil.ALL2 <- Soil.ALL2[,c(40,1:39)]
    SoilChem.var <- data.frame()  ## prepare DF to keep track of Var types
        for (i in 1:length(names(Soil.ALL2))) {
            class_i <- class(Soil.ALL2[,i])
            name_i <- names(Soil.ALL2[i])
            num.NAs <- sum(is.na(Soil.ALL2[,i]))
            SoilChem.var <- rbind(SoilChem.var, data.frame(i, name_i, class_i, num.NAs))    };
######    
## Fit Soil Chem to Veg-NMDS Ordination #######
    data <- Soil.ALL2[,c(6:27,29:39)]
veg2.SoilChem.fit6 <- envfit(veg2.nmds2, data[,c(-1,-3)], permu = 999, na.rm=T) # Cont.Vars
    fit.r2 <- data.frame(vect.r2=veg2.SoilChem.fit6$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=veg2.SoilChem.fit6$vectors[[4]])
    fit.vars <- data.frame(fit.r2, fit.SIG)
    fit.vars$Var <- rownames(fit.vars)
write.csv(fit.vars, file="Veg2.SoilChem.Vectors.csv", row.names=F) # extract ENVFIT values
#### plot - SiteVars Biplot (vs Species)
jpeg(file="Veg2_ord_Sig_SoilChemVars.jpg", width=7, height=7, units="in", quality=80, res=500);
    plot(veg2.nmds2, display="sites", type="none")
    text(veg2.nmds2, "species", cex=0.5)
    plot(veg2.SoilChem.fit6, cex=0.7, p.max=0.001)
dev.off()
###### Now, Correlations b/w Axes and Vars  #####
Veg.SoilChem.scores <- merge(VEG2.SCORES3.ENV2[,c(1:3)], Soil.ALL2[,c(1,6:27,29:39)],
                          by.x="key.dist", by.y="key.dist", all.x=T)
CorX.dat <- Veg.SoilChem.scores[,c(2,3)]
CorY.dat <- Veg.SoilChem.scores[,c(5,7:length(names(Veg.SoilChem.scores)))]
Veg2.SoilChem.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Veg2.SoilChem.Cor1$r,2)
    signif(Veg2.SoilChem.Cor1$P,4)
cor.dat <- Veg2.SoilChem.Cor1$r
sig.dat <- Veg2.SoilChem.Cor1$P
    write.csv(Veg2.SoilChem.Cor1$r, file="Veg2.Ord.SoilChem.Cor.csv", row.names=F);
    write.csv(Veg2.SoilChem.Cor1$P, file="Veg2.Ord.SoilChem.CorSIG.csv", row.names=F);
# plotting ::  This runs the whole matrix (square), not just the 2 x k matrix...#####
jpeg(file="Corrplot_Veg2Ord-x-SoilChem_Pearsn_1.jpg", width=7, height=7, units="in", 
                            quality=80, res=500);
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
             p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5, order="original");
dev.off();
#####################
###  Fit Plant Leaf Chemistry Variables ######
Veg.ALL1 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/GSL_Fringe2015/FR15_R/Veg.MASTER2.CNP_160328.csv")
#### Work up Veg Chem data (drop metals and Algal Mats for now)   #######
VegCNP.var <- data.frame()  ## prepare DF to keep track of Var types
    for (i in 1:length(names(Veg.ALL1))) {
        class_i <- class(Veg.ALL1[,i])
        name_i <- names(Veg.ALL1[i])
        num.NAs <- sum(is.na(Veg.ALL1[,i]))
        VegCNP.var <- rbind(VegCNP.var, data.frame(i, name_i, class_i, num.NAs))    };
Veg.ALL2 <- Veg.ALL1
Veg.ALL2$Veg.CP <- (Veg.ALL2$Veg.C / Veg.ALL2$Veg.P)
Veg.ALL2$Veg.NP <- (Veg.ALL2$Veg.N / Veg.ALL2$Veg.P)
Veg.ALL2 <- subset(Veg.ALL2, DIST_m != 0)
Veg.ALL2 <- ddply(Veg.ALL2, .(key.dist), numcolwise(mean, na.rm=T))
    VegCNP.var <- data.frame()  ## prepare DF to keep track of Var types
        for (i in 1:length(names(Veg.ALL2))) {
            class_i <- class(Veg.ALL2[,i])
            name_i <- names(Veg.ALL2[i])
            num.NAs <- sum(is.na(Veg.ALL2[,i]))
            VegCNP.var <- rbind(VegCNP.var, data.frame(i, name_i, class_i, num.NAs))    };
Veg.ALL3 <- Veg.ALL2[,c(1,6:11,32,33)]
#######
## Fit Leaf Chem to Veg-NMDS Ordination #######
    data <- Veg.ALL3[,c(-1)]
veg2.VegChem.fit7 <- envfit(veg2.nmds2, data, permu = 999, na.rm=T) # Cont.Vars
    fit.r2 <- data.frame(vect.r2=veg2.VegChem.fit7$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=veg2.VegChem.fit7$vectors[[4]])
    fit.vars <- data.frame(fit.r2, fit.SIG)
    fit.vars$Var <- rownames(fit.vars)
write.csv(fit.vars, file="Veg2.VegChem.Vectors.csv", row.names=F) # extract ENVFIT values
#### plot - SiteVars Biplot (vs Species)
jpeg(file="Veg2_ord_Sig_VegChemVars-2.jpg", width=7, height=7, units="in", quality=80, res=500);
    plot(veg2.nmds2, display="sites", type="none")
    text(veg2.nmds2, "species", cex=0.5)
    plot(veg2.VegChem.fit7, cex=0.7, p.max=0.006)
dev.off()
###### Now, Correlations b/w Axes and Vars  #####
Veg.VegChem.scores <- merge(VEG2.SCORES3.ENV2[,c(1:3)], Veg.ALL3,
                          by.x="key.dist", by.y="key.dist", all.x=T)
CorX.dat <- Veg.VegChem.scores[,c(2,3)]
CorY.dat <- Veg.VegChem.scores[,c(4:length(names(Veg.VegChem.scores)))]
Veg2.VegChem.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Veg2.VegChem.Cor1$r,2)
    signif(Veg2.VegChem.Cor1$P,4)
cor.dat <- Veg2.VegChem.Cor1$r
sig.dat <- Veg2.VegChem.Cor1$P
    write.csv(Veg2.VegChem.Cor1$r, file="Veg2.Ord.VegChem.Cor.csv", row.names=F);
    write.csv(Veg2.VegChem.Cor1$P, file="Veg2.Ord.VegChem.CorSIG.csv", row.names=F);
# plotting ::  This runs the whole matrix (square), not just the 2 x k matrix...#####
jpeg(file="Corrplot_Veg2Ord-x-VegChem_Pearsn_1.jpg", width=7, height=7, units="in", 
                            quality=80, res=500);
    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
             p.mat=sig.dat, sig.level=0.01, insig="blank", tl.cex=0.5, order="original");
dev.off();
#############  160328 / 160329  ########








##  plot(veg2.nmds2, display="species")  ## compare variables against SPP space
text(veg2.nmds2, display="species")
plot(veg2.fit2)   ## keep these two plots together  -- biplot of cont.vars

## Now fitting categorical Vars
veg2.fit3 <- envfit(veg2.nmds2 ~ Year + Dist + Soil.H2S + Wtrshd + Water.Src, 
                    data=veg2.vars, permu = 999) # Categ. vars

plot(veg2.nmds2, display="sites")
plot(veg2.fit3)   ## keep these two plots together  -- biplot of categ.vars

## break up all the categorical Vars (w/ p< 0.05) :: Year // Soil.H2S // Wtrshd // Water.Src
# Years
plot(veg2.nmds2, display="sites")
plot(envfit(veg2.nmds2 ~ Year, data=veg2.vars, permu = 999))
with(veg2.scores3, ordihull(veg2.nmds2, as.factor(Year), col=factor(Year)))

plot(veg2.nmds2, display="species")
plot(envfit(veg2.nmds2 ~ Year, data=veg2.vars, permu = 999))
with(veg2.scores3, ordihull(veg2.nmds2, as.factor(Year)))
# Distance
plot(veg2.nmds2, display="sites")
plot(envfit(veg2.nmds2 ~ Dist, data=veg2.vars, permu = 999))
with(veg2.scores3, ordihull(veg2.nmds2, as.factor(Dist)))

# H2S
plot(veg2.nmds2, display="sites")
plot(envfit(veg2.nmds2 ~ Soil.H2S, data=veg2.vars, permu = 999))
with(veg2.scores3, 
     ordihull(veg2.nmds2, Soil.H2S, draw="polygon", col="blue", alpha=80, show.groups="N"))
with(veg2.scores3,
     ordihull(veg2.nmds2, Soil.H2S, draw="polygon", col="red", alpha=80, show.groups="Y"))

# Wtrshd
plot(veg2.nmds2, display="sites")
plot(envfit(veg2.nmds2 ~ Wtrshd, data=veg2.vars, permu = 999))
with(veg2.scores3, 
     ordihull(veg2.nmds2, Wtrshd, draw="polygon", col="blue", alpha=80, show.groups="Bear"))
with(veg2.scores3,
     ordihull(veg2.nmds2, Wtrshd, draw="polygon", col="red", alpha=80, show.groups="Farm"))
with(veg2.scores3,
     ordihull(veg2.nmds2, Wtrshd, draw="polygon", col="yellow", alpha=80, show.groups="SV"))

# Water.Src
plot(veg2.nmds2, display="sites")
plot(envfit(veg2.nmds2 ~ Water.Src, data=veg2.vars, permu = 999))
with(veg2.scores3, 
     ordihull(veg2.nmds2, Water.Src, draw="polygon", col="blue", alpha=80, show.groups="Chan"))
with(veg2.scores3,
     ordihull(veg2.nmds2, Water.Src, draw="polygon", col="red", alpha=80, show.groups="GW"))
with(veg2.scores3,
     ordihull(veg2.nmds2, Water.Src, draw="polygon", col="yellow", alpha=80, show.groups="Inter"))
with(veg2.scores3,
     ordihull(veg2.nmds2, Water.Src, draw="polygon", col="green", alpha=80, show.groups="UPDES"))

####################################################





















