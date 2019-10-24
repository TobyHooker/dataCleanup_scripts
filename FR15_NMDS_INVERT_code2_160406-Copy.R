####  Invertebrate Ordination and Fitting of Environmental Factors ##
##############    160331    

Inv.dist2 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Inv.dist2_160331.csv")
# this file is the trimmed (>5% rel. freq.) taxa abundance

Inv.dist2.FULL <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Inv.dist2.FULL_160331.csv")
# this file is the full (xDIST) invert dataset, 67 taxa + 4 metrics...to be used for SI calcs

##########  Calculate Simpson's Diversity Index #############
library(vegan);
dat <- Inv.dist2.FULL[,c(8:74)]

a <- diversity(dat, index="simpson")

Inv.dist2.FULL.1 <- data.frame(Inv.dist2.FULL, Simp=a)  ##  this  worked
## cycle through all var's
inv.var <- data.frame()  ## prepare DF
for (i in 1:length(names(Inv.dist2.FULL.1))) {
  class_i <- class(Inv.dist2.FULL.1[,i])
  name_i <- names(Inv.dist2.FULL.1[i])
  num.NAs <- sum(is.na(Inv.dist2.FULL.1[,i]))
inv.var <- rbind(inv.var, data.frame(i, name_i, class_i, num.NAs))     };
###########
Inv.dist2.metrics <- Inv.dist2.FULL.1[,c(1,76:79)]  ## Invertebrate Metrics
###########################################################################
######################  N M D S   O r d i N a t I o N  --  Inverts  #######
###########################################################################
inv.dat <- Inv.dist2
row.names(inv.dat) <- inv.dat$key.dist
inv.dat2 <- inv.dat[,c(8:47)]   ## this works...the matrix should be trackable for subseq. analyses
#  write.csv(inv.dat2, "invert.dat.matrix.csv")
# data eval::            #######
inv.var2 <- data.frame()  ## prepare DF
for (i in 1:length(names(inv.dat2))) {
  class_i <- class(inv.dat2[,i])
  name_i <- names(inv.dat2[i])
  num.NAs <- sum(is.na(inv.dat2[,i]))
inv.var2 <- rbind(inv.var2, data.frame(i, name_i, class_i, num.NAs))     };
###########
# No NAs !!  Good !! ###

inv.nmds.2 <- metaMDS(inv.dat2, distance="bray", k=2)   ## This is the Ordination ###
inv.nmds.3 <- metaMDS(inv.dat2, distance="bray", k=3)   ## k=3 is better...

inv.stress <- stressplot(inv.nmds.2)  # shat = 0.21
inv.stress.3 <- stressplot(inv.nmds.3)  # shat = 0.16

######  NEW  160331  ####  NMDS  SCREE PLOT  #######
nmds.scree <- function(x){
    plot(rep(1,10), replicate(10, metaMDS(x, k=1)$stress),
         xlim=c(1,6), ylim=c(0,0.5), xlab="# Dimensions", ylab="Stress", 
         main="NMDS Stress plot: Invertebrates")
    for (i in 1:6) {
        points(rep(i+1, 10), replicate(10, metaMDS(x, k=i+1)$stress))   }   };

jpeg("NMDS_invert_Scree.jpeg", width=5, height=5, units="in", quality=80, res=500)
nmds.scree(inv.dat2)
dev.off();
########################## Exploring NMDS Results #################

plot(inv.nmds.2)

plot(inv.nmds.3, choices=c(1,2))
plot(inv.nmds.3, choices=c(1,3))
plot(inv.nmds.3, choices=c(2,3))

plot(inv.nmds.3, choices=c(1,2), display="sp", type="t")
plot(inv.nmds.3, choices=c(1,3), display="sp", type="t")
plot(inv.nmds.3, choices=c(2,3), display="sp", type="t")

#############  Use k = 3 for invertebrates  **  PLOTTING  ###################
jpeg("INVnmds.Taxa-3d_species_160405.jpeg", width=7, height=5, units="in", quality=80, res=500)
a <- ordiplot3d(inv.nmds.3, display="species", choices=1:3, col="black", 
                ax.col="green", arr.col="blue", type="n")
text(a, "points", col="red", cex=0.7)
dev.off();

##################################################
###  New plots for FACTORS  060404 *** ###########
cols <- c("Chan" = "black", "GW" = "brown", "Inter" = "darkgreen", "IW" = "blue", "UPDES" = "red")
colZ <- c("brown", "black", "darkgreen", "blue", "red")

plot(inv.nmds.3, choices=c(1,2), display="si")
#ordihull(inv.nmds.3, groups = inv.3.scores4$Water.Src, choices=c(1,2), label=T, 
#         col=colZ, draw="lines", lty = 'dotted')
ordispider(inv.nmds.3, groups = inv.3.scores4$Water.Src, choices=c(1,2), label=T)

plot(inv.nmds.3, choices=c(1,3), display="si")
#ordihull(inv.nmds.3, groups = inv.3.scores4$Water.Src, choices=c(1,3), label=T)
ordispider(inv.nmds.3, groups = inv.3.scores4$Water.Src, choices=c(1,3), label=T)

plot(inv.nmds.3, choices=c(2,3), display="si")
#ordihull(inv.nmds.3, groups = inv.3.scores4$Water.Src, choices=c(2,3), label=T)
ordispider(inv.nmds.3, groups = inv.3.scores4$Water.Src, choices=c(2,3), label=T)

dev.off()

a <- plot(inv.nmds.3, choices=c(1,2), display="si")
#ordihull(inv.nmds.3, groups = inv.3.scores4$Wtrshd, choices=c(1,2), label=T)
ordispider(a, groups = inv.3.scores4$Wtrshd, choices=c(1,2), label=T)

b <- plot(inv.nmds.3, choices=c(1,3), display="si")
#ordihull(inv.nmds.3, groups = inv.3.scores4$Wtrshd, choices=c(1,3), label=T)
ordispider(b, groups = inv.3.scores4$Wtrshd, choices=c(1,3), label=T)

c <- plot(inv.nmds.3, choices=c(2,3), display="si")
#ordihull(inv.nmds.3, groups = inv.3.scores4$Wtrshd, choices=c(2,3), label=T)
ordispider(inv.nmds.3, groups = inv.3.scores4$Wtrshd, choices=c(2,3), label=T)

dev.off()


## If needed, can use grid to combine plot objects into single figure...
## ^^^ THIS WILL NOT WORK !!
###########  Newer plots ^^^^   160404  ######


#points(a, "points", pch=13, col="red", cex=0.5)   ########

b <- ordiplot3d(inv.nmds.3, display="sites", choices=1:3, col="black", 
                ax.col="green", arr.col="blue", type="h")

c <- ordiplot3d(inv.nmds.3, display="sites", choices=1:3, col="black", 
                ax.col="green", arr.col="blue", type="n")

# Extract Scores for additional work....[k=2 & k=3] ################
inv.2.scores <- as.data.frame(scores(inv.nmds.2))
inv.2.TAXA.scores <- as.data.frame(scores(inv.nmds.2, "species"))
inv.2.TAXA.scores$TAXA <- rownames(inv.2.TAXA.scores)

inv.3.scores <- as.data.frame(scores(inv.nmds.3))
inv.3.TAXA.scores <- as.data.frame(scores(inv.nmds.3, "species"))
inv.3.TAXA.scores$TAXA <- rownames(inv.3.TAXA.scores)
# Add site classification vars...############
SiteVar.Dist2 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/SiteVar.Dist_2.csv")

##  ^^^ Site Var data
Inv.dist2.metrics ## invert metrics...

inv.dat2 ## taxa (input to NMDS)
inv.taxa <- inv.dat2
inv.taxa$key.dist <- rownames(inv.taxa)
##  A good start
combine vars w/ Scores...#############
inv.2.scores$key.dist <- rownames(inv.2.scores)
inv.2.scores2 <- merge(inv.2.scores, SiteVar.Dist2, by="key.dist", sort=F)
inv.2.scores3 <- merge(inv.2.scores2, Inv.dist2.metrics, by="key.dist", sort=F)
inv.2.scores4 <- merge(inv.2.scores3, inv.taxa, by="key.dist", sort=F)

inv.3.scores$key.dist <- rownames(inv.3.scores)
inv.3.scores2 <- merge(inv.3.scores, SiteVar.Dist2, by="key.dist", sort=F)
inv.3.scores3 <- merge(inv.3.scores2, Inv.dist2.metrics, by="key.dist", sort=F)
inv.3.scores4 <- merge(inv.3.scores3, inv.taxa, by="key.dist", sort=F)

## k=3 [base] plots or Invert ordination ####
# ord.dat <-  inv.nmds.3
# scores:  inv.3.scores     &       inv.3.TAXA.scores
# env factors:  inv.3.scores4           ########################

#### Invert TAXA Plots (1:3 axes)  #######

z12 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                                aes(x=NMDS1, y=NMDS2, label=TAXA), alpha=0.5, size=2, col="red");

z13 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                                aes(x=NMDS1, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="red");

z23 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                                aes(x=NMDS2, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="red");

##### blank place-holder plot  [  blankPlot  ] ######
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
   axis.line = element_blank()   ); 
######  Invert Taxa #####
jpeg(filename="INV.NMDS_3_TAXA.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(z12, z13, z23, blankPlot, ncol=2, nrow=2, widths=4, heights=4,
             main=textGrob("NMDS for Invertebrate Community Composition [k=3 axes]", vjust=1))
dev.off()
#####

#### Invert Site Plots (1:3 axes)  #######
cols <- c("Chan" = "black", "GW" = "brown", "Inter" = "darkgreen", "IW" = "blue", "UPDES" = "red")

x12 <- ggplot() + theme_bw() + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "IW", "UPDES"),
                values=cols, guide=guide_legend(title="Water Source")) +
        geom_point(data=inv.3.scores4, aes(x=NMDS1, y=NMDS2, col=Water.Src), size=2);

x13 <- ggplot() + theme_bw() + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "IW", "UPDES"),
                            values=cols) + guides(col=FALSE) +
        geom_point(data=inv.3.scores4, aes(x=NMDS1, y=NMDS3, col=Water.Src), size=2);

x23 <- ggplot() + theme_bw() + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "IW", "UPDES"),
                            values=cols) + guides(col=FALSE) +
        geom_point(data=inv.3.scores4, aes(x=NMDS2, y=NMDS3, col=Water.Src), size=2);

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

x_legend <- g_legend(x12)
#  print(x_legend)

######  Invert x Sites ##
jpeg(filename="INV.NMDS_3_Sites.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(x12 + theme(legend.position="none"), x13, x23, x_legend, 
        ncol=2, nrow=2, widths=4, heights=4,
        main=textGrob("NMDS for Invertebrate Community Composition {Sites} [k=3 axes]", vjust=1))
dev.off()
#####

#### Invert Site Plots (1:3 axes) + lines for Site x Transect  #######
cols <- c("Chan" = "black", "GW" = "brown", "Inter" = "darkgreen", "IW" = "blue", "UPDES" = "red")

q12 <- ggplot() + theme_bw() + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "Impnd WetL", "UPDES"),
                values=cols, guide=guide_legend(title="Water Source")) +
        geom_point(data=inv.3.scores4, aes(x=NMDS1, y=NMDS2, col=Water.Src), size=2) +
        geom_line(data=inv.3.scores4, aes(x=NMDS1, y=NMDS2, group=Site, col=Water.Src), alpha=0.2);

q13 <- ggplot() + theme_bw() + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "Impnd WetL", "UPDES"),
                            values=cols) + guides(col=FALSE) +
        geom_point(data=inv.3.scores4, aes(x=NMDS1, y=NMDS3, col=Water.Src), size=2) +
    geom_line(data=inv.3.scores4, aes(x=NMDS1, y=NMDS3, group=Site, col=Water.Src), alpha=0.2);

q23 <- ggplot() + theme_bw() + 
        scale_colour_manual(labels=c("Channel", "Groundwater", "Interior", "Impnd WetL", "UPDES"),
                            values=cols) + guides(col=FALSE) +
        geom_point(data=inv.3.scores4, aes(x=NMDS2, y=NMDS3, col=Water.Src), size=2) +
    geom_line(data=inv.3.scores4, aes(x=NMDS2, y=NMDS3, group=Site, col=Water.Src), alpha=0.2);

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

x_legend <- g_legend(q12)
#  print(x_legend)
######  Invert x Sites ##
jpeg(filename="INV.NMDS_3_Sites_2b.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(q12 + theme(legend.position="none"), q13, q23, x_legend, 
        ncol=2, nrow=2, widths=4, heights=4,
        main=textGrob("NMDS for Invertebrate Community Composition {Sites} [k=3 axes]", vjust=1))
dev.off()
#####
#####################################################################
#################  Fit Environmental Variables  #####################
#####################################################################
### Export Environmental Site & Species Scores and Add in Response Vars ######
write.csv(inv.3.scores, file="inv3.SCORES.ENV1.csv", row.names=F)
write.csv(inv.3.TAXA.scores, file="inv3.taxa.SCORES.ENV1.csv", row.names=F);
write.csv(inv.3.scores4, file="inv3.SCORES4.ENV1.csv", row.names=F); ### *** ###
### Examine Env.Scores.Vars data **  ##########
inv3.Envar <- data.frame()  ## prepare DF
for (i in 1:length(names(inv.3.scores4))) {
  class_i <- class(inv.3.scores4[,i])
  name_i <- names(inv.3.scores4[i])
  num.NAs <- sum(is.na(inv.3.scores4[,i]))
inv3.Envar <- rbind(inv3.Envar, data.frame(i, name_i, class_i, num.NAs))  };  ## ** ##
###########
##      Look at Individual Plant Taxa vs. NMDS Axes     ######
## Use data from NMDS:  data == inv.dat2 //  nmds.object == inv.nmds.3
Inv3.taxa.fit <- envfit(inv.nmds.3, inv.dat2, choices=1:3, permu = 999) ## run vs Veg taxa
    fit.r2 <- data.frame(vect.r2=Inv3.taxa.fit$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=Inv3.taxa.fit$vectors[[4]])
    Inv3.taxa.fit.vars <- data.frame(fit.r2, fit.SIG)
    Inv3.taxa.fit.vars$Var <- rownames(Inv3.taxa.fit.vars)
write.csv(Inv3.taxa.fit.vars, file="Invert-3-Taxa.Vectors.csv", row.names=F) # extract ENVFIT values
Inv3.TaxaScores.sig <- merge(inv.3.TAXA.scores, Inv3.taxa.fit.vars, by.x="TAXA", by.y="Var", sort=F)
    write.csv(Inv3.TaxaScores.sig, file="Invert-3-Taxa.VectorSIG.csv", row.names=F)
#   Inv3.TaxaScores.sig <- merge(inv.3.TAXA.scores, Inv3.taxa.fit.vars, by.x="TAXA", by.y="Var")
###########  ENVFIT  (Invert Taxa)  xx  B I P L O T  xx  ###########
## Biplot of significant Invert Taxa  [use:  Inv3.TaxaScores.sig  [^^^]]
z12 <- ggplot() + theme_bw() + geom_text(data=Inv3.TaxaScores.sig, 
                    aes(x=NMDS1, y=NMDS2, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.TaxaScores.sig, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS1*(sqrt(vect.r2)), y=0, yend=NMDS2*(sqrt(vect.r2))),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F);

z13 <- ggplot() + theme_bw() + geom_text(data=Inv3.TaxaScores.sig, 
                    aes(x=NMDS1, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.TaxaScores.sig, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS1*(sqrt(vect.r2)), y=0, yend=NMDS3*(sqrt(vect.r2))),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F);

z23 <- ggplot() + theme_bw() + geom_text(data=Inv3.TaxaScores.sig, 
                    aes(x=NMDS2, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.TaxaScores.sig, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS2*(sqrt(vect.r2)), y=0, yend=NMDS3*(sqrt(vect.r2))),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F);
##### blank place-holder plot  [  blankPlot  ] ######
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
   axis.line = element_blank()   ); 
######  Invert Taxa #####
jpeg(filename="INV.NMDS_3_TAXAsig.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(z12, z13, z23, blankPlot, ncol=2, nrow=2, widths=4, heights=4,
             main=textGrob("Significant Invertebrate Taxa", vjust=1))
dev.off()
#####
##### Vector Correlation Analysis  ######
#  Not appropriate - deleted

##########################################################
##      Look at Invert & Site Metrics vs. NMDS Axes     ######
## Use data from NMDS:  data == inv.3.scores4 //  nmds.object == inv.nmds.3  
Inv3.SiteVMET.fit <- envfit(inv.nmds.3 ~ Sum.Aq + veg.Rich + Bare.mud + Em.Veg + 
            Litter.cov + Water.cov + Water.z.cm + Veg.Ht.cm + RelCov.INV + veg.BioVol + 
            Inv.Biom.m2 + Inv.Abund + Inv.Rich + Simp, 
            inv.3.scores4, choices=1:3, permu = 999) ## Cont. vars
    fit.r2 <- data.frame(vect.r2=Inv3.SiteVMET.fit$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=Inv3.SiteVMET.fit$vectors[[4]])
    fit.SCORES <- data.frame(vect.SCORE=Inv3.SiteVMET.fit$vectors[[1]])
        colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
    Inv3.SiteVMET.fit.vars <- data.frame(fit.r2, fit.SIG, fit.SCORES)
    Inv3.SiteVMET.fit.vars$Var <- rownames(Inv3.SiteVMET.fit.vars)
write.csv(Inv3.SiteVMET.fit.vars, file="Invert-3-SiteVar+InvMetrics.Vectors.csv", row.names=F) # extract ENVFIT values
###########  ENVFIT  (Invert Taxa)  xx  B I P L O T  xx  ###########
## Biplot of significant Invert Taxa  [use:  Inv3.TaxaScores.sig  [^^^]]
z12 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS1, y=NMDS2, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS1*(sqrt(vect.r2)), y=0, yend=NMDS2*(sqrt(vect.r2))),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                        aes(x=NMDS1*(sqrt(vect.r2)), y=NMDS2*(sqrt(vect.r2)),label=Var),
                      size=2.5, col="blue", alpha=0.8, 
                      position=position_dodge(0.9), vjust=0, hjust=0);

z13 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS1, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS1*(sqrt(vect.r2)), y=0, yend=NMDS3*(sqrt(vect.r2))),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                        aes(x=NMDS1*(sqrt(vect.r2)), y=NMDS3*(sqrt(vect.r2)),label=Var),
                      size=2.5, col="blue", alpha=0.8, 
                      position=position_dodge(0.9), vjust=0, hjust=0);

z23 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS2, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS2*(sqrt(vect.r2)), y=0, yend=NMDS3*(sqrt(vect.r2))),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                        aes(x=NMDS2*(sqrt(vect.r2)), y=NMDS3*(sqrt(vect.r2)),label=Var),
                      size=2.5, col="blue", alpha=0.8, 
                      position=position_dodge(0.9), vjust=0, hjust=0);
##### blank place-holder plot  [  blankPlot  ] ######
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
   axis.line = element_blank()   ); 
######  Invert Taxa x Site Vars & Invert Metriccs #####
jpeg(filename="INV.NMDS_3_SiteV+InvMET_sig.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(z12, z13, z23, blankPlot, ncol=2, nrow=2, widths=4, heights=4,
             main=textGrob("Significant Site Variables", vjust=1))
dev.off()
######
#### PLOT REDO ::  160406  ########
### Site Vars x TAXA
jpeg(file="INV3.ord_Sig-SiteVars_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(inv.nmds.3, display="spec", type="t", choices=c(1,2))
    plot(Inv3.SiteVMET.fit, p.max=0.005, choices=c(1,2), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(1,3))
    plot(Inv3.SiteVMET.fit, p.max=0.005, choices=c(1,3), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(2,3))
    plot(Inv3.SiteVMET.fit, p.max=0.005, choices=c(2,3), cex=0.7)

dev.off();
#####




#################################################################################
#####  REDO of plot above -- leave out Inv Taxa Scores !!! [too crowded]  ######
## Biplot of significant Invert Taxa  [use:  Inv3.TaxaScores.sig  [^^^]]  #######
z12 <- ggplot() + theme_bw() + geom_blank(data=inv.3.TAXA.scores, aes(x=NMDS1, y=NMDS2)) +
            geom_segment(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue") +
            geom_text(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                        aes(x=NMDS1, y=NMDS2,label=Var),
                      size=2, col="blue", alpha=0.8, check_overlap=T,
                      position=position_dodge(0.9), vjust=0.5, hjust=0.5);

z13 <- ggplot() + theme_bw() + geom_blank(data=inv.3.TAXA.scores, aes(x=NMDS1, y=NMDS3)) +
            geom_segment(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS1, y=0, yend=NMDS3),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue") +
            geom_text(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                        aes(x=NMDS1, y=NMDS3,label=Var),
                      size=2, col="blue", alpha=0.8,  check_overlap=T,
                      position=position_dodge(0.9), vjust=0.5, hjust=0.5);

z23 <- ggplot() + theme_bw() + geom_blank(data=inv.3.TAXA.scores, aes(x=NMDS2, y=NMDS3)) +
            geom_segment(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                         aes(x=0, xend=NMDS2, y=0, yend=NMDS3),
                         arrow=arrow(length=unit(0.25, "cm")), colour="blue") +
            geom_text(data=subset(Inv3.SiteVMET.fit.vars, vect.sig < 0.05), 
                        aes(x=NMDS2, y=NMDS3,label=Var),
                      size=2, col="blue", alpha=0.8,  check_overlap=T,
                      position=position_dodge(0.9), vjust=0.5, hjust=0.5);
##### blank place-holder plot  [  blankPlot  ] ######
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
   axis.line = element_blank()   ); 
######  Invert Taxa x Site Vars & Invert Metriccs #####
jpeg(filename="INV.NMDS_3_SiteV+InvMET_sig_v2.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(z12, z13, z23, blankPlot, ncol=2, nrow=2, widths=4, heights=4,
             main=textGrob("Significant Site Variables", vjust=1))
dev.off()
#####
##### Vector Correlation Analysis  ######
# pull in key.dist from data to nmds data.matrix for Corr analysis
## data ==  inv.3.scores4       ## key.dist + NMDS scores + Vars...[subset]
# see:  inv3.Envar      for listing...
Inv.3.Scor.Taxa <- inv.3.scores4[,c(1:4, 31:70)]
##
CorX.dat <- inv.3.scores4[,c(2:4)]
CorY.dat <- inv.3.scores4[,c(14,23,12,15,17,21,22,20,24,26,27,28,29,30)]
Inv.SiteVMETx.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Inv.SiteVMETx.Cor1$r,2)
    signif(Inv.SiteVMETx.Cor1$P,2)
cor.dat <- Inv.SiteVMETx.Cor1$r
sig.dat <- Inv.SiteVMETx.Cor1$P
    write.csv(Inv.SiteVMETx.Cor1$r, file="Invert-3.Ord.SiteV+Met.Cor.csv", row.names=F);
    write.csv(Inv.SiteVMETx.Cor1$P, file="Invert-3.Ord.SiteV+Met.SIG.csv", row.names=F);
################
#### Site VAR -- FACTORs x NMD  -- ########
Inv3.SiteVfactors.fit2 <- envfit(inv.nmds.3 ~ Wtrshd + Water.Src, 
            inv.3.scores4, choices=1:3, permu = 999) ## Cont. vars
    fit.SCORES <- data.frame(vect.SCORE=Inv3.SiteVfactors.fit2$factors[[1]])
        colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
    Inv3.SiteVfactors.fit.vars <- data.frame(fit.SCORES)
    Inv3.SiteVfactors.fit.vars$Var <- rownames(Inv3.SiteVfactors.fit.vars)
write.csv(Inv3.SiteVfactors.fit.vars, file="Invert-3-SiteVar.Factors.csv", row.names=F) # extract ENVFIT values




##      Look at Water Chem vs. INV-NMDS Axes     ######
WChem.dist0 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/GSL_Fringe2015/FR15_R/WChem.dist_160325.csv")

WChem0.var <- data.frame()  ## prepare DF to keep track of Var types #######
    for (i in 1:length(names(WChem.dist0))) {
        class_i <- class(WChem.dist0[,i])
        name_i <- names(WChem.dist0[i])
        num.NAs <- sum(is.na(WChem.dist0[,i]))
    WChem0.var <- rbind(WChem0.var, data.frame(i, name_i, class_i, num.NAs)) };
## Fit Water Chem to Veg-NMDS Ordination #######
WChem.dist1 <- subset(WChem.dist0, Dist.m != 0)
WChem.dist1 <- WChem.dist1[,c(2,6:9,11,13:60)]
inv.key <- data.frame(key.dist=inv.3.scores4$key.dist)
    data1 <- merge(inv.key, WChem.dist1, by.x="key.dist", by.y="key.dist2")
row.names(data1) <- data1$key.dist
data1 <- data1[,-1]     ### use for WChem data...
## ok !!
## Use data from NMDS:  data == inv.3.scores4 //  nmds.object == inv.nmds.3  
Inv3.Wchem.fit <- envfit(inv.nmds.3, data1, choices=1:3, permu = 999) ## Cont. vars
    fit.r2 <- data.frame(vect.r2=Inv3.Wchem.fit$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=Inv3.Wchem.fit$vectors[[4]])
    fit.SCORES <- data.frame(vect.SCORE=Inv3.Wchem.fit$vectors[[1]])
        colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
    Inv3.Wchem.fit.vars <- data.frame(fit.r2, fit.SIG, fit.SCORES)
    Inv3.Wchem.fit.vars$Var <- rownames(Inv3.Wchem.fit.vars)
write.csv(Inv3.Wchem.fit.vars, file="Invert-3-WChem.Vectors.csv", row.names=F) # extract ENVFIT values
###########  ENVFIT  (Invert Taxa)  xx  B I P L O T  xx  ###########
### WaterChem Vars x TAXA :: 160406

jpeg(file="INV3.ord_Sig-WChem_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(inv.nmds.3, display="spec", type="t", choices=c(1,2))
    plot(Inv3.Wchem.fit, p.max=0.005, choices=c(1,2), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(1,3))
    plot(Inv3.Wchem.fit, p.max=0.005, choices=c(1,3), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(2,3))
    plot(Inv3.Wchem.fit, p.max=0.005, choices=c(2,3), cex=0.7)

dev.off();
#####


## Biplot of significant WCHem  [use:  Inv3.TaxaScores.sig  [^^^]]
z12 <- ggplot() + theme_bw() + geom_blank(data=inv.3.TAXA.scores,aes(x=NMDS1, y=NMDS2)) +
            geom_segment(data=subset(Inv3.Wchem.fit.vars, vect.sig < 0.005), 
            aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.Wchem.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS1, y=NMDS2,label=Var),
                    size=2.5, col="blue", alpha=0.8, 
                    position=position_dodge(0.9), vjust=-0.5, hjust=-0.5);

z13 <- ggplot() + theme_bw() + geom_blank(data=inv.3.TAXA.scores, 
                    aes(x=NMDS1, y=NMDS3)) +
            geom_segment(data=subset(Inv3.Wchem.fit.vars, vect.sig < 0.005), 
                 aes(x=0, xend=NMDS1, y=0, yend=NMDS3),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.Wchem.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS1, y=NMDS3,label=Var),
                    size=2.5, col="blue", alpha=0.8, 
                    position=position_dodge(0.9), vjust=0, hjust=0);

z23 <- ggplot() + theme_bw() + geom_blank(data=inv.3.TAXA.scores, 
                    aes(x=NMDS2, y=NMDS3)) +
            geom_segment(data=subset(Inv3.Wchem.fit.vars, vect.sig < 0.005), 
                aes(x=0, xend=NMDS2, y=0, yend=NMDS3),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.Wchem.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS2, y=NMDS3,label=Var),
                  size=2.5, col="blue", alpha=0.8, 
                  position=position_dodge(0.75), vjust=-0.5, hjust=-0.5);
##### blank place-holder plot  [  blankPlot  ] ######
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
   axis.line = element_blank()   ); 
######  Invert Taxa x Water Chem #####
jpeg(filename="INV.NMDS_3_WChem_sig2_v2.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(z12, z13, z23, blankPlot, ncol=2, nrow=2, widths=4, heights=4,
             main=textGrob("Significant Water Chemistry Variables (p<0.005)", vjust=1))
dev.off()
#####
##### Vector Correlation Analysis  ######
# pull in key.dist from data to nmds data.matrix for Corr analysis
## data ==  inv.3.scores4       ## key.dist + NMDS scores + Vars...[subset]
# see:  inv3.Envar      for listing...
Inv.3.Scor.Taxa <- inv.3.scores4[,c(1:4, 31:70)]
##
CorX.dat <- inv.3.scores4[,c(2:4)]
CorY.dat <- data1
Inv.WChem.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Inv.WChem.Cor1$r,2)
    signif(Inv.WChem.Cor1$P,3)
cor.dat <- Inv.WChem.Cor1$r
sig.dat <- Inv.WChem.Cor1$P
    write.csv(Inv.WChem.Cor1$r, file="Invert-3.Ord.WChem.Cor.csv", row.names=F);
    write.csv(Inv.WChem.Cor1$P, file="Invert-3.Ord.WChem.SIG.csv", row.names=F);
################
##      Look at Soil Chem vs. INV-NMDS Axes     ######
Soil.ALL1 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/GSL_Fringe2015/FR15_R/Soil.MASTER2_160328.csv")

Soil.ALL2 <- ddply(Soil.ALL1, .(MLID, Year, Dist), numcolwise(mean, na.rm=T))
Soil.ALL2$key.dist <- paste(Soil.ALL2$MLID, ".", Soil.ALL2$Year, ".", Soil.ALL2$Dist, sep="")
Soil.ALL2 <- Soil.ALL2[,c(40,1:39)]
SoilChem.var <- data.frame()  ## prepare DF to keep track of Var types
    for (i in 1:length(names(Soil.ALL2))) {
        class_i <- class(Soil.ALL2[,i])
        name_i <- names(Soil.ALL2[i])
        num.NAs <- sum(is.na(Soil.ALL2[,i]))
        SoilChem.var <- rbind(SoilChem.var, data.frame(i, name_i, class_i, num.NAs)) };

## Fit Water Chem to Veg-NMDS Ordination #######
Soil.ALL2 <- subset(Soil.ALL2, Dist != 0)
Soil.dat <- Soil.ALL2[,c(1,6:27,29:39)]
inv.key <- data.frame(key.dist=inv.3.scores4$key.dist)
    data1 <- merge(inv.key, Soil.dat, by.x="key.dist", by.y="key.dist")
    row.names(data1) <- data1$key.dist
data1 <- data1[,-1]     ### use for WChem data... ok !!  ####
## Use data from NMDS:  data == inv.3.scores4 //  nmds.object == inv.nmds.3  
Inv3.SoilCh.fit <- envfit(inv.nmds.3, data1, choices=1:3, permu = 999, na.rm=T) ## Cont. vars
    fit.r2 <- data.frame(vect.r2=Inv3.SoilCh.fit$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=Inv3.SoilCh.fit$vectors[[4]])
    fit.SCORES <- data.frame(vect.SCORE=Inv3.SoilCh.fit$vectors[[1]])
        colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
    Inv3.SoilCh.fit.vars <- data.frame(fit.r2, fit.SIG, fit.SCORES)
    Inv3.SoilCh.fit.vars$Var <- rownames(Inv3.SoilCh.fit.vars)
write.csv(Inv3.SoilCh.fit.vars, file="Invert-3-SoilCh.Vectors.csv", row.names=F) # extract ENVFIT values
###########  ENVFIT  (Invert Taxa)  xx  B I P L O T  xx  ###########

### Soil Chem Vars x TAXA :: 160406

jpeg(file="INV3.ord_Sig-SoilChem_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(inv.nmds.3, display="spec", type="t", choices=c(1,2))
    plot(Inv3.SoilCh.fit, p.max=0.0011, choices=c(1,2), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(1,3))
    plot(Inv3.SoilCh.fit, p.max=0.0011, choices=c(1,3), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(2,3))
    plot(Inv3.SoilCh.fit, p.max=0.0011, choices=c(2,3), cex=0.7)

dev.off();
#####

## Biplot of significant Invert Taxa  [use:  Inv3.TaxaScores.sig  [^^^]]
z12 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS1, y=NMDS2, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.SoilCh.fit.vars, vect.sig < 0.005), 
            aes(x=0, xend=NMDS1*0.7, y=0, yend=NMDS2*0.7),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.SoilCh.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS1*0.7, y=NMDS2*0.7,label=Var),
                    size=2.5, col="blue", alpha=0.8, 
                    position=position_dodge(0.9), vjust=-0.5, hjust=-0.5);

z13 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS1, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.SoilCh.fit.vars, vect.sig < 0.005), 
                 aes(x=0, xend=NMDS1*0.7, y=0, yend=NMDS3*0.7),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.SoilCh.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS1*0.7, y=NMDS3*0.7,label=Var),
                    size=2.5, col="blue", alpha=0.8, 
                    position=position_dodge(0.9), vjust=0, hjust=0);

z23 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS2, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.SoilCh.fit.vars, vect.sig < 0.005), 
                aes(x=0, xend=NMDS2*0.7, y=0, yend=NMDS3*0.7),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.SoilCh.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS2*0.7, y=NMDS3*0.7,label=Var),
                  size=2.5, col="blue", alpha=0.8, 
                  position=position_dodge(0.75), vjust=-0.5, hjust=-0.5);
##### blank place-holder plot  [  blankPlot  ] ######
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
   axis.line = element_blank()   ); 
######  Invert Taxa x Water Chem #####
jpeg(filename="INV.NMDS_3_SoilChem_sig.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(z12, z13, z23, blankPlot, ncol=2, nrow=2, widths=4, heights=4,
             main=textGrob("Significant Soil Chemistry Variables (p<0.005)", vjust=1))
dev.off()
#####
##### Vector Correlation Analysis  ######
# pull in key.dist from data to nmds data.matrix for Corr analysis
## data ==  inv.3.scores4       ## key.dist + NMDS scores + Vars...[subset]
# see:  inv3.Envar      for listing...
##
CorX.dat <- inv.3.scores4[,c(2:4)]
CorY.dat <- data1
Inv.SoilCh.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Inv.SoilCh.Cor1$r,2)
    signif(Inv.SoilCh.Cor1$P,3)
cor.dat <- Inv.SoilCh.Cor1$r
sig.dat <- Inv.SoilCh.Cor1$P
    write.csv(Inv.SoilCh.Cor1$r, file="Invert-3.Ord.SoilChem.Cor.csv", row.names=F);
    write.csv(Inv.SoilCh.Cor1$P, file="Invert-3.Ord.SiolChem.SIG.csv", row.names=F);
################
##      Look at Veg Chem vs. INV-NMDS Axes     ######
Veg.CNP0 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/GSL_Fringe2015/FR15_R/Veg.MASTER2.CNP_160328.csv")
#### Work up Veg Chem data (drop metals and Algal Mats for now)   #######
Veg.ALL2 <- Veg.CNP0
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
## Fit Water Chem to Veg-NMDS Ordination #######
inv.key <- data.frame(key.dist=inv.3.scores4$key.dist)
    data1 <- merge(inv.key, Veg.ALL3, by.x="key.dist", by.y="key.dist")
    row.names(data1) <- data1$key.dist
data1 <- data1[,-1]     ### use for WChem data... ok !!  ####
## Use data from NMDS:  data == inv.3.scores4 //  nmds.object == inv.nmds.3  
Inv3.VegCNP.fit <- envfit(inv.nmds.3, data1, choices=1:3, permu = 999, na.rm=T) ## Cont. vars
    fit.r2 <- data.frame(vect.r2=Inv3.VegCNP.fit$vectors[[2]])
    fit.SIG <- data.frame(vect.sig=Inv3.VegCNP.fit$vectors[[4]])
    fit.SCORES <- data.frame(vect.SCORE=Inv3.VegCNP.fit$vectors[[1]])
        colnames(fit.SCORES) <- c("NMDS1", "NMDS2", "NMDS3")
    Inv3.VegCNP.fit.vars <- data.frame(fit.r2, fit.SIG, fit.SCORES)
    Inv3.VegCNP.fit.vars$Var <- rownames(Inv3.VegCNP.fit.vars)
write.csv(Inv3.VegCNP.fit.vars, file="Invert-3-VegCNP.Vectors.csv", row.names=F) # extract ENVFIT values
###########  ENVFIT  (Invert Taxa)  xx  B I P L O T  xx  ###########

### Soil Chem Vars x TAXA :: 160406

jpeg(file="INV3.ord_Sig-LeafCNP_v1.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(3.8,3.8,0.5,0.5))
plot(inv.nmds.3, display="spec", type="t", choices=c(1,2))
    plot(Inv3.VegCNP.fit, p.max=0.0011, choices=c(1,2), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(1,3))
    plot(Inv3.VegCNP.fit, p.max=0.0011, choices=c(1,3), cex=0.7)

plot(inv.nmds.3, display="spec", type="t", choices=c(2,3))
    plot(Inv3.VegCNP.fit, p.max=0.0011, choices=c(2,3), cex=0.7)

dev.off();
#####

## Biplot of significant Invert Taxa  [use:  Inv3.TaxaScores.sig  [^^^]]
z12 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS1, y=NMDS2, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.VegCNP.fit.vars, vect.sig < 0.005), 
            aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.VegCNP.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS1, y=NMDS2,label=Var),
                    size=2.5, col="blue", alpha=0.8, 
                    position=position_dodge(0.9), vjust=-0.5, hjust=-0.5);

z13 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS1, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.VegCNP.fit.vars, vect.sig < 0.005), 
                 aes(x=0, xend=NMDS1, y=0, yend=NMDS3),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.VegCNP.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS1, y=NMDS3,label=Var),
                    size=2.5, col="blue", alpha=0.8, 
                    position=position_dodge(0.9), vjust=0, hjust=0);

z23 <- ggplot() + theme_bw() + geom_text(data=inv.3.TAXA.scores, 
                    aes(x=NMDS2, y=NMDS3, label=TAXA), alpha=0.5, size=2, col="black") +
            geom_segment(data=subset(Inv3.VegCNP.fit.vars, vect.sig < 0.005), 
                aes(x=0, xend=NMDS2, y=0, yend=NMDS3),
                 arrow=arrow(length=unit(0.25, "cm")), colour="blue", inherit_aes=F) +
            geom_text(data=subset(Inv3.VegCNP.fit.vars, vect.sig < 0.005), 
                    aes(x=NMDS2, y=NMDS3,label=Var),
                  size=2.5, col="blue", alpha=0.8, 
                  position=position_dodge(0.75), vjust=-0.5, hjust=-0.5);

##### blank place-holder plot  [  blankPlot  ] ######
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
   axis.line = element_blank()   ); 
######  Invert Taxa x Water Chem #####
jpeg(filename="INV.NMDS_3_VegCNP_sig.jpeg", width=7.5, heigh=6, units="in", res=500)
grid.arrange(z12, z13, z23, blankPlot, ncol=2, nrow=2, widths=4, heights=4,
             main=textGrob("Significant Leaf Chemistry Variables (p<0.005)", vjust=1))
dev.off()
#####
##### Vector Correlation Analysis  ######
# pull in key.dist from data to nmds data.matrix for Corr analysis
## data ==  inv.3.scores4       ## key.dist + NMDS scores + Vars...[subset]
# see:  inv3.Envar      for listing...
##
CorX.dat <- inv.3.scores4[,c(2:4)]
CorY.dat <- data1
Inv.VegCNP.Cor1 <- rcorr(x=as.matrix(CorY.dat), y=as.matrix(CorX.dat), type="pearson");
    signif(Inv.VegCNP.Cor1$r,2)
    signif(Inv.VegCNP.Cor1$P,3)
cor.dat <- Inv.VegCNP.Cor1$r
sig.dat <- Inv.VegCNP.Cor1$P
    write.csv(Inv.VegCNP.Cor1$r, file="Invert-3.Ord.VegCNP.Cor.csv", row.names=F);
    write.csv(Inv.VegCNP.Cor1$P, file="Invert-3.Ord.VegCNP.SIG.csv", row.names=F);
################

#######################################################################
#######################################################################
######   N E W  F I G s   ---  Invert Ordinations    ##################
##############################   160406   #############################
#######################################################################

##  [fig:  Invert NMDS Taxa + Sites (Water.Src)]
NMDS ==>  inv.nmds.3 
invert.nmds.3.copy <- inv.nmds.3
nmds[3].SCORES ==> inv.3.scores4
## try it w/ hulls and polys   This one is good ######
jpeg(file="Invert3.ord_TAXA-x-WaterSrc_3b.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(4,4,0.5,0.5))

plot(inv.nmds.3, display="sp", type="t", choices=c(1,2), cex=0.6)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="Chan",col="brown", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="GW",col="blue", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="Inter",col="green", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="IW",col="black", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="UPDES",col="red", 
               choices=c(1,2), label=T, cex=0.7, draw="polygon", alpha=80)

plot(inv.nmds.3, display="spec", type="t", choices=c(1,3), cex=0.6)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="Chan",col="brown", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="GW",col="blue", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="Inter",col="green", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="IW",col="black", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="UPDES",col="red", 
               choices=c(1,3), label=T, cex=0.7, draw="polygon", alpha=80)

plot(inv.nmds.3, display="spec", type="t", choices=c(2,3), cex=0.6)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="Chan",col="brown", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="GW",col="blue", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="Inter",col="green", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="IW",col="black", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)
    ordihull(inv.nmds.3, group=inv.3.scores4$Water.Src, show.groups="UPDES",col="red", 
               choices=c(2,3), label=T, cex=0.7, draw="polygon", alpha=80)

dev.off();
######

#####################   160408  ::  Simple Taxa NMDS Plot  #############
## ######
jpeg(file="Invert3.ord_TAXA_4.jpeg", width=7, height=7, units="in", quality=8, res=500);
    par(mfrow=c(2,2))
    par(mar=c(4,4,0.5,0.5))

plot(inv.nmds.3, display="sp", type="t", choices=c(1,2), cex=0.6)

plot(inv.nmds.3, display="spec", type="t", choices=c(1,3), cex=0.6)

plot(inv.nmds.3, display="spec", type="t", choices=c(2,3), cex=0.6)

dev.off();
######









