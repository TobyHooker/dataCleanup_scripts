#### Combining Datasets and Running (rank) Correlations :: 160105 ######
####
# Water Chemistry  ::  WChem3  [1654 x 115]
# Invertebrates    ::  INV1  [546 x 95]
# SAV and Mats     ::  SAVdat_dat4  [471 x 60]
SAVdat_dat3X <- data3X    ##  [347 x 61]

SAV.key.3X <- SAVdat_dat3X[,c(2:11,60)]
SAV.key.4 <- SAVdat_dat4[,c(2,3,5,6,55,56,57,58,59,60)]
SAV.key.3X$VAR3X <- "yes"

SAV.key <- join(SAV.key.4, SAV.key.3X, by=c("STORET", "Date2"))
SAV.key2 <- merge(SAV.key.4, SAV.key.3X, by=c("STORET", "Date"), all.x=T)

### Merge two SAV datasets, to include the SAV_SCORE variables in addition to the grab sample data...
SAVdat0 <- merge(SAVdat_dat4, SAVdat_dat3X, by=c("STORET", "Date"), all.x=T)
   sav0.nam <- as.data.frame(names(SAVdat0))
# cleanup
SAVdat1 <- SAVdat0[,c(1,2,4,5,55:60,8:11,16,116:119)]
#####   Merge Data sets, Export to XL, cleanup, then restart #######
Chem0 <- WChem3
Invert0 <- INV1
SAVdat1  ## just compiled above ^^^^  

Chem.Invert <- merge(Chem0, Invert0, by.x=c("MLID", "Date"), by.y=c("STORET", "Date"),
                     all.x=T)
Chem.Invert.SAV <- merge(Chem.Invert, SAVdat1, by.x=c("MLID", "Date"), by.y=c("STORET", "Date"),
                         all.x=T)
write.csv(Chem.Invert.SAV, file="Chem.Invert.SAV_0.csv", row.names=F)
####  Import from XLS  #######
Chem.Invert.SAV_1 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/Chem.Invert.SAV_1.csv")
   COR.dat0 <- Chem.Invert.SAV_1  ## backup copy
   COR.dat1 <- COR.dat0  ## working copy...UPDATED...See Below
#### Examine the VAR classes  ######
   COR1.var <- data.frame()
for (i in 1:length(names(COR.dat1))){
  class_i <- class(COR.dat1[,i])
  name_i <- names(COR.dat1[i])
  num.NAs <- sum(is.na(COR.dat1[,i]))
  COR1.var <- rbind(COR1.var, data.frame(i, name_i, class_i, num.NAs))  
#  WChem4[,i] <- as.numeric(WChem4[,i])
}
###  FIXING SOME VARs...  ####
# Metab_DO.p = factor  :: FIX
COR.dat1$Metab_DO.p <- as.numeric(COR.dat1$Metab_DO.p)
summary(COR.dat1$Metab_DO.p)
summary(COR.dat1$Metab_DO.c)
   COR.dat1[(COR.dat1$Metab_DO.p==1),43] <- NA   ## fixit
## trim Var list
COR.dat1 <- COR.dat1[,c(1:97)]
## fix DO data  ####################
COR.dat1  <- COR.dat1[with(COR.dat1, order(iden)),]

#####  Metab_DO.p is corrupted -- DO NOT USE !!!  ##  UPDATE 160107  ######

############
WChem.cordat <- COR.dat1[,c(15:53)]
SAV.cordat <- COR.dat1[,c(54:59)]
INV.cordat <- COR.dat1[,c(60:97)]
BIOLRESP.cordat <- COR.dat1[,c(54,56:58,41,42,44,60:65,67:69,71:88)]
CHEMxRESP.cordat <- COR.dat1[,c(16,17,20,21,22,24,25,40,41,42,44,47,50,51,53,52,54,56,57,58,
                                62,63,64,67,72,74,75,85,86,87,88)]
CHEMxRESP.cordat2 <- COR.dat1[,c(16,17,20,21,22,24,25,40,27:29,31,33,35:39,
                                 41,42,44,47,50,51,53,52,54,56,57,58,
                                62,63,64,67,72,74,75,85,86,87,88)]
###

##### Internal Correlations (among datasets) ###############################
library(Hmisc)

# Corr 1  :: WChem x Spearman   #################
Chem.cor1 <- rcorr(as.matrix(WChem.cordat), type="spearman")
   signif(Chem.cor1$r,2)
   signif(Chem.cor1$P,2)
#   cor.dat <- Chem.cor1$r
#   sig.dat <- Chem.cor1$P
write.csv(Chem.cor1$r, file="Chem.cor1.csv", row.names=FALSE)
write.csv(Chem.cor1$P, file="Chem.cor1SIG.csv", row.names=FALSE)
write.csv(Chem.cor1$n, file="Chem.cor1Samp.csv", row.names=FALSE)
## plotting
cor.dat <- Chem.cor1$r
sig.dat <- Chem.cor1$P

jpeg(file="Corrplot_WChem_Sprmn_1.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()
#############
# Corr 3  :: Invertebrates x Spearman   ################
data <- INV.cordat[,c(1:29)]
INV.cor1 <- rcorr(as.matrix(data), type="spearman")
   write.csv(INV.cor1$r, file="Invert.cor1.csv", row.names=FALSE)
   write.csv(INV.cor1$P, file="Invert.cor1SIG.csv", row.names=FALSE)
   write.csv(INV.cor1$n, file="Invert.cor1Samp.csv", row.names=FALSE)
## plotting
cor.dat <- INV.cor1$r
sig.dat <- INV.cor1$P

jpeg(file="Corrplot_Invert_Sprmn_1.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()

#####################
# Corr 4  :: Biological Response Measures x Spearman   ################

data <- BIOLRESP.cordat

BIOLRESP.cor1 <- rcorr(as.matrix(data), type="spearman")
   write.csv(BIOLRESP.cor1$r, file="BIOLRESP.cor1.csv", row.names=FALSE)
   write.csv(BIOLRESP.cor1$P, file="BIOLRESP.cor1SIG.csv", row.names=FALSE)
   write.csv(BIOLRESP.cor1$n, file="BIOLRESP.cor1Samp.csv", row.names=FALSE)
cor.dat <- BIOLRESP.cor1$r
sig.dat <- BIOLRESP.cor1$P
## plotting ##
jpeg(file="Corrplot_BiolResp_Sprmn_1.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()

#####################
# Corr 5  :: Biological Response + WChem x Spearman   ################

data <- CHEMxRESP.cordat
data <- CHEMxRESP.cordat2

CHEMxRESP.cor1 <- rcorr(as.matrix(data), type="spearman")
   write.csv(CHEMxRESP.cor1$r, file="CHEMxRESP.cor1.csv", row.names=FALSE)
   write.csv(CHEMxRESP.cor1$P, file="CHEMxRESP.cor1SIG.csv", row.names=FALSE)
   write.csv(CHEMxRESP.cor1$n, file="CHEMxRESP.cor1Samp.csv", row.names=FALSE)
cor.dat <- CHEMxRESP.cor1$r
sig.dat <- CHEMxRESP.cor1$P
## plotting ##
jpeg(file="Corrplot_CHEMxRESPp_Sprmn_1.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()

#####################
## Corr 6  :: Biological Response + WChem x Spearman [BIGGER !!]  ################
data <- CHEMxRESP.cordat2

CHEMxRESP.cor2 <- rcorr(as.matrix(data), type="spearman")
   write.csv(CHEMxRESP.cor2$r, file="CHEMxRESP.cor2.csv", row.names=FALSE)
   write.csv(CHEMxRESP.cor2$P, file="CHEMxRESP.cor2SIG.csv", row.names=FALSE)
   write.csv(CHEMxRESP.cor2$n, file="CHEMxRESP.cor2Samp.csv", row.names=FALSE)
cor.dat <- CHEMxRESP.cor2$r
sig.dat <- CHEMxRESP.cor2$P
## plotting ##
jpeg(file="Corrplot_CHEMxRESPp_Sprmn_2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()

##############################################################################
###### UPDATED DATA :: 160107  :: For CORRs and Rf ANALYSES  #################
##############################################################################
Chem.Invert.SAV_2X <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/Chem.Invert.SAV_2X.csv")
COR.dat2 <- Chem.Invert.SAV_2X

#### Examine the VAR classes  ######
   COR2.var <- data.frame()
for (i in 1:length(names(COR.dat2))){
  class_i <- class(COR.dat2[,i])
  name_i <- names(COR.dat2[i])
  num.NAs <- sum(is.na(COR.dat2[,i]))
  num.Obs <- sum(!is.na(COR.dat2[,i]))
  COR2.var <- rbind(COR2.var, data.frame(i, name_i, class_i, num.NAs,num.Obs))  }
#  WChem4[,i] <- as.numeric(WChem4[,i])
## Note (160107)  COR.dat2 [data] looks good !!
qplot(x=Metab_DO.c, y=Metab_DO.p, data=subset(COR.dat2))  ## DO is fixed !!
###################
##############    R E D O   C O R R E L A T I O N S  !!    #################
######## REDO  ::  CORR  DATASETS  >>>>>>   ################
##  data  ==>  COR.dat2
WChem.cordat2 <- COR.dat2[,c(13:33)]
SAV.cordat2 <- COR.dat2[,c(34:37)]
INV.cordat2 <- COR.dat2[,c(38:59)]
BIOLRESP.cordat2 <- COR.dat2[,c(34:37,38:59,26)]
CHEMxRESP.cordat2 <- COR.dat2[,c(13:33,34:37,38:59)]

# Corr 1X  :: WChem x Spearman   #################

Chem.cor1 <- rcorr(as.matrix(WChem.cordat2), type="spearman")
write.csv(signif(Chem.cor1$r,3), file="Chem.cor1.csv", row.names=FALSE)
write.csv(signif(Chem.cor1$P,3), file="Chem.cor1SIG.csv", row.names=FALSE)
write.csv(Chem.cor1$n, file="Chem.cor1Samp.csv", row.names=FALSE)
## plotting
cor.dat <- Chem.cor1$r
sig.dat <- Chem.cor1$P

jpeg(file="Corrplot_WChem_Sprmn_2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()
#############
# Corr 3X  :: Invertebrates x Spearman   ################
data <- INV.cordat2
INV.cor1 <- rcorr(as.matrix(data), type="spearman")
   write.csv(signif(INV.cor1$r,3), file="Invert.cor1.csv", row.names=FALSE)
   write.csv(signif(INV.cor1$P,3), file="Invert.cor1SIG.csv", row.names=FALSE)
   write.csv(INV.cor1$n, file="Invert.cor1Samp.csv", row.names=FALSE)
## plotting
cor.dat <- INV.cor1$r
sig.dat <- INV.cor1$P

jpeg(file="Corrplot_Invert_Sprmn_2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()

#####################
# Corr 4X  :: Biological Response Measures x Spearman   ################
data <- BIOLRESP.cordat2

BIOLRESP.cor1 <- rcorr(as.matrix(data), type="spearman")
   write.csv(signif(BIOLRESP.cor1$r,3), file="BIOLRESP.cor1.csv", row.names=FALSE)
   write.csv(signif(BIOLRESP.cor1$P,3), file="BIOLRESP.cor1SIG.csv", row.names=FALSE)
   write.csv(BIOLRESP.cor1$n, file="BIOLRESP.cor1Samp.csv", row.names=FALSE)
cor.dat <- BIOLRESP.cor1$r
sig.dat <- BIOLRESP.cor1$P
## plotting ##
jpeg(file="Corrplot_BiolResp_Sprmn_2.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()
#####################
# Corr 5X  :: Biological Response + WChem x Spearman   ################
data <- CHEMxRESP.cordat2

CHEMxRESP.cor1 <- rcorr(as.matrix(data), type="spearman")
   write.csv(signif(CHEMxRESP.cor1$r,3), file="CHEMxRESP.cor1.csv", row.names=FALSE)
   write.csv(signif(CHEMxRESP.cor1$P,3), file="CHEMxRESP.cor1SIG.csv", row.names=FALSE)
   write.csv(CHEMxRESP.cor1$n, file="CHEMxRESP.cor1Samp.csv", row.names=FALSE)
cor.dat <- CHEMxRESP.cor1$r
sig.dat <- CHEMxRESP.cor1$P
## plotting ##
jpeg(file="Corrplot_CHEMxRESPp_Sprmn_2x.jpg", width=6.5, height=5.5, units="in", quality=80, res=500)

    corrplot(cor.dat, method="ellipse", type="lower", diag=FALSE, tl.col="black", tl.srt=45,
         p.mat=sig.dat, sig.level=0.001, insig="blank", tl.cex=0.5,order="original")

dev.off()
#####################















#######  O T H E R   P L O T S   #############
qplot(x=Metab_pH.fld, y=Metab_DO.p, data=subset(COR.dat1, 
                  (!is.na(IP_calc) & SET != "WSpur" & Metab_pH.fld < 10.5)))

qplot(x=Metab_pH.fld, y=Metab_DO.c, data=subset(COR.dat1, 
                  (!is.na(IP_calc) & SET != "WSpur" & Metab_pH.fld < 10.5 & Metab_DO.c < 50)))
## split out low vs. high SAV cover (color) -- 
qplot(x=Metab_DO.c, y=Metab_DO.p, data=subset(COR.dat1, Metab_DO.c < 50))
qplot(x=Metab_DO.c, y=as.numeric(Metab_DO.p), data=subset(COR.dat0))


qplot(x=Metab_DO.c, y=Pond_mat.x, data=subset(COR.dat1, Metab_DO.c < 50))

