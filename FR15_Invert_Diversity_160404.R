## calculations for invertebrate diversity metrics -- 160404

data = inv.dat2     ## for nmds

data = Inv.dist2.FULL   ## extracting metrics and supple vars...
data = Inv.dist2.FULL.1  ## updated [1]


### Simpson's Index
dat <- Inv.dist2.FULL[,c(8:74)]

a <- diversity(dat, index="simpson")

Inv.dist2.FULL.1 <- data.frame(Inv.dist2.FULL, Simp=a)  ##  see Line10 in NMDS_Invert code

### Shannon Diversity

dat <- Inv.dist2.FULL.1[,c(8:74)]

b <- diversity(dat, index="shannon")

Inv.dist2.FULL.2 <- data.frame(Inv.dist2.FULL.1, Shann=b)

### Evenness

c <- diversity(dat, index="shannon")/log(specnumber(dat))

Inv.dist2.FULL.2 <- data.frame(Inv.dist2.FULL.2, Even=c)

###############################

write.csv(Inv.dist2.FULL.2, "Invert_Tax+Metrics_160404.csv", row.names=F)

##################  FIX INVERT TAXA NAMES  ##############
#  data == inv.var2

invert.var.names <- inv.var2
invert.var.names$TAXA <- c("Tanyp", "Chiron", "Hyal", "Calli", "Coris", "Orthoc", "Enoch", "Noton",
        "Physa", "Tanyt", "Isch", "Enall", "Chir.x", "Stagn", "Aeshn", "Physel", "Hesper", "Beros",
        "Naid", "Stict", "Chrys", "Lacco", "Caec", "Caeni", "Phryg", "Trop", "Ephy", "Somat", "Calop",
        "Erpob", "Eryth", "Gyr", "Helob", "Hydop", "Notip", "Symp", "Trico", "Cerato", "Cheum", "Gamm")







