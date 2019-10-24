# All IW Invertebrate Data -- 151130
# data = WetL_inverts_151130.csv
# data <- AllWetL_invert.0
AllWetL_invert.0 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/WetL_inverts_151130.csv")

AllIW_invert.0 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/REFSTD_R/WetL_IW_Inverts_151201.csv")
## use this one ^^^ !!

## Need to crosstab (dcast) these data, then run PMI and SI (1-D) calcs...

## Step 1:  Crosstab (cast) datafile

# invert <- AllWetL_invert.0
invert <- AllIW_invert.0

names(invert)
invert.nam <- as.data.frame(names(invert))
i=8

for (i in c(8:10,12:14)) {
  b <- names(invert[i])
  a <- table(invert[,i]) 
  names(invert[i])
  print(b)
  print(a)
  print(levels(invert[,i]))
  print(length(levels(invert[,i])))
  c <- summary(a)
  print(c)
  } ;


invert.len <- dcast(invert, Set + STORET + Date + Samp.Type ~ Taxon,
                    value.var="Count", length)

invert.dat1 <- dcast(invert, Set + STORET + Date + Samp.Type ~ Taxon,
                    value.var="Count", fill=0, mean);

invert.dat1 <- invert.dat1[with(invert.dat1, order(Set, STORET, Date, Samp.Type)),]

write.csv(invert.len, file="Wetland_invertebrates_NObs_151130.csv", row.names=F)

write.csv(invert.dat1, file="Wetland_invertebrates_151130.csv", row.names=F)
#####

## Step 2:  Calculate PMI
## Calc'd in Excel to save time...
invert.dat2 <- read.csv("C:/Users/tobyhooker/Dropbox/REFSTD_R/Wetland_invertebrates_151130_v1.csv")
invert.dat2 <- read.csv("C:/Users/Toby-n-Steph/Desktop/WorkDROP/Dropbox/REFSTD_R/Wetland_invertebrates_151130_v1.csv")

# added 3 vars:  zAbund  / zTaxa.rich / zPMI.f  

Invert.IW.metrics.1 <- invert.dat2[,c(1:4,85:87)]

# trim data for diversity Calcs (only need Col Names, not plots)
invert.dat3 <- invert.dat2[,c(5:84)]

library(vegan)

#  diversity(x, index="simpson", MARGIN=1, )  
#  based on data(BCI), need to remove row.names, and columns are Taxa abundance / only -- Done !
#  need to code NAs as 0's (zeros) -- Done (fill=0 !!)
#  Need to trim out any FRNG or sheetflow sites...(do this first !!) -- Done (see Master List in XL)

invert.simpsonComp <- diversity(invert.dat3, index="simpson")

invert.simp.df <- as.data.frame(invert.simpsonComp)

Invert.IW.metrics.2 <- data.frame(Invert.IW.metrics.1, invert.simpsonComp)

names(Invert.IW.metrics.2)[names(Invert.IW.metrics.2) == "invert.simpsonComp"] <- "SI.comp"

## All finished -- now to work the data / results up !!

# Combine metrics w/ data:

Invert.IW.All.3 <- data.frame(invert.dat2, invert.simpsonComp)

names(Invert.IW.All.3)[names(Invert.IW.All.3) == "invert.simpsonComp"] <- "SI.comp"

write.csv(Invert.IW.All.3, file="Inverts_IWAll_151202.csv", row.names=F)












