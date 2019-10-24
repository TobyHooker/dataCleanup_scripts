## Inverts
Inv.Taxa1 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Inv.TaxaFlat_1.csv")

Inv.Met1 <- read.csv("C:/Users/tobyhooker/Dropbox/GSL_Fringe2015/FR15_R/Inv.MetFlat_1.csv")
####
Inv.Taxa2 <- Inv.Taxa1[c(1:643),c(1,3:6,8,9)]
Inv.Tax.All.cast <- dcast(Inv.Taxa2, Site + MLID + Year + Dist + Date ~ Taxon, 
                        value.var="Count",length)
summary(Inv.Tax.All.cast)
      ### Looks Good !
Inv.Tax.All.cast1 <- dcast(Inv.Taxa2, Site + MLID + Year + Dist + Date ~ Taxon, 
                        value.var="Count",fill=0, mean)
      ## okay !!

### Invert Metrics
Inv.Met2 <- Inv.Met1[,c(1:9)]
Inv.Metr.All.cast <- dcast(Inv.Met2, Site + MLID + Year + Dist + Date ~ Metric, 
                        value.var="Value",length)
Inv.Metr.All.cast1 <- dcast(Inv.Met2, Site + MLID + Year + Dist + Date ~ Metric, 
                        value.var="Value")

###### Export Datasets...

  write.csv(Inv.Tax.All.cast1, "InvertAll.Taxa.DIST.csv", row.names=F)

  write.csv(Inv.Metr.All.cast1, "InvertAll.Metr.DIST.csv", row.names=F)


#################

summarize for Sites...


Invert.Site <- ddply(INV.Dist.trim_2, .(MLID, Year), numcolwise(sum, na.rm=T))
  write.csv(Invert.Site, "Invert.Sites.csv", row.names=F)



