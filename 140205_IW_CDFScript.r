# ggplots of CDFDat (from IW12 CDW_Est$CDF analyses)

#Create CDFs
# Purpose: Create CDFs of desired varaible
# Programmer: Bolgrien (based on original from Dave Peck)
# Modified by Shupryt and T. Hooker
# Plot programs from Tony Olsen. 
# Must have Site_ID, Final_Status (TS), Final_Weight, XMarinus, and YMarinus, and data in input file.

# Set up data frames for extent and cdf estimation
datnew <- IWDAT.X2
datnew <- SDAT99x
sites <- data.frame(siteID=datnew$Site_ID,Use=datnew$Fil_Status == "TS")

subpop <- data.frame(siteID=datnew$Site_ID,EstimateUnit=datnew$stratum)
subpop <- data.frame(siteID=datnew$Site_ID, Basin=datnew$stratum)

subpop <- data.frame(siteID=datnew$Site_ID, GSL_IW=rep('IW2012', nrow(datnew)),
                     Basin=datnew$stratum)

# Specify design information                  
dsgn <- data.frame(siteID=datnew$Site_ID, stratum=datnew$STRATUM,
    		wgt=datnew$AdjWgt, xcoord=datnew$Xmarinus, ycoord=datnew$Ymarinus)	

# You can change "Variable" to actual variable name to make tracking clearer especially 
#when doing multiple calculations
# Add variables by using datnew$XXXXX

data.cont <- data.frame(siteID=datnew$Site_ID)
data.cont <- data.frame(data.cont, datnew[,c(86,87,89,91,92,94,95,102,103,107,109)]) 	
data.cont <- data.cont[,-11]
data.cont.old <- data.cont
# Some added vars...
data.cont <- SDAT99[,c(3,50,51,208:220,342)]

# adjust some MMIs::
SDAT99$PMISI.mmi <- rowMeans(SDAT99[,c(195,198)], na.rm=T)  # combine two MIs into a new MMI
head(SDAT99[,c(195, 198, 342)])

data2 <- SDAT99[,c(3, 131, 132, 188, 189, 194, 195, 197, 198, 200, 201, 342, 217, 218)]
data3 <- SDAT99[,c(3, 216)]
data.cont <- data2
# Create cdfs for sites 
CDF <- cont.analysis(sites=sites,subpop=subpop,design=dsgn,
            data.cont=data.cont, pctval=c(5, 10, 25, 50, 75, 90, 95))
CDF.dat4 <- data.frame(CDF$CDF)

levels(subpop3$Basin)
# Test CDFs
subpop3 <- subpop[,c(1,3)]
framesize <- c("Jordan"=70, "Lower Bear-Malad"=44, "Lower Weber"=41)
CDF_Test <- cont.cdftest(sites, subpop=subpop3, design=dsgn, data.cont, popsize=list(Basin=as.list(framesize)))


write.table(CDF_Test, 'CDF_test5_Feb14.csv', sep=",", col.names=NA)

#The write.table function will overwrite files with same name, be careful
write.table(CDF$CDF,'CDF_set5-EH5_Feb14.csv',sep = ",",col.names=NA)
# Returns data to create CDFs, can easily be graphed in excel with confidence intervals
# XXXXX.P is percent of total, XXXXX.U is actual stream length (km) 

write.table(CDF$Pct,'Percentiles_Feb14Set.csv',sep = ",",col.names=NA)
# Returns data on percentiles           ### End of CDFs

# subset data...
# pSAV <- CDFDat[ which(CDFDat$Indicator=='PondSAV'),]  ## Works ok
# Vars <- levels(CDFDat$Indicator)
# pSAV2 <- subset(CDFDat, Indicator=='PondSAV')   ## This works well

# 1) convert factor level to numeric for convenience
{
SubP <- levels(CDF.dat3$Subpopulation)
CDF.dat3$Subpop2 <- as.numeric(CDF.dat3$Subpopulation)
nSPops <- max(CDF.dat3$Subpop2)
xrange <- range(CDF.dat3$Value)
yrange <- range(CDF.dat3$Estimate.P)
Vars <- as.data.frame(levels(CDF.dat3$Indicator))
plot(xrange, yrange, type="n", xlab="Value", ylab="% of data below value")
colors <- rainbow(nSPops)
linetype <- c(1:nSPops)
plotchar <- seq(18,18+nSPops, 1)
}
for (i in 1:nSPops) {
  subP <- subset(CDF.dat3, Subpop2==i)
  lines(CDF.dat3$Value, CDF.dat3$Estimate.P, type="b", lwd=1.5, col=colors[i], pch=plotchar[i])    }
# add legend
legend(xrange[1], yrange[2], 1:nSPops, cex=0.8, pch=plotchar, lty=linetype[1], col=colors, title="Stuff")

### 3) Splits CDFDat data.frame into separate files by Indicator (variable), saved as CSVs
for (j in 1:length(levels(CDF.dat3$Indicator))) {
  name.j <- paste(Vars[j,], sep="")
  name.df <- as.data.frame(subset(CDF.dat3, Indicator==Vars[j,]))
  ## insert plotting routine ##
  name.df$Subpop2 <- as.numeric(name.df$Subpopulation)
  nSPop <- max(name.df$Subpop2)
  xrange <- range(name.df$Value)
  yrange <- range(name.df$Estimate.P)
j.name <- paste(name.j, ".jpeg", sep="")
jpeg(filename=j.name, width=5, height=5, units="in", res=300 , quality=100)
    plot(xrange, yrange, type="n", xlab="Value", ylab="% of data below Value", main=name.j)
    colors <- rainbow(nSPop)
    linetype <- c(1:nSPop)
    plotchar <- seq(18, 18+nSPop, 1)
    abline(h=c(25, 50, 75), lty=2, col="black")
  for (i in 1:nSPop)  {
    subP <- subset(name.df, Subpop2==i)
    lines(subP$Value, subP$Estimate.P, type="l", lwd=2, col=colors[i], pch=plotchar[i])
  }
legend(x="topleft",legend=SubP, cex=0.8, lty=linetype[1], col=colors, title=name.j)
#  legend((sxrange[2]*0.9), (yrange[1]+30), 1:nSPop, cex=0.8, pch=plotchar, lty=linetype[1], col=colors, title=name.j)
  dev.off()
#  write.csv(name.df, file=paste(name.j,".csv", sep=""),)   
}



{
###
qplot(Value, Estimate.P*Subpop2, data=Cdat, color=Subpopulation, geom="smooth")
i = 2
Cdat.2 <- as.data.frame(subset(Cdat,Subpop2==2))
Cdat.3 <- as.data.frame(subset(Cdat,Subpop2==3))
Cdat.4 <- as.data.frame(subset(Cdat,Subpop2==4))

p1 <- ggplot(Cdat.2, aes(Value))
q1 <- ggplot(Cdat.3, aes(Value))
r1 <- ggplot(Cdat.4, aes(Value))
p2 <- p1 + geom_line(aes(x=Cdat.2$Value, y=Cdat.2$Estimate.P), color=Cdat.2$Subpop2)
p3 <- p2 + geom_line(aes(x=Cdat.2$Value, y=Cdat.2$LCB95Pct.P), color=Cdat.2$Subpop2, lty="dotted")
p4 <- p3 + geom_line(aes(x=Cdat.2$Value, y=Cdat.2$UCB95Pct.P), color=Cdat.2$Subpop2, lty="dotted")
q2 <- q1 + geom_line(aes(x=Cdat.3$Value, y=Cdat.3$Estimate.P), color=Cdat.3$Subpop2)
q3 <- q2 + geom_line(aes(x=Cdat.3$Value, y=Cdat.3$LCB95Pct.P), color=Cdat.3$Subpop2, lty="dotted")
q4 <- q3 + geom_line(aes(x=Cdat.3$Value, y=Cdat.3$UCB95Pct.P), color=Cdat.3$Subpop2, lty="dotted")
r2 <- r1 + geom_line(aes(x=Cdat.4$Value, y=Cdat.4$Estimate.P), color=Cdat.4$Subpop2)
r3 <- r2 + geom_line(aes(x=Cdat.4$Value, y=Cdat.4$LCB95Pct.P), color=Cdat.4$Subpop2, lty="dotted")
r4 <- r3 + geom_line(aes(x=Cdat.4$Value, y=Cdat.4$UCB95Pct.P), color=Cdat.4$Subpop2, lty="dotted")
px <- p4 + q4

ggplot(Cdat, aes(x=Value, color=Subpopulation)) + 
    geom_line(aes(y=Estimate.P, group=Subpopulation))


p1 <- ggplot(Cdat.i, aes(Value))
p2 <- p1 + geom_line(aes(x=Cdat.i$Value, y=Cdat.i$Estimate.P), color=Cdat.i$Subpop2)
p3 <- p2 + geom_ribbon(aes(x=Cdat.i$Value, y=Cdat.i$LCB95Pct.P), color=Cdat.i$Subpop2, lty="dotted")
p4 <- p3 + geom_ribbon(aes(x=Cdat.i$Value, y=Cdat.i$UCB95Pct.P), color=Cdat.i$Subpop2, lty="dotted")
p4

ggplot(Cdat.i, aes(Value)) + geom_line(aes(y=Estimate.P), color=Cdat.i$Subpop2) +
    geom_ribbon(aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P), alpha=0.2, fill=Cdat.i$Subpop2)
## need to fool w/ this a little more...but this is the way to go !!   
}