# Code of Wetland IR Chapter Figures 
# Eaerly Feb. 2014
# Preceding analysis based on code in:
# 1) 13209_RR-Code9-E.r
# 2) 140205_IW_CDFScript.r
# 3) 130304_t4.r

levels(CDF.dat3$Indicator)
{# source data = SDAT99
# CDF analysis = CDF.dat
# also: CDF.dat2  (for Pond mat and Inv vars...)
CDF.nm <- as.data.frame(names(CDF.dat))
CDF.Inds <- as.data.frame(levels(CDF.dat$Indicator))
CDF.subs <- as.data.frame(levels(CDF.dat$Subpopulation))

CDF2.nm <- as.data.frame(names(CDF.dat2))
CDF2.Inds <- as.data.frame(levels(CDF.dat2$Indicator))
CDF2.subs <- as.data.frame(levels(CDF.dat2$Subpopulation))

# 1) extract SAV_SCR_X metric from CDF.dat for plotting  (separate subs vs. all dat in diff DFs!)
SAV.dat <- subset(CDF.dat, Indicator=="SAV_SCR_X")
SAV2.dat <- subset(SAV.dat, Subpopulation=="IW2012")    # only full dataset
SAVx.dat <- subset(SAV.dat, Subpopulation != "IW2012")  # all subwatersheds
#
# 1b) extract for Pond_mat_2 metric and Pond_mat_MI ...
Pmat.dat <- subset(CDF.dat2, Indicator=="Pond_mat_MI")
Pmat2.dat <- subset(Pmat.dat, Subpopulation=="IW2012")
Pmatx.dat <- subset(Pmat.dat, Subpopulation != "IW2012")  

PmatX.dat <- subset(CDF.dat2, Indicator=="Pond_mat_MI")
PmatX2.dat <- subset(PmatX.dat, Subpopulation=="IW2012")
PmatXx.dat <- subset(PmatX.dat, Subpopulation != "IW2012") 

MAT.dat <- subset(CDF.dat3, Indicator=="MAT_mmi")
MAT2.dat <- subset(MAT.dat, Subpopulation=="IW2012")
MATx.dat <- subset(MAT.dat, Subpopulation != "IW2012") }
#   Plot CDFs for all watersheds && GSL Basin as a whole
{
x <- ggplot(SAVx.dat, aes(x=Value, color=Subpopulation, group=Subpopulation)) + theme_bw()
x2 <- x + geom_line(aes(y=Estimate.P), size=0.8, lty="dashed") + ylab("Proportion of data below Value") +
    xlab("SAV Index Score") + geom_hline(yintercept=c(25, 50, 75), lty=2, col="black")
x3 <- x2 + geom_ribbon(aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P, fill=Subpopulation, color=Subpopulation,
                           linetype=NA), alpha=0.25)
x4 <- x3 + geom_line(data=SAV2.dat, aes(y=Estimate.P, group=1, color=Subpopulation), 
                     color="black", size=1.0) 
x4
x5 <- x4 + geom_abline(intercept=100, slope=-0.5)
    ggsave(file=paste(SAV.dat$Indicator[1],"_CDFfig.jpeg", sep=""), width=6, height=4, dpi=500)
}
# for Pond_mat_MI
{x <- ggplot(Pmatx.dat, aes(x=Value, color=Subpopulation, group=Subpopulation)) + theme_bw()
x2 <- x + geom_line(aes(y=Estimate.P), size=0.8, lty="dashed") + ylab("Proportion of data below Value") +
    xlab("Surface Mat Cover (Index Period #2)") + geom_hline(yintercept=c(25, 50, 75), lty=2, col="black")
x3 <- x2 + geom_ribbon(aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P, fill=Subpopulation, color=Subpopulation,
                           linetype=NA), alpha=0.25)
x4 <- x3 + geom_line(data=Pmat2.dat, aes(y=Estimate.P, group=1, color=Subpopulation), 
                     color="black", size=1.0) 
x4
    ggsave(file=paste(Pmat.dat$Indicator[1],"_CDFfig.jpeg", sep=""), width=6, height=4, dpi=500)
}
# for Pond_mat_MI
{x <- ggplot(PmatXx.dat, aes(x=Value, color=Subpopulation, group=Subpopulation)) + theme_bw()
x2 <- x + geom_line(aes(y=Estimate.P), size=0.8, lty="dashed") + ylab("Proportion of data below Value") +
    xlab("Pond Surface Mat Index Score") + geom_hline(yintercept=c(25, 50, 75), lty=2, col="black")
x3 <- x2 + geom_ribbon(aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P, fill=Subpopulation, color=Subpopulation,
                           linetype=NA), alpha=0.25)
x4 <- x3 + geom_line(data=PmatX2.dat, aes(y=Estimate.P, group=1, color=Subpopulation), 
                     color="black", size=1.0) 
x4
    ggsave(file=paste(PmatX.dat$Indicator[1],"_CDFfig.jpeg", sep=""), width=6, height=4, dpi=500)
}
# for Surface MAT MMI (orig)
{
x <- ggplot(MATx.dat, aes(x=Value, color=Subpopulation, group=Subpopulation)) + theme_bw()
x2 <- x + geom_line(aes(y=Estimate.P), size=0.8, lty="dashed") + ylab("Proportion of data below Value") +
    xlab("Pond Surface Mat MMI Score") + geom_hline(yintercept=c(25, 50, 75), lty=2, col="black")
x3 <- x2 + geom_ribbon(aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P, fill=Subpopulation, color=Subpopulation,
                           linetype=NA), alpha=0.25)
x4 <- x3 + geom_line(data=MAT2.dat, aes(y=Estimate.P, group=1, color=Subpopulation), 
                     color="black", size=1.0) 
x4
    ggsave(file=paste(MAT.dat$Indicator[1],"_CDFfig.jpeg", sep=""), width=6, height=4, dpi=500) 
}
# for Invertebrate metrics (multiple)
{
CDF.dat3$Subpop2 <- as.numeric(CDF.dat3$Subpopulation)
CDF.dat3
CDF3.all <- subset(CDF.dat3, Subpopulation=="IW2012")  # GSL IWs only
CDF3.sub <- subset(CDF.dat3, Subpopulation != "IW2012") # by subwatersheds
bug2.ls <- as.data.frame(unique(CDF.dat3$Indicator))
colnames(bug2.ls) <- "Var"
bug2.ls$Name <- c("Chl-A Index Period #2", "Chl-A MI", "P MMI",
                  "Chl-A MMI", "N MMI", "Chemistry MMI #1", 
                  "Chemistry MMI #2", "Chemistry MMI #3", "Chemistry MMI #4",
                  "SAV MMI", "Surface Mat MMI", "Invert MMI #1", 
                  "Invert MMI #2", "Ecosys Health MMI #1", "Ecosys Health MMI #2", "PMI+SI MMI" )
i = 4       +       bug2.ls[i,]

CDF2.sub <- CDF3.sub
CDF2.all <- CDF3.all
CDF.dat2 <- CDF.dat3

for (i in 3:length(bug2.ls[,1])) {
x <- ggplot(data=(subset(CDF2.sub, CDF2.sub[,3]==bug2.ls[i,1])), aes(x=Value, color=Subpopulation, group=Subpopulation)) + theme_bw()
x2 <- x + geom_line(aes(y=Estimate.P), size=0.8, lty="dashed") + ylab("Proportion of data below Value") +
    xlab(bug2.ls[i,2]) + geom_hline(yintercept=c(25, 50, 75), lty=2, col="black")
x3 <- x2 + geom_ribbon(aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P, fill=Subpopulation, color=Subpopulation,
                           linetype=NA), alpha=0.25)
x4 <- x3 + geom_line(data=(subset(CDF2.all, CDF2.all[,3]==bug2.ls[i,1])), aes(y=Estimate.P, group=1, color=Subpopulation), 
                     color="black", size=1.0) 
x4
    ggsave(file=paste(bug2.ls[i,1],"_CDFfig-2.jpeg", sep=""), width=6, height=4, dpi=500) 
}
}

##  For all Indicators (as above)  ??
levels(CDF.dat3$Indicator)
# for multiple metrics
{
CDF.dat4
CDF.dat2 <- CDF.dat4
CDF.dat3
CDF2.all <- subset(CDF.dat2, Subpopulation=="IW2012")  # GSL IWs only
CDF2.sub <- subset(CDF.dat2, Subpopulation != "IW2012") # by subwatersheds
bug2.ls <- as.data.frame(unique(CDF.dat2$Indicator))
colnames(bug2.ls) <- "Var"
bug2.ls$Name <- c("Ecosystem Health MMI [1]", "Ecosystem Health MMI [2]", "Surface Mat Index",
                  "SAV Score", "SAV Index", "PMI Metric Index", 
                  "PMI Metric Index [rescaled]", "PMI & SI Indices", "PMI+SI MMI",
                  "Surf. Mat / SAV / PMISI", "Ecosystem Health MMI")
i = 4       +       bug2.ls[i,]

for (i in 3:length(bug2.ls[,1])) {
x <- ggplot(data=(subset(CDF2.sub, CDF2.sub[,3]==bug2.ls[i,1])), aes(x=Value, color=Subpopulation, group=Subpopulation)) + theme_bw()
x2 <- x + geom_line(aes(y=Estimate.P), size=0.8, lty="dashed") + ylab("Proportion of data below Value") +
    xlab(bug2.ls[i,2]) + geom_hline(yintercept=c(25, 50, 75), lty=2, col="black")
x3 <- x2 + geom_ribbon(aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P, fill=Subpopulation, color=Subpopulation,
                           linetype=NA), alpha=0.25)
x4 <- x3 + geom_line(data=(subset(CDF2.all, CDF2.all[,3]==bug2.ls[i,1])), aes(y=Estimate.P, group=1, color=Subpopulation), 
                     color="black", size=1.0) 
x4
    ggsave(file=paste(bug2.ls[i,1],"_CDFfig.jpeg", sep=""), width=6, height=4, dpi=500) 
}
}
##
#   Example--Generic Plot CDF for TEXT ***
{
x <- ggplot(SAVx.dat, aes(x=Value)) + theme_bw()
x4 <- x + geom_line(data=SAV2.dat, aes(y=Estimate.P, group=1), 
                     size=1.0) + ylab("Proportion of data below Value") +
        xlab("Response Variable Value") + geom_hline(yintercept=c(25, 50, 75), lty=2, col="black")
x4
x5 <- x4 + geom_ribbon(data=SAV2.dat, aes(ymin=LCB95Pct.P, ymax=UCB95Pct.P, linetype=NA), alpha=0.25)
d <- data.frame(x1 = 180, y1 = 87.5, t1 = 24, r1 = "blue")
x6 <- x5 + geom_text(data=d, aes(x=x1, y=y1, label="Upper Quartile = 'GOOD' \ncondition class"), 
                size=I(4)) +
    geom_segment(aes(x=90, y=75, xend=90, yend=100), 
                 arrow=arrow(ends="both", type="closed", angle=30, length=unit(0.25, "cm")))
e <- data.frame(x1 = 220, y1 = 12.5, t1 = 24, r1 = "red")
x7 <- x6 + geom_text(data=e, aes(x=x1, y=y1, label="Lower Quartile = 'POOR' \ncondition class"), 
                size=I(4)) +
    geom_segment(aes(x=130, y=0, xend=130, yend=25), 
                 arrow=arrow(ends="both", type="closed", angle=30, length=unit(0.25, "cm")))
x7
ggsave(file="XX_CDFfig.jpeg", width=6, height=4, dpi=500)  ## This code is good.
ggsave(file="XX_CDFfig2.jpeg", width=5, height=4, dpi=500)
}

{##### ** GENERIC BOXPLOT FOR REPORT TEXT ***  ####################
j=4
names(PMI.Q1[j])
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(PMI.Q1, .(Grp), numcolwise(nDiff))
    z2 <- PMI.Summ[(PMI.Summ$PARAM=="median"),]
    z3 <- z2[,-2]
    zD <- ddply(z3, .(PARAM), numcolwise(diff))
    zD <- data.frame(zD[,1], (round(zD[2:length(zD)],2)))
px <- qplot(Grp, PMI.Q1[,j], data=PMI.Q1, geom="boxplot", main="Stressor Variable", 
    xlab=paste("Median Diff = ", zD[1,(j-2)] , "\nLower and Upper quartiles from Response Variable"),
    scale_y_continuous(PMI.Q1[,j]), ylab="Value of Stressor Variable",na.rm=T )
px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))

px2
    
ggsave(file="XX_BOXplot.jpeg", width=5, height=5, dpi=500)
}
## extra plots   //   data = SDAT99
{
SDAT99.nm = as.data.frame(names(SDAT99))
qplot(POND_SAV2, SAV_SCR_X, data=SDAT99, geom="point", facets=stratum~., na.rm=T)

ggplot(SDAT99, aes(x=POND_SAV2, y=SAV_SCR_X, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + 
        geom_smooth(na.rm=T, aes(fill=stratum), alpha=0.2)

    d <- data.frame(x = c(0,100,100, 0,0,100), y = c(100,100,50, 0,50,0), 
                    t = c('GOOD', 'GOOD', 'GOOD', 'POOR', 'POOR', 'POOR'), 
                    r = c("red", "red", "red", "purple", "purple", "purple" ))
a <- ggplot() + geom_polygon(data=d, aes(x=x, y=y, group=t, alpha=0.2))

ggplot(SDAT99, aes(x=POND_SAV1, y=POND_SAV2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + 
        geom_abline(intercept=100, slope=-0.5, col="red", lty="dashed", size=1.1) +
        geom_abline(intercept=50, slope = -0.5, col="purple", lty="dashed", size=1.1) + 
        geom_text(data=NULL, x=60, y=85, label=" 'GOOD' SAV Scores", color="red", size=3) + 
        geom_text(data=NULL, x=18, y=20, label=" 'POOR' SAV Scores", color="purple", size=3)
    
    ggsave(file="SAVxIP2.jpeg", width=6, height=4, dpi=500)

ggplot(SDAT99, aes(x=Inv_mmi2, y=PMISI.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0)

ggplot(SDAT99, aes(x=Pond_mat_1, y=Pond_mat_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0)
ggsave(file="pMATxIP.jpeg", width=6, height=4, dpi=500)

ggplot(SDAT99, aes(x=Pond_mat_1, y=Pond_mat_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T)

ggplot(SDAT99, aes(x=SAV_SCR, y=Pond_mat_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0)

ggplot(SDAT99, aes(x=PMI_1, y=PMI_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0)

cor(SDAT99[,c(193,194)], use="pairwise.complete.obs", method="spearman")

ggplot(SDAT99, aes(x=POND_SA_MI, y=PMI_MI, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, method="rlm")

ggplot(SDAT99, aes(x=SAV_AVG_MI, y=PMI_MI, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=100)

ggplot(SDAT99, aes(x=SAV_SCR_MI, y=PMI_MI, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=100)

ggplot(SDAT99, aes(x=SAV_SCR_X_MI, y=PMI_MI, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, method="rlm")

ggplot(SDAT99, aes(x=SAV_SCR_X, y=PMI_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)    #removed y=x
ggplot(SDAT99, aes(x=SAV_SCR_X, y=PMI_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T, method="lm")    #removed y=x
z4 <- lm(PMI_2 ~ SAV_SCR_X, data=SDAT99)
summary(z4)
anova(z4)

ggplot(SDAT99, aes(x=SAV_SCR_X, y=log(PMI_2 +0.1), group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T, method="rlm")    #removed y=x
z4.ln <- lm(log(PMI_2+0.1) ~ SAV_SCR_X, data=SDAT99)
summary(z4.ln)
anova(z4.ln)
{## elements of linear model
z4.ln[[1]]
z4.ln[[2]]
z4.ln[[3]]
z4.ln[[4]]
z4.ln[[5]]
z4.ln[[6]]
z4.ln[[7]]
z4.ln[[8]]
z4.ln[[9]]
z4.ln[[10]]
z4.ln[[11]]
z4.ln[[12]]
z4.ln[[13]]
    names(z4.ln[[1]])
#
}
z4.nl <- nls(log(PMI_2+0.1) ~ SAV_SCR_X, 
             data=SDAT99, 
             start=list(xmid=150, scal=1), 
             algorithm = "plinear")
summary(z4.nl)
anova(z4.nl)

ggplot(SDAT99, aes(x=log(SAV_SCR_X+1), y=log(PMI_2 +0.1), group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T)    #removed y=x

ggplot(SDAT99, aes(y=SAV_SCR_X, x=Pond_mat_1, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T)    #removed y=x
ggplot(SDAT99, aes(y=SAV_SCR_X, x=Pond_mat_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T)    #removed y=x

### New EH5 MMI ##   SDAT99x
SDAT99x <- SDAT99x[-54,]
EH5.mmi
PMSI2.mi
Pm2.mi
SAVSx.mi
Pmat2.mi

ggplot(SDAT99x, aes(x=PMI_MI, y=EH5.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)
ggplot(SDAT99x, aes(x=PMSI2.mi, y=EH5.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)
ggplot(SDAT99x, aes(x=Pm2.mi, y=EH5.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)
ggplot(SDAT99x, aes(x=SAVSx.mi, y=EH5.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)
ggplot(SDAT99x, aes(x=Pmat2.mi, y=EH5.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)

eh5.lm <- lm(EH5.mmi ~ Pmat2.mi + SAVSx.mi + PMSI2.mi, data=SDAT99x)
summary(eh5.lm)
anova(eh5.lm)
yhat.se <- summary(eh5.lm)$sigma
influence(eh5.lm)

#
ggplot(SDAT99, aes(x=SAV_SCR_X_MI, y=PMI_MI, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)    #removed y=x
ggsave(file="SAV-v-PMI.jpeg", width=6, height=4, dpi=500)

z1 <- lm(PMI_MI ~ SAV_SCR_X_MI, data=SDAT99)
summary(z1)
anova(z1)
z1.se.yhat <- summary(z1)$sigma
z1.r <- rlm(PMI_MI ~ SAV_SCR_X_MI, data=SDAT99, method="M")
summary(z1.r)
anova.lm(z1.r)
z1.r2 <- rlm(PMI_MI ~ SAV_SCR_X_MI, data=SDAT99, method="MM")

ggplot(SDAT99, aes(x=POND_SAV2, y=PMI_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T, span=1.0)    #removed y=x
ggplot(SDAT99, aes(x=POND_SAV2, y=PMI_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), na.rm=T, method="lm")    #removed y=x
z3 <- lm(PMI_2 ~ POND_SAV2, data=SDAT99)
z3.sm <- summary(z3)
z3.se.yhat <- summary(z3)$sigma    # S.E. of the estimate (y-hat)
anova(z3)
# (MSE = StEr[y-hat]^2)




ggplot(SDAT99, aes(x=SAV_SCR_X_MI, y=PMISI.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, method="rlm")
z2 <- lm(PMISI.mmi ~ SAV_SCR_X_MI, data=SDAT99)
summary(z2)
anova.lm(z2)
ggplot(SDAT99, aes(x=SAV_SCR_X_MI, y=PMISI.mmi, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + geom_smooth(aes(group=GRP), na.rm=T, span=1)
z2.rd <- lm.ridge(PMISI.mmi ~ SAV_SCR_X_MI, data=SDAT99)
anova.lm(z2.rd)

SDAT99$GRP <- "IW2012"

ggplot(SDAT99, aes(x=SAV_SCR_X, y=Totbm_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), data=SDAT99, na.rm=T)

ggplot(SDAT99, aes(x=SAV_SCR_X, y=Totbm_2)) + 
    theme_bw() + geom_point(aes(group=stratum, color=stratum),na.rm=T) + 
    geom_smooth(aes(group=GRP), data=SDAT99, na.rm=T, method="rlm")

method="rlm"
family="symmetric"


ggplot(SDAT99, aes(x=EC_avg, y=D_ON_1, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=D_ON_2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=D_TP_2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=T_ON_2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC25_2, y=T_ON_2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=TDS_1, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=TDS_2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=SAV_AVG_2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=SAV_SCR, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=SAV_SCR_X, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=POND_SAV2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 
ggplot(SDAT99, aes(x=EC_avg, y=EcoH_mmi2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 


# new Apr 2014::
ggplot(SDAT99, aes(x=Pond_mat_2, y=D_TP_2, group=stratum, color=stratum)) + theme_bw() + geom_point(na.rm=T) 

ggplot(SDAT99, aes(x=D_TP_2, y=Pond_mat_2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), data=SDAT99, na.rm=T)

ggplot(SDAT99, aes(x=D_TP_MI, y=Pond_mat_MI, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_smooth(aes(group=GRP), data=SDAT99, na.rm=T)

ggplot(SDAT99, aes(x=D_TP_2, y=Pond_mat_2)) + 
    theme_bw() + geom_point(aes(group=stratum, color=stratum),na.rm=T) + 
    geom_smooth(aes(group=GRP), data=SDAT99, na.rm=T, method="rlm")

ggplot(SDAT99, aes(x=D_TP_1, y=Pond_mat_1)) + 
    theme_bw() + geom_point(aes(group=stratum, color=stratum),na.rm=T) + 
    geom_smooth(aes(group=GRP), data=SDAT99, na.rm=T, method="rlm")

ggplot(SDAT99, aes(x=D_TP_1, y=D_TP_2)) + 
    theme_bw() + geom_point(aes(group=stratum, color=stratum),na.rm=T) +  geom_abline(y=0) +
    geom_smooth(aes(group=stratum, color=stratum), data=SDAT99, na.rm=T, method="lm")
ggsave(file="DTP1-v-2.jpeg", width=6, height=5, dpi=500)

dtp1 <- lm(D_TP_2 ~ D_TP_1*stratum + D_TP_1 + stratum, data=SDAT99)
dtp2 <- lm(D_TP_2 ~ D_TP_1*stratum + stratum, data=SDAT99)
dtp3 <- lm(D_TP_2 ~ D_TP_1*stratum, data=SDAT99)
dtp4 <- lm(D_TP_2 ~ D_TP_1 + stratum, data=SDAT99)

summary(dtp3)
plot(dtp3)


ggplot(SDAT99, aes(x=D_TP_2, y=Pond_mat_2)) + 
    theme_bw() + geom_point(aes(group=stratum, color=stratum),na.rm=T) + 
    geom_smooth(aes(group=stratum), data=SDAT99, na.rm=T, method="lm")

    z <- lm(Pond_mat_2 ~ D_TP_2*stratum, data=SDAT99)
    z2 <- lm(Pond_mat_2 ~ D_TP_2 + stratum, data=SDAT99)
z3 <- lm(Pond_mat_2 ~ D_TP_2*stratum + D_TP_2 + stratum, data=SDAT99)
summary(z3)
anova.lm(z)
str(z)
ls(z)

}
#
##
###
####
#####
##          Relative Risk BAr Charts
{# data = RRdat
# Level1 == Response (k=5: EH5, SAVx, pMat2, PMI, SI)
#   Level2 == Group (k=3; FACETs -- stressor group)
#       Level3 == Stressor
#           Level4 ==>
#               Bar = RelRisk
#               error bars ::  [lower // upper]
# keywords:  facets // geom_bar() // coord_flip()
levels(RRdat$Response)  # EH5.mmi.COND
levels(RRdat$Stressor)
levels(RRdat$Group)     # "Cult Nut" "Phys"     "Toxics"
eh <- subset(RRdat, Response == "EH5.mmi.COND", select=1:length(names(RRdat)))
eh2 <- subset(eh, Group == "Cult Nut", select=1:length(names(RRdat)))

z <- ggplot(eh2, aes(x=RRdat$Stressor,y=RRdat$RelRisk))

z + geom_bar() + coord_flip()

facet_grid(~Group) 

}


ggplot(SDAT99, aes(x=POND_SAV2, y=SAV_SCR_X, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + 
        geom_smooth(na.rm=T, aes(fill=stratum), alpha=0.2)

ggplot(SDAT99, aes(x=POND_SAV1, y=POND_SAV2, group=stratum, color=stratum)) + theme_bw() +
        geom_point(na.rm=T) + geom_abline(y=0) + 
        geom_abline(intercept=100, slope=-0.5, col="red", lty="dashed", size=1.1) +
        geom_abline(intercept=50, slope = -0.5, col="purple", lty="dashed", size=1.1) + 
        geom_text(data=NULL, x=60, y=85, label=" 'GOOD' SAV Scores", color="red", size=3) + 
        geom_text(data=NULL, x=18, y=20, label=" 'POOR' SAV Scores", color="purple", size=3)


#####
####
###
##
#  Relative Extent Charts -- Stacked Bars
#  data = EH5_RelEx2
# facet.group = Subpop  {IW2012 = GSL-IW > Lower Bear-Malad > Lower Weber > Jordan}
# bars.group = Categ  {GOOD > FAIR > POOR  - omit: Total}
# value (x-axis) = Estimate.P
# error bars = UCB95Pct.P  &  LCB95Pct.P
# Variable = Indicator = EH5.mmi.COND
# p3 <- p2 + facet_wrap(~ Subpop, ncol=1) + coord_flip() + guides(fill=guide_legend(title="Condition Class"))

EH5.dat <- subset(EH5_RelEx2, Indicator == "EH5.mmi.COND")
EH5.dat$Categ <- factor(EH5.dat$Categ, levels = c("POOR", "FAIR","GOOD" ))
EH5.dat$clrs <- c("red", "yellow", "green")

p <- ggplot(EH5.dat, aes(x = Categ, y = Estimate.P, fill=Categ, ymax=100))
    p2 <- p + geom_bar(stat="identity") + expand_limits(y = c(0,100))
    p3 <- p2 + facet_wrap(~ Subpop, ncol=1) + coord_flip() + guides(fill="none")
    p4 <- p3 + xlab("Condition Class") + ylab("% of GSL Impounded Wetlands") + 
            ggtitle("Ecosystem Health") + theme(plot.title = element_text(face="bold"))
    p5 <- p4 + geom_errorbar(aes(ymax = UCB95Pct.P, ymin = LCB95Pct.P, width=0.4))
ggsave(file="EH5SummFig2.jpeg", width=6, height=5, dpi=500)


