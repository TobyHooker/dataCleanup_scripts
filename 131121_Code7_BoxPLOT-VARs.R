# *** RESET from 131119Code_6.R, w/ updated (FIXd) Dataset as SDAT8 ***
SDAT8 <- read.csv("C:/Users/tobyhooker/Dropbox/IW_IR/IR12/SDAT8.csv")
SDAT8.nm <- as.data.frame(names(SDAT8))
SMET8.list <- SDAT8[,c(1,2,8,9,13,16,22,23,28,29,30,31,38,41,42,47,54,40,57,58,63,64,65,70,
                       71,72,77,78,79,84,85,86,91,92,93,94,95,100,101,102,107,115,116,117,118,
                       130,131,137,138,156,157,185,186,191,194,195,200,352,353,358,359,360,365,
                       366,367,372,373,374,379,380,381,386,387,388,393,394,395,400,
                       250,251,252,253,258,259,260,265,266,267,272,273,275,277,279,281,
                       298,301,305,308,311,314,321,324,325,330,331,332,337,345,346,351)]
SMET8 <- SMET8.list[order(SMET8.list[,2]),]
SMET8.nm <- as.data.frame(names(SMET8))
SMET8 <- SMET8[-c(54:56),]
write.csv(SMET8, "SMET8.csv")
write.csv(SDAT8, "SDAT8.csv")

## Summary data for Response Variables...
# [1] Define Lower / Upper Quantiles of Response Variable [1]: SAV_SCR (#90) & [2]: SASV_SCR_X (#93)
    SAV.Up <- subset(SMET8, SAV_SCR >= quantile(SAV_SCR, 0.75, na.rm=T), select=1:length(names(SMET8)))
    SAV.Up$Grp <- "75%ile"
    SAV.Up <- as.data.frame(SAV.Up[,c(length(names(SAV.Up)), 1:(length(names(SAV.Up))-1))])
    SAV.Lo <- subset(SMET8, SAV_SCR <= quantile(SAV_SCR, 0.25, na.rm=T), select=1:length(names(SMET8)))
    SAV.Lo$Grp <- "25%ile"
    SAV.Lo <- as.data.frame(SAV.Lo[,c(length(names(SAV.Lo)), 1:(length(names(SAV.Lo))-1))])
SAV.Q1 <- rbind(SAV.Up, SAV.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
################## Sumamry table for upper / lower quantiles.
x1 <- ddply(SAV.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(SAV.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(SAV.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(SAV.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
SAV1.Summ <- xx[c(a,1:(a-1))]
SAV1.Summ <- SAV1.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
j=4
names(SAV.Q1[j])
##
for (j in 4:length(SAV.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(SAV.Q1, .(Grp), numcolwise(nDiff))
        px <- qplot(Grp, SAV.Q1[,j], data=SAV.Q1, geom="boxplot", main=names( SAV.Q1[j]), 
            xlab="Lower and Upper quartiles from SAV_SCR",  scale_y_continuous(SAV.Q1[,j]),
            ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-SAV1_', names(SAV.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

#### [2] Define Lower / Upper Quantiles of Response Variable [2]: SAV_SCR_X (#93)
    SAV.Up <- subset(SMET8, SAV_SCR_X >= quantile(SAV_SCR_X, 0.75, na.rm=T), select=1:length(names(SMET8)))
    SAV.Up$Grp <- "75%ile"
    SAV.Up <- as.data.frame(SAV.Up[,c(length(names(SAV.Up)), 1:(length(names(SAV.Up))-1))])
    SAV.Lo <- subset(SMET8, SAV_SCR_X <= quantile(SAV_SCR_X, 0.25, na.rm=T), select=1:length(names(SMET8)))
    SAV.Lo$Grp <- "25%ile"
    SAV.Lo <- as.data.frame(SAV.Lo[,c(length(names(SAV.Lo)), 1:(length(names(SAV.Lo))-1))])
SAV.Q2 <- rbind(SAV.Up, SAV.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
###### Summary table for upper / lower quantiles.
x1 <- ddply(SAV.Q2, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(SAV.Q2, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(SAV.Q2, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(SAV.Q2, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
SAV2.Summ <- xx[c(a,1:(a-1))]
SAV2.Summ <- SAV2.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
for (j in 4:length(SAV.Q2)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(SAV.Q2, .(Grp), numcolwise(nDiff))
        px <- qplot(Grp, SAV.Q2[,j], data=SAV.Q2, geom="boxplot", main=names( SAV.Q2[j]), 
            xlab="Lower and Upper quartiles from SAV_SCR",  scale_y_continuous(SAV.Q2[,j]),
            ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-SAV2_', names(SAV.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

#### [3] Define Lower / Upper Quantiles of Response Variable [3]: COTE_MI (#60)
    COTE.Up <- subset(SMET8, COTE_MI >= quantile(COTE_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    COTE.Up$Grp <- "75%ile"
    COTE.Up <- as.data.frame(COTE.Up[,c(length(names(COTE.Up)), 1:(length(names(COTE.Up))-1))])
    COTE.Lo <- subset(SMET8, COTE_MI <= quantile(COTE_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    COTE.Lo$Grp <- "25%ile"
    COTE.Lo <- as.data.frame(COTE.Lo[,c(length(names(COTE.Lo)), 1:(length(names(COTE.Lo))-1))])
COTE.Q1 <- rbind(COTE.Up, COTE.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
###### Summary table for upper / lower quantiles.
x1 <- ddply(COTE.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(COTE.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(COTE.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(COTE.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
COTE.Summ <- xx[c(a,1:(a-1))]
COTE.Summ <- COTE.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
for (j in 4:length(COTE.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(COTE.Q1, .(Grp), numcolwise(nDiff))
        px <- qplot(Grp, COTE.Q1[,j], data=COTE.Q1, geom="boxplot", main=names( COTE.Q1[j]), 
            xlab="Lower and Upper quartiles from COTE_MI",  scale_y_continuous(COTE.Q1[,j]),
            ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-COTE_', names(SAV.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

#### [4] Define Lower / Upper Quantiles of Response Variable [4]: T3_MI (#63)
    T3.Up <- subset(SMET8, T3_MI >= quantile(T3_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    T3.Up$Grp <- "75%ile"
    T3.Up <- as.data.frame(T3.Up[,c(length(names(T3.Up)), 1:(length(names(T3.Up))-1))])
    T3.Lo <- subset(SMET8, T3_MI <= quantile(T3_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    T3.Lo$Grp <- "25%ile"
    T3.Lo <- as.data.frame(T3.Lo[,c(length(names(T3.Lo)), 1:(length(names(T3.Lo))-1))])
T3.Q1 <- rbind(T3.Up, T3.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
###### Summary table for upper / lower quantiles.
x1 <- ddply(T3.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(T3.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(T3.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(T3.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
T3.Summ <- xx[c(a,1:(a-1))]
T3.Summ <- T3.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
for (j in 4:length(T3.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(T3.Q1, .(Grp), numcolwise(nDiff))
        px <- qplot(Grp, T3.Q1[,j], data=T3.Q1, geom="boxplot", main=names( T3.Q1[j]), 
            xlab="Lower and Upper quartiles from T3_MI",  scale_y_continuous(T3.Q1[,j]),
            ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-T3_', names(T3.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

#### [5] Define Lower / Upper Quantiles of Response Variable [5]: PMI_MI (#66)
    PMI.Up <- subset(SMET8, PMI_MI >= quantile(PMI_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    PMI.Up$Grp <- "75%ile"
    PMI.Up <- as.data.frame(PMI.Up[,c(length(names(PMI.Up)), 1:(length(names(PMI.Up))-1))])
    PMI.Lo <- subset(SMET8, PMI_MI <= quantile(PMI_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    PMI.Lo$Grp <- "25%ile"
    PMI.Lo <- as.data.frame(PMI.Lo[,c(length(names(PMI.Lo)), 1:(length(names(PMI.Lo))-1))])
PMI.Q1 <- rbind(PMI.Up, PMI.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
###### Summary table for upper / lower quantiles.
x1 <- ddply(PMI.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(PMI.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(PMI.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(PMI.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
PMI.Summ <- xx[c(a,1:(a-1))]
PMI.Summ <- PMI.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
for (j in 4:length(PMI.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(PMI.Q1, .(Grp), numcolwise(nDiff))
        px <- qplot(Grp, PMI.Q1[,j], data=PMI.Q1, geom="boxplot", main=names( PMI.Q1[j]), 
            xlab="Lower and Upper quartiles from PMI_MI",  scale_y_continuous(PMI.Q1[,j]),
            ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-PMI_', names(PMI.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

#### [6] Define Lower / Upper Quantiles of Response Variable [6]: SI_MI (#69)
    SI.Up <- subset(SMET8, SI_MI >= quantile(SI_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    SI.Up$Grp <- "75%ile"
    SI.Up <- as.data.frame(SI.Up[,c(length(names(SI.Up)), 1:(length(names(SI.Up))-1))])
    SI.Lo <- subset(SMET8, SI_MI <= quantile(SI_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    SI.Lo$Grp <- "25%ile"
    SI.Lo <- as.data.frame(SI.Lo[,c(length(names(SI.Lo)), 1:(length(names(SI.Lo))-1))])
SI.Q1 <- rbind(SI.Up, SI.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
###### Summary table for upper / lower quantiles.
x1 <- ddply(SI.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(SI.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(SI.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(SI.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
SI.Summ <- xx[c(a,1:(a-1))]
SI.Summ <- SI.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
for (j in 4:length(SI.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(SI.Q1, .(Grp), numcolwise(nDiff))
        px <- qplot(Grp, SI.Q1[,j], data=SI.Q1, geom="boxplot", main=names( SI.Q1[j]), 
            xlab="Lower and Upper quartiles from SI_MI",  scale_y_continuous(SI.Q1[,j]),
            ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-SI_', names(SI.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

#### [7] Define Lower / Upper Quantiles of Response Variable [7]: InvTaxa_MI (#72)
    InvTaxa.Up <- subset(SMET8, InvTaxa_MI >= quantile(InvTaxa_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    InvTaxa.Up$Grp <- "75%ile"
    InvTaxa.Up <- as.data.frame(InvTaxa.Up[,c(length(names(InvTaxa.Up)), 1:(length(names(InvTaxa.Up))-1))])
    InvTaxa.Lo <- subset(SMET8, InvTaxa_MI <= quantile(InvTaxa_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    InvTaxa.Lo$Grp <- "25%ile"
    InvTaxa.Lo <- as.data.frame(InvTaxa.Lo[,c(length(names(InvTaxa.Lo)), 1:(length(names(InvTaxa.Lo))-1))])
InvTaxa.Q1 <- rbind(InvTaxa.Up, InvTaxa.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
###### Summary table for upper / lower quantiles.
x1 <- ddply(InvTaxa.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(InvTaxa.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(InvTaxa.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(InvTaxa.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
InvTaxa.Summ <- xx[c(a,1:(a-1))]
InvTaxa.Summ <- InvTaxa.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
for (j in 4:length(InvTaxa.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(InvTaxa.Q1, .(Grp), numcolwise(nDiff))
        px <- qplot(Grp, InvTaxa.Q1[,j], data=InvTaxa.Q1, geom="boxplot", main=names( InvTaxa.Q1[j]), 
            xlab="Lower and Upper quartiles from InvTaxa_MI",  scale_y_continuous(InvTaxa.Q1[,j]),
            ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-InvTaxa_', names(InvTaxa.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

## Combine some summaries (*.Q's) for direct comparisons...
SAV1.Summ2 <- SAV1.Summ
SAV1.Summ2$Metric <- "SAV_SCR"
SAV2.Summ2 <- SAV2.Summ
SAV2.Summ2$Metric <- "SAV_SCR_X"
    SAV.Sum2 <- rbind(SAV1.Summ2, SAV2.Summ2)
        names(SAV.Sum2)
    SAV.Sum2 <- SAV.Sum2[,c(length(names(SAV.Sum2)), 1:(length(names(SAV.Sum2)))-1)]
    write.csv(SAV.Sum2, "SAV_Sum2.csv")

##############

# Would like to compare summary stats, by variable, for all [7] REF groups...
# Derive dataset from *.Summ, used median + IQR (75-25th %iles)

SAV1.sm <- SAV1.Summ
SAV1.sm$Metric <- "SAV_SCR"
SAV2.sm <- SAV2.Summ
SAV2.sm$Metric <- "SAV_SCR_X"
COTE.sm <- COTE.Summ
COTE.sm$Metric <- "COTE_MI"
T3.sm <- T3.Summ
T3.sm$Metric <- "T3_MI"
PMI.sm <- PMI.Summ
PMI.sm$Metric <- "PMI_MI"
SI.sm <- SI.Summ
SI.sm$Metric <- "SI_MI"
InvTaxa.sm <- InvTaxa.Summ
InvTaxa.sm$Metric <- "InvTaxa_MI"

REFs.sm <- rbind(SAV1.sm, SAV2.sm, COTE.sm, T3.sm, PMI.sm, SI.sm, InvTaxa.sm)
REFs.sm <- REFs.sm[,c(length(names(REFs.sm)), 1:(length(names(REFs.sm)))-1)]

REFs.sm2 <- REFs.sm[order(REFs.sm[,2], REFs.sm[,3], REFs.sm[,1]),]
Pars <- c("25%", "median", "75%")
    unique(REFs.sm2$PARAM)
REFs.sm3d <- REFs.sm2[c(15:42,71:84),]
REFs.sm4 <- REFs.sm3d[order(REFs.sm3d[,1], REFs.sm3d[,3], REFs.sm3d[,2]),]
REFs.med <- REFs.sm4[REFs.sm4$PARAM=="median",]
REFs.lo <- REFs.sm4[REFs.sm4$PARAM=="25%",]
REFs.up <- REFs.sm4[REFs.sm4$PARAM=="75%",]
REFs.iqr <- REFs.lo
    REFs.iqr <- REFs.up[,4:length(names(REFs.up))]-REFs.lo[,4:length(names(REFs.lo))]
REFs.iqr$Metric <- REFs.lo$Metric
REFs.iqr$PARAM <- "IQR"
REFs.iqr$Grp <- REFs.lo$Grp
REFs.iqr2 <- REFs.iqr[,c(length(names(REFs.iqr))-2,length(names(REFs.iqr))-1, length(names(REFs.iqr)))],
                        1:length(names(REFs.iqr))-3)]
REFs.iqr3 <- REFs.iqr[,c(109, 110,111,1:108)]

#################################
#  combine all *.Q datasets, use geom=jitter to plot summary values...

SAV1.q <- SAV.Q1
SAV1.q$Metric <- "SAV_SCR"
SAV2.q <- SAV.Q2
SAV2.q$Metric <- "SAV_SCR_X"
COTE.q <- COTE.Q1
COTE.q$Metric <- "COTE_MI"
T3.q <- T3.Q1
T3.q$Metric <- "T3_MI"
PMI.q <- PMI.Q1
PMI.q$Metric <- "PMI_MI"
SI.q <- SI.Q1
SI.q$Metric <- "SI_MI"
InvTaxa.q <- InvTaxa.Q1
InvTaxa.q$Metric <- "InvTaxa_MI"

REF.q <- rbind(SAV1.q, SAV2.q, COTE.q, T3.q, PMI.q, SI.q, InvTaxa.q)
REF.q <- REF.q[,c(length(names(REF.q)), 1:(length(names(REF.q)))-1)]
    REF.q2 <- REFs.sm[order(REF.q[,2], REF.q[,3], REF.q[,1]),]
    RFq.nm <- as.data.frame(names(REF.q))
    RFq2.nm <- as.data.frame(names(REF.q2))

names(REF.q[,c(1:5)])
# box plots

j=6
names(REF.q3[j])
names(WXdifs[(j-2)])
REFs.iqr3 <- REFs.iqr3[order(REFs.iqr3[,3], REFs.iqr3[,3]),]
    REF.q$Metric <- as.factor(REF.q$Metric)
    REF.q$Grp <- as.factor(REF.q$Grp)
    REF.q3 <- REF.q[,c(length(names(REF.q)),1:(length(names(REF.q))-1))]
    levels(REF.q3$Grp2)
    REF.q3$Grp2 <- revalue(REF.q3$Grp2, c("25%ile"="Lower", "75%ile"="REF"))
#

    # WX <- WX[order(WX[,2], WX[,1]),]
    # WX2 <- ddply(REF.q3, .(Metric, Grp2), numcolwise(mean))
    WX <- ddply(REF.q3, .(Metric, Grp2), numcolwise(median, na.rm=T))
    WXdif <- ddply(WX, .(Metric), numcolwise(diff))
    WXdifs <- data.frame(WXdif[,1],(round(WXdif[2:length(WXdif)],2)))
    colnames(WXdifs)[1] <- "Metric"

j = 11
for (j in 6:length(REF.q3)) {
    w <- qplot(Metric, REF.q3[,j], data=REF.q3, geom="boxplot", main=names(REF.q3[j]),
           xlab="Response Variables", scale_y_continuous(REF.q3[,j]), ylab="",
           na.rm=TRUE, fill=Grp2 ) + theme_bw()
    w2 <- w + scale_x_discrete(labels=paste(WXdifs$Metric, "\nDiff = ", WXdifs[,(j-2)], sep=""))
    w2
    XName.j <- paste(j, '_RefSm_', names(REF.q3[j]), '.jpg', sep='')
    ggsave(XName.j, width=9, height=5, dpi=300)
    print(w)
    dev.off()   }

##
#xDiff <- function(x) 
#w2 <- w + scale_x_discrete(labels=paste(REFs.iqr3$Metric, "\nDiff = ", REFs.iqr3[,(j-2)], sep=" "))
# vnmissing <- function(x) sum(is.na(x))
#    nDiff <- function(x) (length(x)-nmissing(x))
#    VX <- ddply(InvTaxa.Q1, .(Grp), numcolwise(nDiff))
#        px <- qplot(Grp, InvTaxa.Q1[,j], data=InvTaxa.Q1, geom="boxplot", main=names( InvTaxa.Q1[j]), 
#            xlab="Lower and Upper quartiles from InvTaxa_MI",  scale_y_continuous(InvTaxa.Q1[,j]),
#            ylab="",na.rm=T )
#        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))

write.csv(REF.q3, "REF_q3.csv")

## Look at POND_MAT as potential biological response (condition) variable...

## Summary data for Response Variables...
# [8] Define Lower / Upper Quantiles of Response Variable [8]: Pond_mat_2 (#82) & [8b]: Pond_mat_MI (#83)
    Pmat2.Up <- subset(SMET8, Pond_mat_2 >= quantile(Pond_mat_2, 0.75, na.rm=T), select=1:length(names(SMET8)))
    Pmat2.Up$Grp <- "75%ile"
    Pmat2.Up <- as.data.frame(Pmat2.Up[,c(length(names(Pmat2.Up)), 1:(length(names(Pmat2.Up))-1))])
    Pmat2.Lo <- subset(SMET8, Pond_mat_2 <= quantile(Pond_mat_2, 0.25, na.rm=T), select=1:length(names(SMET8)))
    Pmat2.Lo$Grp <- "25%ile"
    Pmat2.Lo <- as.data.frame(Pmat2.Lo[,c(length(names(Pmat2.Lo)), 1:(length(names(Pmat2.Lo))-1))])
Pmat2.Q1 <- rbind(Pmat2.Up, Pmat2.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
################## Sumamry table for upper / lower quantiles.
x1 <- ddply(Pmat2.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(Pmat2.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(Pmat2.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(Pmat2.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
Pmat2.Summ <- xx[c(a,1:(a-1))]
Pmat2.Summ <- Pmat2.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
j=4
names(Pmat2.Q1[j])
##
for (j in 4:length(Pmat2.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(Pmat2.Q1, .(Grp), numcolwise(nDiff))
    z2 <- Pmat2.Summ[(Pmat2.Summ$PARAM=="median"),]
    z3 <- z2[,-2]
    zD <- ddply(z3, .(PARAM), numcolwise(diff))
    zD <- data.frame(zD[,1], (round(zD[2:length(zD)],2)))
        px <- qplot(Grp, Pmat2.Q1[,j], data=Pmat2.Q1, geom="boxplot", main=names( Pmat2.Q1[j]), 
            xlab=paste("Median Diff = ", zD[1,(j-2)] ,"\nLower and Upper quartiles from Pond_mat_2"),  
                    scale_y_continuous(Pmat2.Q1[,j]), ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-Pmat2_', names(SAV.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

# [8b] Define Lower / Upper Quantiles of Response Variable [8b]: Pond_mat_MI (#83)
    PmatMI.Up <- subset(SMET8, Pond_mat_MI >= quantile(Pond_mat_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    PmatMI.Up$Grp <- "75%ile"
    PmatMI.Up <- as.data.frame(PmatMI.Up[,c(length(names(PmatMI.Up)), 1:(length(names(PmatMI.Up))-1))])
    PmatMI.Lo <- subset(SMET8, Pond_mat_MI <= quantile(Pond_mat_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    PmatMI.Lo$Grp <- "25%ile"
    PmatMI.Lo <- as.data.frame(PmatMI.Lo[,c(length(names(PmatMI.Lo)), 1:(length(names(PmatMI.Lo))-1))])
PmatMI.Q1 <- rbind(PmatMI.Up, PmatMI.Lo)  # all data for upper & lower quants RESP-VAR#1, by Grp
################## Summary table for upper / lower quantiles.
x1 <- ddply(PmatMI.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(PmatMI.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(PmatMI.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(PmatMI.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
PmatMI.Summ <- xx[c(a,1:(a-1))]
PmatMI.Summ <- PmatMI.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
j=4
names(PmatMI.Q1[j])
##
for (j in 4:length(PmatMI.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(PmatMI.Q1, .(Grp), numcolwise(nDiff))
    z2 <- PmatMI.Summ[(PmatMI.Summ$PARAM=="median"),]
    z3 <- z2[,-2]
    zD <- ddply(z3, .(PARAM), numcolwise(diff))
    zD <- data.frame(zD[,1], (round(zD[2:length(zD)],2)))
        px <- qplot(Grp, PmatMI.Q1[,j], data=PmatMI.Q1, geom="boxplot", main=names( PmatMI.Q1[j]), 
            xlab=paste("Median Diff = ", zD[1,(j-2)] ,"\nLower and Upper quartiles from Pond_mat_MI"),  
                    scale_y_continuous(PmatMI.Q1[,j]), ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-PmatMI_', names(PmatMI.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

# [9] Define Lower / Upper Quantiles of Response Variable [9]: Chla_MI (#21)
    ChlaMI.Up <- subset(SMET8, Chla_MI >= quantile(Chla_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    ChlaMI.Up$Grp <- "75%ile"
    ChlaMI.Up <- as.data.frame(ChlaMI.Up[,c(length(names(ChlaMI.Up)), 1:(length(names(ChlaMI.Up))-1))])
    ChlaMI.Lo <- subset(SMET8, Chla_MI <= quantile(Chla_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    ChlaMI.Lo$Grp <- "25%ile"
    ChlaMI.Lo <- as.data.frame(ChlaMI.Lo[,c(length(names(ChlaMI.Lo)), 1:(length(names(ChlaMI.Lo))-1))])
ChlaMI.Q1 <- rbind(ChlaMI.Up, ChlaMI.Lo)  # all data for upper & lower quants RESP-VAR#1, by Grp
################## Summary table for upper / lower quantiles.
x1 <- ddply(ChlaMI.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(ChlaMI.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(ChlaMI.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(ChlaMI.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
ChlaMI.Summ <- xx[c(a,1:(a-1))]
ChlaMI.Summ <- ChlaMI.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
j=4
names(ChlaMI.Q1[j])
##
for (j in 4:length(ChlaMI.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(ChlaMI.Q1, .(Grp), numcolwise(nDiff))
    z2 <- ChlaMI.Summ[(ChlaMI.Summ$PARAM=="median"),]
    z3 <- z2[,-2]
    zD <- ddply(z3, .(PARAM), numcolwise(diff))
    zD <- data.frame(zD[,1], (round(zD[2:length(zD)],2)))
        px <- qplot(Grp, ChlaMI.Q1[,j], data=ChlaMI.Q1, geom="boxplot", main=names( ChlaMI.Q1[j]), 
            xlab=paste("Median Diff = ", zD[1,(j-2)] ,"\nLower and Upper quartiles from Chla_MI"),  
                    scale_y_continuous(ChlaMI.Q1[,j]), ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-ChlaMI_', names(ChlaMI.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

zz2 <- as.data.frame(quantile(SMET8[,82],probs=c(0.1,0.25,0.5,0.75,0.9), na.rm=T))


#
##
###
####
#####
# replaced SMET8 w/ new data of MMI values
SMET8.set1 <- SMET8
SMET8.set2 <-  SDAT99[,c(2,3,142, 188, 189, 194, 195, 197, 198, 208:220, 342 )]
SMET8 <- SMET8.set2
## Redo - SAV_SCR_X Boxplots...
# [8] Define Lower / Upper Quantiles of Response Variable [2-b]: SAV_SCR_X (#93)
    SAV.Up <- subset(SMET8, SAV_SCR_X >= quantile(SAV_SCR_X, 0.75, na.rm=T), select=1:length(names(SMET8)))
    SAV.Up$Grp <- "75%ile"
    SAV.Up <- as.data.frame(SAV.Up[,c(length(names(SAV.Up)), 1:(length(names(SAV.Up))-1))])
    SAV.Lo <- subset(SMET8, SAV_SCR_X <= quantile(SAV_SCR_X, 0.25, na.rm=T), select=1:length(names(SMET8)))
    SAV.Lo$Grp <- "25%ile"
    SAV.Lo <- as.data.frame(SAV.Lo[,c(length(names(SAV.Lo)), 1:(length(names(SAV.Lo))-1))])
SAV.Q1 <- rbind(SAV.Up, SAV.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
################## Sumamry table for upper / lower quantiles.
x1 <- ddply(SAV.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(SAV.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(SAV.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(SAV.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
SAV.Summ <- xx[c(a,1:(a-1))]
SAV.Summ <- SAV.Summ[,-c(3,4)]
write.csv(SAV.Summ, "SAV.summ3.csv")
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
j=4
names(SAV.Q1[j])
##
for (j in 4:length(SAV.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(SAV.Q1, .(Grp), numcolwise(nDiff))
    z2 <- SAV.Summ[(SAV.Summ$PARAM=="median"),]
    z3 <- z2[,-2]
    zD <- ddply(z3, .(PARAM), numcolwise(diff))
    zD <- data.frame(zD[,1], (round(zD[2:length(zD)],2)))
        px <- qplot(Grp, SAV.Q1[,j], data=SAV.Q1, geom="boxplot", main=names( SAV.Q1[j]), 
            xlab=paste("Median Diff = ", zD[1,(j-2)] ,"\nLower and Upper quartiles from SAV Index"),  
                    scale_y_continuous(SAV.Q1[,j]), ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-SAVx2_', names(SAV.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End


#
##
###
####
#####
## Redo - PMI_MI Boxplots...
# [8] Define Lower / Upper Quantiles of Response Variable [5-b]: PMI_MI (#66)
    PMI.Up <- subset(SMET8, PMI_MI >= quantile(PMI_MI, 0.75, na.rm=T), select=1:length(names(SMET8)))
    PMI.Up$Grp <- "75%ile"
    PMI.Up <- as.data.frame(PMI.Up[,c(length(names(PMI.Up)), 1:(length(names(PMI.Up))-1))])
    PMI.Lo <- subset(SMET8, PMI_MI <= quantile(PMI_MI, 0.25, na.rm=T), select=1:length(names(SMET8)))
    PMI.Lo$Grp <- "25%ile"
    PMI.Lo <- as.data.frame(PMI.Lo[,c(length(names(PMI.Lo)), 1:(length(names(PMI.Lo))-1))])
PMI.Q1 <- rbind(PMI.Up, PMI.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
################## Sumamry table for upper / lower quantiles.
x1 <- ddply(PMI.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(PMI.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(PMI.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(PMI.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
PMI.Summ <- xx[c(a,1:(a-1))]
PMI.Summ <- PMI.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
j=4
names(PMI.Q1[j])
##
for (j in 4:length(PMI.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(PMI.Q1, .(Grp), numcolwise(nDiff))
    z2 <- PMI.Summ[(PMI.Summ$PARAM=="median"),]
    z3 <- z2[,-2]
    zD <- ddply(z3, .(PARAM), numcolwise(diff))
    zD <- data.frame(zD[,1], (round(zD[2:length(zD)],2)))
        px <- qplot(Grp, PMI.Q1[,j], data=PMI.Q1, geom="boxplot", main=names( PMI.Q1[j]), 
            xlab=paste("Median Diff = ", zD[1,(j-2)] ,"\nLower and Upper quartiles from PMI Index"),  
                    scale_y_continuous(PMI.Q1[,j]), ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-PMIx2_', names(PMI.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End

##### ** GENERIC BOXPLOT FOR REPORT TEXT *** ####################
{j=4
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
ggsave(file="XX_BOXplot2.jpeg", width=4, height=5, dpi=500)

print(px2)
    dev.off() ###

px3 <- qplot(Grp, PMI.Q1[,j], data=PMI.Q1, geom="boxplot", main="Stressor Variable", 
    xlab=paste("Median Diff = ", zD[1,(j-2)] , "\nLower and Upper quartiles from Response Variable"),
    scale_y_continuous(PMI.Q1[,j]), ylab="Value of Stressor Variable",na.rm=T, notch=T )
} 


#
##
###
####        ECOSYSTEM HEALTH 
#####                               ** Genearlized **
## New EH5-MMI Boxplots
# [xx 9 xx] Define Lower / Upper Quantiles of Response Variable EH5-MMI:  VAR ==  EH5.mmi
# Define new dataset, based on SDAT99x...
x.nm <- as.data.frame(names(SAV2.Summ))
SMET8.set2 <-  SDAT99x[,c(2,109, 16, 17,18,20,21,22,25:27,28:30, 31:87, 89,91,92,94,95,97,
                          99,100,102,103,105,107)]
SMET8 <- SMET8.set2
#
VAR.Up <- subset(SMET8, EH5.mmi >= quantile(EH5.mmi, 0.75, na.rm=T), select=1:length(names(SMET8)))
    VAR.Up$Grp <- "75%ile"
    VAR.Up <- as.data.frame(VAR.Up[,c(length(names(VAR.Up)), 1:(length(names(VAR.Up))-1))])
    VAR.Lo <- subset(SMET8, EH5.mmi <= quantile(EH5.mmi, 0.25, na.rm=T), select=1:length(names(SMET8)))
    VAR.Lo$Grp <- "25%ile"
    VAR.Lo <- as.data.frame(VAR.Lo[,c(length(names(VAR.Lo)), 1:(length(names(VAR.Lo))-1))])
VAR.Q1 <- rbind(VAR.Up, VAR.Lo)  # all data for upper and lower quantiles of RESPONSE-VAR#1, by Grp
################## Sumamry table for upper / lower quantiles.
x1 <- ddply(VAR.Q1, .(Grp), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
    cnt <- function(x) count(x)
x2 <- ddply(VAR.Q1, .(Grp), numcolwise(length))
    nmissing <- function(x) sum(is.na(x))
x3 <- ddply(VAR.Q1, .(Grp), numcolwise(nmissing))
    nDiff <- function(x) (length(x)-nmissing(x))
x4 <- ddply(VAR.Q1, .(Grp), numcolwise(nDiff))
xx <- rbind(x1, x2, x3, x4)
    xx <- xx[order(xx[,1]),]
xx$PARAM <- c("10%","25%", "median", "75%", "90%", "count", "Nmiss", "Nobs")
a <- length(xx)
VAR.Summ <- xx[c(a,1:(a-1))]
VAR.Summ <- VAR.Summ[,-c(3,4)]
##### ** Boxplots of Upper / Lower Quantiles ***  ####################
j=4
names(VAR.Q1[j])
##
for (j in 4:length(VAR.Q1)) {
    nmissing <- function(x) sum(is.na(x))
    nDiff <- function(x) (length(x)-nmissing(x))
    VX <- ddply(VAR.Q1, .(Grp), numcolwise(nDiff))
    z2 <- VAR.Summ[(VAR.Summ$PARAM=="median"),]
    z3 <- z2[,-2]
    zD <- ddply(z3, .(PARAM), numcolwise(diff))
    zD <- data.frame(zD[,1], (round(zD[2:length(zD)],2)))
        px <- qplot(Grp, VAR.Q1[,j], data=VAR.Q1, geom="boxplot", main=names( VAR.Q1[j]), 
            xlab=paste("Median Diff = ", zD[1,(j-2)] ,"\nLower and Upper quartiles from EH MMI"),  
                    scale_y_continuous(VAR.Q1[,j]), ylab="",na.rm=T )
        px2 <- px + scale_x_discrete(labels=paste(VX$Grp, "(n=", c(VX[1,j],VX[2,j]), ")", sep=' '))
    px2
    XName.j <- paste(j, '_Bx_Qnt-EH5_3_', names(VAR.Q1[j]), '.jpg', sep='')
    ggsave(XName.j, width=5, height=5, dpi=300)
    print(px2)
    dev.off()   }
# End