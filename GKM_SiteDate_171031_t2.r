## sample collection chart for GKM - CShope
#
# 171030
# associated w/ review of CShope's GKM report (due 10/31/17)

# Source data from : U:\PERMITS\MONITORS\Incident Reports and Compliance Sampling\GKM\Working\Shope\161202-Tbl_AllEDD.xlsx

# data extracted to new file: L:\DATA MANAGEMENT & QA\GKM Checks\WChem-Site-x-Date_GKM_171030.xlsx

## Import data
library(openxlsx)
options(scipen = 999, digits = 4)
##
filename <- choose.files(caption="Select xlsx File to Import", multi=FALSE)
DATA.file <- openxlsx::read.xlsx(filename, sheet="dat2", startRow = 1, colNames=TRUE, rowNames=FALSE, detectDates=FALSE);
path.filename <- dirname(filename)
setwd(path.filename)
##
loadWorkbook(filename)
openxlsx::getSheetNames(filename)
###

DATA0 <- DATA.file
names(DATA0)

# MLID
sort(unique(paste(DATA0$MLID, DATA0$Site.Name, sep="."))) # just use MLIDs
# QC sites (to ignore)
QC.sites <- c(4953582, 4953852, 4953853, 999)
# should give 11 sites...
DATA.gkm <- DATA0[!(DATA0$MLID %in% QC.sites),]
#
length(unique(DATA0$MLID))
length(unique(DATA.gkm$MLID))
##
# fix dates == SDate
DATA.gkm$SDate <- as.Date(DATA.gkm$CollectionDate, origin="1899-12-30")
#as.Date(DATA.gkm$CollectionDate[1], origin="1899-12-30")
## Select Matrix2 == Aqueous
unique(DATA.gkm$Matrix2)
DATA.gkm2 <- DATA.gkm[DATA.gkm$Matrix2 == "Aqueous",]
#
unique(DATA.gkm2$TestCode)
unique(DATA.gkm2$Analyte)
unique(DATA.gkm2$Fraction3)
## Metals-Only
metals <- c("Ag","Silver","Al","Aluminum","Arsenic","As","B","Ba","Barium","Be","Beryllium",
            "Ca","Cadmium","Calcium","Cd","Chromium","Co","Cobalt","Copper","Cr","Cu",
            "Fe","Hg","Iron","K","Lead","Magnesium","Manganese","Mercury","Mg","Mn","Mo",
            "Molybdenum", "Na","Ni","Nickel","Pb","Potassium","Sb","Antimony","Se","Selenium",
            "Sodium","Sr","Strontium","Thallium","Tl","V","Vanadium","Zinc","Zn")
DATA.gkm2$Param.Grp <- ifelse(DATA.gkm2$Analyte %in% metals, "METALS", "other")
DATA.gkm2_x <- DATA.gkm2[DATA.gkm2$Param.Grp == "METALS",]
# Result_x
DATA.gkm2_x$Result_x <- ifelse(is.na(DATA.gkm2_x$Rslt2), DATA.gkm2_x$PQL,
                               DATA.gkm2_x$Rslt2)
## Delete records w/ Result_x == 0
DATA.gkm3 <- DATA.gkm2_x[!is.na(DATA.gkm2_x$Result_x) & DATA.gkm2_x$Result_x != 0,]
sum(is.na(DATA.gkm2$Rslt2))
sum(is.na(DATA.gkm2_x$Result_x))
sum(is.na(DATA.gkm3$Result_x))
#y
unique(DATA.gkm3$Analyte)
unique(DATA.gkm3$Fraction3)
# delete white spaces
DATA.gkm3$Fraction3 <- trimws(DATA.gkm3$Fraction3)
unique(DATA.gkm3$Fraction3)
unique(DATA.gkm3$Matrix2)
sort(unique(DATA.gkm3$MLID))
# remove 4953253 [dupe site]
DATA.gkm4 <- DATA.gkm3[DATA.gkm3$MLID != 4953253,]
DATA.gkm4$SKey1 <- paste(DATA.gkm4$MLID, as.integer(DATA.gkm4$CollectionDate), DATA.gkm4$Analyte, DATA.gkm4$Matrix2, DATA.gkm4$Fraction3, sep=".")
#
DATA.gkm4$SKey2 <- paste(DATA.gkm4$MLID, as.integer(DATA.gkm4$CollectionDate), DATA.gkm4$Matrix2, DATA.gkm4$Fraction3, sep=".")
DATA.gkm4$SKey3 <- paste(DATA.gkm4$MLID, DATA.gkm4$Matrix2, DATA.gkm4$Fraction3, sep=".")
#
DATA.gkm4.trim <- DATA.gkm4[,c(1,6,12,28,43,47)]

## summary data.gkm5.uniq
# Need to summarize the info...into blocks
# should plot year.month (or date) for sites... How many brackets ?
DATA.gkm4.trim$YR <- as.numeric(format(DATA.gkm4.trim$SDate, "%Y"))
DATA.gkm4.trim$MN <- (format(DATA.gkm4.trim$SDate, "%m", width=2))
DATA.gkm4.trim$YR.MN <- (paste(DATA.gkm4.trim$YR, DATA.gkm4.trim$MN, sep="."))
DATA.gkm4.trim$YR.WK <- (paste(DATA.gkm4.trim$YR, (format(DATA.gkm4.trim$SDate, "%U", width=2)), sep="."))
##
DATA.gkm5 <- unique(DATA.gkm4.trim)
#DATA.gkm5$Fraction4 <- DATA.gkm5$Fraction3
#DATA.gkm5$Fraction4 <- factor(DATA.gkm5$Fraction4, c(""))....
##
fig.dat <- DATA.gkm5
library(ggplot2); library(scales)
# workable
z0 <- ggplot(data=fig.dat, aes(y=SDate, x=factor(MLID))) + theme_bw()
z1 <- z0 + geom_point(aes(group=factor(Fraction3), color=Fraction3), na.rm=T, 
                      position = position_dodge(width=0.5), size=1) + coord_flip() +
    scale_color_manual(values=c("red", "black")) + 
    scale_y_date(labels=date_format("%y.%U"), breaks=date_breaks("4 weeks"))
z1
ggsave("gkm_fig1_1.jpeg", z1, width=6, height=4, dpi=500)
dev.off()
####
#### Trial #2
DATA.gkm5_xx <- DATA.gkm5

Samp.Dates <- data.frame(SDates = sort(unique(trunc.Date(DATA.gkm5_xx$SDate))))
Samp.Dates$Delta <- lag(Samp.Dates$SDates, k = 1, diff.Date())
Samp.Dates$Delta <- as.difftime(Samp.Dates$SDates[c(2:nrow(Samp.Dates))], Samp.Dates$SDates[c(1:(nrow(Samp.Dates)-1))], units="auto")
Delta <- diff.Date(Samp.Dates$SDates, lag=1, 1)
Delta2 <- c(NA, Delta)
Samp.Dates <- cbind(Samp.Dates, Delta2)
Samp.Dates$Note <- NA
Samp.Dates$Note[3] <- "Start sampling"
Samp.Dates$Note[15] <- "Phase I sampling"
Samp.Dates$Note[16] <- "Phase II"
Samp.Dates$Note[21] <- "Start Longer-term monitoring"
Samp.Dates$Note[36] <- "Start weekly runoff monitoring"

    # add some dummy data to space out the ranges
space.dat <- NULL
space.dat <- data.frame(MLID = 4954000, CollectionDate = as.numeric(42309), Matrix2 = "Aqueous", 
                    Fraction3 = "Total", SDate = as.Date(42309, origin="1899-12-30"),
                    SKey2 = paste("4954000","42309", "Aqueous", "Total", sep="."),
                    YR = 2015, MN = 11, YR.MN = "2015.11", YR.WK = "2015.45")
DATA.gkm5_xx <- rbind(DATA.gkm5_xx, space.dat)
DATA.gkm5_xx <- rbind(DATA.gkm5_xx, space.dat)
DATA.gkm5_xx <- rbind(DATA.gkm5_xx, space.dat) # add 3 rows
DATA.gkm5_xx$Matrix2[DATA.gkm5_xx$YR.MN == "2015.11"] <- NA
DATA.gkm5_xx$Fraction3[DATA.gkm5_xx$YR.MN == "2015.11"] <- NA
rownames(DATA.gkm5_xx) <- NULL
DATA.gkm5_xx$CollectionDate[486] <- as.numeric(42339)
DATA.gkm5_xx$SDate[486] <- as.Date(42339, origin="1899-12-30")
DATA.gkm5_xx$SKey2[486] <- paste("4954000","42339", "Aqueous", "Total", sep=".")
DATA.gkm5_xx$MN[486] <- 12
DATA.gkm5_xx$YR.MN[486] <- "2015.12"
DATA.gkm5_xx$YR.WK[486] <- "2015.49"
DATA.gkm5_xx$YR.mon <- (paste(DATA.gkm5_xx$YR, (format(DATA.gkm5_xx$SDate, "%b")), sep="."))
DATA.gkm5_xx$Month <- format(DATA.gkm5_xx$SDate, "%b")
DATA.gkm5_xx[487,2] <- as.numeric(42217)
DATA.gkm5_xx[487,5] <- as.Date(42217, origin="1899-12-30")
DATA.gkm5_xx[487,6] <- paste("4954000","42217", "Aqueous", "Total", sep=".")
DATA.gkm5_xx[487,8] <- "08"
DATA.gkm5_xx[487,9] <- "2015.08"
DATA.gkm5_xx[487,10] <- "2015.31"
DATA.gkm5_xx[487,11] <- "2015.Aug"
DATA.gkm5_xx[487,12] <- "Aug"
DATA.gkm5_xx <- DATA.gkm5_xx[c(1:487, 487),]
DATA.gkm5_xx[488,5] <- as.Date(42213, origin="1899-12-30")
#
fig.dat <- DATA.gkm5_xx
library(ggplot2); library(scales)
# another try
z0 <- ggplot(data=fig.dat, aes(y=SDate, x=factor(MLID))) + theme_bw()
z1 <- z0 + geom_point(aes(group=factor(Fraction3), color=Fraction3), na.rm=T, 
                      position = position_dodge(width=0.5), size=1) + coord_flip() +
    scale_color_manual(values=c("red", "black")) +
    scale_y_date(limits =c(as.Date(42217,origin="1899-12-30"), as.Date(42579,origin="1899-12-30")),
                 labels=date_format("%b"), breaks=date_breaks("1 month"))
z1
ggsave("gkm_fig1_2.jpeg", z1, width=6, height=4, dpi=500)
dev.off()
##
##
##
#### FIG nunmber 3 / 4
fig.dat <- DATA.gkm5_xx
# 
z0 <- ggplot(data=fig.dat[!is.na(fig.dat$Fraction3),], 
             aes(y=CollectionDate, x=factor(MLID))) + theme_bw()
z1 <- z0 + geom_point(aes(group=factor(Fraction3), color=Fraction3), na.rm=T, 
                      position = position_dodge(width=0.5), size=1) + 
    scale_color_manual(values=c("red", "black")) +
    scale_y_continuous(limits = c(42217, 42580), breaks = c(42217, 42248, 42278, 42309, 42339,
                                                         42370, 42401, 42430, 42461, 42491,
                                                         42522, 42552, 42580),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug")) + 
    xlab("Monitoring Location ID") + ylab("Sample Date") + coord_flip() + labs(color="Sample Fraction")
z1
#
ggsave("gkm_fig1_3.jpeg", z1, width=6, height=4, dpi=500)
ggsave("gkm_fig1_4.jpeg", z1, width=7, height=3.5, dpi=500)
#
dev.off()
####
##
### Another version, try FACET ~ YR 
fig.dat <- DATA.gkm5_xx
#
library(ggplot2); library(scales)
v0 <- ggplot(data=fig.dat[!is.na(fig.dat$Fraction3),], 
             aes(y=SDate, x=factor(MLID))) + theme_bw() + geom_blank(data=fig.dat[fig.dat$MN < 12,]) +
    facet_grid(~YR, scales="free", space="free")
v1 <- v0 + geom_point(aes(group=factor(Fraction3), color=Fraction3), na.rm=T, 
                      position = position_dodge(width=0.5), size=1) + 
    scale_color_manual(values=c("red", "black")) + xlab("Monitoring Location ID") + 
    ylab("Sample Date") + labs(color="Sample \n   Fraction") +
    scale_y_date(labels=date_format("%d %b"), breaks = as.Date(c(42217, 42248, 42278, 42309,
            42401, 42430, 42461, 42491,42522, 42552, 42580), origin="1899-12-30")) + coord_flip()
v1


ggsave("gkm_fig1_5x.jpeg", width=7, height=3.5, dpi=500)


#
##
### Another version #2, try FACET ~ YR; revise some size issues
fig.dat <- DATA.gkm5_xx
#
library(ggplot2); library(scales)
c0 <- ggplot(data=fig.dat[!is.na(fig.dat$Fraction3),], 
             aes(y=SDate, x=factor(MLID))) + theme_bw() + geom_blank(data=fig.dat[fig.dat$MN < 12,]) +
    facet_grid(~YR, scales="free", space="free")
c1 <- c0 + geom_point(aes(group=factor(Fraction3), color=Fraction3), na.rm=T, 
                      position = position_dodge(width=0.5), size=1) + 
    scale_color_manual(values=c("red", "black")) + 
    ylab("Sample Date") + labs(color="Sample Fraction") + xlab("Monitoring Location ID") + 
    scale_y_date(labels=date_format("%b"), breaks = as.Date(c(42217, 42248, 42278, 42309,
            42401, 42430, 42461, 42491, 42522, 42552, 42583), origin="1899-12-30")) + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_text(size=rel(0.8)), legend.position = c(0.87,0.1), legend.margin = margin(3,5,1,5), legend.text = element_text(size=rel(0.7)), legend.title = element_text(size=rel(0.7)),
          legend.spacing = unit(1, "lines"), legend.key.size = unit(1, "lines"), legend.background = element_rect(color="gray80")) + 
    guides(color = guide_legend(nrow=1)) +
    coord_flip()
c1


ggsave("gkm_fig1_6x.jpeg", c1, width=7, height=3, dpi=500)




