### eCDFs of data...  151019
## data = WATSAVINV2

levels(WATSAVINV2$SiteSAV.x)
var.list <- c(17:68, 78:88, 96:98, 112, 125, 139:143)
k = 17
names(WATSAVINV2[k])




for (k in var.list) {
    d0 <- ggplot(WATSAVINV2, aes(x=WATSAVINV2[,k]), na.rm=T) + stat_ecdf(na.rm=T) + theme_bw();
    d1 <- d0 + geom_hline(yintercept=c(0.75, 0.25), lty=2, color="black") +
        ylab("Proportion of data occurring below X-value") + xlab(paste(WATSAVINV2.nam[k,1])) +
        ggtitle(paste("CDF plot for ", WATSAVINV2.nam[k,1], sep="")) +
        scale_y_continuous(labels=percent);
    d1
ggsave(file=paste("WS_cdfs_WSIdat",(WATSAVINV2.nam[k,1]),"_0.jpeg", sep=""),
           width=6, height=6, dpi=500)    
}


### other distributions for comparison...
qplot(rnorm(1000), stat = "ecdf", geom = "step")

qplot(rbeta(1000, 2, 10, ncp=0), stat = "ecdf", geom = "step")
qplot(qbeta(1000, 2, 10, ncp=0), stat = "ecdf", geom = "step")


qplot(rchisq(1000, 5, ncp=0), stat = "ecdf", geom = "step")

qplot(rf(1000, 5, 10, ncp=0), stat = "ecdf", geom = "step") ## F-dist ?

qplot(rlnorm(1000, meanlog=0, sdlog=1), stat = "ecdf", geom = "step")

qplot(rpois(1000, 2), stat = "ecdf", geom = "step")

qplot(runif(1000, min=0, max=1), stat = "ecdf", geom = "step")



#### This is one is okay ... but the ggplot one (above) is better ########
for (k in var.list) {
    plot.ecdf(WATSAVINV2[,k], ylab="Proportion of data occurring below X-value");
    ggsave(file=paste("WS_cdfs_WSIdat",(WATSAVINV2.nam[k,1]),"_0.jpeg", sep=""),
           width=6, height=6, dpi=500)    
}
###

##########
#3 var list ::  
## 17:68, 78:88, 96:98, 112, 125, 139:143

######
### older code ### from FRNG_codE_Aggregate+CDFs_141207.R ######
###
option [2]
for (i in 4:10) {
  plot.ecdf(SiteC4.trans[,i], ylab="Proportion of data occuring below 
            x-value");
  ggsave(file=paste("ENVtrans",(SiteC4.trans.nm[i,1]),"-0.jpeg", sep=""),
         width=6, height=6, dpi=500)    }
}  #  earlier work and data preparation

# option [3] -- Hand calcs (percentile as 'Relative Rank')
# run through continuous vars, and create temp files w/ data + percentiles (rel. ranks)
##################  running through different versions...SiteC4.trans is NOT maintained
# data for relativized Environmental Variables == SiteC4T.dat
SiteC4.trans <- SiteC4T.dat
names(SiteC4.trans)
# ID.VARs == 1:9            // DATA.VARs == 10:18

for (j in 10:(length(names(SiteC4.trans)))) {
  v0 <- as.data.frame(SiteC4.trans[,c(2:4,j)])
  v0$prnk <- rank(v0[,4], ties.method="min")/(length(v0[,4]))
  assign(paste("ENVar.",names(SiteC4.trans[j]),sep=""),v0)    }
## This seems to work ^^^  

####


