# DAT == SDAT9 to start...
SDAT9 <- read.csv("C:/Users/tobyhooker/Dropbox/IW_IR/IR12/IW_IR_R/SDAT9_2.csv")
SDAT9.nm <- as.data.frame(names(SDAT9))
#should summarize response vars [again?]...need to code thresholds for stressor vars...
{SDAT9.summ <- ddply(SDAT9, .(Final_Status), numcolwise(quantile, probs=c(0.1,0.25,0.5,0.75,0.9)), 
            na.rm=T, names=T)
SDAT9.summ$PARAM <- c("10%","25%", "median", "75%", "90%")
    a <- length(SDAT9.summ)
SDAT9.summ <- SDAT9.summ[,c(a, 1:(a-1))]
write.csv(SDAT9.summ, "SDAT9_summ2.csv")
## re-calc DO as departures from Numeric criteria (5.0 and 3.0 mg/L)...
SDAT9 <- within(SDAT9, {
    DOdep5_1 <- ifelse(DOconc_1 < 5.0,(5.0 - DOconc_1),0)
    DOdep5_2 <- ifelse(DOconc_2 < 5.0,(5.0 - DOconc_2),0)
    DOdep3_1 <- ifelse(DOconc_1 < 3.0,(3.0 - DOconc_1),0)
    DOdep3_2 <- ifelse(DOconc_2 < 3.0,(3.0 - DOconc_2),0)   })
{# SAV_SCR_X; Pond_Mat_2; CHLa_2; Invertebrate MIs:  COTE_MI; PMI_MI; T3_MI; SI_MI; InvTaxa_MI
# MMIs (from sharook):  SAV_mmi; Inv_mmi1; Inv_mmi2; MAT_mmi; EcoH_mmi1; EcoH_mmi2
S9.resp1 <- data.frame((SDAT9[,c(3,142,131,50,189,195,192,198,201,215,217,218,216,219,220)]))
S9.resp.nm <- as.data.frame(names(S9.resp1))
S9.resp.nm$Good <- "upper"   # now code where "Good" condition lies from varialbe (upper v lower)
S9.resp.nm$Good[1] <- "NA"
S9.resp.nm$Good[c(3,4,7)] <- "lower"

SDAT9 <- data.frame(SDAT9, H2S1)    # add H2S to SDAT9 :
    SDAT9 <- data.frame(SDAT9, Hardness)
SDAT9.nm <- as.data.frame(names(SDAT9))
# Toxics..."Toxics1"
Tox.nm <- as.data.frame(names(Toxics1))
ToxCrit <- Toxics1[,c(1,10,11,15,16,20,21,25,26,30,31,35,36,40,41,45,46,50,51,55,56,
                      58,59,61,62,64,65,67,68,70,71,73,74,76,77,79,80,82,83,85,86,
                      88,89,91,92,94,95)]
ToxCrit.nm <- as.data.frame(names(ToxCrit))
ToxCrit.2 <- ToxCrit[,c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47)]    }
}
#stressors...{see list in IW notebook... }
{S9.strs1 <- data.frame((SDAT9[,c(3,32,33,221,223,43,44,45,48,50,51,53,56,57,59,60,61,62,68,69,70,
                        74,75,81,82,83,86,87,88,95,94,97,96,159,161,171,164,169,168,166,176,
                        208,209,210,211,212,213,214,17,18,22,25,28,35,38,42,89,
                        100,103,106,109,112,118,121,124,127,144,145,146,147,149,151,152,153,156,
                        225,226,76,77)]))
S9.Eut <- data.frame((SDAT9[,c(3,32,33,221,223,43,44,45,48,50,51,53,56,57,59,60,61,62,68,69,70,
                        74,75,81,82,83,86,87,88,95,94,97,96,159,161,171,164,169,168,166,176,
                        208,209,210,211,212,213,214)]))
S9.Phys <- data.frame((SDAT9[,c(3,17,18,22,25,28,35,38,42,89)]))
S9.Tox <- data.frame((SDAT9[,c(3,100,103,106,109,112,118,121,124,127,144,145,146,147,149,151,
                               152,153,156,225,226,76,77)]))    }
# Add ToxCrit (toxic criteria and Efx Ratios [0,1]) to S9.Tox...
{S9.Tox2 <- merge(S9.Tox, ToxCrit.2, by.x="Site_ID", by.y="Site_ID", all.x=T)
S9.stress <- data.frame(S9.strs1,S9.Tox2)
S9.stress <- unique(S9.stress)
S9.st.nm <- as.data.frame(names(S9.stress))  #3 names of all stressors / indicator variables
S9.stress <- S9.stress[,-c(81:102)]   ## All stressors, combined
S9.st.nm$Good <- "lower"    # for VArs that decrease w/ increasing condition (Good << Poor)
S9.st.nm$Good[1] <- "NA"
# for VArs that increase w/ condition (Good >> Poor)
S9.st.nm$Good[c(2,3,6,7,8,9,11,14,15,18,21,26,29,30,32,33,37,38,39,42:48,49:52,55,57)] <- "upper"   
#
write.csv(S9.resp1, "S9Response.csv")
write.csv(S9.stress, "S9Stressors.csv")
write.csv(S9.st.nm, "S9stressGrps.csv")
write.csv(S9.resp.nm, "S9respndGrps.csv")
# REsponse and Stress metrics re-coded as discrete classes as:
S9.RESPCOND
S9StressCOND2
}
## Now incorporate datasets into RelRisk Calcs:...
# Have X,Y Marinus already calc'd from earlier.  Weights are correct too.
###############################;
### Step 3 -- Estimate relative extents;  *** OF STRESSOR VARIABLES !!! ***
# Use cat.analysis function from psurvey.analysis;
# Start by setting up the several small data frames needed by cat.analysis;
#data frame to specify the sites in datnew to be used for extent estimates.;
# DO NOT change siteID to match row names in original file.  the function row.names(datnew) does this.
# siteID is a required name for cat.analysi function (MS)
## DATA ==> SDAT9
Sites1 <- SDAT9[,c(3:16)] ;   Sites1.nm <- as.data.frame(names(Sites1))
Sites5 <- as.data.frame(S99x.rr[,(1)])
sites.ext <- data.frame(siteID=S99x.rr$Site_ID,Use=rep(TRUE,nrow(S99x.rr))); 

#data frame to specify subpopulations.;
subpop.ext<-data.frame(siteID=row.names(SDAT99),Region_10=rep('allsites', nrow(SDAT99)));
subpop.ext2 <- data.frame(siteID=SDAT99$Site_ID, GSL_IW=rep('IW2012', nrow(SDAT99)), 
                          Basin=SDAT99$stratum)
subpop.ext <- subpop

#data frame with design information (weights and lat/lon);
design.ext2 <- data.frame(siteID=SDAT99$Site_ID, wgt=SDAT99$AdjWgt, xcoord=SDAT99$Xmarinus, 
                          ycoord=SDAT99$Ymarinus);
design5.ext5 <- design.ext2
## combine all Variables into SDAT9 --> as SDAT99
{SDAT99 <- data.frame(SDAT9, S9.RESPCOND, S9StressCOND2)
SDAT99.nm <- as.data.frame(names(SDAT99))
SDAT99[3] == SDAT99[227]; SDAT99[3] == SDAT99[242]
SDAT99 <- SDAT99[,-c(227,242)]  }

data.cat.ext1 <- data.frame(siteID=SDAT99$Site_ID,SDAT99[,227:341]);
data.cat.ext5 <- data.frame(siteID=S99x.rr$Site_ID,S99x.rr[,c(227,228,231,233,393,394,432,435,438,440,443,
                                                              446,448,452)])
#input data frames are complete, can now execute the cat.analysis function to estimate extent;
levels(SDAT99$stratum)
framesize <- c("Jordan"=70, "Lower Bear-Malad"=44, "Lower Weber"=41)
popsize <- list(GSL_IW=sum(framesize), Basin=as.list(framesize))

extent.est5 <- cat.analysis(sites = sites.ext, subpop=subpop.ext, design = design5.ext5, 
      data.cat = data.cat.ext5, popsize, vartype = "Local", conf = 95);
write.csv(extent.est5, "RelExtent5.csv")