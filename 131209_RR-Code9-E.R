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
# SAV_SCR_X; Pond_Mat_2; CHLa_2; Invertebrate MIs:  COTE_MI; PMI_MI; T3_MI; SI_MI; InvTaxa_MI
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
{ # Use cat.analysis function from psurvey.analysis;
# Start by setting up the several small data frames needed by cat.analysis;
#data frame to specify the sites in datnew to be used for extent estimates.;
# DO NOT change siteID to match row names in original file.  the function row.names(datnew) does this.
# siteID is a required name for cat.analysi function (MS)
## DATA ==> SDAT9
Sites1 <- SDAT9[,c(3:16)] ;   Sites1.nm <- as.data.frame(names(Sites1))
sites.ext <- data.frame(siteID=SDAT99$Site_ID,Use=rep(TRUE,nrow(SDAT99))); 

#data frame to specify subpopulations.;
subpop.ext<-data.frame(siteID=SDAT99$Site_ID,GSL_IW=rep('IW2012', nrow(SDAT99)));
subpop.ext2 <- data.frame(siteID=SDAT99$Site_ID, GSL_IW=rep('IW2012', nrow(SDAT99)), 
                          Basin=SDAT99$stratum)

#data frame with design information (weights and lat/lon);
design.ext2 <- data.frame(siteID=SDAT99$Site_ID, wgt=SDAT99$AdjWgt, xcoord=SDAT99$Xmarinus, 
                          ycoord=SDAT99$Ymarinus);

## combine all Variables into SDAT9 --> as SDAT99
{SDAT99 <- data.frame(SDAT9, S9.RESPCOND, S9StressCOND2)
SDAT99.nm <- as.data.frame(names(SDAT99))
SDAT99[3] == SDAT99[227]; SDAT99[3] == SDAT99[242]
SDAT99 <- SDAT99[,-c(227,242)]  }

data.cat.ext1 <- data.frame(siteID=SDAT99$Site_ID,SDAT99[,227:341]);
#input data frames are complete, can now execute the cat.analysis function to estimate extent;
levels(SDAT99$stratum)
framesize <- c("Jordan"=70, "Lower Bear-Malad"=44, "Lower Weber"=41)
popsize <- list(GSL_IW=sum(framesize), Basin=as.list(framesize))

extent.est3 <- cat.analysis(sites = sites.ext, subpop=subpop.ext2, design = design.ext2, 
      data.cat = data.cat.ext1, popsize, vartype = "Local", conf = 95);
write.csv(extent.est3, "RelExtent3.csv")
}
{#Note 1 -- Output data frame, extent.est, has estimates , SE's and confidence bounds ;
# For percent of stream miles (.P columns) Output w/ estimates for total stream length (.U columns),;
# reported in the same length units as the sampling weights; 
#If Estimate.U Total does not equal stream length (km) for basin, need to adjust wieghts (MS)
#Note 2-- Note the different vert condition categories, ;
# including the missing data (not explicit, must infer from reduced sample size);
#  can extract the rows of extent.est having category = 'Poor', and make a barplot;
barplot(extent.est$Estimate.P[extent.est$Category=='POOR'],
        ylab='Percent of total length in POOR condition',
           names.arg=c('OE_COND','TP_COND'));}
################## REL. EXTENT IS DONE.  NOW FOR REL. RISK --------->
# ** Code [below] seesm fuckt.  Mike S. said to check to make sure each response-stressor 
#pair contains a full POOR x GOOD contingency table... use "table" command
data.cat.RR
# Stressor VARS:  2:15   [14]  //  REsponse VARS:  16:116   [101]
###  TABLE of 2x2 Contingency Values for POOR / GOOD PAIRS...
j = 13
i = 16
for(j in 2:15) {
    for(i in 16:116) {
        x <- as.data.frame.matrix(table(data.cat.RR[,j], data.cat.RR[,i], exclude="FAIR",
                            dnn=c(names(data.cat.RR[j]),names(data.cat.RR[i]))))
        x$response <- names(data.cat.RR[j])
        x$stress <- names(data.cat.RR[i])
        print(x)
        write.table(x, "Data.table2.csv", append=TRUE, sep=",", col.names=NA, row.names=T)
    }     }
#Results of Table-analyses in:  RRtable.out; Cols = Response, Rows = Stressors;
# Vals = 0,1 (0 if no POOR/GOOD pair available, 1 if ok)
# STep1: set data.frames by response VAR, but leave the "FAIR" classes alone...
for(k in 2:15) {   
    dat.k <- data.cat.RR[,c(1,k,16:116)]
    for(L in 1:103){
        dat.k[,L] <- factor(dat.k[,L])      }
    assign(paste("data.cat.",k,sep =""),dat.k)   }
# STEP2: prepare analysis data.frames:
## For datafiles  {data.cat.[X]}:  Col1 = siteID, Col2 = RESPONSE, Cols[3:103] = STRESSORs
data.cat.X.nm <- as.data.frame(names(data.cat.2))
{# Data for analysis:      # SITES == sites.ext  //  # POP/SubPOPs == subpop.ext (no sub-pops for this analysis)
    # DESIGN == design.ext2
# data.risk <== {siteIDs, Response and Stressor Vars} -- names required and assigned to *_vars
# DF of RESPONSE [col] x STRESSOR [row] pairs (1 = ok, 0 = don't use) -- as: RRtable.out
colnames(RRtable.out)[1] = "Stressors"
names(RRtable.out[1])  }
###  Melt RRtable.out into FLAT file for easier indexing...
RRtab.melt <- melt(RRtable.out, id="Stressors", value.name="RR_ok", variable.name="Responses")
RR.a <- RRtab.melt   # make working copy...
RRtab.melt[RRtab.melt[,2]==names(data.RR[2]),]
#
k = 13
j = 4
levels(data.RR[,4])
data.MMI <- data.cat.RR[,c(1,k,17)]
### *****  THIS IS THE RELTIVE RISK CODE FOR IW2012 DATA [BELOW]  *******
for(k in 3:15) {  
    dat.k <- data.cat.RR[,c(1,k,16:116)]    #    dat.k == data.cat.2
    data.RR <- dat.k    # col1 = ID, Col2 = response, Cols3+ = Stressors
    respns <- names(data.RR[2])
for(j in 3:length(names(data.RR))){
        
    stress <- names(data.RR[j])
        a2 <- RR.a[RR.a$Responses==respns,]
        b3 <- a2[a2$Stressors==stress,] 
    ifelse(b3[,3]==0,
            print(paste("No Rel Risk for ",respns," & ",stress,sep="")),
            RRa.1 <- relrisk.est(response=data.RR[,2], stressor=data.RR[,j],
                     response.levels=levels(response), stressor.levels=levels(stressor),
                     xcoord=design.ext2[,"xcoord"],ycoord=design.ext2[,"ycoord"],
                     wgt=design.ext2[,"wgt"],vartype="Local",conf=90))
           #assign(paste("RR_",respns,"_",j,sep=""),RRa.1)
    v1 <- as.data.frame(RRa.1$RelRisk)
        colnames(v1)[1]="RelRisk"
    v1$Response <- names(data.RR[2])
    v1$Stressor <- names(data.RR[j])
    v1$lower <- RRa.1$ConfLimits[1]
    v1$upper <- RRa.1$ConfLimits[2]
#datafile:  
    if(j == 3){
        write.table(v1, paste("RR_",respns,"_out1.csv",sep=""),sep=",", col.names=NA, row.names=T)
    } else {
        write.table(v1, paste("RR_",respns,"_out1.csv",sep=""), append=T, sep=",", col.names=F)
    }   }   }
### THIS CODE FUCKING WORKS!!



###  Clean up RR code for EH5 response...
###
### *****  THIS IS THE RELTIVE RISK CODE FOR IW2012 DATA [BELOW]  *******
##
# Fix data
S99x.rr <- merge(SDAT99, SDAT99x, by.x="Site_ID", by.y="Site_ID", all.x=T)
EH5.rr.nm <- as.data.frame(names(EH5.rr))
    data5.RR <- EH5.rr[,c(2,16, 17:108,9:11,13,14)]    # col1 = ID, Col2 = response, Cols3+ = Stressors
    respns <- names(data.RR[2])

#data frame to specify subpopulations.;
subpop2 <- subpop[,c(1,2)]
subpop.ext<-data.frame(siteID=SDAT99x$Site_ID,GSL_IW=rep('IW2012', nrow(SDAT99x)));
subpop.ext2 <- data.frame(siteID=SDAT99x$Site_ID, GSL_IW=rep('IW2012', nrow(SDAT99x)), Basin=SDAT99x$stratum)

#data frame with design information (weights and lat/lon);
design5.ext <- data.frame(siteID=EH5.rr$Site_ID, wgt=EH5.rr$AdjWgt, xcoord=EH5.rr$Xmarinus, 
                          ycoord=EH5.rr$Ymarinus);

## Remove cases w/ ANY NAs...REDO data import...clean it up in excel...


for(j in 3:length(names(data5.RR))){
        stress <- names(data5.RR[j])
        respns <- names(data5.RR[2])
    RRa.1 <- relrisk.est(response=data5.RR[,2], stressor=data5.RR[,j],
                 response.levels=levels(response), stressor.levels=levels(stressor),
                 xcoord=design5.ext[,"xcoord"],ycoord=design5.ext[,"ycoord"],
                 wgt=design5.ext[,"wgt"],vartype="Local",conf=90)
           #assign(paste("RR_",respns,"_",j,sep=""),RRa.1)
    v1 <- as.data.frame(RRa.1$RelRisk)
        colnames(v1)[1]="RelRisk"
    v1$Response <- names(data5.RR[2])
    v1$Stressor <- names(data5.RR[j])
    v1$lower <- RRa.1$ConfLimits[1]
    v1$upper <- RRa.1$ConfLimits[2]
#datafile:  
    if(j == 3){
        write.table(v1, paste("RR_",respns,"_out1.csv",sep=""),sep=",", col.names=NA, row.names=T)
    } else {
        write.table(v1, paste("RR_",respns,"_out1.csv",sep=""), append=T, sep=",", col.names=F)
    }   }  
 ### THIS CODE FUCKING WORKS!!
j