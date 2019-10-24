## Random Forest Code (cribbed from JVL 151007) ##
##### Willard Spur (2015) Random Forest Data Analysis (151012)  ########

library(randomForest)
#### For Package notes (of RF), see the bottom lines... ######

# data == WATSAVINV2
#   data.name == WATSAVINV2.nam
# MSI.all <- WATSAVINV2[,c(62,63,64,17,67,68,31,
#                        18,20,23,22,25,44,53,54,55,51,52,78,79,86,88,96,97,98,112,125,139,140,141,142,143)]
# data == MSI.all
    ## MSI.chem <- MSI.all[,c(1:18)]
    ## MSI.SAV <- MSI.all[,c(1:18,19:25)]
    ## MSI.INV <- MSI.all[,c(1:18,26:32)]
    ## MSI.SAVINV <- MSI.all[,c(19:32)]

### Fix dataset ####
# 151013 #  WATSAVINV2
count(WATSAVINV2$TON.TN2 < 0)
summary(WATSAVINV2[,c(17:68)])

WATSAVINV2$TIN[WATSAVINV2$TIN > 18] <- 1.522
WATSAVINV2$TON[WATSAVINV2$TON < 0] <- 0.678
WATSAVINV2$TON.TN2[WATSAVINV2$TON.TN2 < 0] <- 0.308

##   U S E WATSAVINV2 as data !!  ###
## [RF #1] CHLA model   #################
data <- WATSAVINV2
CHLA_rf <- randomForest(CHLA ~ DOY + T_ALK + TSS + Hardness + TDS + TP + TN2 + TIN + 
                            TN2.TP + DN.DP + Depth + Temp + pH + DO.c + DO.p +
                            Algmat.avg + SurfMat + SAVcov.avg + SAV.SCR + Count + PMI, 
                        data=data, importance=TRUE, ntree=1500, na.action=na.omit, keep.forest=TRUE);

CHLA_rf  # % of variance explained (OOB value)

# Variable Importance Plot    ####################
varImpPlot(CHLA_rf, main="Variable Importance from Random Forest model", cex=0.8)

###  Model Predictions and GoF  ###############
a <- predict(CHLA_rf)
a <- as.data.frame(CHLA_rf$predicted) # row.names are case / ID numbers
a$id <- row.names(a)
c <- CHLA_rf$y

Pred.v.Obs <- cbind(Obs = CHLA_rf$y, Pred = CHLA_rf$predicted)  # works
plot(Pred.v.Obs)                                                # works !!

#spare parts  #########
    d <- cbind(a,c)
    d <- d[,c(2,1)]
    plot(d)
    data.a <- cbind(data,id=row.names(data))
    b <- merge(a, data.a, by="id", all.x=TRUE)
    b.nam <- as.data.frame(names(b));
    plot(b$CHLA, b[,2])  ## these two plots are equivalent... (need to cbind "y" and "predicted")
    plot(d)
    CHLa_mod1 <- cbind(data, a)
####

# varUsed(CHLA_rf, count=T)

# partial dependence plots   #################
partialPlot(CHLA_rf,data,DOY)
partialPlot(CHLA_rf,data,T_ALK)
partialPlot(CHLA_rf,data,POP)
partialPlot(CHLA_rf,data,TP)
partialPlot(CHLA_rf,data,DP)
partialPlot(CHLA_rf,data,Hardness)
partialPlot(CHLA_rf,data,TN2)
partialPlot(CHLA_rf,data,TON)
partialPlot(CHLA_rf,data,TON2)
partialPlot(CHLA_rf,data,pH)
partialPlot(CHLA_rf,data,SAV.SCR)
partialPlot(CHLA_rf,data,TSS)
partialPlot(CHLA_rf,data,SurfMat)
partialPlot(CHLA_rf,data,TDS)
partialPlot(CHLA_rf,data,TIN)
partialPlot(CHLA_rf,data,Depth)
partialPlot(CHLA_rf,data,SAVcov.avg)
partialPlot(CHLA_rf,data,Algmat.avg)
partialPlot(CHLA_rf,data,DO.p)
partialPlot(CHLA_rf,data,DO.c)
partialPlot(CHLA_rf,data,Temp)
partialPlot(CHLA_rf,data,Taxa)
partialPlot(CHLA_rf,data,TN2.TP)

## [RF #2] DO.p model   #############
data = WATSAVINV2
DOperc_rf <- randomForest(DO.p ~ DOY + T_ALK + TSS + Hardness + TDS + TP + TN2 + TIN + 
                            TN2.TP + DN.DP + Depth + Temp + pH + CHLA + Month + 
                            Algmat.avg + SurfMat + SAVcov.avg + SAV.SCR + Count + PMI, 
                        data=data, importance=TRUE, ntree=1500, na.action=na.omit);

DOperc_rf  # % of variance explained (OOB value)

# Variable Importance Plot
varImpPlot(DOperc_rf)
varUsed(DOperc_rf, count=T)

# partial dependence plots
partialPlot(DOperc_rf, data, Temp)
partialPlot(DOperc_rf,data,pH)
partialPlot(DOperc_rf,data,SAVcov.avg)

a <- predict(DOperc_rf)

#####














################################################# # 
### Random Forest Structure #######
##################################################
#3 from ls(CHLA_rf); 
 [1] "call"            "coefs"           "forest"          "importance"      "importanceSD"    "inbag"          
 [7] "localImportance" "mse"             "mtry"            "na.action"       "ntree"           "oob.times"      
[13] "predicted"       "proximity"       "rsq"             "terms"           "test"            "type"           
[19] "y" 

CHLA_rf[1];     ## call: model structure
CHLA_rf[2];     ## type: regression or classification
CHLA_rf[3];     ## predicted
CHLA_rf[4];     ## mse
CHLA_rf[5];     ## rsq
CHLA_rf[6];     ## oob.times
CHLA_rf[7];     ## importance:  IncMSE and IncNodePurity [!!]
CHLA_rf[8];     ## importanceSD
CHLA_rf[9];     ## local Importance
CHLA_rf[10];    ## proximity
CHLA_rf[11];    ## ntree (number of trees grown)
CHLA_rf[12];    ## mtry (number of predictors sampled for splitting at each node)
CHLA_rf[13];    ## ? forest ?
CHLA_rf[14];    ## coefs
CHLA_rf[15];    ## y
CHLA_rf[16];    ## test
CHLA_rf[17];    ## inbag
CHLA_rf[18];    ## terms
CHLA_rf[19];    ## na.action
CHLA_rf$predicted












