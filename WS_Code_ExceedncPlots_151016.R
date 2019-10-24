###  Exceedences (150916)   Plots...
data = OW6.Exceed
names(OW6.Exceed)
OW6.Excd.nam <- as.data.frame(names(OW6.Exceed))
# plot NH3 std x pH, add Exceeding values as separate symbol...
NH3_std [59]
pH [56]
NH3_excd [60]

#######   Not this one #####
q0 <- ggplot(data=subset(OW6.Exceed, NH3_excd = ""), aes(x=pH, y=NH3_std, col=NH3_excd), na.rm=T) + 
    theme_bw() + scale_colour_manual(values=c("red", "black"));
q3 <- q0 + geom_point(na.rm=T);
q3

q1 <- q0 + geom_point(colour="red", na.rm=T);
q2 <- q1 + geom_point(data=subset(OW6.Exceed, NH3_excd != ""), aes(x=pH, y=NH3_std, col=NH3_excd),
                      colour="black",na.rm=T)

#######  U S E  T H I S  O N E   !!!  ###########
w0 <- ggplot(data=NH3calc, aes(x=pH.x)) + theme_bw() + scale_colour_manual(values=rainbow(4));
w1 <- w0 + geom_line(aes(y=T14.5, colour="14.5 C"), na.rm=T) +
            geom_line(aes(y=T20, colour="20 C"), na.rm=T) +
            geom_line(aes(y=T25, colour="25 C"), na.rm=T) +
            geom_line(aes(y=T35, colour="35 C"), na.rm=T) + labs(col="Water Temp") ;
w2 <- w1 + geom_point(data=subset(OW6.Exceed, NH3_excd = ""), aes(x=pH, y=NH4.T), shape=1,
                      col="darkgreen", na.rm=T) + scale_shape(solid=FALSE);
w3 <- w2 + geom_point(data=subset(OW6.Exceed, NH3_excd != ""), aes(x=pH, y=NH4.T), 
                      shape=21, col="black", fill="darkred",size=2.5, na.rm=T);
w4 <- w3 + ggtitle(expression(NH[3]* " Toxicity Criteria (3B) vs pH and Temperature")) +
            xlab("Water pH") + ylab(expression(NH[3]* " Concentration (mg/L)" )) +
            theme(legend.position=c(0.87,0.78)) + scale_y_continuous(limits=c(0,2.5)) +
            scale_x_continuous(limits=c(7,10.6));
w4
ggsave(file=paste("WS_NH3crit_x_pH_v1.jpeg", sep=""), width=5, height=4, dpi=500);
############






