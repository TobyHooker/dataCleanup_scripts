## Boxplot -- Rel. Cov Invasives x Water source
data <- VEG2.SCORES3.ENV2


x0 <- ggplot(data=data, aes(x=Water.Src, y=RelCov.INV)) + scale_color_tableau() + theme_bw();
x1 <- x0 + geom_boxplot() + ylab("Rel. Cov. Invasive Plant Species") + xlab(NULL) +
        scale_x_discrete(labels=c("Stream/Canal", "Groundwater", "Wetland \nInterior",
                                      "Impounded\nWetland", "WWTP \nDischarge")) +
        scale_y_continuous(labels=percent);
x1

ggsave(file="INVrc_x_WaterSrc_box-1.jpeg", width=6, height=5, dpi=500);

### might be better to use site means... ###




