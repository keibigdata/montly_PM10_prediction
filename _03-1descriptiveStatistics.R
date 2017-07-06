##mean of sigungu

#remove variable
rm(list=ls())

#package
library(ggplot2)

#work directory
setwd("D:\\01 Study\\08 pm\\processing\\input")

#input
dataMonth <- read.csv("pm10_sig_matching_75_mean.csv", header=T, sep=",")
head(dataMonth, 6)
length(dataMonth[dataMonth == -999])
dataMonth[dataMonth == -999] <- NA

##PM10 descriptive statistics
#year
PM10_mean_plot <- aggregate(dataMonth$PM10~dataMonth$year, dataMonth, mean, na.rm=TRUE)
colnames(PM10_mean_plot) <- c("year", "PM10")
pm10Label = as.character(signif(round(PM10_mean_plot$PM10, digits = 1),3))

ggplot(data=PM10_mean_plot, aes(x=year, y=PM10, group=1)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  ylab(expression("PM "[10] ~ "³óµµ"  ~ (mu ~ g/m^{3}))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(2001, 2016, 1)) +
  scale_y_continuous(breaks=seq(45, 65, 5), limits = c(42.5,65.5)) +
  geom_text(aes(y = PM10, label = as.character(pm10Label), size = 4, hjust = -0.3, vjust = 0.1))

#month
PM10_mean_plot <- aggregate(dataMonth$PM10~dataMonth$month, dataMonth, mean, na.rm=TRUE)
colnames(PM10_mean_plot) <- c("month", "PM10")
pm10Label = as.character(signif(round(PM10_mean_plot$PM10, digits = 1),3))

ggplot(data=PM10_mean_plot, aes(x=month, y=PM10, group=1)) +
  geom_line() +
  geom_point(shape=21, colour="black", size=2) +
  ylab(expression("PM "[10] ~ "³óµµ"  ~ (mu ~ g/m^{3}))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  scale_x_continuous(breaks=seq(1, 12, 1)) +
  scale_y_continuous(breaks=seq(30, 80, 5), limits = c(32.5,72.5)) +
  geom_text(aes(y = PM10, label = as.character(pm10Label), size = 4, hjust = -0.3, vjust = 0.1))
#year$month




#sig_cd  
PM10_mean_plot <- aggregate(dataMonth$PM10~dataMonth$sig_cd, dataMonth, mean, na.rm=TRUE)
colnames(PM10_mean_plot) <- c("sig_cd", "PM10")
pm10Label = as.character(signif(round(PM10_mean_plot$PM10, digits = 1),3))  


ggplot(data=PM10_mean_plot, aes(x=PM10)) +
  geom_histogram(color="black", fill="white",breaks=seq(24, 70, by=1)) +
  scale_x_continuous(breaks=seq(20, 75, 5), limits = c(24,70)) +
  #scale_y_continuous(breaks=seq(30, 80, 5), limits = c(32.5,72.5))
  theme_bw() +
  theme(panel.grid.major.x = element_line(color = "grey60", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  xlab(expression("PM "[10] ~ "³óµµ"  ~ (mu ~ g/m^{3}))) +
  ylab("")

write.csv(PM10_mean_plot, "D:\\01 Study\\08 pm\\processing\\input\\result\\sig_cd.csv")