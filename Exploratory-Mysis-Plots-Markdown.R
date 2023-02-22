library(tidyverse)
library(rmarkdown)
library(mgcv)

GLNPO <- read.csv("C:\\Users\\sarah\\Documents\\Cornell\\Mysid Data\\Exploratory Plots\\Mysis-Plots\\MI_OutputByStation_CSV.csv")
summary(GLNPO)

colnames(GLNPO)

ggplot(data = GLNPO, mapping = aes(x = Station_Depth_m, y = AvgDens)) + 
  geom_point(mapping = aes(color = Year_), shape = 1) + 
  geom_smooth(method = "lm", se = F)

ggplot(data = GLNPO, mapping = aes(x = AvgBmss_mg, y = AvgDens)) + 
  geom_point(mapping = aes(color = Year_), shape = 1) + 
  geom_smooth(method = "lm", se = F)

ggplot(data = GLNPO, mapping = aes(x = AvgBmss_mg, y = AvgDens)) + 
  geom_point(mapping = aes(color = Season), shape = 1) + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~Year_)

ggplot(data = GLNPO, mapping = aes(x = AvgBmss_mg, y = AvgDens)) + 
  geom_point(mapping = aes(color = Season), shape = 1) + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~Season)

ggplot(data = GLNPO) + 
  geom_point(mapping = aes(x = Year_, y = AvgDens, color = Season, size = AvgBmss_mg), shape = 9)

ggplot(data = GLNPO) + geom_point(mapping = aes(x = Year_, y = AvgDens, color = Season, size = AvgBmss_mg), shape=1) + 
  geom_smooth(mapping = aes(x = Year_, y = AvgDens)) + 
  facet_wrap(~Season)

ggplot(data = GLNPO) + 
  geom_point(mapping = aes(x = Year_, y = AvgDens, color = Season)) + 
  geom_smooth(mapping = aes(x = Year_, y = AvgDens)) +
  facet_wrap(~Season)

ggplot(data = GLNPO) + 
  geom_point(mapping = aes(x = Year_, y = AvgBmss_mg, color = Season)) + 
  geom_smooth(mapping = aes(x = Year_, y = AvgBmss_mg)) +
  facet_wrap(~Season)

ggplot(GLNPO, aes(x = Year_, y = AvgDens, color = Season, shape = Season)) + 
  geom_point() + 
  geom_smooth(aes(linetype = Season)) +
  facet_wrap(~Season)

Dens <- GLNPO$AvgDens
Year <- GLNPO$Year_
DensGAM <- gam(Dens ~ s(Year))
plot(DensGAM)
summary(DensGAM)

plot(DensGAM, bg="green", col="#336666", xlab = "Year", ylab = "Average Density", main = "Mysis Density in Lake Michigan 2006-2021 Using GLNPO Data")

Bmss <- GLNPO$AvgBmss_mg
BmssGAM <- gam(Bmss ~ s(Year))
summary(BmssGAM)

plot(BmssGAM, col="blue", xlab = "Year", ylab = "Average Biomass", main = "Mysis Biomass in Lake Michigan 2006-2021 Using GLNPO Data")

require(smooth)
library(tidyverse)
library(ggpubr)

Depth <- GLNPO$Station_Depth_m
Season <- GLNPO$Season

## Linear regression

gg.lm <- ggplot(GLNPO,aes(x = Year_,y = AvgDens, col=Season)) + geom_point() + geom_smooth(method = 'lm') +
  facet_wrap(~Season)
print(gg.lm)

gg.lm <- ggplot(GLNPO,aes(x = Year_,y = AvgDens)) + geom_point(mapping = aes(size = Depth), shape = 1, color = "darkcyan") + geom_smooth(method = 'lm') + 
  facet_wrap(~Season)
print(gg.lm)

## Quadratic regression

gg.qua <- ggplot(GLNPO,aes(x = Year_, y=AvgDens, col=Season)) + geom_point() + geom_smooth(method = 'lm', formula = y~poly(x,2)) + facet_grid(~Season)
print(gg.qua)

## Cubic regression

gg.cub <- ggplot(GLNPO,aes(x = Year_, y = AvgDens, col=Season)) + geom_point() + geom_smooth(method='lm', formula = y ~ poly(x,2)) + facet_wrap(~Season)
print(gg.cub)

## Loess

gg.lo <- ggplot(GLNPO, aes(x = Year_, y=AvgDens, col=Season)) + geom_point() + geom_smooth(method = 'loess') + facet_wrap(~Season)
print(gg.lo)

## Natural splines

gg.ns2 <- ggplot(GLNPO, aes(x = Year_, y = AvgDens, col=Season)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ splines::ns(x,3)) +
  facet_wrap(~Season)
print(gg.ns2)

gg.ns3 <- ggplot(GLNPO, aes(x = Year_, y = AvgDens, col = Season)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ splines::ns(x,3)) +
  facet_wrap(~Season)
print(gg.ns3)

gg.bs2 <- ggplot(GLNPO, aes(x = Year_, y = AvgDens, col=Season)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ splines::bs(x,2)) +
  facet_wrap(~Season)
print(gg.bs2)

gg.ns30 <- ggplot(GLNPO, aes(x = Year_, y = AvgDens, col=Season)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ splines::ns(x,30)) +
  facet_wrap(~Season)
plot(gg.ns30)

gg.bs30 <- ggplot(GLNPO, aes(x = Year_, y = AvgDens, col=Season)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ splines::bs(x,30)) +
  facet_wrap(~Season)
plot(gg.bs30)

## Significance of Station Depth

ggplot(data = GLNPO, mapping = aes(x = Station_Depth_m, y = AvgDens)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

summary(lm(AvgDens~Station_Depth_m, data=GLNPO))

## Summary shows that for every 1 meter deeper, average density at a station goes up by 0.95 with a small p-value

ggplot(data = GLNPO, mapping = aes(x = AvgOfLatitude, y = AvgDens)) + 
  geom_point() + geom_smooth(method = "lm", se = F)

## Not as much of a signal for location (avg latitude) but shows slight increase with higher lat.

