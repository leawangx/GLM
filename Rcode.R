data<-read.csv("C:/Users/69544/Desktop/MGEC11/Report/FishMN.csv")

area<-data$area.hectares 
depth<-data$max_depth_m 
gdd<-data$mean.gdd 
proportion_disturbed<-data$proportion_disturbed_ws 
littoral<-data$littoral.zone 
secchi<-data$secchi.m
blackbullhead<-data$blackbullhead

#a)
par(mfrow=c(2,3))
boxplot(area~blackbullhead)
boxplot(depth~blackbullhead)
boxplot(gdd~blackbullhead)
boxplot(proportion_disturbed~blackbullhead)
boxplot(littoral~blackbullhead)
boxplot(secchi~blackbullhead)

model1 <- glm(blackbullhead~area + depth + gdd + proportion_disturbed 
                 + littoral + secchi,family=binomial,data=data)
summary(model1)
# p-value of max_depth_m > 0.05, depth is not significant, so need to remove it 
library(car)
vif(model1)

model2 <- glm(blackbullhead~area + gdd + proportion_disturbed + littoral + secchi,family=binomial)
summary(model2)
# littoral.zone is not significant, remove it
model3 <- glm(blackbullhead~area+gdd + proportion_disturbed + secchi,family=binomial)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
# normal QQ plot not good, try log transformation
model4<-glm(blackbullhead~log(area+gdd+proportion_disturbed+secchi),family=binomial)
plot(model4)
summary(model4)




#c)
sort(proportion_disturbed,TRUE)[121]
proportion1<-proportion_disturbed/(1+(proportion_disturbed>0.7546438))
n1<--14.73+0.001332*area+0.006205*gdd+2.763*proportion1+0.8461*littoral-0.237*secchi
p1<-exp(n1)/(1+exp(n1))
sum(p1)

sort(secchi,decreasing = FALSE)[121]
secchi1<-secchi*(1+(secchi<0.8))
n2<--14.73+0.001332*area+0.006205*gdd+2.763*proportion_disturbed+0.8461*littoral-0.237*secchi1
p2<-exp(n2)/(1+exp(n2))
sum(p2)

