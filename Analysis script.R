library(dplyr)
library(miceadds)
library(stringr)

####Data preparation----
#Open the Excel file containing the data and save each tab as an individual csv: Baseline data, Evaluation data, and Follow-up data
#Load the three files


Baseline=read.csv("...../Baseline data.csv")
Evaluation=read.csv("...../Evaluation data.csv")
FollowUp=read.csv("...../Follow-up data.csv")



####BASELINE: Summary statistics for SI table----
Baseline1=subset(Baseline,Baseline$Water.supply.facility=="WSF1 (manual chlorination)")
Baseline2=subset(Baseline,Baseline$Water.supply.facility=="WSF2 (Water Mission erosion chlorinator)")

#>>>WSF1: FCR, all----

summary(Baseline1$FCR..mg.L.)

round(prop.table(table(Baseline1$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Baseline1$FCR..mg.L.>=0.1 & Baseline1$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Baseline1$FCR..mg.L.>=0.2 & Baseline1$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Baseline1$FCR..mg.L.>2)),2)*100

#>>>WSF1: FCR, near tap vs far taps----

Baseline1_near=subset(Baseline1, Baseline1$Standpipe.type=="Near")
Baseline1_far=subset(Baseline1, Baseline1$Standpipe.type=="Far")

wilcox.test(Baseline1_near$FCR..mg.L., Baseline1_far$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=0.007
wilcox.test(Baseline1_near$FCR..mg.L., Baseline1_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p=0.003


summary(Baseline1_near$FCR..mg.L.)
round(prop.table(table(Baseline1_near$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Baseline1_near$FCR..mg.L.>=0.1 & Baseline1_near$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Baseline1_near$FCR..mg.L.>=0.2 & Baseline1_near$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Baseline1_near$FCR..mg.L.>2)),2)*100

summary(Baseline1_far$FCR..mg.L.)
round(prop.table(table(Baseline1_far$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Baseline1_far$FCR..mg.L.>=0.1 & Baseline1_far$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Baseline1_far$FCR..mg.L.>=0.2 & Baseline1_far$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Baseline1_far$FCR..mg.L.>2)),2)*100

#>>>WSF1: Physico-chemical parameters----
#pH
summary(Baseline1$pH)
summary(Baseline1_near$pH)
summary(Baseline1_far$pH)

#Temperature
summary(Baseline1$Temperature..Celcius.)
summary(Baseline1_near$Temperature..Celcius.)
summary(Baseline1_far$Temperature..Celcius.)

#Conductivity
summary(Baseline1$Conductivity..µS.cm.)
summary(Baseline1_near$Conductivity..µS.cm.)
summary(Baseline1_far$Conductivity..µS.cm.)

#Turbidity
summary(subset(Baseline1, Baseline1$Turbidity..NTU. != "999" & Baseline1$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Baseline1_near, Baseline1_near$Turbidity..NTU. != "999" & Baseline1_near$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Baseline1_far, Baseline1_far$Turbidity..NTU. != "999" & Baseline1_far$Turbidity..NTU. != "99")$Turbidity..NTU.)

#>>>WSF2: FCR, all----

summary(Baseline2$FCR..mg.L.)

round(prop.table(table(Baseline2$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Baseline2$FCR..mg.L.>=0.1 & Baseline2$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Baseline2$FCR..mg.L.>=0.2 & Baseline2$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Baseline2$FCR..mg.L.>2)),2)*100

#>>>WSF2: FCR, near tap vs far taps----

Baseline2_near=subset(Baseline2, Baseline2$Standpipe.type=="Near")
Baseline2_far=subset(Baseline2, Baseline2$Standpipe.type=="Far")

wilcox.test(Baseline2_near$FCR..mg.L., Baseline2_far$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=0.001
wilcox.test(Baseline2_near$FCR..mg.L., Baseline2_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p<0.001

summary(Baseline2_near$FCR..mg.L.)
round(prop.table(table(Baseline2_near$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Baseline2_near$FCR..mg.L.>=0.1 & Baseline2_near$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Baseline2_near$FCR..mg.L.>=0.2 & Baseline2_near$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Baseline2_near$FCR..mg.L.>2)),2)*100

summary(Baseline2_far$FCR..mg.L.)
round(prop.table(table(Baseline2_far$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Baseline2_far$FCR..mg.L.>=0.1 & Baseline2_far$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Baseline2_far$FCR..mg.L.>=0.2 & Baseline2_far$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Baseline2_far$FCR..mg.L.>2)),2)*100

#>>>WSF2: Physico-chemical parameters----

#pH
summary(Baseline2$pH)
summary(Baseline2_near$pH)
summary(Baseline2_far$pH)

#Temperature
summary(Baseline2$Temperature..Celcius.)
summary(Baseline2_near$Temperature..Celcius.)
summary(Baseline2_far$Temperature..Celcius.)

#Conductivity
summary(Baseline2$Conductivity..µS.cm.)
summary(Baseline2_near$Conductivity..µS.cm.)
summary(Baseline2_far$Conductivity..µS.cm.)

#Turbidity
summary(subset(Baseline2, Baseline2$Turbidity..NTU. != "999" & Baseline2$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Baseline2_near, Baseline2_near$Turbidity..NTU. != "999" & Baseline2_near$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Baseline2_far, Baseline2_far$Turbidity..NTU. != "999" & Baseline2_far$Turbidity..NTU. != "99")$Turbidity..NTU.)





#### EVALUATION: Summary statistics for SI table----
Evaluation1=subset(Evaluation,Evaluation$Water.supply.facility=="WSF1 (manual chlorination)")
Evaluation2=subset(Evaluation,Evaluation$Water.supply.facility=="WSF2 (Water Mission erosion chlorinator)")

#>>>WSF1: FCR, all----

summary(Evaluation1$FCR..mg.L.)

round(prop.table(table(Evaluation1$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Evaluation1$FCR..mg.L.>=0.1 & Evaluation1$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Evaluation1$FCR..mg.L.>=0.2 & Evaluation1$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Evaluation1$FCR..mg.L.>2)),2)*100

#>>>WSF1: FCR, near tap vs far taps----

Evaluation1_near=subset(Evaluation1, Evaluation1$Standpipe.type=="Near")
Evaluation1_far=subset(Evaluation1, Evaluation1$Standpipe.type=="Far")

wilcox.test(Evaluation1_near$FCR..mg.L., Evaluation1_far$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=0.007
wilcox.test(Evaluation1_near$FCR..mg.L., Evaluation1_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p=0.003


summary(Evaluation1_near$FCR..mg.L.)
round(prop.table(table(Evaluation1_near$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Evaluation1_near$FCR..mg.L.>=0.1 & Evaluation1_near$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Evaluation1_near$FCR..mg.L.>=0.2 & Evaluation1_near$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Evaluation1_near$FCR..mg.L.>2)),2)*100

summary(Evaluation1_far$FCR..mg.L.)
round(prop.table(table(Evaluation1_far$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Evaluation1_far$FCR..mg.L.>=0.1 & Evaluation1_far$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Evaluation1_far$FCR..mg.L.>=0.2 & Evaluation1_far$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Evaluation1_far$FCR..mg.L.>2)),2)*100

#>>>WSF1: Physico-chemical parameters----
#pH
summary(Evaluation1$pH)
summary(Evaluation1_near$pH)
summary(Evaluation1_far$pH)

#Temperature
summary(Evaluation1$Temperature..Celcius.)
summary(Evaluation1_near$Temperature..Celcius.)
summary(Evaluation1_far$Temperature..Celcius.)

#Conductivity
summary(Evaluation1$Conductivity..µS.cm.)
summary(Evaluation1_near$Conductivity..µS.cm.)
summary(Evaluation1_far$Conductivity..µS.cm.)

#Turbidity
summary(subset(Evaluation1, Evaluation1$Turbidity..NTU. != "999" & Evaluation1$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Evaluation1_near, Evaluation1_near$Turbidity..NTU. != "999" & Evaluation1_near$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Evaluation1_far, Evaluation1_far$Turbidity..NTU. != "999" & Evaluation1_far$Turbidity..NTU. != "99")$Turbidity..NTU.)

#>>>WSF2: FCR, all----

summary(Evaluation2$FCR..mg.L.)

round(prop.table(table(Evaluation2$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Evaluation2$FCR..mg.L.>=0.1 & Evaluation2$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Evaluation2$FCR..mg.L.>=0.2 & Evaluation2$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Evaluation2$FCR..mg.L.>2)),2)*100

#>>>WSF2: FCR, near tap vs far taps----

Evaluation2_near=subset(Evaluation2, Evaluation2$Standpipe.type=="Near")
Evaluation2_far=subset(Evaluation2, Evaluation2$Standpipe.type=="Far")

wilcox.test(Evaluation2_near$FCR..mg.L., Evaluation2_far$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=0.001
wilcox.test(Evaluation2_near$FCR..mg.L., Evaluation2_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p<0.001

summary(Evaluation2_near$FCR..mg.L.)
round(prop.table(table(Evaluation2_near$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Evaluation2_near$FCR..mg.L.>=0.1 & Evaluation2_near$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Evaluation2_near$FCR..mg.L.>=0.2 & Evaluation2_near$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Evaluation2_near$FCR..mg.L.>2)),2)*100

summary(Evaluation2_far$FCR..mg.L.)
round(prop.table(table(Evaluation2_far$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(Evaluation2_far$FCR..mg.L.>=0.1 & Evaluation2_far$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(Evaluation2_far$FCR..mg.L.>=0.2 & Evaluation2_far$FCR..mg.L.<=2)),2)*100
round(prop.table(table(Evaluation2_far$FCR..mg.L.>2)),2)*100

#>>>WSF2: Physico-chemical parameters----

#pH
summary(Evaluation2$pH)
summary(Evaluation2_near$pH)
summary(Evaluation2_far$pH)

#Temperature
summary(Evaluation2$Temperature..Celcius.)
summary(Evaluation2_near$Temperature..Celcius.)
summary(Evaluation2_far$Temperature..Celcius.)

#Conductivity
summary(Evaluation2$Conductivity..µS.cm.)
summary(Evaluation2_near$Conductivity..µS.cm.)
summary(Evaluation2_far$Conductivity..µS.cm.)

#Turbidity
summary(subset(Evaluation2, Evaluation2$Turbidity..NTU. != "999" & Evaluation2$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Evaluation2_near, Evaluation2_near$Turbidity..NTU. != "999" & Evaluation2_near$Turbidity..NTU. != "99")$Turbidity..NTU.)
summary(subset(Evaluation2_far, Evaluation2_far$Turbidity..NTU. != "999" & Evaluation2_far$Turbidity..NTU. != "99")$Turbidity..NTU.)




#### EVALUATION: Near vs. Far taps----

#>>>WSF1: Statistical test of difference----

hist(Evaluation1$FCR..mg.L.) #Log normal

wilcox.test(Evaluation1_near$FCR..mg.L., Evaluation1_far$FCR..mg.L., paired = FALSE, alternative = "two.sided")
wilcox.test(Evaluation1_near$FCR..mg.L., Evaluation1_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p<0.001

#>>>WSF1: Analysis by day----
DailyMeans_WSF1=Evaluation1 %>% group_by(Date..mm.dd.year.) %>%
  summarize(n_near=sum(Standpipe.type=="Near"),
            mean_near=mean(FCR..mg.L.[Standpipe.type=="Near"]),
            n_far=sum(Standpipe.type=="Far"),
            mean_far=mean(FCR..mg.L.[Standpipe.type=="Far"]),
            mean_diff=mean(FCR..mg.L.[Standpipe.type=="Near"])-mean(FCR..mg.L.[Standpipe.type=="Far"]))

summary(DailyMeans_WSF1$mean_diff)
prop.table(table(DailyMeans_WSF1$mean_diff>0)) #70%, i.e. average FCR levels at near taps exceeded far taps on 70% of days.
prop.table(table(DailyMeans_WSF1$mean_diff>=0.1)) #28%, i.e. average FCR levels at near taps were over 0.1 mg/L above far taps on 28% of days.

#>>>WSF2: Statistical test of difference----

hist(Evaluation2$FCR..mg.L.) #Log normal

wilcox.test(Evaluation2_near$FCR..mg.L., Evaluation2_far$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p =0.06
wilcox.test(Evaluation2_near$FCR..mg.L., Evaluation2_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p=0.03


#>>>WSF2: Analysis by day----

DailyMeans_WSF2=Evaluation2 %>% group_by(Date..mm.dd.year.) %>%
  summarize(n_near=sum(Standpipe.type=="Near"),
            mean_near=mean(FCR..mg.L.[Standpipe.type=="Near"]),
            n_far=sum(Standpipe.type=="Far"),
            mean_far=mean(FCR..mg.L.[Standpipe.type=="Far"]),
            mean_diff=mean(FCR..mg.L.[Standpipe.type=="Near"])-mean(FCR..mg.L.[Standpipe.type=="Far"]))

summary(DailyMeans_WSF2$mean_diff)
prop.table(table(DailyMeans_WSF2$mean_diff>0)) #69%, i.e. average FCR levels at near taps exceeded far taps on 69% of days.
prop.table(table(DailyMeans_WSF2$mean_diff>=0.1)) #33%, i.e. average FCR levels at near taps were over 0.1 mg/L above far taps on 33% of days.






#### EVALUATION: WSF1 vs. WSF2 ----
wilcox.test(Evaluation1$FCR..mg.L., Evaluation2$FCR..mg.L., paired = FALSE, alternative = "two.sided")
wilcox.test(Evaluation1$FCR..mg.L., Evaluation2$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001



#### EVALUATION: Mean FCR vs. Distance----
# WSF1
Mean_Evaluation1=Evaluation1 %>% group_by(Sampling.point) %>%
  summarize(n=n(),
            mean_FCR=mean(FCR..mg.L.),
            median_FCR=median(FCR..mg.L.),
            distance=mean(Distance.from.storage.tank..m.))

plot(Mean_Evaluation1$distance,Mean_Evaluation1$mean_FCR)
plot(Mean_Evaluation1$distance,Mean_Evaluation1$median_FCR)

# WSF2
Mean_Evaluation2=Evaluation2 %>% group_by(Sampling.point) %>%
  summarize(n=n(),
            mean_FCR=mean(FCR..mg.L.),
            median_FCR=median(FCR..mg.L.),
            distance=mean(Distance.from.storage.tank..m.))

plot(Mean_Evaluation2$distance,Mean_Evaluation2$mean_FCR)
plot(Mean_Evaluation2$distance,Mean_Evaluation2$median_FCR)


#### EVALUATION: Regressions----
plot(Evaluation$FCR..mg.L.,Evaluation$pH) #no trend
plot(Evaluation1$FCR..mg.L.,Evaluation1$pH) #no trend
plot(Evaluation2$FCR..mg.L.,Evaluation2$pH) #no trend

plot(Evaluation$FCR..mg.L.,Evaluation$Temperature..Celcius.) #upward trend
plot(Evaluation$FCR..mg.L.,Evaluation$Conductivity..µS.cm.) #no trend
plot(Evaluation$FCR..mg.L.,Evaluation$Turbidity..NTU.) #no trend

Evaluation1$minutes=as.numeric(word(Evaluation1$Time..24hrs., 1, sep=":"))*60+as.numeric(word(Evaluation1$Time..24hrs., 2, sep=":"))
Evaluation2$minutes=as.numeric(word(Evaluation2$Time..24hrs., 1, sep=":"))*60+as.numeric(word(Evaluation2$Time..24hrs., 2, sep=":"))
plot(Evaluation1$minutes,Evaluation1$FCR..mg.L.) #no trend
plot(Evaluation2$minutes,Evaluation2$FCR..mg.L.) #no trend

#>>>Bivariate regressions----

summary(lm(FCR..mg.L.~pH,data=Evaluation1))
summary(lm(FCR..mg.L.~pH,data=Evaluation2))

summary(lm(FCR..mg.L.~Temperature..Celcius.,data=Evaluation1))
summary(lm(FCR..mg.L.~Temperature..Celcius.,data=Evaluation2))

summary(lm(FCR..mg.L.~Conductivity..µS.cm.,data=Evaluation1))
summary(lm(FCR..mg.L.~Conductivity..µS.cm.,data=Evaluation2))

summary(lm(FCR..mg.L.~Turbidity..NTU.,data=Evaluation1))
summary(lm(FCR..mg.L.~Turbidity..NTU.,data=Evaluation2))

summary(lm(FCR..mg.L.~Distance.from.storage.tank..m.,data=Evaluation1))
summary(lm(FCR..mg.L.~Distance.from.storage.tank..m.,data=Evaluation2))

summary(lm(FCR..mg.L.~minutes,data=Evaluation1))
summary(lm(FCR..mg.L.~minutes,data=Evaluation2))


#>>>Multi-variable regressions----

summary(lm.cluster(FCR..mg.L.~pH + Temperature..Celcius. + Conductivity..µS.cm. + Turbidity..NTU.,data=Evaluation1,cluster="Sampling.point"))
summary(lm.cluster(FCR..mg.L.~pH + Temperature..Celcius. + Conductivity..µS.cm. + Turbidity..NTU.,data=Evaluation2,cluster="Sampling.point"))

summary(lm.cluster(FCR..mg.L.~pH + Temperature..Celcius. + Conductivity..µS.cm. + Turbidity..NTU. + Distance.from.storage.tank..m.,data=Evaluation1,cluster="Sampling.point"))
summary(lm.cluster(FCR..mg.L.~pH + Temperature..Celcius. + Conductivity..µS.cm. + Turbidity..NTU. + Distance.from.storage.tank..m.,data=Evaluation2,cluster="Sampling.point"))

summary(lm.cluster(FCR..mg.L.~pH + Temperature..Celcius. + Conductivity..µS.cm. + Turbidity..NTU. + Distance.from.storage.tank..m. + minutes,data=Evaluation1,cluster="Sampling.point"))
summary(lm.cluster(FCR..mg.L.~pH + Temperature..Celcius. + Conductivity..µS.cm. + Turbidity..NTU. + Distance.from.storage.tank..m. + minutes,data=Evaluation2,cluster="Sampling.point"))


           

#### BASELINE vs. EVALUATION----

#>>>WSF1----

#All sampling points
wilcox.test(Baseline1$FCR..mg.L., Evaluation1$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001

#By sampling point
Baseline1_GasStation=subset(Baseline1, Baseline1$Sampling.point=="AGL_Gas_Station")
Baseline1_SP18=subset(Baseline1, Baseline1$Sampling.point=="SP18")
Baseline1_HH2=subset(Baseline1, Baseline1$Sampling.point=="HH2")

Evaluation1_GasStation=subset(Evaluation1, Evaluation1$Sampling.point=="AGL_Gas_Station")
Evaluation1_SP18=subset(Evaluation1, Evaluation1$Sampling.point=="SP18")
Evaluation1_HH2=subset(Evaluation1, Evaluation1$Sampling.point=="HH2")

wilcox.test(Baseline1_GasStation$FCR..mg.L., Evaluation1_GasStation$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001
wilcox.test(Baseline1_SP18$FCR..mg.L., Evaluation1_SP18$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001
wilcox.test(Baseline1_HH2$FCR..mg.L., Evaluation1_HH2$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001

#>>>WSF2----

#All sampling points
wilcox.test(Baseline2$FCR..mg.L., Evaluation2$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001

#By sampling point
Baseline2_SP9=subset(Baseline2, Baseline2$Sampling.point=="SP9")
Baseline2_SP7=subset(Baseline2, Baseline2$Sampling.point=="SP7")
Baseline2_SP1=subset(Baseline2, Baseline2$Sampling.point=="SP1")

Evaluation2_SP9=subset(Evaluation2, Evaluation2$Sampling.point=="SP9")
Evaluation2_SP7=subset(Evaluation2, Evaluation2$Sampling.point=="SP7")
Evaluation2_SP1=subset(Evaluation2, Evaluation2$Sampling.point=="SP1")

wilcox.test(Baseline2_SP9$FCR..mg.L., Evaluation2_SP9$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001
wilcox.test(Baseline2_SP7$FCR..mg.L., Evaluation2_SP7$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001
wilcox.test(Baseline2_SP1$FCR..mg.L., Evaluation2_SP1$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001



####FOLLOW-UP----

FollowUp1=subset(FollowUp,FollowUp$Water.supply.facility=="WSF1 (manual chlorination)")
FollowUp2=subset(FollowUp,FollowUp$Water.supply.facility=="WSF2 (Water Mission erosion chlorinator)")

#>>>WSF1: FCR, all----

summary(FollowUp1$FCR..mg.L.)

round(prop.table(table(FollowUp1$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(FollowUp1$FCR..mg.L.>=0.1 & FollowUp1$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(FollowUp1$FCR..mg.L.>=0.2 & FollowUp1$FCR..mg.L.<=2)),2)*100
round(prop.table(table(FollowUp1$FCR..mg.L.>2)),2)*100

#>>>WSF1: FCR, near tap vs far taps----

FollowUp1_near=subset(FollowUp1, FollowUp1$Standpipe.type=="Near")
FollowUp1_far=subset(FollowUp1, FollowUp1$Standpipe.type=="Far")

wilcox.test(FollowUp1_near$FCR..mg.L., FollowUp1_far$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=0.007
wilcox.test(FollowUp1_near$FCR..mg.L., FollowUp1_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p=0.003


summary(FollowUp1_near$FCR..mg.L.)
round(prop.table(table(FollowUp1_near$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(FollowUp1_near$FCR..mg.L.>=0.1 & FollowUp1_near$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(FollowUp1_near$FCR..mg.L.>=0.2 & FollowUp1_near$FCR..mg.L.<=2)),2)*100
round(prop.table(table(FollowUp1_near$FCR..mg.L.>2)),2)*100

summary(FollowUp1_far$FCR..mg.L.)
round(prop.table(table(FollowUp1_far$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(FollowUp1_far$FCR..mg.L.>=0.1 & FollowUp1_far$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(FollowUp1_far$FCR..mg.L.>=0.2 & FollowUp1_far$FCR..mg.L.<=2)),2)*100
round(prop.table(table(FollowUp1_far$FCR..mg.L.>2)),2)*100


#>>>WSF2: FCR, all----

summary(FollowUp2$FCR..mg.L.)

round(prop.table(table(FollowUp2$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(FollowUp2$FCR..mg.L.>=0.1 & FollowUp2$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(FollowUp2$FCR..mg.L.>=0.2 & FollowUp2$FCR..mg.L.<=2)),2)*100
round(prop.table(table(FollowUp2$FCR..mg.L.>2)),2)*100

#>>>WSF2: FCR, near tap vs far taps----

FollowUp2_near=subset(FollowUp2, FollowUp2$Standpipe.type=="Near")
FollowUp2_far=subset(FollowUp2, FollowUp2$Standpipe.type=="Far")

wilcox.test(FollowUp2_near$FCR..mg.L., FollowUp2_far$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=0.001
wilcox.test(FollowUp2_near$FCR..mg.L., FollowUp2_far$FCR..mg.L., paired = FALSE, alternative = "greater") #p<0.001

summary(FollowUp2_near$FCR..mg.L.)
round(prop.table(table(FollowUp2_near$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(FollowUp2_near$FCR..mg.L.>=0.1 & FollowUp2_near$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(FollowUp2_near$FCR..mg.L.>=0.2 & FollowUp2_near$FCR..mg.L.<=2)),2)*100
round(prop.table(table(FollowUp2_near$FCR..mg.L.>2)),2)*100

summary(FollowUp2_far$FCR..mg.L.)
round(prop.table(table(FollowUp2_far$FCR..mg.L.<0.1)),2)*100
round(prop.table(table(FollowUp2_far$FCR..mg.L.>=0.1 & FollowUp2_far$FCR..mg.L.<0.2)),2)*100
round(prop.table(table(FollowUp2_far$FCR..mg.L.>=0.2 & FollowUp2_far$FCR..mg.L.<=2)),2)*100
round(prop.table(table(FollowUp2_far$FCR..mg.L.>2)),2)*100




####FOLLOW-UP vs. EVALUATION----
wilcox.test(Evaluation1$FCR..mg.L., FollowUp1$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=0.05
wilcox.test(Evaluation1$FCR..mg.L., FollowUp1$FCR..mg.L., paired = FALSE, alternative = "greater") #p=0.03

wilcox.test(Evaluation2$FCR..mg.L., FollowUp2$FCR..mg.L., paired = FALSE, alternative = "two.sided") #p=<0.001
wilcox.test(Evaluation2$FCR..mg.L., FollowUp2$FCR..mg.L., paired = FALSE, alternative = "less") #p<0.001
