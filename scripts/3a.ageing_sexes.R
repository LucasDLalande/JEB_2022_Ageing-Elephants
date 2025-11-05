### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## 3a. Testing for sex-differences in ageing trajectories
##
## Lalande Lucas D., Lummaa Virpi, Aung Htoo H., Htut Win, Nyein Kyaw U., Berger Verane, Briga Michael  
## February 2022
##
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

rm(list=ls())


{
  data <- read.csv("sex-specific-dataset.csv", h=T, sep=";", dec=".")
  
  # Dataset formatting 
  str(data)
  
  data$id <- as.factor(data$id) # elephants id
  data$sex <- as.factor(data$sex)
  data$cw <- as.factor(data$cw) # elephants origin (wild-caught, captive-born, unknown
  data$alive <- as.factor(data$alive) # whether elephant still alive at the time of the analyses (yes (Y), no (N))
  data$season_measure <- as.factor(data$season_measure) # season at the moment of the measurement (cold, hot, monsun)
  data$Township <- as.factor(data$Township) # township elephant was last seen
  data$division <- as.factor(data$division) # A region regroups several Townships
  data$measure <- as.factor(data$measure) # whether body masses are measured or estimated
  data$year_birth <- as.factor(data$year_birth)
  data$terminal <- as.factor(data$terminal) # whether the elephant died within the year of the measurement (no (0), yes (1))
  
  # calculate mean age
  data$mean_age <- ave(data$age, data$id, FUN=mean)
  data$mean_age_s <- scale(data$mean_age)
  
  # calculate age at last measurement
  data$age_last <- ave(data$age, data$id, FUN = function(x) max(as.numeric(x)))
  data$age_last_s <- scale(data$age_last)
  
  # calculate delta age
  data$delta_age <- data$age - data$mean_age
  data$delta_age_s <- scale(data$delta_age)
  
}


#Sex specific differences in mean weight
#Linear models
msex <- lmer(log(bm) ~ sex + (1|id) + (1|Township), data=data, control=lmerControl("bobyqa"))
m0 <- lmer(log(bm) ~ 1 + (1|id) + (1|Township), data=data, control=lmerControl("bobyqa"))
AICc(msex, m0)
AICc(msex)
AICc(m0)
AICc(msex)-AICc(m0)
# -122.6 AICc (l. 197)

#Approach 1: gamm
#Information available at: https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/
m0 <- gamm(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, by=sex, bs="cr", k=15) + sex,
          random=list(id=~1|id),
          data=data)
m <- gamm(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, by=sex, bs="cr", k=15) + sex,
            random=list(id=~1|id, Township=~1|Township),
            data=data)
summary(m$gam)
plot(m$gam, shade=T, pages=1, scale=0)

m2 <- gamm(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, bs="cr", k=15) + sex,
          random=list(id=~1|id, Township=~1|Township),
          data=data)
AICc(m2, m, m0) #df, weights
AICc(m) # -8809.1 (df = 17.9, w = 1)
AICc(m2) # -8743.4 (df = 10.3, w = 0)
AICc(m2)-AICc(m)
# 65.7 AICc (l. 202 + Table S2)
am <- model.avg(m2, m)
Weights(am)
summary(m$gam) #count df
summary(m2$gam) #count df


#Linear models
m3 <- lmer(log(bm) ~ terminal +  age_last_s + I(age_last_s^2) + delta_age_s + sex + delta_age_s*sex + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))
summary(m3)
r.squaredGLMM(m3)

m4 <- lmer(log(bm) ~ terminal +  age_last_s + I(age_last_s^2) + delta_age_s + sex + (1|id) + (1|Township),
           data=data, control=lmerControl("bobyqa"))
AICc(m3, m4)
AICc(m3)
AICc(m4)
AICc(m3)-AICc(m4)
am <- model.avg(m3, m4)
Weights(am)
