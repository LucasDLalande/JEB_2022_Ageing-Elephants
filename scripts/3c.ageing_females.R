### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## 3c. Testing ageing trajectories in females
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


data <- data[data$sex=="A",] # A corresponds to 'females' in the anonymised dataset
}

####################
### To run analyses with only individuals with 3 or more observations, run the following code before model comparison ###
# a <- data.frame(table(factor(data$id)))
# a <- a[a$Freq>=3,]

# data <- merge(data, a, by.x="id", by.y="Var1", all=F)
# unique(factor(data$id))

# yielding consistent results
####################

# MODEL COMPARISON ----
# 0) Null model ----
m0 <- lmer(log(bm) ~ (1|id)  + (1|Township),
           data=data, control=lmerControl("bobyqa"))

# 1) Linear ----
m1a <- lmer(log(bm) ~ delta_age_s + age_last_s + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))

m1b <- lmer(log(bm) ~ terminal + delta_age_s + age_last_s + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))
summary(m1b)

# 2) Mean age^2 ----
m2a <- lmer(log(bm) ~ delta_age_s + age_last_s + I(age_last_s^2) + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))

m2b <- lmer(log(bm)~ terminal + delta_age_s + age_last_s + I(age_last_s^2) + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))
AICc(m2b)
unique(factor(data$id))
mean(data$age)
datappeak <- data[data$delta_age_s > 1.4,]
unique(factor(datappeak$id))
datappeak2 <- data[data$age > 48,]
unique(factor(datappeak2$id))

m2b.town <- lmer(log(bm)~ terminal + delta_age_s + age_last_s + I(age_last_s^2) + (1|id),
            data=data, control=lmerControl("bobyqa"))
AICc(m2b.town)
AICc(m2b.town)-AICc(m2b)

# 3) Delta age^2 ----
m3a <- lmer(log(bm) ~ delta_age_s + I(delta_age_s^2) + age_last_s + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))

m3b <- lmer(log(bm) ~ terminal + delta_age_s + I(delta_age_s^2) + age_last_s + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))

# 4) Quadratic ----
m4a <- lmer(log(bm) ~ delta_age_s + I(delta_age_s^2) + age_last_s + I(age_last_s^2) + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))

m4b <- lmer(log(bm) ~ terminal + delta_age_s + I(delta_age_s^2) + age_last_s + I(age_last_s^2) + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))

# 5a) Treshold model----
# Estimate the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  sequence <- seq(3,10,0.1)

  for (i in sequence){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
  
    m5a <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
               data = data, control = lmerControl(optimizer = "bobyqa"))
  
    k<- AICc(m5a)
    thresholds <- rbind(thresholds, k)}
  age <- sequence
  thresholds <- cbind(age-7, "AICc"=thresholds)
  colnames(thresholds) <- c("delta_age","AICc")
  thresholds$pos_delta <- thresholds$delta_age +7
  par(mfrow=c(1,1))
  plot(AICc ~ delta_age, thresholds,
     xlab="delta_age", ylab="AICc")}

# Apply threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 7.7
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m5a <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
                 data = data, control = lmerControl(optimizer = "bobyqa"))}

summary(m5a)}


# 5b) Threshold model + Terminal ----
# Estimate the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
data$delta_age.2 <- data$delta_age_s + 7
data$final_delta_age <- data$delta_age_s + 7
thresholds <- data.frame()
sequence <- seq(3,10,0.1)

for (i in sequence){
  print(paste('Fitting threshold model with breakpoint age', i))
  data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
  data$delta_age.1 [data$delta_age.1 > i] <- i
  data$delta_age.2 [data$delta_age.2 <= i] <- 0
  data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
  
  m5b <-  lmer(log(bm) ~ terminal + age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
               data = data, control = lmerControl(optimizer = "bobyqa"))
  
  k<- AICc(m5b)
  thresholds <- rbind(thresholds, k)}
age <- sequence
thresholds <- cbind(age-7, "AICc"=thresholds)
colnames(thresholds) <- c("delta_age","AICc")
thresholds$pos_delta <- thresholds$delta_age +7
par(mfrow=c(1,1))
plot(AICc ~ delta_age, thresholds,
     xlab="delta_age", ylab="AICc")}

# Estimate the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 7.7
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m5b <-  lmer(log(bm) ~ terminal + age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
                 data = data, control = lmerControl(optimizer = "bobyqa"))}
summary(m5b)}

# 6a) Treshold model - Age_last^2 ----
# Estimate the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
data$delta_age.2 <- data$delta_age_s + 7
data$final_delta_age <- data$delta_age_s + 7
thresholds <- data.frame()
sequence <- seq(3,10,0.1)

for (i in sequence){
  print(paste('Fitting threshold model with breakpoint age', i))
  data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
  data$delta_age.1 [data$delta_age.1 > i] <- i
  data$delta_age.2 [data$delta_age.2 <= i] <- 0
  data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
  
  m6a <-  lmer(log(bm) ~ age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
               data = data, control = lmerControl(optimizer = "bobyqa"))
  
  k<- AICc(m6a)
  thresholds <- rbind(thresholds, k)}
age <- sequence
thresholds <- cbind(age-7, "AICc"=thresholds)
colnames(thresholds) <- c("delta_age","AICc")
thresholds$pos_delta <- thresholds$delta_age +7
par(mfrow=c(1,1))
plot(AICc ~ delta_age, thresholds,
     xlab="delta_age", ylab="AICc")}

# Apply threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 7.7
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m6a <-  lmer(log(bm) ~ age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
                 data = data, control = lmerControl(optimizer = "bobyqa"))}
  
  summary(m6a)}


# 6b) Threshold model - Age_last^2 + Terminal ----
# Estimate the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
data$delta_age.2 <- data$delta_age_s + 7
data$final_delta_age <- data$delta_age_s + 7
thresholds <- data.frame()
sequence <- seq(3,10,0.1)

for (i in sequence){
  print(paste('Fitting threshold model with breakpoint age', i))
  data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
  data$delta_age.1 [data$delta_age.1 > i] <- i
  data$delta_age.2 [data$delta_age.2 <= i] <- 0
  data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
  
  m6b <-  lmer(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
               data = data, control = lmerControl(optimizer = "bobyqa"))
  
  k<- AICc(m6b)
  thresholds <- rbind(thresholds, k)}
age <- sequence
thresholds <- cbind(age-7, "AICc"=thresholds)
colnames(thresholds) <- c("delta_age","AICc")
thresholds$pos_delta <- thresholds$delta_age +7
par(mfrow=c(1,1))
plot(AICc ~ delta_age, thresholds,
     xlab="delta_age", ylab="AICc")}

# Estimate the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 7.7
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m6b <-  lmer(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
                 data = data, control = lmerControl(optimizer = "bobyqa"))}
  summary(m6b)}

# The model selected is the model m2b
AICc(m0, m1a, m1b, m2a, m2b, m3a, m3b, m4a, m4b, m5a, m5b, m6a, m6b)

# We have 3 models within 7 delta AICc ==> model averaging
avgm <- model.avg(m2a, m2b, m6b)
summary(avgm)
confint(avgm)

# 7) GAM
m0gam <- gamm(log(bm) ~ 1,
           random=list(id=~1|id, Township=~1|Township),
           data=data)

m7a <- gamm(log(bm) ~ age_last_s + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)
m7b <- gamm(log(bm) ~ terminal + age_last_s + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)
m8a <- gamm(log(bm) ~ age_last_s + I(age_last_s^2) + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)
m8b <- gamm(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)
AICc(m0gam, m7a, m7b, m8a, m8b)

summary(m0gam$gam)
summary(m7a$gam)
summary(m7b$gam)
summary(m8a$gam)
summary(m8b$gam)
summary(m8b$lme)
r.squaredGLMM(m8b$lme)

# CONFOUNDING VARIABLE ----
# Base model
data$terminal <- relevel(data$terminal, ref="0")
m2b <-  lmer(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + delta_age_s + (1|id) + (1|Township),
             data = data, control = lmerControl(optimizer = "bobyqa"))

# Season
m2b.1 <-  lmer(log(bm) ~ season_measure + terminal + age_last_s + I(age_last_s^2) + delta_age_s + (1|id) + (1|Township),
              data = data, control = lmerControl(optimizer = "bobyqa"))

# Alive status
m2b.2 <-  lmer(log(bm) ~ alive + terminal + age_last_s + I(age_last_s^2) + delta_age_s + (1|id) + (1|Township),
              data = data, control = lmerControl(optimizer = "bobyqa"))

# Birth origin
m2b.3 <-  lmer(log(bm) ~ cw + terminal + age_last_s + I(age_last_s^2) + delta_age_s + (1|id) + (1|Township),
              data = data, control = lmerControl(optimizer = "bobyqa"))

# Measurement
data$measure <- relevel(data$measure, ref="measured")
m2b.4 <-  lmer(log(bm) ~ measure + terminal + age_last_s + I(age_last_s^2) + delta_age_s + (1|id) + (1|Township),
              data = data, control = lmerControl(optimizer = "bobyqa"))

#Model check
AICc(m2b, m2b.1, m2b.2, m2b.3, m2b.4) # We keep the base model

data$terminal <- relevel(data$terminal, ref="0")
summary(m2b)

m2b.0 <- lmer(log(bm)~ terminal + delta_age_s + age_last_s + I(age_last_s^2) 
            + season_measure 
            + alive 
            + cw 
            + measure
            + (1|id) + (1|Township),
            data=data, control=lmerControl("bobyqa"))
options(na.action="na.fail")
d1 <- dredge(m2b.0, evaluate=T, rank="AICc", fixed= ~terminal + delta_age_s + age_last_s + I(age_last_s^2))
avgm <- model.avg(d1, subset= delta < 7)
avgm$coefficients
confint(avgm)

#GAM check
data$terminal <- relevel(data$terminal, ref="0")

m7a <- gamm(log(bm) ~ terminal + age_last_s + I(age_last_s^2)+ s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)
summary(m7a$gam)
AICc(m7a)

m7a.1 <- gamm(log(bm) ~ season_measure + terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, bs="cr", k=15),
              random=list(id=~1|id, Township=~1|Township),
              data=data)

m7a.2 <- gamm(log(bm) ~ alive + terminal + age_last_s + I(age_last_s^2)+ s(delta_age_s, bs="cr", k=15),
              random=list(id=~1|id, Township=~1|Township),
              data=data)

m7a.3 <- gamm(log(bm) ~ cw + terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, bs="cr", k=15),
              random=list(id=~1|id, Township=~1|Township),
              data=data)

data$measure <- relevel(data$measure, ref="measured")
m7a.4 <- gamm(log(bm) ~ measure + terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, bs="cr", k=15),
              random=list(id=~1|id, Township=~1|Township),
              data=data)

AICc(m7a, m7a.1, m7a.2, m7a.3, m7a.4) #some effect of season
summary(m7a.1$gam)

m7a.01 <- uGamm(log(bm) ~ terminal + age_last_s + I(age_last_s^2)+ s(delta_age_s, bs="cr", k=15)
            + season_measure 
            + alive 
            + cw 
            + measure,
            random=list(id=~1|id, Township=~1|Township),
            data=data)
options(na.action="na.fail")
d1 <- dredge(m7a.01, evaluate=T, rank="AICc", fixed=~ terminal + age_last_s + I(age_last_s^2)+ s(delta_age_s, bs="cr", k=15))
avgm <- model.avg(d1, subset= delta < 7)
avgm$coefficients
confint(avgm)

#effect of township: equal good fit as without

m7a.2town <- gamm(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + s(delta_age_s, bs="cr", k=15),
                  random=list(id=~1|id), data=data)
AICc(m7a, m7a.1, m7a.1town, m7a.2town)
AICc(m7a.1) - AICc(m7a.1town)
AICc(m7a) - AICc(m7a.2town)


# CALCULATIONS ----
summary(m2b)
# Weight at minimum age (18 years old)
exp(7.839778 + 0.015610*min(data$delta_age_s)) # 2306.077
exp((7.839778-0.018732*1.96) + (0.015610-0.001305*1.96)*min(data$delta_age_s)) # 2258.365
exp((7.839778+0.018732*1.96) + (0.015610+0.001305*1.96)*min(data$delta_age_s)) # 2354.797

# Terminal decline in females
exp(7.839778) # 2539.641
exp(7.839778-0.070738) # 2366.199
exp(7.839778) - exp(7.839778-0.070738) # 173.4423
(exp(7.839778) - exp(7.839778-0.070738)) / exp(7.839778) # 6.8%

exp(7.839778) # 2539.641
exp(7.839778-(0.070738 - 1.96*0.019684)) # 2459.272
exp(7.839778) - exp(7.839778-(0.070738 - 1.96*0.019684)) # 80.369
(exp(7.839778) - exp(7.839778-(0.070738 - 1.96*0.019684))) / exp(7.839778) # 3.16%

exp(7.839778) # 2539.641
exp(7.839778-(0.070738 + 1.96*0.019684)) # 2276.648
exp(7.839778) - exp(7.839778-(0.070738 + 1.96*0.019684)) # 262.9932
(exp(7.839778) - exp(7.839778-(0.070738 + 1.96*0.019684))) / exp(7.839778) # 10.36%

# effect size
-0.070738/(0.019684*sqrt(2570))
(-0.070738-1.96*0.019684)/(0.019684*sqrt(2570))
(-0.070738+1.96*0.019684)/(0.019684*sqrt(2570))

# Mass change (kg/year)
# kg
(exp(7.839778+0.015610) - exp(7.839778))/4.4647 # 1 delta_age = 4.5 years
(exp(7.839778+(0.015610+1.96*0.001305)) - exp(7.839778))/4.4647
(exp(7.839778+(0.015610-1.96*0.001305)) - exp(7.839778))/4.4647
# %
((exp(7.839778+0.015610) - exp(7.839778))/exp(7.839778))/4.4647
((exp(7.839778+(0.015610+1.96*0.001305)) - exp(7.839778))/exp(7.839778))/4.4647
((exp(7.839778+(0.015610-1.96*0.001305)) - exp(7.839778))/exp(7.839778))/4.4647
