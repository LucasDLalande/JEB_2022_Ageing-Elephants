### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## 3b. Testing ageing trajectories in males
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
data$cw <- as.factor(data$cw) # elephants origin (wild-caught, captive-born, unknown)
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
  
data <- data[data$sex=="B",] # B corresponds to 'males' in the anonymised dataset
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
m0 <- lmer(log(bm) ~ 1+ (1|id) + (1|Township) ,
           data=data, control=lmerControl("bobyqa"))

# 1) Linear ----
m1a <- lmer(log(bm) ~ delta_age_s + age_last_s + (1|id) + (1|Township) ,
            data=data, control=lmerControl("bobyqa"))

m1b <- lmer(log(bm) ~ terminal + delta_age_s + age_last_s + (1|id)+ (1|Township) ,
            data=data, control=lmerControl("bobyqa"))

# 2) Age_last^2 ----
m2a <- lmer(log(bm) ~ delta_age_s + age_last_s + I(age_last_s^2) + (1|id)+ (1|Township) ,
            data=data, control=lmerControl("bobyqa"))

m2b <- lmer(log(bm) ~ terminal + delta_age_s + age_last_s + I(age_last_s^2) + (1|id)+ (1|Township) ,
            data=data, control=lmerControl("bobyqa"))

# 3) Delta age^2 ----
m3a <- lmer(log(bm) ~ delta_age_s + I(delta_age_s^2) + age_last_s + (1|id)+ (1|Township) ,
            data=data, control=lmerControl("bobyqa"))
summary(m3a)

m3b <- lmer(log(bm) ~ terminal + delta_age_s + I(delta_age_s^2) + age_last_s + (1|id)+ (1|Township) ,
            data=data, control=lmerControl("bobyqa"))

# 4) Quadratic ----
m4a <- lmer(log(bm) ~ delta_age_s + I(delta_age_s^2) + age_last_s + I(age_last_s^2) + (1|id)+ (1|Township) ,
            data=data, control=lmerControl("bobyqa"))

m4b <- lmer(log(bm) ~ terminal + delta_age_s + I(delta_age_s^2) + age_last_s + I(age_last_s^2) + (1|id)+ (1|Township) ,
            data=data, control=lmerControl("bobyqa"))

# 5a) Threshold model ----
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
  
    m5a <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
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


# Apply the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 8.9
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m5a <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
                 data = data, control = lmerControl(optimizer = "bobyqa"))}

  summary(m5a)}
r.squaredGLMM(m5a)

# Effect size of the post-peak decline
(-0.056879)/(0.014129*sqrt(47)) # -0.111
(-0.056879-1.96*0.014129)/(0.014129*sqrt(47)) # -0.165
(-0.056879+1.96*0.014129)/(0.014129*sqrt(47)) # -0.057


# To test the post-peak decline, we need to fix the intercept -- offset model
# a) calculate intercept
int <- 7.71467+0.040851*8.9 # intercept at delta age = 8.9 --> 8.078244

# b) fix this intercept
datappeak <- data[data$delta_age_s > 1.9,]
dataprepeak <- data[data$delta_age_s < 1.9,]
unique(factor(datappeak$id))
m5appeak <-  lmer(I(log(bm) - int) ~ 0 + age_last_s + delta_age_s + (1|id),
                  data = datappeak, control = lmerControl(optimizer = "bobyqa"))
summary(m5appeak)
# delta age = -0.04336 +/- 0.01589

(exp(7.707422)-exp(7.707422-0.04336))/4.3027 # 21.94kg
(exp(7.707422)-exp(7.707422-(0.04336-1.96*0.01589)))/4.3027 # 6.28kg
(exp(7.707422)-exp(7.707422-(0.04336+1.96*0.01589)))/4.3027 # 37.12kg

(((exp(7.707422)-exp(7.707422-0.04336))/exp(7.707422))/4.3027)*100 # 0.99%
(((exp(7.707422)-exp(7.707422-(0.04336-1.96*0.01589)))/exp(7.707422))/4.3027)*100 # 0.28%
(((exp(7.707422)-exp(7.707422-(0.04336+1.96*0.01589)))/exp(7.707422))/4.3027)*100 # 1.67%

# Power analysis - delta_age.2
m5a <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
             data = data, control = lmerControl(optimizer = "bobyqa"))

sim_age <- powerSim(m5a, nsim=1000, fcompare(log(bm) ~ age_last_s + delta_age.1))
sim_age 
# 63% [60, 66]

# 5b) Threshold model + Terminal----
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
  
    m5b <-  lmer(log(bm) ~ terminal + age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
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


# Apply the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 9.2
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m5b <-  lmer(log(bm) ~ terminal + age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
                 data = data, control = lmerControl(optimizer = "bobyqa"))}

  
  summary(m5b)}

data$terminal <- relevel(data$terminal, ref="1")

cohens_d(log(data$bm), data$terminal, ci=0.95)

# 6a) Threshold model - Age last^2 ----
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
  
  m6a <-  lmer(log(bm) ~ age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
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

# Apply the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 8.9
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m6a <-  lmer(log(bm) ~ age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
                 data = data, control = lmerControl(optimizer = "bobyqa"))}
  
  summary(m6a)}

# 6b) Threshold model - Age_last^2 + Terminal----
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
  
  m6b <-  lmer(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
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


# Apply the threshold
{data$delta_age.1 <- data$delta_age_s + 7 # To have delta_age positive
  data$delta_age.2 <- data$delta_age_s + 7
  data$final_delta_age <- data$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 9.2
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    data$delta_age.1 <- data$delta_age.2 <- data$final_delta_age
    data$delta_age.1 [data$delta_age.1 > i] <- i
    data$delta_age.2 [data$delta_age.2 <= i] <- 0
    data$delta_age.2 [data$delta_age.2 > i] <- data$delta_age.2 [data$delta_age.2 > i] - i
    
    m6b <-  lmer(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
                 data = data, control = lmerControl(optimizer = "bobyqa"))}
  
  
  summary(m6b)}

AICc(m0,m1a,m1b,m2a,m2b,m3a,m3b,m4a,m4b,m5a,m5b,m6a,m6b)
# The model selected is the model m5a
# We have 3 models within 7 delta AICc ==> model averaging
avgm <- model.avg(m5a, m5b, m6a)
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
summary(m7a$lme)
r.squaredGLMM(m7a$lme)
summary.gam(m7a$gam)
summary(m7b$gam)
summary(m8a$gam)
summary(m8b$gam)
avgm <- model.avg(m7a, m7b, m8a, m8b)
avgm$coefficients
confint(avgm)


# CONFOUNDING VARIABLES ----
# Base model
m5a <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township) ,
             data = data, control = lmerControl(optimizer = "bobyqa"))
AICc(m5a)
m5a.town <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id),
             data = data, control = lmerControl(optimizer = "bobyqa"))
AICc(m5a.town)
AICc(m5a)-AICc(m5a.town)

# Season
m5a.1 <-  lmer(log(bm) ~ season_measure + age_last_s + delta_age.1 + delta_age.2 + (1|id)+ (1|Township) ,
             data = data, control = lmerControl(optimizer = "bobyqa"))

# Alive status
m5a.2 <-  lmer(log(bm) ~ alive + age_last_s + delta_age.1 + delta_age.2 + (1|id)+ (1|Township) ,
             data = data, control = lmerControl(optimizer = "bobyqa"))

# Birth origin
m5a.3 <-  lmer(log(bm) ~ cw + age_last_s + delta_age.1 + delta_age.2 + (1|id)+ (1|Township) ,
             data = data, control = lmerControl(optimizer = "bobyqa"))

# Measurement
data$measure <- relevel(data$measure, ref="measured")
m5a.4 <-  lmer(log(bm) ~ measure + age_last_s + delta_age.1 + delta_age.2 + (1|id)+ (1|Township) ,
             data = data, control = lmerControl(optimizer = "bobyqa"))
mean(data$bm[data$measure=="measured"]) - mean(data$bm[data$measure=="estimated"])
summary(m5a.4)

#Model check
AICc(m5a, m5a.1, m5a.2, m5a.3, m5a.4) # We keep the base model

m5a.0 <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 
               + season_measure 
               + alive 
               + cw 
               + measure
               + (1|id) + (1|Township) ,
             data = data, control = lmerControl(optimizer = "bobyqa"))

options(na.action="na.fail")
d1 <- dredge(m5a.0, evaluate=T, rank="AICc", fixed=~age_last_s + delta_age.1 + delta_age.2)
avgm <- model.avg(d1, subset= delta < 7)
avgm$coefficients
confint(avgm)

#GAM check

m7a <- gamm(log(bm) ~ age_last_s + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)

m7a.1 <- gamm(log(bm) ~ season_measure + age_last_s + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)

m7a.2 <- gamm(log(bm) ~ alive + age_last_s + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)

m7a.3 <- gamm(log(bm) ~ cw + age_last_s + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)

data$measure <- relevel(data$measure, ref="measured")
m7a.4 <- gamm(log(bm) ~ measure + age_last_s + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=data)

AICc(m7a, m7a.1, m7a.2, m7a.3, m7a.4) # actually there is an effect of measurement
summary(m7a.4$gam)

m7a.0 <- uGamm(log(bm) ~ age_last_s + s(delta_age_s, bs="cr", k=15)
              + season_measure 
              + alive 
              + cw 
              + measure,
            random=list(id=~1|id, Township=~1|Township),
            data=data)
options(na.action="na.fail")
d2 <- dredge(m7a.0, evaluate=T, rank="AICc", fixed=~age_last_s + s(delta_age_s, bs="cr", k=15))
avgm <- model.avg(d2, subset= delta < 7)
avgm$coefficients
confint(avgm)

m7a.0 <- uGamm(log(bm) ~ age_last_s + s(delta_age_s, bs="cr", k=15)
               + alive + measure + season_measure,
               random=list(id=~1|id, Township=~1|Township),
               data=data)
summary(m7a.0$gam)

#effect of township: only minimal impovement in model fit
m7a.4town <- gamm(log(bm) ~ age_last_s + s(delta_age_s, bs="cr", k=15),
                  random=list(id=~1|id), data=data)
AICc(m7a, m7a.4town)
AICc(m7a)-AICc(m7a.4town)


# CALCULATIONS ----
# Weight at minimum age (18 years old)
summary(m5a)
exp(7.707422+0.040851*min(data$delta_age.1)) # 2540.882
exp((7.707422-1.96*0.021072)+(0.040851-1.96*0.002069)*min(data$delta_age.1)) # 2406.137
exp((7.707422+1.96*0.021072)+(0.040851+1.96*0.002069)*min(data$delta_age.1)) # 2683.172
2541-2306

# Terminal decline in males
summary(m5b)

exp(7.715450) #2242.732
exp(7.715450-0.043636) #2146.973
exp(7.715450) - exp(7.715450-0.043636) #95.76
(exp(7.715450) - exp(7.715450-0.043636))/exp(7.715450)#4.27%

exp(7.715450) #2242.732
exp(7.715450-(0.043636-1.96*0.026672)) #2262.196
exp(7.715450) - exp(7.715450-(0.043636-1.96*0.026672)) #-19.46
(exp(7.715450) - exp(7.715450-(0.043636-1.96*0.026672)))/exp(7.715450)#-0.87%

exp(7.715450) #2242.732
exp(7.715450-(0.043636+1.96*0.026672)) #2037.618
exp(7.715450) - exp(7.715450-(0.043636+1.96*0.026672)) #205.114
(exp(7.715450) - exp(7.715450-(0.043636+1.96*0.026672)))/exp(7.715450)#9.15%

# effect size
-0.043636/(0.026672*sqrt(1316))
(-0.043636-1.96*0.026672)/(0.026672*sqrt(1316))
(-0.043636+1.96*0.026672)/(0.026672*sqrt(1316))

# Mass change (kg/year)
summary(m5a)
r.squaredGLMM(m5a)
# During the growing phase
# kg
(exp(7.707422+0.040851) - exp(7.707422))/4.3027 # 1 delta_age = 4.3027 years
(exp(7.707422+(0.040851+1.96*0.002069)) - exp(7.707422))/4.3027
(exp(7.707422+(0.040851-1.96*0.002069)) - exp(7.707422))/4.3027
# %
((exp(7.707422+0.040851) - exp(7.707422))/exp(7.707422))/4.3027
((exp(7.707422+(0.040851+1.96*0.002069)) - exp(7.707422))/exp(7.707422))/4.3027
((exp(7.707422+(0.040851-1.96*0.002069)) - exp(7.707422))/exp(7.707422))/4.3027

# During the decreasing phase
# kg
(exp(7.707422) - exp(7.707422-0.056879))/4.3027
(exp(7.707422) - exp(7.707422-(0.056879+1.96*0.014129)))/4.3027
(exp(7.707422) - exp(7.707422-(0.056879-1.96*0.014129)))/4.3027
# %
((exp(7.707422) - exp(7.707422-0.056879))/exp(7.707422))/4.3027
((exp(7.707422) - exp(7.707422-(0.056879+1.96*0.014129)))/exp(7.707422))/4.3027
((exp(7.707422) - exp(7.707422-(0.056879-1.96*0.014129)))/exp(7.707422))/4.3027

