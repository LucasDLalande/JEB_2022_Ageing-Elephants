### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## 2. Data description
##
## Lalande Lucas D., Lummaa Virpi, Aung Htoo H., Htut Win, Nyein Kyaw U., Berger Verane, Briga Michael  
## February 2022
##
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

rm(list=ls())

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

females <- data[data$sex=="A",]
unique(factor(females$id))
males <- data[data$sex=="B",]
unique(factor(males$id))

####################
### To run analyses with only individuals with 3 or more observations, run the following code before model comparison ###
a <- data.frame(table(factor(data$id)))
a <- a[a$Freq>=3,]

data <- merge(data, a, by.x="id", by.y="Var1", all=F)
unique(factor(data$id))
####################


# BOTH SEXES ----
# Number of individuals and observations
unique(data$id)
# 493 individuals - 3886 body masses

# Number of measured and estimated body masses
meas <- data[data$measure=="measured",]
meas$id <- factor(meas$id)
unique(meas$id)
# 1901 bm for 347 elephants
measfem <- meas[meas$sex=="A",]
unique(factor(measfem$id))
# 1297 bm for 230 females
measmal <- meas[meas$sex=="B",]
unique(factor(measmal$id))
# 604 bm for 117 males

est <- data[data$measure=="estimated",]
est$id <- factor(est$id)
unique(est$id)
# 1985 estimations for 342 elephants
estfem <- est[est$sex=="A",]
unique(factor(estfem$id))
# 1273 bm for 226 females
estmal <- est[est$sex=="B",]
unique(factor(estmal$id))
# 712 bm for 116 males


# Median number of obs/ind [2.5th percentile, 97.5th percentile]
data$obs <- 1
obs <- data.frame(tapply(data$obs, data$id, sum))
quantile(a[,2], c(0.025, 0.5, 0.975))
# 4.0 [1, 36.4]

# How long they have been measured: median [2.5th percentile, 97.5th percentile]
data$id <- factor(data$id)
min <- data.frame(tapply(data$age, data$id, min))
min <- add_rownames(min, "id")
min()
colnames(min)[2] <- "min"
max <- data.frame(tapply(data$age, data$id, max))
max <- add_rownames(max, "id")
colnames(max)[2] <- "max"
time <- merge(min, max, by="id")
time$time <- time$max-time$min
quantile(time$time, c(0.025, 0.5, 0.975))
# 2.8 [0, 36.6]

summary(data$age)
# 18-72 year old (mean = 39.3yo)

males <- data[data$sex=="B",]
summary(males$age)
unique(factor(males$id))

females <- data[data$sex=="A",]
summary(females$age)
unique(factor(females$id))

summary(data$bm)
mean(data$bm)
# 1334-4582kg (mean = 2626kg)

summary(females$bm)

# How many old individuals (based on Lahdenper? et al., 2018. Nature Communication)
males <- subset(data, age>=30.8 & sex=="B")
males$id <- factor(males$id)
unique(males$id)
# 865 measures for 114 males over 30.8 (median lifespan)
females <- subset(data, age>=44.7 & sex=="A")
females$id <- factor(females$id)
unique(females$id)
# 995 measures for 108 females over 44.7 (median lifespan)

# How many retired individuals
old <- subset(data, age>=53)
males <- subset(old, sex=="B")
males$id <- factor(males$id)
unique(males$id)
# 141 measures for 15 males over 53
females <- subset(old, sex=="A")
females$id <- factor(females$id)
unique(females$id)
# 573 measures for 57 females over 53

dead <- subset(data, alive=="N")
deada <- subset(dead, sex=="A")
unique(factor(deada$id))
deada <- deada[!deada$id==393,] # died at 26 so removed from the dead individuals.
deada1 <- deada[deada$terminal==1,]
unique(factor(deada1$id))
deadb <- subset(dead, sex=="B")
unique(factor(deadb$id))
deadb <- deadb[!deadb$id==431,] # died at 23 so removed from the dead individuals.

# MALES ----
males <- data[data$sex=="B",]
# Number of observations and individuals
males$id <- factor(males$id)
unique(males$id)
# 1316 bm for 171 elephants

summary(males$age)
mean(males$age)
# 18-64 year old (mean = 37.4yo)

range(males$bm)
mean(males$bm)
# 1334-4582kg (mean = 3001.6kg)


males <- data[data$sex=="B",]
# delta age in years:
males$grp <- cut(males$delta_age_s, seq(-4.5, 4.5, 1))
mean <- data.frame("mean_age"=rep(0,9))
mean$mean_age <- with(males, tapply(age, grp, mean))
mean$delta <- with(males, tapply(delta_age_s, grp, mean))
mean <- mean[-9,]
plot(mean$mean_age~ mean$delta, main="males")
abline(lm(mean$mean_age~ mean$delta), col="red")
summary(lm(mean$mean_age~ mean$delta))
# age = 36.9688 + 4.3027*delta_age
(18-36.9688)/4.3027
# delta age at 18 ans = -4.4




# FEMALES ----
females <- data[data$sex=="A",]
# Number of observations and individuals
females$id <- factor(females$id)
unique(females$id)
# 2570 bm for 322 elephants

summary(females$age)
# 18-72 year old (mean = 40.2yo)

summary(females$bm)
mean(females$bm)
# 1458-4181kg (mean = 2434.0kg)

# delta age in years:
females$grp <- cut(females$delta_age_s, seq(-6.5, 4.5, 1))
mean <- data.frame("mean_age"=rep(0,11))
mean$mean_age <- with(females, tapply(age, grp, mean))
mean$delta <- with(females, tapply(delta_age_s, grp, mean))
plot(mean$mean_age~ mean$delta, main="females")
abline(lm(mean$mean_age~ mean$delta), col="red")
summary(lm(mean$mean_age~ mean$delta))
# age = 42.08 + 4.46*delta_age
(18-42.0812)/4.4647 # For females, 18years of age=-5.4 delta age


# delta age in years:
data$grp <- cut(data$delta_age_s, seq(-6.5, 4.5, 1))
mean <- data.frame("mean_age"=rep(0,11))
mean$mean_age <- with(data, tapply(age, grp, mean))
mean$delta <- with(data, tapply(delta_age_s, grp, mean))
plot(mean$mean_age~ mean$delta)
abline(lm(mean$mean_age~ mean$delta), col="red")
summary(lm(mean$mean_age~ mean$delta))
# age = 40.9328 + 4.3161*delta_age

mean(data$age)
mean(males$age)
mean(females$age)



data$newdelta <- data$delta_age_s * attr(data$delta_age_s, 'scaled:scale') + attr(data$delta_age_s, 'scaled:center')
