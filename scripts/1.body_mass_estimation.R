### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## 1. Body mass estimation
##
## Lalande Lucas D., Lummaa Virpi, Aung Htoo H., Htut Win, Nyein Kyaw U., Berger Verane, Briga Michael  
## February 2022
##
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

rm(list=ls())

{data <- read.csv("sex-specific-dataset.csv", h=T, sep=";", dec=".")

data$id <- as.factor(data$id)
data$sex <- as.factor(data$sex)

# We keep only individuals for which body mass, chest girth AND height are measured
body <- data[!is.na(data$body_mass),]
body <- body[!is.na(body$chest),]
body <- body[!is.na(body$height),]

male <- body[body$sex=="B",] 
female <- body[body$sex=="A",] 
female$id <- factor(female$id)
unique(female$id)
male$id <- factor(male$id)
unique(male$id)
unique(factor(body$id))
# 1470 body mass measurements (979 for females, 491 for males)
# 319 individuals (211 females, 108 males)
}

cor.test(data$chest, data$body_mass)

cor.test(male$chest, male$height)
cor.test(female$chest, female$height)
cor.test(male$body_mass, male$height)
cor.test(female$body_mass, female$height)
cor.test(male$chest, male$body_mass)
cor.test(female$chest, female$body_mass)

{# Explaining body mass by CHEST ----
m1 <- lm(body_mass ~ chest, data=male)
summary(m1)
f1 <- lm(body_mass ~ chest, data=female)
summary(f1)

# Using model estimates for body mass equations
male$bm <- -2725.3429 + 16.4618*male$chest
  # Estimating correlations for males
corm1 <- cor.test(male$body_mass, male$bm)

female$bm <- -1467.3473 + 11.8015*female$chest
  #Estimating correlations for females
corf1 <- cor.test(female$body_mass, female$bm)

AICc(m1, f1)

newdata <- rbind(male, female)
  # Estimating correlations for both sexes
cor1 <- cor.test(newdata$body_mass, newdata$bm)
}

{# Explaining body mass by CHEST + CHEST^2 ----
m2 <- lm(body_mass ~ chest + I(chest^2), data=male)
summary(m2)
f2 <- lm(body_mass ~ chest + I(chest^2), data=female)
summary(f2)

male$bm <- 3845.75392 - 20.67919*male$chest + 0.05217*male$chest^2
corm2 <- cor.test(male$body_mass, male$bm)
female$bm <- 4877 - 27.36*female$chest + 0.06015*female$chest^2
corf2 <- cor.test(female$body_mass, female$bm)


AICc(m2, f2)

newdata <- rbind(male, female)
cor2 <- cor.test(newdata$body_mass, newdata$bm)
}

{# Explaining body mass by CHEST + HEIGHT ----
m3 <- lm(body_mass ~ chest + height, data=male)
summary(m3)
f3 <- lm(body_mass ~ chest + height, data=female)
summary(f3)

male$bm <- -4530.7291 + 9.6699*male$chest + 17.3245*male$height
corm3 <- cor.test(male$body_mass, male$bm)
female$bm <- -3089.3311 + 9.0948*female$chest + 11.3868*female$height
corf3 <- cor.test(female$body_mass, female$bm)


AICc(m3, f3)

newdata <- rbind(male, female)
cor3 <- cor.test(newdata$body_mass, newdata$bm)
}

{# Explaining body mass by CHEST + CHEST^2 + HEIGHT ----
m4 <- lm(body_mass ~ chest + I(chest^2) + height, data=male)
summary(m4)
f4 <- lm(body_mass ~ chest + I(chest^2) + height, data=female)
summary(f4)

male$bm <- 2829 - 32.17*male$chest + 0.05863*male$chest^2 + 17.57*male$height 
corm4 <- cor.test(male$body_mass, male$bm)
female$bm <- 2307 - 23.75*female$chest + 0.05061*female$chest^2 + 10.98*female$height
corf4 <- cor.test(female$body_mass, female$bm)


AICc(m4, f4)

newdata <- rbind(male, female)
cor4 <- cor.test(newdata$body_mass, newdata$bm)
}

{# Explaining body mass by CHEST + HEIGHT + HEIGHT^2----
m5 <- lm(body_mass ~ chest + height + I(height^2), data=male)
summary(m5)
f5 <- lm(body_mass ~ chest + height + I(height^2), data=female)
summary(f5)

male$bm <- 611.35304 + 9.76547*male$chest - 25.94196*male$height + 0.08998*male$height^2
corm5 <- cor.test(male$body_mass, male$bm)
female$bm <- 5661.01300 + 9.02285*female$chest - 67.86094*female$height + 0.17956*female$height^2
corf5 <- cor.test(female$body_mass, female$bm)


AICc(m5, f5)

newdata <- rbind(male, female)
cor5 <- cor.test(newdata$body_mass, newdata$bm)
}

{# Explaining body mass by CHEST + CHEST^2 + HEIGHT + HEIGHT^2----
m6 <- lm(body_mass ~ chest + I(chest^2) + height + I(height^2), data=male)
summary(m6)
f6 <- lm(body_mass ~ chest + I(chest^2) + height + I(height^2), data=female)
summary(f6)

male$bm <- 4025 - 30.21*male$chest + 0.05593*male$chest^2 + 4.643*male$height - 0.02686*male$height^2
corm6 <- cor.test(male$body_mass, male$bm)
female$bm <- 7697 - 16.40*female$chest + 0.03919*female$chest^2 - 48.77*female$height + 0.1356*female$height^2
corf6 <- cor.test(female$body_mass, female$bm)


AICc(m6, f6)

newdata <- rbind(male, female)
cor6 <- cor.test(newdata$body_mass, newdata$bm)
}


# Selecting the best model for males and females to explain body mass ----
rbind(AICc(m1, m2, m3, m4, m5, m6),AICc(f1, f2, f3, f4, f5, f6))

model <- c("Chest", "Chest+Chest^2", "Chest+Height", "Chest+Chest^2+Height", "Chest+Height+Height^2", "Chest+Chest^2+Height+Height^2")
AICc_males <- rbind(AICc(m1, m2, m3, m4, m5, m6))
AICc_females <- rbind(AICc(f1, f2, f3, f4, f5, f6))
cor_males <- rbind(corm1[4], corm2[4], corm3[4], corm4[4], corm5[4], corm6[4])
cor_females <- rbind(corf1[4], corf2[4], corf3[4], corf4[4], corf5[4], corf6[4])
overall <- rbind(cor1[4], cor2[4], cor3[4], cor4[4], cor5[4], cor6[4])
recap <- cbind(model, overall, AICc_males[2], cor_males, AICc_females[2], cor_females)
colnames(recap) <- c("Model", "Overall correlation", "AICc_males", "cor_males", "AICc_females", "cor_females")
recap
# The models selected are the model m6 and f6


{# Estimations ----
data$measure <- ifelse(is.na(data$body_mass), "estimated", "measured")
male <- data[data$sex=="B",]
female <- data[data$sex=="A",]
male$bm <- ifelse(male$measure=="measured", male$body_mass,
                  2829 - 32.17*male$chest + 0.05863*male$chest^2 + 17.57*male$height )

female$bm <- ifelse(female$measure=="measured", female$body_mass,
                    7697 - 16.40*female$chest + 0.03919*female$chest^2 - 48.77*female$height + 0.1356*female$height^2)


data <- rbind(male, female)
data <- data[!is.na(data$bm),]
male <- data[data$sex=="B",]
female <- data[data$sex=="A",]
data$id <- factor(data$id)
male$id <- factor(male$id)
female$id <- factor(female$id)
unique(data$id)
unique(male$id)
unique(female$id)
}


# Correction ----
# We correct the previous equations by the difference between the mean of the measured and the estimated values
mean(data$bm[data$measure=="measured"]) - mean(data$bm[data$measure=="estimated"]) # -21.25867

male$measure <- ifelse(is.na(male$body_mass), "estimated", "measured")
female$measure <- ifelse(is.na(female$body_mass), "estimated", "measured")



male$bm <- ifelse(male$measure=="measured", male$body_mass,
                  2829 - 32.17*male$chest + 0.05863*male$chest^2 + 17.57*male$height - 63.25867)

female$bm <- ifelse(female$measure=="measured", female$body_mass,
                    7697 - 16.40*female$chest + 0.03919*female$chest^2 - 48.77*female$height + 0.1356*female$height^2 - 21.25867)

data <- rbind(male, female)

# export as csv
