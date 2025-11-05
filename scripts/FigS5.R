### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## Figure S5. Age at maximum body mass in males
##
## Lalande Lucas D., Lummaa Virpi, Aung Htoo H., Htut Win, Nyein Kyaw U., Berger Verane, Briga Michael  
## February 2022
##
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

rm(list=ls())

{data <- read.csv("sex-specific-dataset.csv", h=T, sep=";", dec=".")


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
data$delta_age_s <- scale(data$delta_age)}

males <- data[data$sex=="B",] # B corresponds to 'males' in the anonymised dataset

# FIGURE S5 ----
#Identify the age at maximum of the gamm using the first order derivative
m7a <- gamm(log(bm) ~ age_last + s(delta_age_s, bs="cr", k=15),
            random=list(id=~1|id, Township=~1|Township),
            data=males)
dgam<-fderiv(m7a)
data$deriv<-fderiv(m7a)
y<-dgam$deriv
y<-data.frame(y)
head(y, n=2)
names(y)[1]<-'deriv'
names(y)[2]<-'sederiv'
y<-y[,c('deriv','sederiv')]
x<-dgam$eval[, 1]
x<-data.frame(x)
names(x)[1]<-'age'
p<-cbind(x, y)
head(p)
gamderiv<-ggplot(p, aes(age, deriv)) + theme_grey(base_size = 16) + labs(x="Age [Years]", y="Slope of gam derivative") + geom_line(size=1.3, color='#619CFF')
gamderiv2<-gamderiv + 
  geom_ribbon(data=p, aes(x=age, ymin = deriv-1.96*sederiv, ymax = deriv+1.96*sederiv), inherit.aes = FALSE, alpha = .20) +
  geom_hline(yintercept = 0, linetype="dashed")

gamderiv2

# Save this figure
ggsave(filename = "figures/figS5.png", gamderiv2, width = 20, height = 20, dpi = 300, units = "cm", device='png')

