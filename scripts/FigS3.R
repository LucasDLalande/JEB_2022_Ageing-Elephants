### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## Figure S3. Testing different terminal decline windows
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

# Males ----
males <- data[data$sex=="B",] # B corresponds to 'males' in the anonymised dataset

# Threshold model with terminal
{males$delta_age.1 <- males$delta_age_s + 7 # To have delta_age positive
  males$delta_age.2 <- males$delta_age_s + 7
  males$final_delta_age <- males$delta_age_s + 7
  thresholds <- data.frame()
  thresh <- 9.2
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    males$delta_age.1 <- males$delta_age.2 <- males$final_delta_age
    males$delta_age.1 [males$delta_age.1 > i] <- i
    males$delta_age.2 [males$delta_age.2 <= i] <- 0
    males$delta_age.2 [males$delta_age.2 > i] <- males$delta_age.2 [males$delta_age.2 > i] - i
    
    m5b <-  lmer(log(bm) ~ terminal + age_last_s + delta_age.1 + delta_age.2 + (1|id),
                 data = males, control = lmerControl(optimizer = "bobyqa"))}
  
  summary(m5b)}
AICc(m5b)

# FIGURE S3 ----
# Figure S3A----
# Making the terminal windows vary between 6 months prior to death, to 5 years prior to death
{term <- data.frame()

for (j in seq(0.5, 5, 0.1)) {
  print(paste('Fitting terminal threshold model with age before death:', j))
  males$terminal1 <- ifelse(males$y_before_death <= j, 1, 0)
  males$terminal1[is.na(males$terminal1)] <- 0
  males$terminal1 <- as.factor(males$terminal1)
  
  m5b <-  lmer(log(bm) ~ terminal1 + age_last_s + delta_age.1 + delta_age.2 + (1|id),
               data = males, control = lmerControl(optimizer = "bobyqa"))
  
  k<- AICc(m5b)
  
  term <- rbind(term, k)}

termi <- seq(0.5,5,0.1)
term <- cbind(termi, "AICc"=term)
colnames(term) <- c("terminal","AICc")
par(mfrow=c(1,1))


figs3a <- ggplot(term, aes(x = terminal, y = AICc+2)) +
  geom_point() +
  geom_smooth(aes(group = 1), colour = 'red', size = 1) +
  scale_x_continuous(name="Years before death")  +
  scale_y_continuous(name="AICc") +
  geom_hline(yintercept = -3194.3) +
  geom_hline(yintercept = -3198.3, linetype="dashed") +
  geom_hline(yintercept = -3190.3, linetype="dashed") +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        axis.ticks.x = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  annotate("text", x=2.75, y=-3193.9, label=expression(paste("Threshold model without a terminal effect")), size=4)

figs3a
}



# Females ----
females <- data[data$sex=="A",] # A corresponds to 'females' in the anonymised dataset

# Figure S3B----
# Making the terminal windows vary between 6 months prior to death, to 5 years prior to death
{term <- data.frame()

for (j in seq(0.5, 5, 0.1)) {
  print(paste('Fitting terminal threshold model with age before death:', j))
  females$terminal1 <- ifelse(females$y_before_death <= j, 1, 0)
  females$terminal1[is.na(females$terminal1)] <- 0
  females$terminal1 <- as.factor(females$terminal1)
  
  
  
  m2b <-  lmer(log(bm) ~ terminal1 + age_last_s + I(age_last_s^2) + delta_age_s + (1|id) + (1|Township),
               data = females, control = lmerControl(optimizer = "bobyqa"))
  AICc(m2b)
  
  k<- AICc(m2b)
  
  term <- rbind(term, k)}


termi <- seq(0.5,5,0.1)
term <- cbind(termi, "AICc"=term)
colnames(term) <- c("terminal","AICc")
par(mfrow=c(1,1))

figs3b <- ggplot(term, aes(x = terminal, y = AICc)) +
  geom_point() +
  geom_smooth(aes(group = 1), colour = 'red', size = 1) +
  scale_x_continuous(name="Years before death")  +
  scale_y_continuous(name="AICc") +
  geom_hline(yintercept = -5598.7) +
  geom_hline(yintercept = -5602.7, linetype="dashed") +
  geom_hline(yintercept = -5594.7, linetype="dashed") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  annotate("text", x=2.75, y=-5598.1, label=expression(paste("Age last"^2, " model without a terminal effect")), size=4) +
  theme(axis.title=element_text(size=19))
  figs3b
  
}


# Figure S3
figs3 <- ggarrange(figs3a, figs3b,
                  labels=c("A", "B"),
                  label.args = list(gp=gpar(font=2), x=unit(1,"line"), hjust=0),
                  nrow=2)
figs3

# Save this figure
ggsave(filename = "figures/figs3.png", figs3, width = 20, height = 30, dpi = 300, units = "cm", device='png')

