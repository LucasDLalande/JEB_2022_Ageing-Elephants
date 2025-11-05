### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## Figure 1(S2) / Figure S4. Ageing trajectories for males and females (GLMM (fig.1/S2), GAM (fig.S4))
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
data$cw <- as.factor(data$cw) # elephants origin (wild-caught, captive-born, unknown)
data$alive <- as.factor(data$alive) # whether elephant still alive at the time of the analyses (yes (Y), no (N))
data$season_measure <- as.factor(data$season_measure) # season at the moment of the measurement (cold, hot, monsun)
data$Township <- as.factor(data$Township) # township elephant was last seen
data$measure <- as.factor(data$measure) # whether body masses are measured or estimated
data$year_birth <- as.factor(data$year_birth)
data$terminal <- as.factor(data$terminal) # whether the elephant died within the year of the measurement (no (0), yes (1))


# calculate mean age
data$mean_age <- ave(data$age, data$id, FUN = function(x) mean(as.numeric(x)))
data$mean_age_s <- scale(data$mean_age)

# calculate age at last measurement
data$age_last <- ave(data$age, data$id, FUN = function(x) max(as.numeric(x)))
data$age_last_s <- scale(data$age_last)

# calculate delta age
data$delta_age <- data$age - data$mean_age
data$delta_age_s <- scale(data$delta_age)

males <- data[data$sex=="B",] # B corresponds to 'males' in the anonymised dataset
}

# Threshold model males
{males$delta_age.1 <- males$delta_age_s +7# To have delta_age positive
  males$delta_age.2 <- males$delta_age_s +7
  males$final_delta_age <- males$delta_age_s +7
  thresholds <- data.frame()
  thresh <- 8.9
  
  for (i in thresh){
    print(paste('Fitting threshold model with breakpoint age', i))
    males$delta_age.1 <- males$delta_age.2 <- males$final_delta_age
    males$delta_age.1 [males$delta_age.1 > i] <- i
    males$delta_age.2 [males$delta_age.2 <= i] <- 0
    males$delta_age.2 [males$delta_age.2 > i] <- males$delta_age.2 [males$delta_age.2 > i] - i
    
    m5a <-  lmer(log(bm) ~ age_last_s + delta_age.1 + delta_age.2 + (1|id) + (1|Township),
                 data = males, control = lmerControl(optimizer = "bobyqa"))}
  
  coef((summary(m5a)))
}

females <- data[data$sex=="A",] # A corresponds to 'females' in the anonymised dataset

{
  females$term <- ifelse(females$terminal=="1", "Terminal", "Non-terminal")
  females$term <- as.factor(females$term)
  females$term <- relevel(females$term, ref="Terminal")
  
  m2b <-  lmer(log(bm) ~ terminal + age_last_s + I(age_last_s^2) + delta_age_s + (1|id) + (1|Township),
               data = females, control = lmerControl(optimizer = "bobyqa"))
  
  str(females)
  
  m2b1 <-  lmer(log(bm) ~ term*delta_age_s + age_last_s + I(age_last_s^2) + (1|id) + (1|Township),
                data = females, control = lmerControl(optimizer = "bobyqa"))
  
  }


# Prediction males ----
summary(m5a)

{
males1 <- males[males$delta_age_s<=2.0,]
males1$predict11 <- exp(7.707422 + males1$delta_age.1*0.040851)
males1$lci11 <- exp(7.707422-1.96*0.021072 + males1$delta_age.1*0.040851-1.96*0.002069)
males1$uci11 <- exp(7.707422+1.96*0.021072 + males1$delta_age.1*0.040851+1.96*0.002069)
exp(7.707422 + 8.9*0.040851)
log(3200.287)

males2 <- males[males$delta_age_s>=1.8,]
males2$predict21 <- exp(8.071 - males2$delta_age.2*0.056879)
males2$lci21 <- exp(8.071-1.96*0.021072 - males2$delta_age.2*0.056879-1.96*0.014129)
males2$uci21 <- exp(8.071+1.96*0.021072 - males2$delta_age.2*0.056879+1.96*0.014129)
exp(8.070996 - 1.5*0.054998)}

# FIGURE 1 ----
# Figure 1A----
grob1 <- grobTree(textGrob("Males", x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="black", fontsize=13)))
fig1a <- ggplot() +
  geom_point(data=males1, aes(x = delta_age.1*5.4, y = bm)) +
  geom_ribbon(data=males1, aes(x= delta_age.1*5.4, ymin=lci11, ymax=uci11), fill="grey", alpha=0.6) +
  geom_smooth(data=males1, aes(x= delta_age.1*5.4, y=predict11), color="cyan3", method='lm',alpha = .5, size=1) +
  geom_point(data=males2, aes(x = (delta_age.2+8.9)*5.4, y = bm)) +
  geom_ribbon(data=males2, aes(x= (delta_age.2+8.9)*5.4, ymin=uci21, ymax=lci21), fill="grey", alpha=0.6) +
  geom_smooth(data=males2, aes(x= (delta_age.2+8.9)*5.4, y=predict21), color="cyan3", method='lm',alpha = .5, size=1) +
  xlab("Age [years]") +
  ylab("Body mass [kg]") +
  annotation_custom(grob1) +
  geom_vline(xintercept= 8.9*5.4, linetype="dashed", color="blue", size=1)+
  geom_vline(xintercept= 8.7*5.4, linetype="dashed", color="blue", size=0.6)+
  geom_vline(xintercept= 9.5*5.4, linetype="dashed", color="blue", size=0.6)+
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())
fig1a

# Figure 1B----
grob2 <- grobTree(textGrob("Females", x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="black", fontsize=13)))
fig1b <- ggplot(females, aes(x = delta_age_s*4.4647+42.018, y = bm)) +
  geom_point(aes(x = delta_age_s*4.4647+42.018, y = bm)) +
  geom_smooth(aes(x= delta_age_s*4.4647+42.018, y=exp(predict(m2b1)), color=term), method='lm',alpha = .5, size=1) +
  xlab(expression(paste("Age [years]"))) +
  scale_y_continuous(name="Body mass [kg]") +
  annotation_custom(grob2) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())
fig1b


fig1 <- ggarrange(fig1a, fig1b,
                  labels=c("A", "B"),
                  label.args = list(gp=gpar(font=2), x=unit(1,"line"), hjust=0),
                  nrow=2)

# Save this figure
ggsave(filename = "figures/fig1.png", fig1, width = 20, height = 30, dpi = 300, units = "cm", device='png')


# FIGURE S2 ----
# Figure S2A----
grob1 <- grobTree(textGrob("Males", x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="black", fontsize=13)))
figs2a <- ggplot() +
  geom_point(data=males1, aes(x = delta_age.1-7, y = bm)) +
  geom_ribbon(data=males1, aes(x= delta_age.1-7, ymin=lci11, ymax=uci11), fill="grey", alpha=0.6) +
  geom_smooth(data=males1, aes(x= delta_age.1-7, y=predict11), color="cyan3", method='lm',alpha = .5, size=1) +
  geom_point(data=males2, aes(x = delta_age.2+1.9, y = bm)) +
  geom_ribbon(data=males2, aes(x= delta_age.2+1.9, ymin=uci21, ymax=lci21), fill="grey", alpha=0.6) +
  geom_smooth(data=males2, aes(x= delta_age.2+1.9, y=predict21), color="cyan3", method='lm',alpha = .5, size=1) +
  ylab("Body mass [kg]") +
  annotation_custom(grob1) +
  geom_vline(xintercept= 1.9, linetype="dashed", color="blue", size=1)+
  geom_vline(xintercept= 1.7, linetype="dashed", color="blue", size=0.6)+
  geom_vline(xintercept= 2.5, linetype="dashed", color="blue", size=0.6)+
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())
figs2a


# Figure S2B ----
grob2 <- grobTree(textGrob("Females", x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="black", fontsize=13)))
figs2b <- ggplot(females, aes(x = delta_age_s, y = bm)) +
  geom_point(aes(x = delta_age_s, y = bm)) +
  geom_smooth(aes(x= delta_age_s, y=exp(predict(m2b1)), color=term), method='lm',alpha = .5, size=1) +
  xlab(expression(paste("Standardized within-individual age (",Delta,"age)"))) +
  scale_y_continuous(name="Body mass [kg]") +
  annotation_custom(grob2) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())
figs2b

figs2 <- ggarrange(figs2a, figs2b,
                   labels=c("A", "B"),
                   label.args = list(gp=gpar(font=2), x=unit(1,"line"), hjust=0),
                   nrow=2)

# Save this figure
ggsave(filename = "figures/figs2.png", figs2, width = 20, height = 30, dpi = 300, units = "cm", device='png')


# FIGURE S4 ----
# Figure S4A ----
m <- lmer(log(bm)~ age_last_s + (1|id) + (1|Township), data=males, control=lmerControl("bobyqa"))
resid<-resid(m)
mean(log(males$bm))
linresid<-7.983609+resid #intercept comes from mean(log(bm))
grob1 <- grobTree(textGrob("Males", x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="black", fontsize=13)))
figs4a <- ggplot(males, aes(y=exp(linresid), x=delta_age_s)) + 
  geom_point(aes(y=bm, x=delta_age_s)) +
  geom_smooth(method='gam', formula = y ~ s(x, bs = "cs", k=35), colour="cyan3", fill="gray50") +
  xlab(expression(paste("Standardized within-individual age (",Delta,"age)"))) +
  scale_y_continuous(name="Body mass [kg]") +
  annotation_custom(grob1) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        panel.border = element_blank())
figs4a

# Figure S4B ----
grob2 <- grobTree(textGrob("Females", x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="black", fontsize=13)))
figs4b <- ggplot(females, aes(y=bm, x=delta_age_s)) + 
  geom_point(aes(y=bm, x=delta_age_s)) +
  geom_smooth(aes(x= delta_age_s, y=exp(predict(m2b1)), color=term), method='gam', formula = y ~ s(x, bs = "cs", k=10)) +
  xlab(expression(paste("Standardized within-individual age (",Delta,"age)"))) +
  scale_y_continuous(name="Body mass [kg]") +
  annotation_custom(grob2) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        plot.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.line.x = element_line(color = "black", size = .5),
        axis.line.y = element_line(color = "black", size = .5),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())
figs4b


figs4 <- ggarrange(figs4a, figs4b,
                   labels=c("A", "B"),
                   label.args = list(gp=gpar(font=2), x=unit(1,"line"), hjust=0),
                   nrow=2)

# Save this figure
ggsave(filename = "figures/figs4.png", figs4, width = 20, height = 30, dpi = 300, units = "cm", device='png')
