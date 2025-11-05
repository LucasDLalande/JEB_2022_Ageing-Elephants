### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Sex-specific body mass ageing trajectories in Asian elephants
##
## 0. Initialisation
##
## Lalande Lucas D., Lummaa Virpi, Aung Htoo H., Htut Win, Nyein Kyaw U., Berger Verane, Briga Michael  
## February 2022
##
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



rm(list=ls(all=TRUE))



### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### 1. Create folders  #### 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


catch_fold_figures <- file.path("figures") #for scripts
dir.create(file.path(catch_fold_figures))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### 2. Install and load packages  #### 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Add package used in the R script
packages <- c("lme4", "MuMIn", "ggplot2", "dplyr", "gridExtra", "mgcv", "gratia", "egg", "grid", "simr")

# Package function

require <- function(x) { 
  if (!base::require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE) ; 
    base::require(x, character.only = TRUE)
  } 
}

# Load package

base::lapply(packages, require)
