#####################################################################################################################################################

#Code to run multi-level Bayesian Poisson and computational models for 

#Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script
library(tidyverse)
library(rethinking)
library(rstan)

#Load data (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T) 

#####################################################################################################################################################
#Poisson regressions pre-processing
#####################################################################################################################################################

#Prepare data for speed Poisson

#Empty vectors
trials_initial  <- rep(NA, length(unique(d$id)))
trials_reversal <- rep(NA, length(unique(d$id)))
sex <-  rep(NA, length(unique(d$id)))
pop <- rep(NA, length(unique(d$id)))
skip_initial <- rep(NA, length(unique(d$id)))
skip_reversal <- rep(NA, length(unique(d$id)))

#Fill empty vectors
for (id in 1:max(d$id)) {
  trials_initial[id] <- max(d$Trial[which(d$id == id & d$Phase == 1)])
  trials_reversal[id] <- max(d$Trial[which(d$id == id & d$Phase == 2)])
  sex[id] <- unique(d$sex[d$id == id])
  pop[id] <- unique(d$Population[d$id == id])
  skip_initial[id] <- max(d$skip[which(d$id == id & d$Phase == 1)])
  skip_reversal[id] <- max(d$skip[which(d$id == id & d$Phase == 2)])
}

#Put vectors into list for Stan 
d_Poisson_speed = list(trials = c(trials_initial,trials_reversal), 
                       sex = c(sex, sex),
                       pop = c(pop,pop),
                       id = rep(1:49,2),
                       phase = as.integer(c(rep(1,49),rep(2,49))),
                       skip = c(skip_initial,skip_reversal))

#Prepare data for switch Poisson

#Calculate switch column
d$switch <- NA
for(i in 1:nrow(d)){
  if(d$Trial[i] > 1){
    if(d$Choice[i] == 1){
      if(d$Choice[i - 1] == 1){
        d$switch[i] <- 0
      } else {
        d$switch[i] <- 1
      }
    } else {
      if(d$Choice[i - 1] == 2){
        d$switch[i] <- 0
      } else {
        d$switch[i] <- 1
      }
    }
  } else {
    d$switch[i] <- 0
  }
}

#Empty vectors
switch_initial  <- rep(NA, length(unique(d$id)))
switch_reversal <- rep(NA, length(unique(d$id)))
sex <-  rep(NA, length(unique(d$id)))
pop <-rep(NA, length(unique(d$id)))
skip_initial <- rep(NA, length(unique(d$id)))
skip_reversal <- rep(NA, length(unique(d$id)))

#Fill empty vectors
for (id in 1:max(d$id)) {
  switch_initial[id] <- sum(d$switch[which(d$id == id & d$Phase == 1)])
  switch_reversal[id] <- sum(d$switch[which(d$id == id & d$Phase == 2)])
  sex[id] <- unique(d$sex[d$id ==id])
  pop[id] <- unique(d$Population[d$id == id])
  skip_initial[id] <- max(d$skip[which(d$id == id & d$Phase == 1)])
  skip_reversal[id] <- max(d$skip[which(d$id == id & d$Phase == 2)])
}

#Put vectors into list for Stan 
d_Poisson_switch = list(switches = c(switch_initial,switch_reversal), 
                        sex = c(sex, sex),
                        pop = c(pop,pop),
                        id = rep(1:49,2),
                        phase = as.integer(c(rep(1,49),rep(2,49))),
                        skip = c(skip_initial,skip_reversal))

#####################################################################################################################################################
#Computational models pre-processing
#####################################################################################################################################################

#Put data into a list for the STAN model
d_stan <- list(N = nrow(d),
               N_id = length(unique(d$id)),
               N_pop = length(unique(d$Population)),
               id = d$id,
               pop = d$Population,
               phase = d$Phase,
               sex_phase = d$sex_phase, 
               sex = d$sex,
               Choice = d$Choice,
               Correct = d$Correct
)

#####################################################################################################################################################
#Run models
#####################################################################################################################################################

#Run stan models - if encounter cmdstan run error, run the following: install_cmdstan()
m_speed_full <- stan(file = "STAN_Poisson_Speed_Full.stan", data = d_Poisson_speed, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_speed_xpop <- stan(file = "STAN_Poisson_Speed_Across_Pop.stan", data = d_Poisson_speed, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_switch_full <- stan(file = "STAN_Poisson_Switch_Full.stan", data = d_Poisson_switch, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_switch_xpop <- stan(file = "STAN_Poisson_Switch_Across_Pop.stan", data = d_Poisson_switch, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m <- stan(file = "STAN_Comp_Full.stan", data = d_stan, iter = 2000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_xpop <- stan(file = "STAN_Comp_Across_Pop.stan", data = d_stan, iter = 2000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))

#Inspect effective model samples - OK if n_eff > 1000 
precis(m_speed_full, depth = 3, pars = c("a"))
precis(m_speed_xpop, depth = 3, pars = c("a"))
precis(m_switch_full, depth = 3, pars = c("a"))
precis(m_switch_xpop, depth = 3, pars = c("a"))
precis(m, depth = 3, pars = c("phi", "lambda"))
precis(m_xpop, depth = 3, pars = c("phi", "lambda"))

#####################################################################################################################################################
#Poisson regressions post-processing
#####################################################################################################################################################

#Extract posteriors
sp1 <- extract.samples(m_speed_full)
sp2 <- extract.samples(m_speed_xpop)
sw1 <- extract.samples(m_switch_full)
sw2 <- extract.samples(m_switch_xpop)

#Process Poisson models posterior for obtaining Table S1 & S2 values, as well as for plotting Figure 2
#That is, access posterior target values by population & sex & phase, name accordingly, and put into parameter-specific data frame 
#Exponentiate to get back on outcome scale

#Speed full
s_speed_full <- as.data.frame(
  list(
    #Core initial
    AZ_init_M = exp(sp1$a[,1,2,1]),                      #Pop AZ, Male, Initial    
    AZ_init_F = exp(sp1$a[,1,1,1]),                      #Pop AZ, Female, Initial     
    AZ_init_X = exp(sp1$a[,1,1,1]) - exp(sp1$a[,1,2,1]), #Pop AZ, Female-Male Initial Contrast
    #Core reversal
    AZ_rev_M = exp(sp1$a[,1,2,2]),                       #Pop AZ, Male, Reversal
    AZ_rev_F = exp(sp1$a[,1,1,2]),                       #Pop AZ, Female, Reversal
    AZ_rev_X = exp(sp1$a[,1,1,2]) - exp(sp1$a[,1,2,2]),  #Pop AZ, Female-Male Reversal Contrast
    #Middle initial
    SB_init_M = exp(sp1$a[,2,2,1]),                      #Pop SB, Male Initial
    SB_init_F = exp(sp1$a[,2,1,1]),                      #Pop SB, Female, Initial
    SB_init_X = exp(sp1$a[,2,1,1]) - exp(sp1$a[,2,2,1]), #Pop SB, Female-Male Initial Contrast
    #Middle reversal
    SB_rev_M = exp(sp1$a[,2,2,2]),                       #Pop SB, Male, Reversal
    SB_rev_F = exp(sp1$a[,2,1,2]),                       #Pop SB, Female, Reversal
    SB_rev_X = exp(sp1$a[,2,1,2]) - exp(sp1$a[,2,2,2]),  #Pop SB, Female-Male Reversal Contrast
    #Edge initial
    WL_init_M = exp(sp1$a[,3,2,1]),                      #Pop WL, Male, Initial
    WL_init_F = exp(sp1$a[,3,1,1]),                      #Pop WL, Female, Initial
    WL_init_X = exp(sp1$a[,3,1,1]) - exp(sp1$a[,3,2,1]), #Pop WL, Female-Male Initial Contrast
    #Edge reversal
    WL_rev_M = exp(sp1$a[,3,2,2]),                       #Pop WL, Male, Reversal
    WL_rev_F = exp(sp1$a[,3,1,2]),                       #Pop WL, Female, Reversal
    WL_rev_X = exp(sp1$a[,3,1,2]) - exp(sp1$a[,3,2,2])   #Pop WL, Female-Male Reversal Contrast
  )
)

#Speed across pop
s_speed_xpop <- as.data.frame(
  list(
    #Initial
    init_M = exp(sp2$a[,2,1]),                     #Male, Initial    
    init_F = exp(sp2$a[,1,1]),                     #Female, Initial
    init_X  = exp(sp2$a[,1,1]) - exp(sp2$a[,2,1]), #Female-Male Initial Contrast
    #Reversal
    rev_M = exp(sp2$a[,2,2]),                      #Male, Reversal                     
    rev_F = exp(sp2$a[,1,2]),                      #Female, Reversal
    rev_X = exp(sp2$a[,1,2]) - exp(sp2$a[,2,2])    #Female-Male Reversal Contrast
  )
)

#Access posterior values
#Switch full
s_switch_full <- as.data.frame(
  list(
    #Core initial
    AZ_init_M = exp(sw1$a[,1,2,1]),                      #Pop AZ, Male, Initial
    AZ_init_F = exp(sw1$a[,1,1,1]),                      #Pop AZ, Female, Initial     
    AZ_init_X = exp(sw1$a[,1,1,1]) - exp(sw1$a[,1,2,1]), #Pop AZ, Female-Male Initial Contrast
    #Core reversal
    AZ_rev_M = exp(sw1$a[,1,2,2]),                       #Pop AZ, Male, Reversal
    AZ_rev_F = exp(sw1$a[,1,1,2]),                       #Pop AZ, Female, Reversal
    AZ_rev_X = exp(sw1$a[,1,1,2]) - exp(sw1$a[,1,2,2]),  #Pop AZ, Female-Male Reversal Contrast
    #Middle initial
    SB_init_M = exp(sw1$a[,2,2,1]),                      #Pop SB, Male, Initial
    SB_init_F = exp(sw1$a[,2,1,1]),                      #Pop SB, Female, Initial
    SB_init_X = exp(sw1$a[,2,1,1]) - exp(sw1$a[,2,2,1]), #Pop SB, Female-Male Initial Contrast
    #Middle reversal
    SB_rev_M = exp(sw1$a[,2,2,2]),                       #Pop SB, Male, Reversal
    SB_rev_F = exp(sw1$a[,2,1,2]),                       #Pop SB, Female, Reversal
    SB_rev_X = exp(sw1$a[,2,1,2]) - exp(sw1$a[,2,2,2]),  #Pop SB, Female-Male Reversal Contrast
    #Edge initial
    WL_init_M = exp(sw1$a[,3,2,1]),                      #Pop WL, Male, Initial
    WL_init_F = exp(sw1$a[,3,1,1]),                      #Pop WL, Female, Initial
    WL_init_X = exp(sw1$a[,3,1,1]) - exp(sw1$a[,3,2,1]), #Pop WL, Female-Male Initial Contrast
    #Edge reversal
    WL_rev_M = exp(sw1$a[,3,2,2]),                       #Pop WL, Male, Reversal
    WL_rev_F = exp(sw1$a[,3,1,2]),                       #Pop WL, Female, Reversal
    WL_rev_X = exp(sw1$a[,3,1,2]) - exp(sw1$a[,3,2,2])   #Pop WL, Female-Male Reversal Contrast
  )
)

#Switch across pop
s_switch_xpop <- as.data.frame(
  list(
    #Initial
    init_M = exp(sw2$a[,2,1]),                     #Male, Initial   
    init_F = exp(sw2$a[,1,1]),                     #Female, Initial
    init_X  = exp(sw2$a[,1,1]) - exp(sw2$a[,2,1]), #Female-Male Initial Contrast
    #Reversal
    rev_M = exp(sw2$a[,2,2]),                      #Male, Reversal
    rev_F = exp(sw2$a[,1,2]),                      #Female, Reversal
    rev_X = exp(sw2$a[,1,2]) - exp(sw2$a[,2,2])    #Female-Male Reversal Contrast
  )
)

#Means for between & across pop Poissons - reported in Table S1 & s2
mu_speed_full <- apply(s_speed_full, 2, mean)
mu_switch_full <- apply(s_switch_full, 2, mean)
mu_speed_xpop <- apply(s_speed_xpop, 2, mean)
mu_switch_xpop <- apply(s_switch_xpop, 2, mean)

#HPDIs for between & across pop Poissons - reported in Table S1 & S2
HPDI_speed_full <- apply(s_speed_full, 2, HPDI)
HPDI_switch_full <- apply(s_switch_full, 2, HPDI)
HPDI_speed_xpop <- apply(s_speed_xpop, 2, HPDI)
HPDI_switch_xpop <- apply(s_switch_xpop, 2, HPDI)

#####################################################################################################################################################
#Computational models post-processing
#####################################################################################################################################################

#Extract posteriors
s <- extract.samples(m)
s2 <- extract.samples(m_xpop)

#Process computational model posterior for obtaining Table S3 & S4 values, as well as for plotting Figure 2
#That is, access posterior target values by population & sex & phase, name accordingly, and put into parameter-specific data frame 

#Phi initial learning
s_phi_init <- as.data.frame(
  list(
    #Core
    AZ_M_I = s$phi[ , 1, 3],                    #Pop AZ, Male/Initial
    AZ_F_I = s$phi[ , 1, 1],                    #Pop AZ, Female/Initial
    AZ_I_X = s$phi[ , 1, 3] - s$phi[ , 1, 1],   #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_I = s$phi[ , 2, 3],                    #Pop SB, Male/Initial
    SB_F_I = s$phi[ , 2, 1],                    #Pop SB, Female/Initial
    SB_I_X = s$phi[ , 2, 3] - s$phi[ , 2, 1],   #Pop SB, Male-Female Contrast
    #Edge
    WL_M_I = s$phi[ , 3, 3],                    #Pop WL, Male/Initial
    WL_F_I = s$phi[ , 3, 1],                    #Pop WL, Female/Initial
    WL_I_X = s$phi[ , 3, 3] - s$phi[ , 3, 1],   #Pop WL, Male-Female Contrast
    #Across
    Pop_I_X = s2$phi[ , 2, 1] - s2$phi[ , 1, 1] #Across Population Male-Female Contrast
  )
)

#Phi reversal learning
s_phi_rev <- as.data.frame(
  list(
    #Core
    AZ_M_R = s$phi[ , 1, 4],                    #Pop AZ, Male/Reversal
    AZ_F_R = s$phi[ , 1, 2],                    #Pop AZ, Female/Reversal
    AZ_R_X = s$phi[ , 1, 4] - s$phi[ , 1, 2],   #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_R = s$phi[ , 2, 4],                    #Pop SB, Male/Reversal
    SB_F_R = s$phi[ , 2, 2],                    #Pop SB, Female/Reversal
    SB_R_X = s$phi[ , 2, 4] - s$phi[ , 2, 2],   #Pop SB, Male-Female Contrast
    #Edge
    WL_M_R = s$phi[ , 3, 4],                    #Pop WL, Male/Reversal
    WL_F_R = s$phi[ , 3, 2],                    #Pop WL, Female/Reversal
    WL_R_X = s$phi[ , 3, 4] - s$phi[ , 3, 2],   #Pop WL, Male-Female Contrast
    #Across
    Pop_R_X = s2$phi[ , 2, 2] - s2$phi[ , 1, 2] #Across Population Male-Female Contrast
  )
)

#Lambda initial learning
s_L_init <- as.data.frame(
  list(
    #Core
    AZ_M_I = s$lambda[ , 1, 3],                       #Pop AZ, Male/Initial
    AZ_F_I = s$lambda[ , 1, 1],                       #Pop AZ, Female/Initial
    AZ_I_X = s$lambda[ , 1, 3] - s$lambda[ , 1, 1],   #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_I = s$lambda[ , 2, 3],                       #Pop SB, Male/Initial
    SB_F_I = s$lambda[ , 2, 1],                       #Pop SB, Female/Initial
    SB_I_X = s$lambda[ , 2, 3] - s$lambda[ , 2, 1],   #Pop SB, Male-Female Contrast
    #Edge
    WL_M_I = s$lambda[ , 3, 3],                       #Pop WL, Male/Initial
    WL_F_I = s$lambda[ , 3, 1],                       #Pop WL, Female/Initial
    WL_I_X = s$lambda[ , 3, 3] - s$lambda[ , 3, 1],   #Pop WL, Male-Female Contrast
    #Across
    Pop_I_X = s2$lambda[ , 2, 1] - s2$lambda[ , 1, 1] #Across Population Male-Female Contrast
  )
)

#Lambda reversal learning
s_L_rev <- as.data.frame(
  list(
    #Core
    AZ_M_R = s$lambda[ , 1, 4],                       #Pop SB, Male/Reversal
    AZ_F_R = s$lambda[ , 1, 2],                       #Pop SB, Female, Reversal
    AZ_R_X = s$lambda[ , 1, 4] - s$lambda[ , 1, 2],   #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_R = s$lambda[ , 2, 4],                       #Pop AZ, Male/Reversal
    SB_F_R = s$lambda[ , 2, 2],                       #Pop AZ, Female/Reversal
    SB_R_X = s$lambda[ , 2, 4] - s$lambda[ , 2, 2],   #Pop SB, Male-Female Contrast
    #Edge
    WL_M_R = s$lambda[ , 3, 4],                       #Pop WL, Male/Reversal
    WL_F_R = s$lambda[ , 3, 2],                       #Pop WL, Female/Reversal
    WL_R_X = s$lambda[ , 3, 4] - s$lambda[ , 3, 2],   #Pop WL, Male-Female Contrast
    #Across
    Pop_R_X = s2$lambda[ , 2, 2] - s2$lambda[ , 1, 2] #Across Population Male-Female Contrast
  )
)

#Means for between & across pop computations - reported in Table S3 & s4
mu_phi_init <- apply(s_phi_init, 2, mean)
mu_phi_rev <- apply(s_phi_rev, 2, mean)
mu_L_init <- apply(s_L_init, 2, mean)
mu_L_rev <- apply(s_L_rev, 2, mean)

#HPDIs for between & across pop Poissons - reported in Table S3 & S4
HPDI_phi_init <- apply(s_phi_init, 2, HPDI)
HPDI_phi_rev <- apply(s_phi_rev, 2, HPDI)
HPDI_L_init <- apply(s_L_init, 2, HPDI)
HPDI_L_rev <- apply(s_L_rev, 2, HPDI)

#End script
#####################################################################################################################################################

