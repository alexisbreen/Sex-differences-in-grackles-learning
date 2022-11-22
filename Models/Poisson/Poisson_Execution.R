#####################################################################################################################################################

#Code to run multi-level Bayesian Poisson models for the manuscript

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#Load required packages
library(tidyverse)
library(rethinking)
library(rstan)

#Load data (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T) 

#Sheet to use for Poisson estimates
d_Poisson <- d %>% filter(Criterion != 2) #No extra learning trials included

#####################################################################################################################################################
#Poisson regressions pre-processing
#####################################################################################################################################################

#Prepare data for speed Poisson

#Empty vectors
trials_initial  <- rep(NA, length(unique(d_Poisson$id)))
trials_reversal <- rep(NA, length(unique(d_Poisson$id)))
sex <-  rep(NA, length(unique(d_Poisson$id)))
pop <- rep(NA, length(unique(d_Poisson$id)))
skip_initial <- rep(NA, length(unique(d_Poisson$id)))
skip_reversal <- rep(NA, length(unique(d_Poisson$id)))

#Fill empty vectors
for (id in 1:max(d_Poisson$id)) {
  trials_initial[id] <- max(d_Poisson$Trial[which(d_Poisson$id == id & d_Poisson$Phase == 1)])
  trials_reversal[id] <- max(d_Poisson$Trial[which(d_Poisson$id == id & d_Poisson$Phase == 2)])
  sex[id] <- unique(d_Poisson$sex[d_Poisson$id == id])
  pop[id] <- unique(d_Poisson$Population[d_Poisson$id == id])
  skip_initial[id] <- max(d_Poisson$skip[which(d_Poisson$id == id & d_Poisson$Phase == 1)])
  skip_reversal[id] <- max(d_Poisson$skip[which(d_Poisson$id == id & d_Poisson$Phase == 2)])
}

#Put vectors into list for Stan 
d_Poisson_speed = list(trials = c(trials_initial,trials_reversal), 
                       sex = c(sex, sex),
                       pop = c(pop,pop),
                       id = rep(1:49,2),
                       phase = as.integer(c(rep(1,49),rep(2,49))),
                       skip = c(skip_initial,skip_reversal))

#Prepare data for switch Poisson

#Empty vectors
switch_initial  <- rep(NA, length(unique(d_Poisson$id)))
switch_reversal <- rep(NA, length(unique(d_Poisson$id)))
sex <-  rep(NA, length(unique(d_Poisson$id)))
pop <-rep(NA, length(unique(d_Poisson$id)))
skip_initial <- rep(NA, length(unique(d_Poisson$id)))
skip_reversal <- rep(NA, length(unique(d_Poisson$id)))

#Fill empty vectors
for (id in 1:max(d_Poisson$id)) {
  switch_initial[id] <- sum(d_Poisson$switch[which(d_Poisson$id == id & d_Poisson$Phase == 1)])
  switch_reversal[id] <- sum(d_Poisson$switch[which(d_Poisson$id == id & d_Poisson$Phase == 2)])
  sex[id] <- unique(d_Poisson$sex[d_Poisson$id ==id])
  pop[id] <- unique(d_Poisson$Population[d_Poisson$id == id])
  skip_initial[id] <- max(d_Poisson$skip[which(d_Poisson$id == id & d_Poisson$Phase == 1)])
  skip_reversal[id] <- max(d_Poisson$skip[which(d_Poisson$id == id & d_Poisson$Phase == 2)])
}

#Put vectors into list for Stan 
d_Poisson_switch = list(switches = c(switch_initial,switch_reversal), 
                        sex = c(sex, sex),
                        pop = c(pop,pop),
                        id = rep(1:49,2),
                        phase = as.integer(c(rep(1,49),rep(2,49))),
                        skip = c(skip_initial,skip_reversal))

#####################################################################################################################################################
#Run models
#####################################################################################################################################################

#Run stan models 

m_speed_full <- stan(file = "Poisson_Speed_Full.stan", data = d_Poisson_speed, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_speed_xpop <- stan(file = "Poisson_Speed_Across_Pop.stan", data = d_Poisson_speed, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_switch_full <- stan(file = "Poisson_Switch_Full.stan", data = d_Poisson_switch, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_switch_xpop <- stan(file = "Poisson_Switch_Across_Pop.stan", data = d_Poisson_switch, iter = 5000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))

#Inspect effective model samples 
precis(m_speed_full, depth = 3, pars = c("a"))
precis(m_speed_xpop, depth = 3, pars = c("a"))
precis(m_switch_full, depth = 3, pars = c("a"))
precis(m_switch_xpop, depth = 3, pars = c("a"))

#####################################################################################################################################################
#Poisson regressions post-processing
#####################################################################################################################################################

#Extract posteriors
sp1 <- extract.samples(m_speed_full)
sp2 <- extract.samples(m_speed_xpop)
sw1 <- extract.samples(m_switch_full)
sw2 <- extract.samples(m_switch_xpop)

#Process Poisson models for obtaining posterior estimates by population & sex & phase
#Exponentiate to get back on outcome scale

#Speed full
s_speed_full <- as.data.frame(
  list(
    #Core initial
    AZ_init_M = exp(sp1$a[,1,2,1]),                      #Pop AZ, Male, Initial    
    AZ_init_F = exp(sp1$a[,1,1,1]),                      #Pop AZ, Female, Initial     
    AZ_init_X = exp(sp1$a[,1,2,1]) - exp(sp1$a[,1,1,1]), #Pop AZ, Male-Female Initial Contrast
    #Core reversal
    AZ_rev_M = exp(sp1$a[,1,2,2]),                       #Pop AZ, Male, Reversal
    AZ_rev_F = exp(sp1$a[,1,1,2]),                       #Pop AZ, Female, Reversal
    AZ_rev_X = exp(sp1$a[,1,2,2]) - exp(sp1$a[,1,1,2]),  #Pop AZ, Male-Female Initial Contrast
    #Middle initial
    SB_init_M = exp(sp1$a[,2,2,1]),                      #Pop SB, Male Initial
    SB_init_F = exp(sp1$a[,2,1,1]),                      #Pop SB, Female, Initial
    SB_init_X = exp(sp1$a[,2,2,1]) - exp(sp1$a[,2,1,1]), #Pop SB, Male-Female Initial Contrast
    #Middle reversal
    SB_rev_M = exp(sp1$a[,2,2,2]),                       #Pop SB, Male, Reversal
    SB_rev_F = exp(sp1$a[,2,1,2]),                       #Pop SB, Female, Reversal
    SB_rev_X = exp(sp1$a[,2,2,2]) - exp(sp1$a[,2,1,2]),  #Pop SB, Male-Female Initial Contrast
    #Edge initial
    WL_init_M = exp(sp1$a[,3,2,1]),                      #Pop WL, Male, Initial
    WL_init_F = exp(sp1$a[,3,1,1]),                      #Pop WL, Female, Initial
    WL_init_X = exp(sp1$a[,3,2,1]) - exp(sp1$a[,3,1,1]), #Pop WL, Male-Female Initial Contrast
    #Edge reversal
    WL_rev_M = exp(sp1$a[,3,2,2]),                       #Pop WL, Male, Reversal
    WL_rev_F = exp(sp1$a[,3,1,2]),                       #Pop WL, Female, Reversal
    WL_rev_X = exp(sp1$a[,3,2,2]) - exp(sp1$a[,3,1,2])   #Pop WL, Male-Female Initial Contrast
  )
)

#Speed across pop
s_speed_xpop <- as.data.frame(
  list(
    #Initial
    init_M = exp(sp2$a[,2,1]),                     #Male, Initial    
    init_F = exp(sp2$a[,1,1]),                     #Female, Initial
    init_X  = exp(sp2$a[,2,1]) - exp(sp2$a[,1,1]), #Male-Female Initial Contrast
    #Reversal
    rev_M = exp(sp2$a[,2,2]),                      #Male, Reversal                     
    rev_F = exp(sp2$a[,1,2]),                      #Female, Reversal
    rev_X = exp(sp2$a[,2,2]) - exp(sp2$a[,1,2])    #Male-Female Initial Contrast
  )
)

#Switch full
s_switch_full <- as.data.frame(
  list(
    #Core initial
    AZ_init_M = exp(sw1$a[,1,2,1]),                      #Pop AZ, Male, Initial
    AZ_init_F = exp(sw1$a[,1,1,1]),                      #Pop AZ, Female, Initial     
    AZ_init_X = exp(sw1$a[,1,2,1]) - exp(sw1$a[,1,1,1]), #Pop AZ, Male-Female Initial Contrast
    #Core reversal
    AZ_rev_M = exp(sw1$a[,1,2,2]),                       #Pop AZ, Male, Reversal
    AZ_rev_F = exp(sw1$a[,1,1,2]),                       #Pop AZ, Female, Reversal
    AZ_rev_X = exp(sw1$a[,1,2,2]) - exp(sw1$a[,1,1,2]),  #Pop AZ, Male-Female Initial Contrast
    #Middle initial
    SB_init_M = exp(sw1$a[,2,2,1]),                      #Pop SB, Male, Initial
    SB_init_F = exp(sw1$a[,2,1,1]),                      #Pop SB, Female, Initial
    SB_init_X = exp(sw1$a[,2,2,1]) - exp(sw1$a[,2,1,1]), #Pop SB, Male-Female Initial Contrast
    #Middle reversal
    SB_rev_M = exp(sw1$a[,2,2,2]),                       #Pop SB, Male, Reversal
    SB_rev_F = exp(sw1$a[,2,1,2]),                       #Pop SB, Female, Reversal
    SB_rev_X = exp(sw1$a[,2,2,2]) - exp(sw1$a[,2,1,2]),  #Pop SB, Male-Female Initial Contrast
    #Edge initial
    WL_init_M = exp(sw1$a[,3,2,1]),                      #Pop WL, Male, Initial
    WL_init_F = exp(sw1$a[,3,1,1]),                      #Pop WL, Female, Initial
    WL_init_X = exp(sw1$a[,3,2,1]) - exp(sw1$a[,3,1,1]), #Pop WL, Male-Female Initial Contrast
    #Edge reversal
    WL_rev_M = exp(sw1$a[,3,2,2]),                       #Pop WL, Male, Reversal
    WL_rev_F = exp(sw1$a[,3,1,2]),                       #Pop WL, Female, Reversal
    WL_rev_X = exp(sw1$a[,3,2,2]) - exp(sw1$a[,3,1,2])   #Pop WL, Male-Female Initial Contrast
  )
)

#Switch across pop
s_switch_xpop <- as.data.frame(
  list(
    #Initial
    init_M = exp(sw2$a[,2,1]),                     #Male, Initial   
    init_F = exp(sw2$a[,1,1]),                     #Female, Initial
    init_X  = exp(sw2$a[,2,1]) - exp(sw2$a[,1,1]), #Male-Female Initial Contrast
    #Reversal
    rev_M = exp(sw2$a[,2,2]),                      #Male, Reversal
    rev_F = exp(sw2$a[,1,2]),                      #Female, Reversal
    rev_X = exp(sw2$a[,2,2]) - exp(sw2$a[,1,2])    #Male-Female Initial Contrast
  )
)

#Means for between & across pop Poissons 
mu_speed_full <- apply(s_speed_full, 2, mean)
mu_switch_full <- apply(s_switch_full, 2, mean)
mu_speed_xpop <- apply(s_speed_xpop, 2, mean)
mu_switch_xpop <- apply(s_switch_xpop, 2, mean)

#HPDIs for between & across pop Poissons 
HPDI_speed_full <- apply(s_speed_full, 2, HPDI)
HPDI_switch_full <- apply(s_switch_full, 2, HPDI)
HPDI_speed_xpop <- apply(s_speed_xpop, 2, HPDI)
HPDI_switch_xpop <- apply(s_switch_xpop, 2, HPDI)

#End script
#####################################################################################################################################################

