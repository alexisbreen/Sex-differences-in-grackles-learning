#####################################################################################################################################################

#Code to run reinforcement learning models for the manuscript

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#Make sure that you are in the correct working directory (".../Sex-differences-in-grackles-learning/Models/Reinforcement learning")

#Load required packages
library(tidyverse)
library(rethinking)
library(rstan)

#Load data (file name: Grackle_data_clean.csv)
d <- read.csv(file.choose(), header = T)

#Sheet to use for initial reinforcement learning model estimates
d_init_RL <- d %>% filter(Criterion != 2) #No extra learning trials included

#Sheet to use for reversal reinforcement learning model estimates
d_rev_RL <- d %>% filter(drop != 1) #No extra learning trials included in reversal, but estimates begin from actual switch point i.e., true attractions 

#####################################################################################################################################################
#Reinforcement learning models pre-processing
#####################################################################################################################################################

#Put data into a list for the STAN model
#Initial
d_stan_init <- list(N = nrow(d_init_RL),
                    N_id = length(unique(d_init_RL$id)),
                    N_pop = length(unique(d_init_RL$Population)),
                    id = d_init_RL$id,
                    pop = d_init_RL$Population,
                    phase = d_init_RL$Phase,
                    sex_phase = d_init_RL$sex_phase, 
                    sex = d_init_RL$sex,
                    Choice = d_init_RL$Choice,
                    Correct = d_init_RL$Correct
)

#Reversal
d_stan_rev <- list(N = nrow(d_rev_RL),
                   N_id = length(unique(d_rev_RL$id)),
                   N_pop = length(unique(d_rev_RL$Population)),
                   id = d_rev_RL$id,
                   pop = d_rev_RL$Population,
                   phase = d_rev_RL$Phase,
                   sex_phase = d_rev_RL$sex_phase, 
                   sex = d_rev_RL$sex,
                   Choice = d_rev_RL$Choice,
                   Correct = d_rev_RL$Correct
)
#####################################################################################################################################################
#Run models
#####################################################################################################################################################

#Run stan models
m_full_init <- stan(file = "RL_Comp_Full.stan", data = d_stan_init, iter = 2000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_full_rev <- stan(file = "RL_Comp_Full.stan", data = d_stan_rev, iter = 2000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_xpop_init <- stan(file = "RL_Comp_Across_Pop.stan", data = d_stan_init, iter = 2000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))
m_xpop_rev <- stan(file = "RL_Comp_Across_Pop.stan", data = d_stan_rev, iter = 2000, cores = 4, chains = 4, refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))

#Inspect effective model samples 
precis(m_full_init, depth = 3, pars = c("phi", "lambda"))
precis(m_full_rev, depth = 3, pars = c("phi", "lambda"))
precis(m_xpop_init, depth = 3, pars = c("phi", "lambda"))
precis(m_xpop_rev, depth = 3, pars = c("phi", "lambda"))

#####################################################################################################################################################
#Reinforcement learning models post-processing
#####################################################################################################################################################

#Extract posteriors
s_full_init <- extract.samples(m_full_init)
s_full_rev <- extract.samples(m_full_rev)
s_xpop_init <- extract.samples(m_xpop_init)
s_xpop_rev <- extract.samples(m_xpop_rev)

#Reinforcement learning models for obtaining posterior estimates by population & sex & phase

#Phi initial learning no extra trials included
s_phi_init <- as.data.frame(
  list(
    #Core
    AZ_M_I = s_full_init$phi[ , 1, 3],                            #Pop AZ, Male/Initial
    AZ_F_I = s_full_init$phi[ , 1, 1],                            #Pop AZ, Female/Initial
    AZ_I_X = s_full_init$phi[ , 1, 3] - s_full_init$phi[ , 1, 1], #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_I = s_full_init$phi[ , 2, 3],                            #Pop SB, Male/Initial
    SB_F_I = s_full_init$phi[ , 2, 1],                            #Pop SB, Female/Initial
    SB_I_X = s_full_init$phi[ , 2, 3] -s_full_init$phi[ , 2, 1],  #Pop SB, Male-Female Contrast
    #Edge
    WL_M_I = s_full_init$phi[ , 3, 3],                            #Pop WL, Male/Initial
    WL_F_I = s_full_init$phi[ , 3, 1],                            #Pop WL, Female/Initial
    WL_I_X = s_full_init$phi[ , 3, 3] - s_full_init$phi[ , 3, 1], #Pop WL, Male-Female Contrast
    #Across
    Pop_I_M = s_xpop_init$phi[ , 2, 1],                           #Male/Initial
    Pop_I_F = s_xpop_init$phi[ , 1, 1],                           #Female/Initial, Male
    Pop_I_X = s_xpop_init$phi[ , 2, 1] - s_xpop_init$phi[ , 1, 1] #Across Population Male-Female Contrast
  )
)

#Phi initial learning extra trials included
#Using s_full_rev & s_xpop_rev b/c includes initial extra trials - see L. 24
#Use these estimates to show extra initial learning trials have negligible if no carry-over effect - see Supplementary Text
s_phi_init2 <- as.data.frame(
  list(
    #Core
    AZ_M_I = s_full_rev$phi[ , 1, 3],                           #Pop AZ, Male/Initial
    AZ_F_I = s_full_rev$phi[ , 1, 1],                           #Pop AZ, Female/Initial
    AZ_I_X = s_full_rev$phi[ , 1, 3] - s_full_rev$phi[ , 1, 1], #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_I = s_full_rev$phi[ , 2, 3],                           #Pop SB, Male/Initial
    SB_F_I = s_full_rev$phi[ , 2, 1],                           #Pop SB, Female/Initial
    SB_I_X = s_full_rev$phi[ , 2, 3] - s_full_rev$phi[ , 2, 1], #Pop SB, Male-Female Contrast
    #Edge
    WL_M_I = s_full_rev$phi[ , 3, 3],                           #Pop WL, Male/Initial
    WL_F_I = s_full_rev$phi[ , 3, 1],                           #Pop WL, Female/Initial
    WL_I_X = s_full_rev$phi[ , 3, 3] - s_full_rev$phi[ , 3, 1], #Pop WL, Male-Female Contrast
    #Across
    Pop_I_X = s_xpop_rev$phi[ , 2, 1] - s_xpop_rev$phi[ , 1, 1] #Across Population Male-Female Contrast
  )
)

#Phi reversal learning no extra trials included
s_phi_rev <- as.data.frame(
  list(
    #Core
    AZ_M_R = s_full_rev$phi[ , 1, 4],                           #Pop AZ, Male/Reversal
    AZ_F_R = s_full_rev$phi[ , 1, 2],                           #Pop AZ, Female/Reversal
    AZ_R_X = s_full_rev$phi[ , 1, 4] - s_full_rev$phi[ , 1, 2], #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_R = s_full_rev$phi[ , 2, 4],                           #Pop SB, Male/Reversal
    SB_F_R = s_full_rev$phi[ , 2, 2],                           #Pop SB, Female/Reversal
    SB_R_X = s_full_rev$phi[ , 2, 4] - s_full_rev$phi[ , 2, 2], #Pop SB, Male-Female Contrast
    #Edge
    WL_M_R = s_full_rev$phi[ , 3, 4],                           #Pop WL, Male/Reversal
    WL_F_R = s_full_rev$phi[ , 3, 2],                           #Pop WL, Female/Reversal
    WL_R_X = s_full_rev$phi[ , 3, 4] - s_full_rev$phi[ , 3, 2], #Pop WL, Male-Female Contrast
    #Across
    Pop_R_M = s_xpop_rev$phi[ , 2, 2],                          #Male/reversal
    Pop_R_F = s_xpop_rev$phi[ , 1, 2],                          #Female/reversal
    Pop_R_X = s_xpop_rev$phi[ , 2, 2] - s_xpop_rev$phi[ , 1, 2] #Across Population Male-Female Contrast
  )
)

#Lambda initial learning no extra trials included
s_L_init <- as.data.frame(
  list(
    #Core
    AZ_M_I = s_full_init$lambda[ , 1, 3],                               #Pop AZ, Male/Initial
    AZ_F_I = s_full_init$lambda[ , 1, 1],                               #Pop AZ, Female/Initial
    AZ_I_X = s_full_init$lambda[ , 1, 3] - s_full_init$lambda[ , 1, 1], #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_I = s_full_init$lambda[ , 2, 3],                               #Pop SB, Male/Initial
    SB_F_I = s_full_init$lambda[ , 2, 1],                               #Pop SB, Female/Initial
    SB_I_X = s_full_init$lambda[ , 2, 3] - s_full_init$lambda[ , 2, 1], #Pop SB, Male-Female Contrast
    #Edge
    WL_M_I = s_full_init$lambda[ , 3, 3],                               #Pop WL, Male/Initial
    WL_F_I = s_full_init$lambda[ , 3, 1],                               #Pop WL, Female/Initial
    WL_I_X = s_full_init$lambda[ , 3, 3] - s_full_init$lambda[ , 3, 1], #Pop WL, Male-Female Contrast
    #Across
    Pop_I_M = s_xpop_init$lambda[ , 2, 1],                              #Male/Initial
    Pop_I_F = s_xpop_init$lambda[ , 1, 1],                              #Female/Initial
    Pop_I_X = s_xpop_init$lambda[ , 2, 1] - s_xpop_init$lambda[ , 1, 1] #Across Population Male-Female Contrast
  )
)

#Lambda initial learning extra trials included
#Using s_full_rev & s_xpop_rev b/c includes initial extra trials - see L. 24
#Use these estimates to show extra initial learning trials have negligible if no carry-over effect - see Supplementary Text
s_L_init2 <- as.data.frame(
  list(
    #Core
    AZ_M_I = s_full_rev$lambda[ , 1, 3],                              #Pop AZ, Male/Initial
    AZ_F_I = s_full_rev$lambda[ , 1, 1],                              #Pop AZ, Female/Initial
    AZ_I_X = s_full_rev$lambda[ , 1, 3] - s_full_rev$lambda[ , 1, 1], #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_I = s_full_rev$lambda[ , 2, 3],                              #Pop SB, Male/Initial
    SB_F_I = s_full_rev$lambda[ , 2, 1],                              #Pop SB, Female/Initial
    SB_I_X = s_full_rev$lambda[ , 2, 3] - s_full_rev$lambda[ , 2, 1], #Pop SB, Male-Female Contrast
    #Edge
    WL_M_I = s_full_rev$lambda[ , 3, 3],                              #Pop WL, Male/Initial
    WL_F_I = s_full_rev$lambda[ , 3, 1],                              #Pop WL, Female/Initial
    WL_I_X = s_full_rev$lambda[ , 3, 3] - s_full_rev$lambda[ , 3, 1], #Pop WL, Male-Female Contrast
    #Across
    Pop_I_X = s_xpop_rev$lambda[ , 2, 1] - s_xpop_rev$lambda[ , 1, 1] #Across Population Male-Female Contrast
  )
)

#Lambda reversal learning no extra trials included
s_L_rev <- as.data.frame(
  list(
    #Core
    AZ_M_R = s_full_rev$lambda[ , 1, 4],                              #Pop SB, Male/Reversal
    AZ_F_R = s_full_rev$lambda[ , 1, 2],                              #Pop SB, Female, Reversal
    AZ_R_X = s_full_rev$lambda[ , 1, 4] - s_full_rev$lambda[ , 1, 2], #Pop AZ, Male-Female Contrast
    #Middle
    SB_M_R = s_full_rev$lambda[ , 2, 4],                              #Pop AZ, Male/Reversal
    SB_F_R = s_full_rev$lambda[ , 2, 2],                              #Pop AZ, Female/Reversal
    SB_R_X = s_full_rev$lambda[ , 2, 4] - s_full_rev$lambda[ , 2, 2], #Pop SB, Male-Female Contrast
    #Edge
    WL_M_R = s_full_rev$lambda[ , 3, 4],                              #Pop WL, Male/Reversal
    WL_F_R = s_full_rev$lambda[ , 3, 2],                              #Pop WL, Female/Reversal
    WL_R_X = s_full_rev$lambda[ , 3, 4] - s_full_rev$lambda[ , 3, 2], #Pop WL, Male-Female Contrast
    #Across
    Pop_R_M = s_xpop_rev$lambda[ , 2, 2],                             #Male/reversal
    Pop_R_F = s_xpop_rev$lambda[ , 1, 2],                             #Female/reversal
    Pop_R_X = s_xpop_rev$lambda[ , 2, 2] - s_xpop_rev$lambda[ , 1, 2] #Across Population Male-Female Contrast
  )
)

#Means for between & across pop computations 
mu_phi_init <- apply(s_phi_init, 2, mean)
mu_phi_init2 <- apply(s_phi_init2, 2, mean)
mu_phi_rev <- apply(s_phi_rev, 2, mean)
mu_L_init <- apply(s_L_init, 2, mean)
mu_L_init2 <- apply(s_L_init2, 2, mean)
mu_L_rev <- apply(s_L_rev, 2, mean)

#HPDIs for between & across pop computations
HPDI_phi_init <- apply(s_phi_init, 2, HPDI)
HPDI_phi_init2 <- apply(s_phi_init2, 2, HPDI)
HPDI_phi_rev <- apply(s_phi_rev, 2, HPDI)
HPDI_L_init <- apply(s_L_init, 2, HPDI)
HPDI_L_init2 <- apply(s_L_init2, 2, HPDI)
HPDI_L_rev <- apply(s_L_rev, 2, HPDI)

#End script
######################################################################################################################################################
