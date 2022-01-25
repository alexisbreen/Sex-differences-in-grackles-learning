#Begin script for:

#Investigating sex differences in learning in a range-expanding bird

#Alexis J. Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#This script contains reproducible code for our data simulation, reinforcement learning model, and figures 

#####################################################################################################################################################
#Required packages
#####################################################################################################################################################

library(rethinking) #For Bayesian reinforcement learning model
library(tidyverse)  #For data wrangling
library(ggplot2)    #For graphing
library(cowplot)    #For combining plots

#####################################################################################################################################################
#Simulation for Grackle reversal learning
#####################################################################################################################################################

#We assume males (1) are faster in initial and reversal color-reward reinforcement learning; and (2) are more deterministic in their learning

#Simulation function
#Define function & input parameters
Sim_fct <- function(N_block = 30,                                 #Maximum number of blocks (10 trials per block)
                    N_trials = 10,                                #Trials per block   
                    sd_pop_phi = 0.1,                             #Population effects for phi
                    sd_id_phi  = 0.1,                             #Individual effects for phi
                    sd_pop_lambda = 0.1,                          #Population effects for lambda
                    sd_id_lambda  = 0.1,                          #Individual effects for lambda
                    real_phi = rbind(c(0.03,0.05), c(0.09,0.11)), #Mean Updating/learning rate (f initial, f reversal),(m initial, m reversal)
                    real_lambda = rbind(c(2,3), c(4,5)) )         #Mean Exploration rate (f initial, f reversal),(m initial, m reversal)
                     
  
{ #Define function operations
  
  #We simulate data corresponding to the format of the actual data
  N_pop <- 3                     #Study site number
  N_per_site <- matrix(0, 3, 2)  #Empty matrix to hold no. male-female birds per population; rows are populations; column 1 is number of females; column 2 of males
  N_per_site[1,] <- c(4,3)       #No. female-male birds in study site 1
  N_per_site[2,] <- c(5,22)      #No. female-male birds in study site 2
  N_per_site[3,] <- c(5,10)      #No. female-male birds in study site 3
  
  #Define unique bird ID parameter based on above matrix 
  N_id <- sum(N_per_site)
  
  #Define scale of our phi and lambda values: We transform to latent logit/log scale to add random offset for each individual and population (see L60 - 61)
  Logit_phi  <- logit(real_phi)
  Log_lambda <- log(real_lambda)
  
  #Create overall output object
  d_Overall <- c() 
  
  #Begin 'following' birds through initial and reversal learning
  #Loop over populations
  for (pop in 1:N_pop){ #For each population in our three populations...
    
    #Generate random population and individual-level variation within each population for phi & lambda values
    phi_offset    <- rnorm(1, 0, sd_pop_phi)    + rnorm(sum(N_per_site[pop,]), 0, sd_id_phi)
    lambda_offset <- rnorm(1, 0, sd_pop_lambda) + rnorm(sum(N_per_site[pop,]), 0, sd_id_lambda)
    
    #Loop over birds
    for (ind in 1:sum(N_per_site[pop,])) { #For every bird in each population...
      
      #Assign a unique phi and lambda value by offsetting with the above randomly generated standard deviation
      #We transform learning parameters back to original scale to be used later in the model
      phi  <- inv_logit(Logit_phi + phi_offset[ind])
      lambda <- exp(Log_lambda + lambda_offset[ind])
      
      #Set initial attractions to 0.1
      #This allows birds to begin with some attraction to the tubes, which is expected from their pre-test habituation to baited tubes
      #And it allows attraction scores to decrease when a 'wrong' choice is made early on                
      A <- c(0.1,0.1)
      
      #Create output matrix to record choices and payoffs
      #id and sex values are assigned based on the number and sex of birds we expect to see in real data
      d <- data.frame(id = sum(N_per_site[which(1:3 < pop) ,]) + ind, sex = ifelse(ind <= N_per_site[pop,1], 1, 2), poss_trials = 1:(N_block*N_trials), trial = NA, trialafter = NA, Choice = NA, Payoff = NA, Phase = NA, Block = NA, Pop = pop)
      
      #Define some local variables used in below trial loop
      phase <- 1     #Starting phase (1 = initial)
      i <- 0         #Counter for trial number
      Switch <- c()  #Empty vector to fill with at what trial number birds switch b/n phases
      criterion <- 0 #Counter for times bird passes criterion; 0 = bird has not yet passed initial; 1 = bird has passed initial; 2 = bird has passed initial & reversal; 1 & 2 states are defined/determined below
      
      #Loop over trials
      for (block in 1:N_block) {                                   #For every block in all blocks and...
        for (trial in 1:N_trials) {                                #For every trial in all trials...
          i <- i+1                                                 #Begin at trial number 1
          d$trial[i] <- i                                          #Assign non-interrupted (by phase switch) trial number to data frame i.e., continuous trial count ACROSS initial & reversal phases
          d$Block[i] <- block                                      #Assign block number to data frame
          d$Phase[i] <- phase                                      #Assign phase number to data frame
          d$trialafter[i] <- ifelse(criterion == 0, i, i - Switch) #Assign interrupted (by phase switch) trial number to data frame i.e., continuous trial count for EACH initial & reversal phase
          
          #Get choice probabilities based on attraction scores
          Prob <- c()                                                                                                  #Empty vector to fill with our choice probabilities
          for (j in 1:2) Prob[j] <- exp(lambda[d$sex[i], d$Phase[i]] *A[j]) / sum(exp(lambda[d$sex[i], d$Phase[i]]*A)) #For choice-option j in our two choice-option environment, the probability of choice-option j is defined by equation 2
          
          #Make choice proportional to attraction scores
          d$Choice[which(d$trial == i)] <- sample(c(1:2), size = 1, prob = Prob) #Make the bird choose among our two choice-options, based on the choice probabilities defined above                
          
          #Determine Payoff based on Phase and Choice
          if (d$Phase[i] == 1){    #If phase within our data frame is phase 1 i.e., initial
            if (d$Choice[i] == 1){ #And if the choice within that initial phase is 1 i.e., correct 
              d$Payoff[i] <- 1     #The payoff is assigned a value of 1 i.e., rewarded
            } else {               #But if the choice within that initial phase is not 1 but 2 i.e., incorrect
              d$Payoff[i] <- 0     #The payoff is assigned a value of 0 i.e., unrewarded
            }                      #End determination of payoff based on choice within phase 1
          } else {                 #If phase is not phase 1 but phase 2 i.e., reversal
            if (d$Choice[i] == 1){ #And if the choice within that reversal phase is 1 i.e., incorrect (because now the rewarded option has switched)
              d$Payoff[i] <- 0     #The payoff within that data frame is assigned a value of 0 i.e., unrewarded
            } else {               #But if the choice within that reversal phase is not 1 but 2 i.e., correct
              d$Payoff[i] <- 1     #The payoff is assigned a value of 1 i.e., rewarded
            }                      #End determination of payoff based on choice within phase 2
          }                        #End determination of payoff 
          
          #Update Attractions (only for chosen option) 
          A[d$Choice[i]] <- (1-phi[d$sex[i], d$Phase[i]]) * A[d$Choice[i]] + phi[d$sex[i], d$Phase[i]] * d$Payoff[i] #Define equation 1 i.e., our attraction updating equation
          
          #Set phase of the experiment (1 = initial, 2 = reversal) based on the passing criterion
          #Criterion = correct choice in at least 17 out of the most recent 20 trials, with a minimum of eight and nine correct choices across the last two blocks
          stop <- 0                                                                                                                #Define variable that will dictate when we stop current experimental block
          if (i > 21){                                                                                                             #If trial number is larger than 21
            if (sum(d$Payoff[d$trial[(i-20):i]]) >= 17){                                                                           #And if, when we tally the payoffs across the most recent 20 trials, that tally equals or exceeds 17
              if ( (sum(d$Payoff[which(d$Block == d$Block[i])]) >= 8) & (sum(d$Payoff[which(d$Block == (d$Block[i]-1))]) >= 8 ) ){ #And if, when we tally the payoffs within EACH of the two trial blocks containing those most recent 20 trials (10 trials/block), that tally for either block equals or exceeds 8           
                criterion <- criterion + 1                                                                                         #Reassign criterion to next phase i.e., bird has passed initial; see line 84
                phase <- 2                                                                                                         #Reassign phase to phase 2 rather than phase 1
                stop <- 1                                                                                                          #Reassign stop point; 1 = we are no longer determining 
                Switch <- i                                                                                                        #Assign the switch point to the trial number at which the bird passed initial
              }                                                                                                                    
            }                                                                                                                      
          }                                                                                                                        #End determination of phase
          
          if (stop == 1) break #Statement to stop current block, because criterion is reached
          
        } #End looping over trials within block
        
        if (criterion == 2) break #Statement to stop experiment for this bird
        
      } #End looping over blocks
      
      d <- d[complete.cases(d), ]      #Keep only data from trials that bird actually went through (out of 300 possible trials)
      d_Overall <- rbind(d_Overall, d) #Add data from each bird to large output object
      
    } #End looping over individual birds
    
  } #End looping over populations
  
  return(d_Overall) #Assign results to our output object
}

dat <- Sim_fct() #Name and execute our data simulation, here you can also assign other parameters values 

######################################################################################################################
#Build reinforcement learning model in Stan
######################################################################################################################

#Stan is a programming language written in C++
#Models written in stan consist of different, defined blocks
#Annotation is denoted by the use of //
#Stan user manual: https://mc-stan.org/users/documentation/

reinforcement_model <- " //Assign model to variable

//Data block: Define and name the size of each observed variable

data{ //Begin block     

   int N;          //Number of observations, i.e. trials
   int N_id;       //Number of individuals
   int id[N];      //Unique individual identification 
   int N_pop;      //Number of populations
   int pop[N];     //Unique population identification
   int phase[N];   //Experimental phase i.e., initial or reversal
   int sex[N];     //Sex of bird i.e., female or male
   int Choice[N];  //Choice of bird e.g., dark or light grey tube
   int Correct[N]; //Category of tube-choice i.e., correct (contained food) or incorrect (no food)

} //End block

//Parameter block: Define and name the size of each unobserved variable. 

parameters{ //Begin block
  
  //Learning parameters phi and lambda
  
  matrix[2, 2] logit_phi; //Matrix for our latent phi values - indexed by phase (1 or 2; n = 2) and sex (f or m; n = 2)
  matrix[2, 2] log_L;     //Matrix for our latent lambda values - indexed by phase (1 or 2; n = 2) and sex (f or m; n =2)

  //Varying effects clustered on individual, we used non-centered approach, where we estimate individual-level offsets as z-scores
  //These z-scores are later multiplied by vector of standard deviations of each parmeter and the cholesky factor to get right covariance structure among parameters
  
  matrix[4, N_id] z_ID;           //Matrix for our latent individual samples (z scores) - indexed by learning phase/parameter (phase 1, phi or phase 1, lambda or phase 2, phi or phase 2 lambda; n = 4) and bird (n = number of individuals)
  vector<lower = 0>[4] sigma_ID;  //Standard deviation of learning parameters among individuals 
  cholesky_factor_corr[4] Rho_ID; //Cholesky factor for covariance of learning parameters among individuals
  
  //Varying effects clustered on population
  
  matrix[4, N_pop] z_pop;          //Matrix for our latent individual samples (z scores) - indexed by learning phase/parameter (phase 1, phi or phase 1, lambda or phase 2, phi or phase 2 lambda; n = 4) and population (n = number of populations)
  vector<lower = 0>[4] sigma_pop;  //Standard deviation of learning parameters among populations 
  cholesky_factor_corr[4] Rho_pop;//Cholesky factor for covariance of learning parameters among populations

} //End block

//Transformed parameter block: Define and name additional parameters of interest to be saved in posterior output

transformed parameters{ //Begin block

 //Here we compute the variance-covariance-matrices for varying effetcs, clustered on individuals and populatons, based on z-scores, standard deviations and Cholesky factors  
 //see p. 467 in Rethinking (https://github.com/Booleans/statistical-rethinking/blob/master/Statistical%20Rethinking%202nd%20Edition.pdf)
 
  matrix[N_id, 4] v_ID;    
  matrix[N_pop, 4] v_pop;  
  
  v_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';    
  v_pop = (diag_pre_multiply(sigma_pop, Rho_pop) * z_pop)'; 

} //End block

//Model block: Name and define the model

model{ //Begin block

  matrix[N_id, 2] A; //Attraction matrix - indexed by bird (n = number of individuals) and choice-options (n = 2)
  
  //Define (weakly-regularizing) priors for baseline parameters; use prior predictive simulations to see what choice behavior they imply
  //Turn into vectors so that it can be used as a vectorized argument to the univariate normal density
  
  to_vector(logit_phi) ~  normal(-2, 2); 
  to_vector(log_L) ~  normal(0, 2);      
  
  //Define prior distribution of varying individual effects
  
  to_vector(z_ID) ~ normal(0, 1);  //Standard normal prior for z-scores
  sigma_ID ~ exponential(1);       //Exponential prior because variances are bound to be positive
  Rho_ID ~ lkj_corr_cholesky(4);   //Cholesky LKJ correlation distribution for correlation matrix; parameter value = 4 says that more prior probability is placed on small correlations
  
  //Define distribution of varying population effects
  
  to_vector(z_pop) ~ normal(0, 1);  //Standard normal prior for z-scores
  sigma_pop ~ exponential(1);       //Exponential prior because variances are bound to be positive
  Rho_pop ~ lkj_corr_cholesky(4);   //Cholesky LKJ correlation distribution for correlation matrix; parameter value = 4 says that more prior probability is placed on small correlations 
  
  //Initialize attraction scores
  
  for (i in 1:N_id) A[i, 1:2] = rep_vector(0.1, 2)'; //For individual i across individuals, the baseline attraction score for either choice-option is set to 0.1
  
  //Loop over Choices
  
  for (i in 1:N) {   //For choice i across choices
  
  //Define and name local variables that update across choices
  
  vector[2] pay;     //vector of payoffs (1=yes or 0=no)
  vector[2] p;       //vector of choice-probabilites 
  real L;            //lambda value on outcome scale as used in the model
  real phi;          //phi value on outcome scale as used in the model
  
  //First, what is the log-probability of observed choice
  
  L =  exp(log_L[sex[i], phase[i]] + v_ID[id[i], (phase[i] - 1) + 1] + v_pop[pop[i], (phase[i] - 1) + 1]); //Main and varying effects on Lambda; log_L[sex[i], phase[i]] = main effect indexed by sex & phase; v_ID[id[i], (phase[i] - 1) + 1] + v_pop[pop[i], (phase[i] - 1) + 1] = full random effects for individual and population
  p = softmax(L * A[id[i], 1:2]' );                                                                        //Softmax function that normalizes attraction scores to sum to 1, so we can interpret it as probability distribution
  Choice[i] ~ categorical(p);                                                                              //Multinomial likelihood for observed choices (so model generalizes to more than 2 choice options)
  
  //Second, update attractions conditional on observed choice
  
  phi =  inv_logit(logit_phi[sex[i], phase[i]] + v_ID[id[i], phase[i] + 2] + v_pop[pop[i], phase[i] + 2]); //Main and varying effects on phi; logit_phi[sex[i], phase[i]] = main effect indexed by sex & phase; v_ID[id[i], phase[i] + 2] + v_pop[pop[i], phase[i] + 2] = full random effects for individual and population
  pay[1:2] = rep_vector(0, 2);                                                                             //Clear payoff vector and set values to 0
  pay[Choice[i]] = Correct[i];                                                                             //Assign payoff for choice i to equal grading of that choice (correct = 1; incorrect = 0)
  A[id[i], Choice[i]] = ((1-phi) * (A[id[i], Choice[i]]) + phi * pay[Choice[i]]);                         //Update attractions based on Equation 1
  }//i                                                                                                     //End looping over choices

} //End block                                                                                                                                                                                                     

//Generated quantities block: Compute learning parameters on outcome scale to return in the posterior

generated quantities{ //Begin block

  matrix[2, 2] phi;    //Matrix for our latent phi values - indexed by phase (1 or 2; n = 2) and sex (f or m; n = 2)
  matrix[2, 2] lambda; //Matrix for our latent lambda values - indexed by phase (1 or 2; n = 2) and sex (f or m; n =2)
  
  for (i in 1:2){                         
   for (j in 1:2){                        
    phi[i,j] = inv_logit(logit_phi[i,j]); //Phi in row i and column j is defined by its inverse logit i.e., the outcome scale
    lambda[i,j] = exp(log_L[i,j]);        //Lambda in row i and column j is defined by its exponential i.e., the outcome scale
   }                                      
  }                                       
  
} //End block                                        

//End model " 

######################################################################################################################
#Run simulated data through reinforcement learning model & extract posterior samples
######################################################################################################################

#First, put simulated data into a list to 'feed' to reinforcement learning model b/c stan doesn't 'like' data frames
d <- list(N = nrow(dat),
          N_id = length(unique(dat$id)),
          N_pop = length(unique(dat$Pop)),
          id = dat$id,
          pop = dat$Pop,
          phase = dat$Phase, 
          sex = dat$sex,
          Choice = dat$Choice,
          Correct = dat$Payoff
)

#Run simulated data through reinforcement learning model
m <- stan(model_code = reinforcement_model, data = d, iter = 5000, cores = 4, chains = 4, refresh = 10, control = list(adapt_delta = 0.99, max_treedepth = 14))

#Extract posterior samples
s <- extract.samples(m) 

######################################################################################################################
#Plots for Figure 2, S1 & 3
######################################################################################################################

#First, put simulated data into a ggplot-friendly data frame
dat_df <- data.frame(dat) 

######################################################################################################################
#Figure 2: Simulated group learning curves
######################################################################################################################

#Build function to fit smoothed binomial conditional means
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

#Binomial smooth function only fits to 0 & 1
#Our Choice data, however, is coded as 1 & 2
#Thus, rename & reorder to make 0 rewarded in initial & 1 in final
dat_df$Choice[dat_df$Choice == 2] <- 3
dat_df$Choice[dat_df$Choice == 1] <- 0
dat_df$Choice[dat_df$Choice == 3] <- 1

#Build Figure 2
#For background on graphing in ggplot, see: https://r-graphics.org/
Group_choice <- ggplot(dat_df,aes(x = trialafter, y = Choice, colour = as.factor(sex))) + 
  geom_point(position = position_jitter(height = 0.3), alpha = 0.2) + 
  scale_color_manual(values = c("#fde725", "#5ec962")) + 
  scale_x_continuous(name = "Trial", 
                     limits = c(0,200), 
                     breaks = c(0,50,100,150,200), 
                     labels = c(0,50,100,150,200)) + 
  scale_y_continuous(breaks = c(0,1), 
                     labels = c(1,2), 
                     expand = c(.2,0)) + 
  facet_grid(sex ~ Phase, 
             labeller = labeller(sex = c("1" = "Female", "2" = "Male"),
                                 Phase = c("1" = "Initial", "2" = "Reversal"))) +
  binomial_smooth(aes(group = Phase), colour = "black", size = 0.2, se = T, level = .89, alpha = 0.8) +
  theme_classic() + 
  theme( 
    legend.position = "none", 
    axis.title = element_text(colour = "black", size = 12), 
    axis.text = element_text(colour = "black", size = 12), 
    axis.ticks = element_line(colour = "black"), 
    axis.line = element_blank(), 
    panel.border=element_rect(colour = "black", fill = NA, size = 1), 
    strip.background = element_blank(), 
    strip.text = element_text(colour = "black", size = 12)) 

Group_choice

#ggsave(file = "Group_Choice.png", width = 8, height = 2.5, dpi = 300)

######################################################################################################################
#Figure S1: Simulated individual learning curves
######################################################################################################################

#Figure S1 orders plots by individual 1 - 49
#We want females first, so need to:

#Rename any males that appear before females
dat_df$id[dat_df$id == 5] <- 50
dat_df$id[dat_df$id == 6] <- 51
dat_df$id[dat_df$id == 7] <- 52
dat_df$id[dat_df$id == 13] <- 53
dat_df$id[dat_df$id == 14] <- 54

#Rename any females that appear after males
dat_df$id[dat_df$id == 35] <- 5
dat_df$id[dat_df$id == 36] <- 6
dat_df$id[dat_df$id == 37] <- 7
dat_df$id[dat_df$id == 38] <- 13
dat_df$id[dat_df$id == 39] <- 14

#Build data frame with the maximum number of trials for each bird in phase 1 i.e., switch point b/n phases
#We feed this independent data frame to geom_vline function
max_ind <- dat_df %>% filter(Phase != 2) %>% group_by(id) %>% summarise(max = which.max(trialafter)) 

#Build Figure S1
#For background on graphing in ggplot, see: https://r-graphics.org/
Ind_Choice <- ggplot(data = dat_df, aes(x = trial, y = Choice, colour = as.factor(sex))) + 
  geom_point(position = position_jitter(height = 0.1), alpha = 0.2) + 
  scale_color_manual(values = c("#fde725", "#5ec962")) +
  scale_x_continuous(name = "Trial", 
                     limits = c(0,300),
                     breaks = c(0,50,100,150,200, 250, 300),
                     labels = c (0,"","","","","", 300)) + 
  scale_y_continuous(breaks = c(0,1), 
                     labels=c(1,2), 
                     expand = c(.2,0)) + 
  facet_wrap(id ~ .) + 
  geom_vline(data = max_ind, aes(xintercept = max), colour = "black", linetype = "dashed", size = .5) +
  geom_smooth(colour = "black", level = .89, method = "loess", size = 0.5) +
  theme_classic() + 
  theme( 
    legend.position = "none", 
    axis.title = element_text(colour = "black", size = 12), 
    axis.text.x = element_text(colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_blank(),
    panel.border = element_rect(size = 1, fill = NA),
    strip.background = element_blank(),
    strip.text = element_blank())
   
Ind_Choice

#ggsave(file = "Ind_Choice.png", width = 8, height = 8, dpi = 300)

######################################################################################################################
#Figure 3: Density plots of phi & lambda posterior samples plus their contrasts by sex and phase
######################################################################################################################

#First, access posterior phi values by sex & phase; name accordingly; and put into parameter-specific data frame for ggplot-manipulation
s_phi <- as.data.frame(
  list(
    F_I = s$phi[ , 1, 1],
    F_R = s$phi[ , 1, 2],
    M_I = s$phi[ , 2, 1],
    M_R = s$phi[ , 2, 2]
  )
)

s_L <- as.data.frame(
  list(
    F_I = s$lambda[ , 1, 1],
    F_R = s$lambda[ , 1, 2],
    M_I = s$lambda[ , 2, 1],
    M_R = s$lambda[ , 2, 2]
  )
)

#Calcualte phi & lambda contrasts by phase
PI <- s_phi$F_I - s_phi$M_I
PR <- s_phi$F_R - s_phi$M_R

LI <- s_L$F_I - s_L$M_I
LR <- s_L$F_R - s_L$M_R

#Perform necessary data wrangling
#For background on tidyverse, see: https://r4ds.had.co.nz/
s_phi2_sex <- s_phi %>% 
  select(F_I:M_R) %>% 
  gather(F_I:M_R, key = "Sex_Phase", value = "Phi") %>% 
  separate(Sex_Phase, into = c("Sex", "Phase")) %>% 
  mutate(Sex = factor(Sex, labels = c("F" = "Females","M" = "Males"))) %>% 
  mutate(Phase = factor(Phase, labels = c("I" = "Initial","R" = "Reversal")))

s_L2_sex <- s_L %>% 
  select(F_I:M_R) %>% 
  gather(F_I:M_R, key = "Sex_Phase", value = "Lambda") %>% 
  separate(Sex_Phase, into = c("Sex", "Phase")) %>% 
  mutate(Sex = factor(Sex, labels = c("F" = "Females","M" = "Males"))) %>% 
  mutate(Phase = factor(Phase, labels = c("I" = "Initial","R" = "Reversal")))

#Build Figure 3A
#For background on graphing in ggplot, see: https://r-graphics.org/
Phi_sex <- ggplot(s_phi2_sex, aes(x = Phi, colour = Sex, fill = Sex)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("#fde725", "#5ec962")) +
  scale_colour_manual(values = c("#fde725", "#5ec962")) +
  scale_x_continuous(name = expression(phi), limits = c(0,0.7),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7), labels = c(0,"","","","","","",.7)) +
  facet_wrap( ~ Phase) + 
  ggtitle("Speed") +
  theme_classic() + 
  theme( 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, colour = "black", size = 12), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = "black", size = 12), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", size = 1, fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black", size = 12))

Phi_sex

#Build Figure 3B
#For background on graphing in ggplot, see: https://r-graphics.org/
Lambda_sex <- ggplot(s_L2_sex, aes(x = Lambda, colour = Sex, fill = Sex)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("#fde725", "#5ec962")) +
  scale_colour_manual(values = c("#fde725", "#5ec962")) +
  scale_x_continuous(name = expression(lambda), limits = c(0,26), 
                     breaks = c(0:26),
                     labels = c(0,"","","","","","","","","","","","","","",
                                "", "","", "", "", "", "", "", "", "",
                                "", 26)) +
  facet_wrap( ~ Phase) + 
  ggtitle("Sampling") +
  theme_classic() + 
  theme( 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, colour = "black", size = 12), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = "black", size = 12), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", size = 1, fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black", size = 12))

Lambda_sex

#For Figure 3C & D
#First, define lower and upper quantile bounds for each phi and lambda contrast by phase
PI_l<- quantile(PI,.055)
PI_u <- quantile(PI,.945)
PR_l<- quantile(PR,.055)
PR_u <- quantile(PR,.945)
LI_l<- quantile(LI,.055)
LI_u <- quantile(LI,.945)
LR_l<- quantile(LR,.055)
LR_u <- quantile(LR,.945)

#Put each phi and lambda contrast into a data frame
PI.df <- data.frame(PI)
PR.df <- data.frame(PR)
LI.df <- data.frame(LI)
LR.df <- data.frame(LR)

#Compute kernal density estimate for each phi & lambda contrast
PI.den <- density(PI)
PR.den <- density(PR)
LI.den <- density(LI)
LR.den <- density(LR)

#Put kernal density estimates into data frame
#This data frame will be fed to geom_area to shade lower and upper 89% intervals
PI.den.df <- data.frame(x = PI.den$x, y = PI.den$y)
PR.den.df <- data.frame(x = PR.den$x, y = PR.den$y)
LI.den.df <- data.frame(x = LI.den$x, y = LI.den$y)
LR.den.df <- data.frame(x = LR.den$x, y = LR.den$y)

#Build Figure 3C left panel
#For background on graphing in ggplot, see: https://r-graphics.org/
PID <- ggplot(PI.df, aes(x = PI)) +
  geom_density(colour = "darkgrey") + 
  geom_area(data = subset(PI.den.df, x >= PI_l & x <= PI_u),
            aes(x = x, y = y), fill = "darkgray") +
  scale_x_continuous(limits = c(-.5,.5), breaks = c(-.5,-.4,-.3,-.2,-.1,0,.1,.2,.3,.4,.5),
                     labels = c(-.5,"","","","",0,"","","","",.5)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  ggtitle("Initial") +
  theme_classic() +
  theme( 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, colour = "black", size = 12), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = "white", size = 12), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", size = 1, fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black", size = 12))

PID

#Build Figure 3C right panel
#For background on graphing in ggplot, see: https://r-graphics.org/
PRD <- ggplot(PR.df, aes(x = PR)) +
  geom_density(colour = "darkgrey") + 
  geom_area(data = subset(PR.den.df, x >= PR_l & x <= PR_u),
            aes(x = x, y = y), fill = "darkgray") +
  scale_x_continuous(limits = c(-0.3,.3), breaks = c(-.3,-.2,-.1,0,.1,.2,.3),
                     labels = c(-.3,"","",0,"","",.3)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  ggtitle("Reversal") +
  theme_classic() +
  theme( 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, colour = "black", size = 12), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = "white", size = 12), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", size = 1, fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black", size = 12))

PRD

#Build Figure 3D left panel
#For background on graphing in ggplot, see: https://r-graphics.org/
LID <- ggplot(LI.df, aes(x = LI)) +
  geom_density(colour = "darkgrey") + 
  geom_area(data = subset(LI.den.df, x >= LI_l & x <= LI_u),
            aes(x = x, y = y), fill = "darkgray") +
  scale_x_continuous(limits = c(-16,16), breaks = c(-16:16),
                     labels = c (-16,"","","","","","","","","","","","","","","",0,
                                 "","","","","","","","","","","","","","","",16)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  ggtitle("Initial") +
  theme_classic() +
  theme( 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, colour = "black", size = 12), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = "white", size = 12), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", size = 1, fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black", size = 12))

LID

#Build Figure 3D right panel
#For background on graphing in ggplot, see: https://r-graphics.org/
LRD <- ggplot(LR.df, aes(x = LR)) +
  geom_density(colour = "darkgrey") + 
  geom_area(data = subset(LR.den.df, x >= LR_l & x <= LR_u),
            aes(x = x, y = y), fill = "darkgray") +
  scale_x_continuous(limits = c(-12,12), breaks = c(-12:12), 
                     labels = c(-12,"","","","","","","","","","","",0,
                                "","","","","","","","","","","",12)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  ggtitle("Reversal") +
  theme_classic() +
  theme( 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, colour = "black", size = 12), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = "white", size = 12), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", size = 1, fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black", size = 12))

LRD

#Combine plots
#top <- plot_grid(Phi_sex, Lambda_sex, labels = c("A", "B"), label_size = 12, nrow = 1)

#bl <- plot_grid(PID, PRD, nrow = 1)
#bl2 <- bl + annotate("text", x = .5, y = .1, label = expression(phi), size = 4)

#br <- plot_grid(LID, LRD, nrow = 1)
#br2 <- br + annotate("text", x = .5, y = .1, label = expression(lambda), size = 4)

#bottom <- plot_grid(bl2, br2, labels = c("C","D"), label_size = 12, nrow = 1)

#ggsave(file="Fig3Top.png",width=8,height=2,dpi=300)
#ggsave(file="Fig3Bottom.png",width=8,height=1.7,dpi=300)

######################################################################################################################
#Figure 4: Parameter Recovery Estimates
######################################################################################################################

#NOTE: This code cannot reproduce Figure 4 without performing the parameter recovery estimation (see model validation step 3); 
#it simply serves as a record/template 

graphics.off()

png("ParameterRecov.png", res = 900, height = 20, width = 20, units = "cm")

par(mfrow = c(2,2), 
    mar= c(2,1,1,2), 
    oma =c(2,4,1,0))

#####
###
##
# PHI
##
###
####

load("") #Here, load stanfit for looped simulations for phi

sequence <- seq(0,0.1,0.01)

sex_contr_init <- list()
sex_contr_rev <- list()

for (i in 1:11) {
  sex_contr_init[[i]] <- s[[i]]$phi[,2,1] - s[[i]]$phi[,1,1]
  sex_contr_rev[[i]] <- s[[i]]$phi[,2,2] - s[[i]]$phi[,1,2]
}

plot(sequence, type="l", yaxt = "n", xaxt = "n", main="Initial", xlab = "Simulated sex effect", ylab = "Recovered sex effect", ylim = c(-0.01,0.12), xlim = c(-0.01,0.12))
axis(side = 1, sequence)
axis(side = 2, sequence)

abline(0,1)
abline(h=0, lty=2)
arrows(sequence,sapply(sex_contr_init, PI)[1,],sequence,sapply(sex_contr_init, PI)[2,], code=3, lwd=1.5, length=0, angle = 90,col = "grey")
points(sequence,sapply(sex_contr_init, median), pch=16)
mtext(side = 2, expression(phi), line = 3, cex = 1.8)

plot(sequence, type="l", yaxt = "n", xaxt = "n",main="Reversal", xlab = "Simulated sex effect", ylab = "Recovered sex effect", ylim = c(-0.01,0.12), xlim = c(-0.01,0.12))
axis(side = 1, sequence)
axis(side = 2, sequence)
abline(h=0, lty=2)

abline(0,1)
arrows(sequence,sapply(sex_contr_rev, PI)[1,],sequence,sapply(sex_contr_rev, PI)[2,], code=3, lwd=1.5, length=0, angle = 90,col = "grey")
points(sequence,sapply(sex_contr_rev, median), pch=16)

#####
###
##
# Lambda
##
###
####

load("") #Here, load stanfit for looped simulations for lambda

sex_contr_init <- list()
sex_contr_rev <- list()

sequence <- seq(0,10,1)

for (i in 1:11) {
  sex_contr_init[[i]] <- s[[i]]$lambda[,2,1] - s[[i]]$lambda[,1,1]
  sex_contr_rev[[i]] <- s[[i]]$lambda[,2,2] - s[[i]]$lambda[,1,2]
}

plot(sequence, type="n", yaxt = "n", xaxt = "n", main="", xlab = "Simulated sex effect", ylab = "Recovered sex effect", ylim = c(-1,15), xlim = c(-1,15))
axis(side = 1, sequence)
axis(side = 2, sequence)

abline(0,1)
abline(h=0, lty=2)
arrows(sequence,sapply(sex_contr_init, PI)[1,],sequence,sapply(sex_contr_init, PI)[2,], code=3, lwd=1.5, length=0, angle = 90,col = "grey")
points(sequence,sapply(sex_contr_init, median), pch=16)
mtext(side = 2, expression(lambda), line = 3, cex = 1.8)

plot(sequence, type="n", yaxt = "n", xaxt = "n", main="", xlab = "Simulated sex effect", ylab = "Recovered sex effect", ylim = c(-1,15), xlim = c(-1,15))
axis(side = 1, sequence)
axis(side = 2, sequence)
abline(h=0, lty=2)

abline(0,1)
arrows(sequence,sapply(sex_contr_rev, PI)[1,],sequence,sapply(sex_contr_rev, PI)[2,], code=3, lwd=1.5, length=0, angle = 90,col = "grey")
points(sequence,sapply(sex_contr_rev, median), pch=16)

mtext(side = 1,line = 1, "Simulated sex effect", outer = TRUE, cex = 1.2)
mtext(side = 2, "Recovered sex effect", outer = TRUE, line = 2, cex = 1.2)

dev.off()

#End script