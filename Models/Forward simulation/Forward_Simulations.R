#####################################################################################################################################################

#Code for performing the agent-based forward simulations for the manuscript

#Leading an urban invasion: risk-sensitive learning is a winning strategy

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#NOTE: The RL_Execution.R script must have been run prior to running this script

#Extract posterior from across population model 
s_init <- extract.samples(m_xpop_init) 
s_rev <- extract.samples(m_xpop_rev) 

#To simulate new birds from the estimated population, we derive full posterior distribution of the random effects variance-covariance matrix
#We do this for initial and reversal estimates

#1) Get variances for 4 parameters (phi and lambda for initial and reversal)
sigma_id_init <- s_init$sigma_ID
sigma_id_rev <- s_rev$sigma_ID

#2) Compute correlation matrix from Cholesky factors
Correlations_init <- array(numeric(),c(4000,4,4))
for (i in 1:4000) {
  Correlations_init[i,,] <- s_init$Rho_ID[i,,] %*% t(s_init$Rho_ID[i,,])
}
Correlations_rev <- array(numeric(),c(4000,4,4))
for (i in 1:4000) {
  Correlations_rev[i,,] <- s_rev$Rho_ID[i,,] %*% t(s_rev$Rho_ID[i,,])
}

#3) Compute variance-covariance matrix from variances and correlations
S_init <- array(numeric(),c(4000,4,4))
for (i in 1:4000) {
  S_init[i,,] <- diag(sigma_id_init[i,]) %*%  Correlations_init[i,,]  %*% diag(sigma_id_init[i,])
}
S_rev <- array(numeric(),c(4000,4,4))
for (i in 1:4000) {
  S_rev[i,,] <- diag(sigma_id_rev[i,]) %*%  Correlations_rev[i,,]  %*% diag(sigma_id_rev[i,])
}

#Extract posteriors for draws
#Phi
phi_post <- array(numeric(),c(4000,2,2)) #Empty array
#Fill phi array
phi_post[,1,1] <- s_init$logit_phi[,1,1] #Female-Initial
phi_post[,1,2] <- s_rev$logit_phi[,1,2]  #Female-Reversal
phi_post[,2,1] <- s_init$logit_phi[,2,1] #Male-Initial
phi_post[,2,2] <- s_rev$logit_phi[,2,2]  #Male-Reversal

#Lambda
lambda_post <- array(numeric(),c(4000,2,2)) #Empty array
#Fill lambda array
lambda_post[,1,1] <- s_init$log_L[,1,1] #Female-Initial
lambda_post[,1,2] <- s_rev$log_L[,1,2]  #Female-Reversal
lambda_post[,2,1] <- s_init$log_L[,2,1] #Male-Initial
lambda_post[,2,2] <- s_rev$log_L[,2,2]  #Male-Reversal

#Build simulation function
#Define function & input parameters
Post_Study_Sim_Fxn <- function(N_trial = 1000, #Number of trials - unrealistically large number
                               N_F = 5000,     #Number of simulated females
                               N_M = 5000,     #Number of simulated females
                               N_sim = 1,      #Number of simulations 
                               mu_on = 0)      #Whether to use full or mean posterior distribution (0 = full; 1 = mean)
{ #Define function operations
  
  #Create overall output object
  d_Overall <- c() 
  
  #Number of individuals based on input 
  N_ind <- N_F + N_M
  
  #Begin looping over simulations
  for(sim in 1:N_sim){
    
    #Decide whether to use full or mean posterior
    if(mu_on == 1){ #Use mean - here, all male and females given the same mean estimates for each learning parameter in each phase
      
      #Generate females and males from mean multivariate normal distribution
      Female_mu <- cbind(mean(lambda_post[,1,1]), mean(lambda_post[,1,2]), mean(phi_post[,1,1]), mean(phi_post[,1,2]))
      Male_mu   <- cbind(mean(lambda_post[,2,1]), mean(lambda_post[,2,2]), mean(phi_post[,2,1]), mean(phi_post[,2,2]))
      
      #Empty matrices to make sure all male and females get same mean learning parameter estimates
      Female <- matrix(NA, N_F, 4)
      Male <- matrix(NA, N_M, 4)
      
      #Fill matrix based on simulated number of females
      for(female in 1:N_F){
        for(param in 1:4){
          Female[female, param] <- Female_mu[param]
        }
      }
      
      #Fill matrix based on simulated number of males
      for(male in 1:N_M){
        for(param in 1:4){
          Male[male, param] <- Male_mu[param]
        }
      }
      
    } else { #Use full - here, all males and females given random draws of learning parameter estimates
      
      #Start sampling posterior
      draw <- sample(1:4000, (N_ind)/2, replace = TRUE)
      
      #If uneven male & female numbers, draws need this sex-specific information
      if(N_F != N_M){
        draw_F <- sample(1:4000, N_F, replace = TRUE)
        draw_M <- sample(1:4000, N_M, replace = TRUE)
      } else { #If even, draws can be the number of individuals divided by two 
        draw_F <- draw
        draw_M <- draw
      }
      
      #Generate females and males from multivariate normal distribution
      #Need to account for two distributions; one for initial; one for reversal
      #And transpose generated matrix from learning parameters as rows & individuals as columns, to individuals as rows & learning parameters as columns
      Female_init <- t(sapply(1:N_F, function(i) rmvnorm(1, c(lambda_post[draw_F[i],1,1], lambda_post[draw_F[i],1,2], phi_post[draw_F[i], 1, 1], phi_post[draw_F[i], 1, 2]), S_init[draw_F[i],,])))
      Female_rev  <- t(sapply(1:N_F, function(i) rmvnorm(1, c(lambda_post[draw_F[i],1,1], lambda_post[draw_F[i],1,2], phi_post[draw_F[i], 1, 1], phi_post[draw_F[i], 1, 2]), S_rev[draw_F[i],,])))
      
      Male_init   <- t(sapply(1:N_M, function(i) rmvnorm(1, c(lambda_post[draw_M[i],2,1], lambda_post[draw_M[i],2,2], phi_post[draw_M[i], 2, 1], phi_post[draw_M[i], 2, 2]), S_init[draw_M[i],,])))  
      Male_rev    <- t(sapply(1:N_M, function(i) rmvnorm(1, c(lambda_post[draw_M[i],2,1], lambda_post[draw_M[i],2,2], phi_post[draw_M[i], 2, 1], phi_post[draw_M[i], 2, 2]), S_rev[draw_M[i],,])))  
      
      #And combine them accordingly
      Female <- cbind(Female_init[, 1], Female_rev[, 2], Female_init[, 3], Female_rev[, 4])
      Male   <- cbind(Male_init[, 1], Male_rev[, 2], Male_init[, 3], Male_rev[, 4])
      
    } #End decision over which posterior to use
    
    #We transform learning parameters back to original scale to be used later in the model
    lambda <- exp(rbind(Female[, 1:2], Male[, 1:2]))
    phi <- inv_logit(rbind(Female[, 3:4], Male[, 3:4]))
    
    #Begin looping over birds through initial and reversal learning
    for(ind in 1:N_ind){ 
      
      print(ind) #To see simulation progress when running
      
      #Set initial attractions to 0.1
      #This allows birds to begin with some attraction to the tubes, which is expected from their pre-test habituation to baited tubes
      #And it allows attraction scores to decrease when a 'wrong' choice is made early on                
      A <- c(0.1, 0.1)
      
      #Create output matrix to record choices and payoffs
      d <- data.frame(id =  ifelse(sim == 1, ind, ((N_ind * sim) - 49) + ind),
                      sex = ifelse(ind <= N_ind/2, 1, 2),  
                      poss_trials = 1:1000, 
                      trial = NA, 
                      trialafter = NA, 
                      Choice = NA, 
                      switch = NA,
                      Criterion = NA,
                      sum_count = NA,
                      Payoff = NA, 
                      Phase = NA,
                      phi = NA,
                      lambda = NA,
                      sim = sim)
      
      #Define some local variables used in below trial loop
      phase <- 1     #Starting phase (1 = initial)
      i <- 0         #Counter for trial number
      Switch <- c()  #Empty vector to fill with at what trial number birds switch b/n phases
      criterion <- 0 #Counter for times bird passes criterion; 0 = bird has not yet passed initial; 1 = bird has passed initial; 2 = bird has passed initial & reversal; 1 & 2 states are defined/determined below
      
      #Begin looping over trials
      for(trial in 1:N_trial){
        
        #Begin at trial number 1
        i <- i+1                                                 
        
        #Assign variables to data frame
        d$trial[i] <- i                                          
        d$trialafter[i] <- ifelse(criterion == 0, i, i - Switch) 
        d$Phase[i] <- phase                                      
        d$Criterion[i] <- criterion                              
        d$phi[i] <- phi[ind, phase]                              
        d$lambda[i] <- lambda[ind, phase]                        
        
        #Get choice probabilities based on attraction scores
        Prob <- c() #Empty vector to fill with our choice probabilities
        for (j in 1:2) Prob[j] <- exp(lambda[ind, phase] *A[j]) / sum(exp(lambda[ind, phase]*A)) #For choice-option j in our two choice-option environment, the probability of choice-option j is defined by equation 2
        
        #Make choice proportional to attraction scores
        d$Choice[which(d$trial == i)] <- sample(c(1:2), size = 1, prob = Prob) #Make the bird choose among our two choice-options, based on the choice probabilities defined above                
        
        #Determine Payoff based on Phase and Choice
        if (d$Phase[i] == 1){   
          if (d$Choice[i] == 1){ 
            d$Payoff[i] <- 1     
          } else {               
            d$Payoff[i] <- 0     
          }                      
        } else {                 
          if (d$Choice[i] == 1){ 
            d$Payoff[i] <- 0     
          } else {               
            d$Payoff[i] <- 1    
          }                     
        }                        
        
        #Update Attractions (only for chosen option) 
        A[d$Choice[i]] <- (1-phi[ind, phase]) * A[d$Choice[i]] + phi[ind, phase] * d$Payoff[i] 
        
        #Determine switch based on Choice
        #For our supplementary Poisson, we need choice-option switch counts
        if(d$trialafter[i] > 1){
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
        
        #Cumulative correct choice counts for below safety checks
        if(d$trialafter[i] == 17 | d$trialafter[i] == 18 | d$trialafter[i] == 19){
          d$sum_count[i] <- sum(d$Payoff[(i - (d$trialafter[i - 1])): i])
        } else if(d$trialafter[i] >= 20){
          d$sum_count[i] <- sum(sum(d$Payoff[(i - 19):i]))
        } else if(d$trialafter[i] < 17){
          d$sum_count[i] <- 0
        }
        
        #Set phase of the experiment (1 = initial, 2 = reversal) based on the passing criterion
        #Criterion = correct choice in 17 out of the most recent 20 trials
        if(d$trialafter[i] == 17 | d$trialafter[i] == 18 | d$trialafter[i] == 19){
          if(sum(d$Payoff[(i - (d$trialafter[i - 1])): i]) >= 17){
            criterion <- criterion + 1
            phase <- 2 
            Switch <- i
          }
        } else if(d$trialafter[i] >= 20){
          if(sum(d$Payoff[(i - 19):i]) >= 17){
            criterion <- criterion + 1
            phase <- 2 
            Switch <- i
          }
        } 
        
        if (criterion == 2) break #Criterion reached
        
      } #End looping over trials
      
      d <- d[complete.cases(d), ]      #Keep only data from trials that bird actually went through (out of 300 possible trials)
      d_Overall <- rbind(d_Overall, d) #Add data from each bird to large output object
      
    } #End looping over individuals
    
  } #End looping over simulations
  
  return(d_Overall) #Assign results to our output object
  
} #End definition of function operations

dat <- Post_Study_Sim_Fxn(N_F = 10, N_M = 10, mu_on = 0, N_sim = 1) #Name and execute our data simulation; here you can change parameter values of the defined function 

#Safety checks: 
#Any passers not exactly at 17?
for(id in 1:max(dat$id)){
  if(dat$trialafter[id] == max(dat$trialafter[which(dat$id == id)]) & dat$Phase[id] == 1){
    if(dat$cum_count[id] != 17){
      print(i)
    }
  } else if(dat$trialafter[id] == max(dat$trialafter[which(dat$id == id)]) & dat$Phase[id] == 2){
    if(dat$cum_count[id] != 17){
      print(i)
    }
  }
} #All OK!

#End script
#####################################################################################################################################################
