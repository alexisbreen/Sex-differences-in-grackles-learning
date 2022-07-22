#####################################################################################################################################################

#Code for performing the post-study simulations for six random male-female draws (Figure 3) for the manuscript 

#Range-expanding male birds buffer environmental change by strategising risk-sensitive learning

#Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

#####################################################################################################################################################
#Begin script

#Extract posterior from across population model - this must have been run prior to executing this script!
s <- extract.samples(m_xpop) #Note this model must have already been run using STAN_Execution.R

#To simulate new birds from the estimated population, we derive full posterior distribution of 
#the random effects variance-covariance matrix

#1) Get variances for 4 parameters (phi+lambda for initial and reversal)
sigma_id <- s$sigma_ID

#2) Compute correlation matrix from Cholesky factors
Correlations <- array(numeric(),c(4000,4,4))
for (i in 1:4000) {
  Correlations[i,,] <- s$Rho_ID[i,,] %*% t(s$Rho_ID[i,,])
}

#3) Compute variance-covariance matrix from variances and correlations
S <- array(numeric(),c(4000,4,4))
for (i in 1:4000) {
  S[i,,] <- diag(sigma_id[i,]) %*%  Correlations[i,,]  %*% diag(sigma_id[i,])
}

#Extract posteriors for means
phi_means <- array(numeric(),c(4000,2,2))

phi_means[,1,1] <- s$logit_phi[,1,1]
phi_means[,1,2] <- s$logit_phi[,1,2]
phi_means[,2,1] <- s$logit_phi[,2,1]
phi_means[,2,2] <- s$logit_phi[,2,2]

lambda_means <- array(numeric(),c(4000,2,2))
lambda_means[,1,1] <- s$log_L[,1,1]
lambda_means[,1,2] <- s$log_L[,1,2]
lambda_means[,2,1] <- s$log_L[,2,1]
lambda_means[,2,2] <- s$log_L[,2,2]

#Build simulation function
#Define function & input parameters
Post_Study_Sim_Pair_Fxn <- function(N_block = 30,  #Maximum number of blocks (10 trials per block)
                                    N_trials = 10, #Trials per block   
                                    N_sim = 1)     #From just one sim get two birds, a female & male
  
{ #Define function operations
  
  #Create overall output object
  d_Overall <- c() 
  
  #Loop over simulations
  for(sim in 1:N_sim){ #For every sim
    
    print(sim) #Print sim no. - generalises to > 1 sim
    
    #Get one random draw from the posterior
    draw <- sample(1:4000,1)
    
    #Generate 17 females and 32 males from multivariate normal distribution
    Female <- rmvnorm(1, c(lambda_means[draw,1,1], lambda_means[draw,1,2], phi_means[draw, 1, 1], phi_means[draw, 1, 2]), S[draw,,])
    Male   <- rmvnorm(1, c(lambda_means[draw,2,1], lambda_means[draw,2,2], phi_means[draw, 2, 1], phi_means[draw, 2, 2]), S[draw,,])
    
    #We transform learning parameters back to original scale to be used later in the model
    lambda <- exp(rbind(Female[ , 1:2], Male[, 1:2]))
    phi <- inv_logit(rbind(Female[ , 3:4], Male[, 3:4]))
    
    #Begin 'following' birds through initial and reversal learning
    for(ind in 1:2) { #For every bird 
      
      #Set initial attractions to 0.1
      #This allows birds to begin with some attraction to the tubes, which is expected from their pre-test habituation to baited tubes
      #And it allows attraction scores to decrease when a 'wrong' choice is made early on                
      A <- c(0.1, 0.1)
      
      #Create output matrix to record choices and payoffs
      #id and sex values are assigned based on the number and sex of birds we expect to see in real data
      d <- data.frame(id = ind + (sim-1)*2, 
                      sex = ifelse(ind == 1, 1, 2), 
                      poss_trials = 1:(N_block*N_trials), 
                      trial = NA, 
                      trialafter = NA, 
                      Choice = NA, 
                      Payoff = NA, 
                      Phase = NA, 
                      Block = NA,
                      phi_init = ifelse(ind == 1, phi[1,1], phi[2,1]), #Used in Figure 3
                      phi_rev = ifelse(ind == 1, phi[1,2], phi[2,2]), #Used in Figure 3
                      lambda_init = ifelse(ind == 1, lambda[1,1], lambda[2,1]), #Used in Figure 3
                      lambda_rev = ifelse(ind == 1, lambda[1,2], lambda[2,2])) #Used in Figure 3
      
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
          if (i >= 21){                                                                                                            #If trial number is larger than 21
            if (sum(d$Payoff[d$trial[(i-20):i]]) >= 17){                                                                           #And if, when we tally the payoffs across the most recent 20 trials, that tally equals or exceeds 17
              if ( (sum(d$Payoff[which(d$Block == d$Block[i])]) >= 8) & (sum(d$Payoff[which(d$Block == (d$Block[i]-1))]) >= 8 ) ){ #And if, when we tally the payoffs within EACH of the two trial blocks containing those most recent 20 trials (10 trials/block), that tally for either block equals or exceeds 8           
                criterion <- criterion + 1                                                                                         #Reassign criterion to next phase i.e., bird has passed initial; see line 84
                phase <- 2                                                                                                         #Reassign phase to phase 2 rather than phase 1
                stop <- 1                                                                                                          #Reassign stop point; 1 = we are no longer determining 
                Switch <- i                                                                                                        #Assign the switch point to the trial number at which the bird passed initial
              }                                                                                                                    #End if statement
            }                                                                                                                      #End if statement
          } else {                                                                                                                 #ELSE: If we are still within first 20 trials, 
            if (sum(d$Payoff[d$trial[1:i]]) >= 17){                                                                                #If more then 17 correct in all previous trials
              if ( (sum(d$Payoff[which(d$Block == d$Block[i])]) >= 8) & (sum(d$Payoff[which(d$Block == (d$Block[i]-1))]) >= 8 ) ){ #And if, when we tally the payoffs within EACH of the two trial blocks containing those most recent 20 trials (10 trials/block), that tally for either block equals or exceeds 8           
                criterion <- criterion + 1                                                                                         #Reassign criterion to next phase i.e., bird has passed initial; see line 84
                phase <- 2                                                                                                         #Reassign phase to phase 2 rather than phase 1
                stop <- 1                                                                                                          #Reassign stop point; 1 = we are no longer determining 
                Switch <- i                                                                                                        #Assign the switch point to the trial number at which the bird passed initial
              }                                                                                                                    #End if statement
            }                                                                                                                      #End if statement
          }                                                                                                                        #End determination of phase
          
          if (stop == 1) break #Statement to stop current block, because criterion is reached
          
        } #End looping over trials within block
        
        if (criterion == 2) break #Statement to stop experiment for this bird
        
      } #End looping over blocks
      
      d <- d[complete.cases(d), ]      #Keep only data from trials that bird actually went through (out of 300 possible trials)
      d_Overall <- rbind(d_Overall, d) #Add data from each bird to large output object
      
    } #End looping over individual birds
    
  }#End looping over sim
  
  return(d_Overall) #Assign results to our output object
}

dat <- Post_Study_Sim_Pair_Fxn() #Name and execute our data simulation, here you can also assign other parameters values 

#Safety check to make sure sims (ca. 100) not breaking trial pass rules - all OK!

#for(i in 1:nrow(dat)){
  #if(dat$Phase[i] == 1 & dat$Phase[i + 1] == 2 & dat$trialafter[i] < 17){
    #print(dat$trialafter[i])
  #} else if(i == max(nrow(dat)) & dat$trialafter[i] < 17){
    #print(dat$trialafter[i])
  #} else if(dat$Phase[i] == 2 & dat$id[i] != max(dat$id) & dat$id[i] != dat$id[i + 1] & dat$trialafter[i] < 17){
    #print(dat$trialafter[i])
  #}
#}

#End script
#####################################################################################################################################################
