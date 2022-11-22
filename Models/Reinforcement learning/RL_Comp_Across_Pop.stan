///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Code to run STAN reversal learning model across populations for the manuscript

//Leading an urban invasion: risk-sensitive learning is a winning strategyg

//Authored by Alexis J Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Stan is a programming language written in C++
//Models written in stan consist of different, defined blocks
//Annotation is denoted by the use of //
//Stan user manual: https://mc-stan.org/users/documentation/

//Data block: Define and name the size of each observed variable

data{ //Begin block

   int N;          //Number of observations, i.e. trials
   int N_id;       //Number of individuals
   int id[N];      //Unique individual identification
   int phase[N];   //Experimental phase i.e., initial or reversal
   int sex[N];     //Sex of bird i.e., female or male
   int Choice[N];  //Choice of bird e.g., dark or light grey tube
   int Correct[N]; //Category of tube-choice i.e., correct (contained food) or incorrect (no food)

} //End block

//Parameter block: Define and name the size of each unobserved variable.

parameters{ //Begin block

  //Learning parameters phi and lambda

  matrix[2, 2] logit_phi; //Matrix for our latent phi values - indexed by sex (1 or 2; n = 2) and phase (f or m; n = 2)
  matrix[2, 2] log_L;     //Matrix for our latent lambda values - indexed by sex (1 or 2; n = 2) and phase (f or m; n =2)

  //Varying effects clustered on individual, we used non-centered approach, where we estimate individual-level offsets as z-scores
  //These z-scores are later multiplied by vector of standard deviations of each parmeter and the cholesky factor to get right covariance structure among parameters

  matrix[4, N_id] z_ID;           //Matrix for our latent individual samples (z scores) - indexed by learning phase/parameter (phase 1, phi or phase 1, lambda or phase 2, phi or phase 2 lambda; n = 4) and bird (n = number of individuals)
  vector<lower = 0>[4] sigma_ID;  //Standard deviation of learning parameters among individuals
  cholesky_factor_corr[4] Rho_ID; //Cholesky factor for covariance of learning parameters among individuals

} //End block

//Transformed parameter block: Define and name additional parameters of interest to be saved in posterior output

transformed parameters{ //Begin block

 //Here we compute the variance-covariance-matrices for varying effects clustered on individuals, based on z-scores, standard deviations and Cholesky factors
 //see p. 467 in Rethinking (https://github.com/Booleans/statistical-rethinking/blob/master/Statistical%20Rethinking%202nd%20Edition.pdf)

  matrix[N_id, 4] v_ID;
  v_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';

} //End block

//Model block: Name and define the model

model{ //Begin block

  matrix[N_id, 2] A; //Attraction matrix - indexed by bird (n = number of individuals) and choice-options (n = 2)

  //Define (weakly-regularizing) priors for baseline parameters; use prior predictive simulations to see what choice behavior they imply
  //Turn into vectors so that it can be used as a vectorized argument to the univariate normal density

  to_vector(logit_phi) ~  normal(-2, 1);
  to_vector(log_L) ~  normal(0, 1);

  //Define prior distribution of varying individual effects

  to_vector(z_ID) ~ normal(0, 1);  //Standard normal prior for z-scores
  sigma_ID ~ exponential(1);       //Exponential prior because variances are bound to be positive
  Rho_ID ~ lkj_corr_cholesky(4);   //Cholesky LKJ correlation distribution for correlation matrix; parameter value = 4 says that more prior probability is placed on small correlations

  //Initialize attraction scores

  for (i in 1:N_id) A[i, 1:2] = rep_vector(0.1, 2)'; //For individual i across individuals, the baseline attraction score for either choice-option is set to 0.1

  //Loop over Choices

  for (i in 1:N) {   //For choice i across choices

  //Define and name local variables that update across choices

  vector[2] pay;     //Vector of payoffs (1=yes or 0=no)
  vector[2] p;       //Vector of choice-probabilites
  real L;            //Lambda value on outcome scale as used in the model
  real phi;          //Phi value on outcome scale as used in the model

  //First, what is the log-probability of observed choice

  L =  exp(log_L[sex[i], phase[i]] + v_ID[id[i], (phase[i] - 1) + 1]); //Main and varying effects on Lambda
  p = softmax(L * A[id[i], 1:2]' );                                    //Softmax function that normalizes attraction scores to sum to 1, so we can interpret it as probability distribution
  Choice[i] ~ categorical(p);                                          //Multinomial likelihood for observed choices (so model generalizes to more than 2 choice options)

  //Second, update attractions conditional on observed choice

  phi =  inv_logit(logit_phi[sex[i], phase[i]] + v_ID[id[i], phase[i] + 2]);      //Main and varying effects on phi
  pay[1:2] = rep_vector(0, 2);                                                    //Clear payoff vector and set values to 0
  pay[Choice[i]] = Correct[i];                                                    //Assign payoff for choice i to equal grading of that choice (correct = 1; incorrect = 0)
  A[id[i], Choice[i]] = ((1-phi) * (A[id[i], Choice[i]]) + phi * pay[Choice[i]]); //Update attractions based on Equation 1
  }//i                                                                            //End looping over choices

} //End block

//Generated quantities block: Compute learning parameters on outcome scale to return in the posterior

generated quantities{ //Begin block

  matrix[2, 2] phi;    //Matrix for our latent phi values - indexed by sex (1 or 2; n = 2) and phase (f or m; n = 2)
  matrix[2, 2] lambda; //Matrix for our latent lambda values - indexed by sex (1 or 2; n = 2) and phase (f or m; n =2)

  for (i in 1:2){                         //For sex in 1 - 2
   for (j in 1:2){                        //For phase in 1 - 2
    phi[i,j] = inv_logit(logit_phi[i,j]); //Phi in row i and column j is defined by its inverse logit i.e., the outcome scale
    lambda[i,j] = exp(log_L[i,j]);        //Lambda in row i and column j is defined by its exponential i.e., the outcome scale
   }                                      //End j
  }                                       //End i

} //End block
