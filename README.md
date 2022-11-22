# Sex-differences-in-grackles-learning

This GitHub repository hosts open materials for the manuscript

**Leading an urban invasion: risk-sensitive learning is a winning strategy**

authored by Alexis J. Breen (alexis_breen@eva.mpg.de) & Dominik Deffner (deffner@mpib-berlin.mpg.de)

**Data Processing folder contains:**

- Data_Processing.R script to collate, clean & curate the utilised data
- The original data sheets: Santa_Barbara_CA_data.csv, Tempe_AZ_data.csv & Woodland_CA_data.csv
- Metadata regarding the original data sheets

**Data folder contains:**
 
- Grackle_data_clean.csv produced from Data_Processing.R script & used for all analyses/graphing

**Figures folder contains:**

- Fig_2.R script to reproduce Figure 2 in the main text
- Fig_3.R script to reproduce the heatmap in Figure 3 in the main text
- Fig_S1.R script to reproduce Figure S1 in the supplementary
- Fig_S2.R script to reproduce Figure S2 in the supplementary
- Fig_S3.R script to reproduce Figure S3 in the supplementary

**Models folder contains:**

**Evolutionary sub-folder, containing:**
- Evolutionary_model.R script to perform the evolutionary algorithm model of optimal learning under urban-like (or not) environments

**Forward simulation sub-folder, containing:**
- Forward_Simulations.R script to perform the agent-based forward simulations using either the full or mean posterior distribution of our reinforcment learning model

**Poisson sub-folder, containing:**
- Poisson_Execution.R script to prepare data for, run, and post-process (e.g., extract posteriors) all multi-level Bayesian Poisson computational models in stan
- Poisson_Speed_Across_Pop.stan script to run multi-level Poisson regression, modelling total-trials-in test across populations
- Poisson_Speed_Full.stan script to run multi-level Poisson regression, modelling total-trials-in-test between populations
- Poisson_Switch_Across_Pop.stan script to run multi-level Poisson regression, modelling total-switches-in-test across populations
- Poisson_Switch_Full.stan script to run multi-level Poisson regression, modelling total-switches-in-test between populations

**Reinforcement learning sub-folder, containing:**
- RL_Execution.R script to prepare data for, run, and post-process (e.g., extract posteriors) all multi-level Bayesian reinforcement learning computational models in stan
- RL_Comp_Across_Pop.stan script to run multi-level reinforcement learning computational model - indexed by sex & phase - estimating influence of phi & sigma
- RL_Comp_Full.stan scrip to run multi-level reinforcement learning computational model - indexed by population & treatment (i.e., sex/phase) - estimating influence of phi & sigma 

**Preregistration folder contains:**

- Breen_Deffner_prereg.pdf copy of our study preregistration 
- Grackle_models_and_graphs.R script to run all material - simulations, models, model-validation checks & graphs - used in Breen_Deffner_prereg.pdf

**Softward requirements:**

- Stan (for running the multi-level models): https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
- Rethinking (for processing fitted model outputs): https://github.com/rmcelreath/rethinking
- R (for running all code): https://www.rstudio.com/

**Potentially useful background material**

- Statistical rethinking: A Bayesian course with examples in R and Stan by Prof. Richard McElreath (free pdf): https://github.com/Booleans/statistical-rethinking/blob/master/Statistical%20Rethinking%202nd%20Edition.pdf
- Statistical rethinking online lectures: https://www.youtube.com/playlist?list=PLDcUM9US4XdMROZ57-OIRtIK0aOynbgZN