# imports
library(lme4)
library(rjags)
library(gplots)
source("util.R")
source("simulation.R")

###            ###
### Parameters ###
###            ###

### True values
# these set the true values of the population
# putting multiple values in will cause simulations to run
# with different true values.
# The first four give the mean value of the four parameters
# that determine participant behavior. The next four give
# the variance in these parameters.
# Warning! var_sexs, var_conds and var_sex_conds do not currently
# do anything!
b_bases <- c(0)
b_sexs <- c(0)
b_conds <- c(0)
b_sex_conds <- c(0)
var_bases <- c(0, 0.25, 0.5, 0.75, 1.0, 1.5, 2)
var_sexs <- c(0)
var_conds <- c(0)
var_sex_conds <- c(0)

### Size parameters
# These determine the size of the simulations for every set of
# parameter values given in the "True values" setction above.
# Warning! n_participants_per_experiment and n_people need to 
# be divisible by 4!
n_repeats <- 10
n_experiments_per_repeat <- 20
n_participants_per_experiment <- 80
n_trials_per_participant <- 25
n_people <- 100000

### Analysis parameters
# These allow you to choose which analyses do you want
# Warning! thhe bglmm and pp analyses are slow!
do_anova <- TRUE
do_glmm <- TRUE
do_bglmm <- TRUE
do_pp <- TRUE

### Posterior-passing parameters
# These give you various options wrt posterior passing
# log only the final experiment from each chain:
pp_final_expt_only <- TRUE

### plotting parameters
# basically tell it what plots you want
plot_sex_cond <- TRUE
plot_var_base <- TRUE

### Vectors to store meta-data
# This function is in util.R
# It creates a number of vectors that will be
# filled with data. Each set of simulations for a set of parameters
# creates a results table. At the end of each set of parameter values,
# the meta_results table is filled in with a single row that gives data
# on the results across those simulations.
prepare_meta_vectors()

###                        ###
### SIMULATIONS START HERE ###
###                        ###

### For loops to iterate through parameter values
for (i in 1:length(b_bases)) {
  b_base <- b_bases[i]
  for (j in 1:length(b_sexs)) {
    b_sex <- b_sexs[j]
    for (k in 1:length(b_conds)) {
      b_cond <- b_conds[k]
      for (l in 1:length(b_sex_conds)) {
        b_sex_cond <- b_sex_conds[l]
        for (m in 1:length(var_bases)) {
          var_base <- var_bases[m]
          for (n in 1:length(var_sexs)) {
            var_sex <- var_sexs[n]
            for (o in 1:length(var_conds)) {
              var_cond <- var_conds[o]
              for (p in 1:length(var_sex_conds)) {
                var_sex_cond <- var_sex_conds[p]
                meta_true_base <- c(meta_true_base, rep(b_base, n_repeats))
                meta_true_sex <- c(meta_true_sex, rep(b_sex, n_repeats))
                meta_true_cond <- c(meta_true_cond, rep(b_cond, n_repeats))
                meta_true_sex_cond <- c(meta_true_sex_cond, rep(b_sex_cond, n_repeats))
                meta_var_base <- c(meta_var_base, rep(var_base, n_repeats))
                meta_var_sex <- c(meta_var_sex, rep(var_sex, n_repeats))
                meta_var_cond <- c(meta_var_cond, rep(var_cond, n_repeats))
                meta_var_sex_cond <- c(meta_var_sex_cond, rep(var_sex_cond, n_repeats))
                print(paste("running simulation with parameters: b_base: ", b_base,
                            ", b_sex: ", b_sex,
                            ", b_cond: ", b_cond,
                            ", b_sex_cond: ", b_sex_cond,
                            ", var_base: ", var_base,
                            ", var_sex: ", var_sex,
                            ", var_cond: ", var_cond,
                            ", var_sex_cond: ", var_sex_cond,
                            sep=" "))
                
### Vectors to store data
# These store the results of simulations within each set
# of parameter values
prepare_data_vectors()

###
### Each repeat starts here
###

for (rep in 1:n_repeats) {
  print(paste(">>>> Repeat", rep, "of", n_repeats, sep=" "))

  ###
  ### Create the population ###
  ###
  # This function is in simulation.R
  # The population is simply a data table with 3 columns:
  # id, sex, d_base
  # It is created with the "doppelganger quadrangle" method
  # so there are equal number of men and women and the true
  # mean and true variance of each sub population is the same
  population <- create_population()
  
  ###
  ### Create the datsets for each experiment ###
  ###
  # This function is in simulation.R
  # The datasets are stored in a single table with 5 columns:
  # data_set - the id of the experiment
  # participant_id - the id of the participant within that experiment
  # sex - the sex of the participant
  # condition - the condition the participant was in
  # response - the mean response of that participant, ranging from 0 to 1
  data_sets <- create_datasets()
  
  ###
  ### run analyses ###
  ###
  # Now that we have all the data_sets we perform the desired analyses over them.
  # It is in these methods that we start to create the results table. Each entry
  # in this table corresponds to a single analysis on a single data_set.
  print(">>>>>>>> Doing analyses")
  
  if (do_anova == TRUE) {
    print(">>>>>>>>>>>> Doing anovas")
    # prep columns
    repeat_id <- c(repeat_id, rep(rep, n_experiments_per_repeat))
    expt <- c(expt, c(1:n_experiments_per_repeat))
    true_base <- c(true_base, rep(b_base, n_experiments_per_repeat))
    true_sex <- c(true_sex, rep(b_sex, n_experiments_per_repeat))
    true_cond <- c(true_cond, rep(b_cond, n_experiments_per_repeat))
    true_sex_cond <- c(true_sex_cond, rep(b_sex_cond, n_experiments_per_repeat))
    analysis_type <- c(analysis_type, rep("anova", n_experiments_per_repeat))

    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
    
      # do the anova
      model <- this_data_set$response ~ this_data_set$sex * this_data_set$condition
      this_anova <- aov(model)
      
      # save the results of the anova
      b_base_med <- c(b_base_med, this_anova[[1]][[1]])
      b_sex_med <- c(b_sex_med, this_anova[[1]][[2]])
      b_cond_med <- c(b_cond_med, this_anova[[1]][[3]])
      b_sex_cond_med <- c(b_sex_cond_med, this_anova[[1]][[4]])
      
      ci <- confint(this_anova)
      b_base_lower <- c(b_base_lower, ci[1])
      b_sex_lower <- c(b_sex_lower, ci[2])
      b_cond_lower <- c(b_cond_lower, ci[3])
      b_sex_cond_lower <- c(b_sex_cond_lower, ci[4])
      
      b_base_upper <- c(b_base_upper, ci[5])
      b_sex_upper <- c(b_sex_upper, ci[6])
      b_cond_upper <- c(b_cond_upper, ci[7])
      b_sex_cond_upper <- c(b_sex_cond_upper, ci[8])
      
      p_vals <- drop1(this_anova,~.,test="F")
      b_sex_p_value <- c(b_sex_p_value, p_vals[[6]][2])
      b_cond_p_value <- c(b_cond_p_value, p_vals[[6]][3])
      b_sex_cond_p_value <- c(b_sex_cond_p_value, p_vals[[6]][4])
    }
  }
  
  if (do_glmm == TRUE) {
    print(">>>>>>>>>>>> Doing glmms")
    # prep columns
    repeat_id <- c(repeat_id, rep(rep, n_experiments_per_repeat))
    expt <- c(expt, c(1:n_experiments_per_repeat))
    true_base <- c(true_base, rep(b_base, n_experiments_per_repeat))
    true_sex <- c(true_sex, rep(b_sex, n_experiments_per_repeat))
    true_cond <- c(true_cond, rep(b_cond, n_experiments_per_repeat))
    true_sex_cond <- c(true_sex_cond, rep(b_sex_cond, n_experiments_per_repeat))
    analysis_type <- c(analysis_type, rep("glmm", n_experiments_per_repeat))
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      # do the glmm
      glmm <- glmer(cbind(this_data_set$response*n_trials_per_participant,  (1-this_data_set$response)*n_trials_per_participant)~ this_data_set$sex + this_data_set$condition + this_data_set$sex*this_data_set$condition + (1 | as.factor(this_data_set$participant_id)), family = binomial)
      ci <- confint(glmm, method="Wald")
      fe <- fixef(glmm)
      
      # save the results of the glmm
      b_base_med <- c(b_base_med, fe[[1]])
      b_sex_med <- c(b_sex_med, fe[[2]])
      b_cond_med <- c(b_cond_med, fe[[3]])
      b_sex_cond_med <- c(b_sex_cond_med, fe[[4]])
      
      b_base_lower <- c(b_base_lower, ci[1])
      b_sex_lower <- c(b_sex_lower, ci[2])
      b_cond_lower <- c(b_cond_lower, ci[3])
      b_sex_cond_lower <- c(b_sex_cond_lower, ci[4])
      
      b_base_upper <- c(b_base_upper, ci[5])
      b_sex_upper <- c(b_sex_upper, ci[6])
      b_cond_upper <- c(b_cond_upper, ci[7])
      b_sex_cond_upper <- c(b_sex_cond_upper, ci[8])
      
      b_sex_p_value <- c(b_sex_p_value, NaN)
      b_cond_p_value <- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <- c(b_sex_cond_p_value, NaN)
    } # end of for each experiment loop
  } # end of do_glmm
  
  if (do_bglmm == TRUE) {
    print(">>>>>>>>>>>> Doing b-glmms")
    # prep columns
    repeat_id <- c(repeat_id, rep(rep, n_experiments_per_repeat))
    expt <- c(expt, c(1:n_experiments_per_repeat))
    true_base <- c(true_base, rep(b_base, n_experiments_per_repeat))
    true_sex <- c(true_sex, rep(b_sex, n_experiments_per_repeat))
    true_cond <- c(true_cond, rep(b_cond, n_experiments_per_repeat))
    true_sex_cond <- c(true_sex_cond, rep(b_sex_cond, n_experiments_per_repeat))
    analysis_type <- c(analysis_type, rep("bglmm", n_experiments_per_repeat))
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      successes <- this_data_set$response*n_trials_per_participant
      np <- rep(n_participants_per_experiment, n_participants_per_experiment)
      nt <- rep(n_trials_per_participant, n_participants_per_experiment)
      this_data_set <- cbind(this_data_set, successes, np, nt)
      
      # do the bglmm
      model <- jags.model("bglmm.R", data=this_data_set, n.chains=3, quiet=TRUE)
      nodes <- c("beta", "tau_participant")
      samples <- coda.samples(model,nodes,2000,1)
      
      # save the results of the glmm
      b_base_samples <- c(samples[,1][[1]], samples[,1][[2]], samples[,1][[3]])
      b_sex_samples <- c(samples[,2][[1]], samples[,2][[2]], samples[,2][[3]])
      b_cond_samples <- c(samples[,3][[1]], samples[,3][[2]], samples[,3][[3]])
      b_sex_cond_samples <- c(samples[,4][[1]], samples[,4][[2]], samples[,4][[3]])
      
      b_base_med <- c(b_base_med, median(b_base_samples))
      b_sex_med <- c(b_sex_med, median(b_sex_samples))
      b_cond_med <- c(b_cond_med, median(b_cond_samples))
      b_sex_cond_med <- c(b_sex_cond_med, median(b_sex_cond_samples))
      
      b_base_lower <- c(b_base_lower, quantile(b_base_samples, probs=c(0.025)))
      b_sex_lower <- c(b_sex_lower, quantile(b_sex_samples, probs=c(0.025)))
      b_cond_lower <- c(b_cond_lower, quantile(b_cond_samples, probs=c(0.025)))
      b_sex_cond_lower <- c(b_sex_cond_lower, quantile(b_sex_cond_samples, probs=c(0.025)))
      
      b_base_upper <- c(b_base_upper, quantile(b_base_samples, probs=c(0.975)))
      b_sex_upper <- c(b_sex_upper, quantile(b_sex_samples, probs=c(0.975)))
      b_cond_upper <- c(b_cond_upper, quantile(b_cond_samples, probs=c(0.975)))
      b_sex_cond_upper <- c(b_sex_cond_upper, quantile(b_sex_cond_samples, probs=c(0.975)))
      
      b_sex_p_value <- c(b_sex_p_value, NaN)
      b_cond_p_value <- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <- c(b_sex_cond_p_value, NaN)
      
      rm(b_base_samples, b_sex_samples, b_cond_samples, b_sex_cond_samples)
    } # end of for each experiment loop
  }# end of do bglmm
  
  if (do_pp == TRUE) {
    print(">>>>>>>>>>>> Doing posterior passing")
    # prep columns
    repeat_id <- c(repeat_id, rep(rep, n_experiments_per_repeat))
    expt <- c(expt, c(1:n_experiments_per_repeat))
    true_base <- c(true_base, rep(b_base, n_experiments_per_repeat))
    true_sex <- c(true_sex, rep(b_sex, n_experiments_per_repeat))
    true_cond <- c(true_cond, rep(b_cond, n_experiments_per_repeat))
    true_sex_cond <- c(true_sex_cond, rep(b_sex_cond, n_experiments_per_repeat))
    analysis_type <- c(analysis_type, rep("pp", n_experiments_per_repeat))
    
    #set initial prior for beta[4]
    pp_u <- rep(0, n_participants_per_experiment)
    pp_prec <- rep(0.01, n_participants_per_experiment)
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      successes <- this_data_set$response*n_trials_per_participant
      np <- rep(n_participants_per_experiment, n_participants_per_experiment)
      nt <- rep(n_trials_per_participant, n_participants_per_experiment)
      this_data_set <- cbind(this_data_set, successes, np, nt, pp_u, pp_prec)
      
      # do the pp analysis
      model <- jags.model("pp.R", data=this_data_set, n.chains=3, quiet=TRUE)
      nodes <- c("beta", "tau_participant")
      samples <- coda.samples(model,nodes,2000,1)
      
      # save the results of the glmm
      b_base_samples <- c(samples[,1][[1]], samples[,1][[2]], samples[,1][[3]])
      b_sex_samples <- c(samples[,2][[1]], samples[,2][[2]], samples[,2][[3]])
      b_cond_samples <- c(samples[,3][[1]], samples[,3][[2]], samples[,3][[3]])
      b_sex_cond_samples <- c(samples[,4][[1]], samples[,4][[2]], samples[,4][[3]])
      
      b_base_med <- c(b_base_med, median(b_base_samples))
      b_sex_med <- c(b_sex_med, median(b_sex_samples))
      b_cond_med <- c(b_cond_med, median(b_cond_samples))
      b_sex_cond_med <- c(b_sex_cond_med, median(b_sex_cond_samples))
      
      b_base_lower <- c(b_base_lower, quantile(b_base_samples, probs=c(0.025)))
      b_sex_lower <- c(b_sex_lower, quantile(b_sex_samples, probs=c(0.025)))
      b_cond_lower <- c(b_cond_lower, quantile(b_cond_samples, probs=c(0.025)))
      b_sex_cond_lower <- c(b_sex_cond_lower, quantile(b_sex_cond_samples, probs=c(0.025)))
      
      b_base_upper <- c(b_base_upper, quantile(b_base_samples, probs=c(0.975)))
      b_sex_upper <- c(b_sex_upper, quantile(b_sex_samples, probs=c(0.975)))
      b_cond_upper <- c(b_cond_upper, quantile(b_cond_samples, probs=c(0.975)))
      b_sex_cond_upper <- c(b_sex_cond_upper, quantile(b_sex_cond_samples, probs=c(0.975)))
      
      b_sex_p_value <- c(b_sex_p_value, NaN)
      b_cond_p_value <- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <- c(b_sex_cond_p_value, NaN)
      
      # update the priors for the next run
      pp_u[1] <- median(b_sex_cond_samples)
      pp_prec[1] <- 1/var(b_sex_cond_samples)
      
      rm(b_base_samples, b_sex_samples, b_cond_samples, b_sex_cond_samples)
    } # end of for each experiment loop
  }# end of do pp
  rm(data_sets, population, participants)
} # end of for each repeat loop

###
### Create the Meta-results table
###
# now we have all our results so we parse them as a single table and collapse
# each repeat simulation (i.e. each run of experiments) into a single entry

print(">>>>>>>> Saving results")
# combine results into a data frame
results <- data.frame(repeat_id, expt, analysis_type,
                      true_base, b_base_lower, b_base_med, b_base_upper,
                      true_sex, b_sex_p_value, b_sex_lower, b_sex_med, b_sex_upper,
                      true_cond, b_cond_p_value, b_cond_lower, b_cond_med, b_cond_upper, 
                      true_sex_cond, b_sex_cond_p_value, b_sex_cond_lower, b_sex_cond_med, b_sex_cond_upper)
rm(this_anova, this_data_set, model, ci, p_vals,
   glmm, fe,
   successes, np, nt,
   repeat_id, expt, analysis_type,
   true_base, b_base_lower, b_base_med, b_base_upper,
   true_sex, b_sex_p_value, b_sex_lower, b_sex_med, b_sex_upper,
   true_cond, b_cond_p_value, b_cond_lower, b_cond_med, b_cond_upper, 
   true_sex_cond, b_sex_cond_p_value, b_sex_cond_lower, b_sex_cond_med, b_sex_cond_upper)

for (rep in 1:n_repeats) {
  anova_results <- results[results$repeat_id == rep & results$analysis_type == "anova",]
  glmm_results <- results[results$repeat_id == rep & results$analysis_type == "glmm",]
  bglmm_results <- results[results$repeat_id == rep & results$analysis_type == "bglmm",]
  pp_results <- results[results$repeat_id == rep & results$analysis_type == "pp",]

  meta_base_estimate_anova <- c(meta_base_estimate_anova, mean(anova_results$b_base_med))
  meta_sex_estimate_anova <- c(meta_sex_estimate_anova, mean(anova_results$b_sex_med))
  meta_cond_estimate_anova <- c(meta_cond_estimate_anova, mean(anova_results$b_cond_med))
  meta_sex_cond_estimate_anova <- c(meta_sex_cond_estimate_anova, mean(anova_results$b_sex_cond_med))
  
  meta_base_estimate_glmm <- c(meta_base_estimate_glmm, mean(glmm_results$b_base_med))
  meta_sex_estimate_glmm <- c(meta_sex_estimate_glmm, mean(glmm_results$b_sex_med))
  meta_cond_estimate_glmm <- c(meta_cond_estimate_glmm, mean(glmm_results$b_cond_med))
  meta_sex_cond_estimate_glmm <- c(meta_sex_cond_estimate_glmm, mean(glmm_results$b_sex_cond_med))
  
  meta_base_estimate_bglmm <- c(meta_base_estimate_bglmm, mean(bglmm_results$b_base_med))
  meta_sex_estimate_bglmm <- c(meta_sex_estimate_bglmm, mean(bglmm_results$b_sex_med))
  meta_cond_estimate_bglmm <- c(meta_cond_estimate_bglmm, mean(bglmm_results$b_cond_med))
  meta_sex_cond_estimate_bglmm <- c(meta_sex_cond_estimate_bglmm, mean(bglmm_results$b_sex_cond_med))
  
  if (pp_final_expt_only == FALSE) {
    meta_base_estimate_pp <- c(meta_base_estimate_pp, mean(pp_results$b_base_med))
    meta_sex_estimate_pp <- c(meta_sex_estimate_pp, mean(pp_results$b_sex_med))
    meta_cond_estimate_pp <- c(meta_cond_estimate_pp, mean(pp_results$b_cond_med))
    meta_sex_cond_estimate_pp <- c(meta_sex_cond_estimate_pp, mean(pp_results$b_sex_cond_med))
  } else {
    meta_base_estimate_pp <- c(meta_base_estimate_pp, pp_results$b_base_med[pp_results$expt == n_experiments_per_repeat])
    meta_sex_estimate_pp <- c(meta_sex_estimate_pp, pp_results$b_sex_med[pp_results$expt == n_experiments_per_repeat])
    meta_cond_estimate_pp <- c(meta_cond_estimate_pp, pp_results$b_cond_med[pp_results$expt == n_experiments_per_repeat])
    meta_sex_cond_estimate_pp <- c(meta_sex_cond_estimate_pp, pp_results$b_sex_cond_med[pp_results$expt == n_experiments_per_repeat])
  }
  
  meta_base_estimate_upper_anova <- c(meta_base_estimate_upper_anova, mean(anova_results$b_base_upper))
  meta_sex_estimate_upper_anova <- c(meta_sex_estimate_upper_anova, mean(anova_results$b_sex_upper))
  meta_cond_estimate_upper_anova <- c(meta_cond_estimate_upper_anova, mean(anova_results$b_cond_upper))
  meta_sex_cond_estimate_upper_anova <- c(meta_sex_cond_estimate_upper_anova, mean(anova_results$b_sex_cond_upper))
  
  meta_base_estimate_upper_glmm <- c(meta_base_estimate_upper_glmm, mean(glmm_results$b_base_upper))
  meta_sex_estimate_upper_glmm <- c(meta_sex_estimate_upper_glmm, mean(glmm_results$b_sex_upper))
  meta_cond_estimate_upper_glmm <- c(meta_cond_estimate_upper_glmm, mean(glmm_results$b_cond_upper))
  meta_sex_cond_estimate_upper_glmm <- c(meta_sex_cond_estimate_upper_glmm, mean(glmm_results$b_sex_cond_upper))
  
  meta_base_estimate_upper_bglmm <- c(meta_base_estimate_upper_bglmm, mean(bglmm_results$b_base_upper))
  meta_sex_estimate_upper_bglmm <- c(meta_sex_estimate_upper_bglmm, mean(bglmm_results$b_sex_upper))
  meta_cond_estimate_upper_bglmm <- c(meta_cond_estimate_upper_bglmm, mean(bglmm_results$b_cond_upper))
  meta_sex_cond_estimate_upper_bglmm <- c(meta_sex_cond_estimate_upper_bglmm, mean(bglmm_results$b_sex_cond_upper))
  
  if (pp_final_expt_only == FALSE) {
    meta_base_estimate_upper_pp <- c(meta_base_estimate_upper_pp, mean(pp_results$b_base_upper))
    meta_sex_estimate_upper_pp <- c(meta_sex_estimate_upper_pp, mean(pp_results$b_sex_upper))
    meta_cond_estimate_upper_pp <- c(meta_cond_estimate_upper_pp, mean(pp_results$b_cond_upper))
    meta_sex_cond_estimate_upper_pp <- c(meta_sex_cond_estimate_upper_pp, mean(pp_results$b_sex_cond_upper))
  } else {
    meta_base_estimate_upper_pp <- c(meta_base_estimate_upper_pp, pp_results$b_base_upper[pp_results$expt == n_experiments_per_repeat])
    meta_sex_estimate_upper_pp <- c(meta_sex_estimate_upper_pp, pp_results$b_sex_upper[pp_results$expt == n_experiments_per_repeat])
    meta_cond_estimate_upper_pp <- c(meta_cond_estimate_upper_pp, pp_results$b_cond_upper[pp_results$expt == n_experiments_per_repeat])
    meta_sex_cond_estimate_upper_pp <- c(meta_sex_cond_estimate_upper_pp, pp_results$b_sex_cond_upper[pp_results$expt == n_experiments_per_repeat])
  }
  
  meta_base_estimate_lower_anova <- c(meta_base_estimate_lower_anova, mean(anova_results$b_base_lower))
  meta_sex_estimate_lower_anova <- c(meta_sex_estimate_lower_anova, mean(anova_results$b_sex_lower))
  meta_cond_estimate_lower_anova <- c(meta_cond_estimate_lower_anova, mean(anova_results$b_cond_lower))
  meta_sex_cond_estimate_lower_anova <- c(meta_sex_cond_estimate_lower_anova, mean(anova_results$b_sex_cond_lower))
  
  meta_base_estimate_lower_glmm <- c(meta_base_estimate_lower_glmm, mean(glmm_results$b_base_lower))
  meta_sex_estimate_lower_glmm <- c(meta_sex_estimate_lower_glmm, mean(glmm_results$b_sex_lower))
  meta_cond_estimate_lower_glmm <- c(meta_cond_estimate_lower_glmm, mean(glmm_results$b_cond_lower))
  meta_sex_cond_estimate_lower_glmm <- c(meta_sex_cond_estimate_lower_glmm, mean(glmm_results$b_sex_cond_lower))
  
  meta_base_estimate_lower_bglmm <- c(meta_base_estimate_lower_bglmm, mean(bglmm_results$b_base_lower))
  meta_sex_estimate_lower_bglmm <- c(meta_sex_estimate_lower_bglmm, mean(bglmm_results$b_sex_lower))
  meta_cond_estimate_lower_bglmm <- c(meta_cond_estimate_lower_bglmm, mean(bglmm_results$b_cond_lower))
  meta_sex_cond_estimate_lower_bglmm <- c(meta_sex_cond_estimate_lower_bglmm, mean(bglmm_results$b_sex_cond_lower))
  
  if (pp_final_expt_only == FALSE) {
    meta_base_estimate_lower_pp <- c(meta_base_estimate_lower_pp, mean(pp_results$b_base_lower))
    meta_sex_estimate_lower_pp <- c(meta_sex_estimate_lower_pp, mean(pp_results$b_sex_lower))
    meta_cond_estimate_lower_pp <- c(meta_cond_estimate_lower_pp, mean(pp_results$b_cond_lower))
    meta_sex_cond_estimate_lower_pp <- c(meta_sex_cond_estimate_lower_pp, mean(pp_results$b_sex_cond_lower))
  } else {
    meta_base_estimate_lower_pp <- c(meta_base_estimate_lower_pp, pp_results$b_base_lower[pp_results$expt == n_experiments_per_repeat])
    meta_sex_estimate_lower_pp <- c(meta_sex_estimate_lower_pp, pp_results$b_sex_lower[pp_results$expt == n_experiments_per_repeat])
    meta_cond_estimate_lower_pp <- c(meta_cond_estimate_lower_pp, pp_results$b_cond_lower[pp_results$expt == n_experiments_per_repeat])
    meta_sex_cond_estimate_lower_pp <- c(meta_sex_cond_estimate_lower_pp, pp_results$b_sex_cond_lower[pp_results$expt == n_experiments_per_repeat])
  }
  
  meta_sex_positive_rate_anova <- c(meta_sex_positive_rate_anova, mean(anova_results$b_sex_p_value < 0.05))
  meta_cond_positive_rate_anova <- c(meta_cond_positive_rate_anova, mean(anova_results$b_cond_p_value < 0.05))
  meta_sex_cond_positive_rate_anova <- c(meta_sex_cond_positive_rate_anova, mean(anova_results$b_sex_cond_p_value < 0.05))
  
  meta_sex_positive_rate_glmm <- c(meta_sex_positive_rate_glmm, mean(glmm_results$b_sex_lower > 0 | glmm_results$b_sex_upper < 0))
  meta_cond_positive_rate_glmm <- c(meta_cond_positive_rate_glmm, mean(glmm_results$b_cond_lower > 0 | glmm_results$b_cond_upper < 0))
  meta_sex_cond_positive_rate_glmm <- c(meta_sex_cond_positive_rate_glmm, mean(glmm_results$b_sex_cond_lower > 0 | glmm_results$b_sex_cond_upper < 0))
  
  meta_sex_positive_rate_bglmm <- c(meta_sex_positive_rate_bglmm, mean(bglmm_results$b_sex_lower > 0 | bglmm_results$b_sex_upper < 0))
  meta_cond_positive_rate_bglmm <- c(meta_cond_positive_rate_bglmm, mean(bglmm_results$b_cond_lower > 0 | bglmm_results$b_cond_upper < 0))
  meta_sex_cond_positive_rate_bglmm <- c(meta_sex_cond_positive_rate_bglmm, mean(bglmm_results$b_sex_cond_lower > 0 | bglmm_results$b_sex_cond_upper < 0))
  
  if (pp_final_expt_only == FALSE) {
    meta_sex_positive_rate_pp <- c(meta_sex_positive_rate_pp, mean(pp_results$b_sex_lower > 0 | pp_results$b_sex_upper < 0))
    meta_cond_positive_rate_pp <- c(meta_cond_positive_rate_pp, mean(pp_results$b_cond_lower > 0 | pp_results$b_cond_upper < 0))
    meta_sex_cond_positive_rate_pp <- c(meta_sex_cond_positive_rate_pp, mean(pp_results$b_sex_cond_lower > 0 | pp_results$b_sex_cond_upper < 0))
  } else {
    meta_sex_positive_rate_pp <- c(meta_sex_positive_rate_pp, 1*(pp_results$b_sex_lower[pp_results$expt == n_experiments_per_repeat] > 0 | pp_results$b_sex_upper[pp_results$expt == n_experiments_per_repeat] < 0))
    meta_cond_positive_rate_pp <- c(meta_cond_positive_rate_pp, 1*(pp_results$b_cond_lower[pp_results$expt == n_experiments_per_repeat] > 0 | pp_results$b_cond_upper[pp_results$expt == n_experiments_per_repeat] < 0))
    meta_sex_cond_positive_rate_pp <- c(meta_sex_cond_positive_rate_pp, 1*(pp_results$b_sex_cond_lower[pp_results$expt == n_experiments_per_repeat] > 0 | pp_results$b_sex_cond_upper[pp_results$expt == n_experiments_per_repeat] < 0))
  }
  
  meta_base_uncertainty_anova <- c(meta_base_uncertainty_anova, mean(anova_results$b_base_upper - anova_results$b_base_lower))
  meta_sex_uncertainty_anova <- c(meta_sex_uncertainty_anova, mean(anova_results$b_sex_upper - anova_results$b_sex_lower))
  meta_cond_uncertainty_anova <- c(meta_cond_uncertainty_anova, mean(anova_results$b_cond_upper - anova_results$b_cond_lower))
  meta_sex_cond_uncertainty_anova <- c(meta_sex_cond_uncertainty_anova, mean(anova_results$b_sex_cond_upper - anova_results$b_sex_cond_lower))
  
  meta_base_uncertainty_glmm <- c(meta_base_uncertainty_glmm, mean(glmm_results$b_base_upper - glmm_results$b_base_lower))
  meta_sex_uncertainty_glmm <- c(meta_sex_uncertainty_glmm, mean(glmm_results$b_sex_upper - glmm_results$b_sex_lower))
  meta_cond_uncertainty_glmm <- c(meta_cond_uncertainty_glmm, mean(glmm_results$b_cond_upper - glmm_results$b_cond_lower))
  meta_sex_cond_uncertainty_glmm <- c(meta_sex_cond_uncertainty_glmm, mean(glmm_results$b_sex_cond_upper - glmm_results$b_sex_cond_lower))
  
  meta_base_uncertainty_bglmm <- c(meta_base_uncertainty_bglmm, mean(bglmm_results$b_base_upper - bglmm_results$b_base_lower))
  meta_sex_uncertainty_bglmm <- c(meta_sex_uncertainty_bglmm, mean(bglmm_results$b_sex_upper - bglmm_results$b_sex_lower))
  meta_cond_uncertainty_bglmm <- c(meta_cond_uncertainty_bglmm, mean(bglmm_results$b_cond_upper - bglmm_results$b_cond_lower))
  meta_sex_cond_uncertainty_bglmm <- c(meta_sex_cond_uncertainty_bglmm, mean(bglmm_results$b_sex_cond_upper - bglmm_results$b_sex_cond_lower))
  
  if (pp_final_expt_only == FALSE) {
    meta_base_uncertainty_pp <- c(meta_base_uncertainty_pp, mean(pp_results$b_base_upper - pp_results$b_base_lower))
    meta_sex_uncertainty_pp <- c(meta_sex_uncertainty_pp, mean(pp_results$b_sex_upper - pp_results$b_sex_lower))
    meta_cond_uncertainty_pp <- c(meta_cond_uncertainty_pp, mean(pp_results$b_cond_upper - pp_results$b_cond_lower))
    meta_sex_cond_uncertainty_pp <- c(meta_sex_cond_uncertainty_pp, mean(pp_results$b_sex_cond_upper - pp_results$b_sex_cond_lower))
  } else {
    meta_base_uncertainty_pp <- c(meta_base_uncertainty_pp, pp_results$b_base_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_base_lower[pp_results$expt == n_experiments_per_repeat])
    meta_sex_uncertainty_pp <- c(meta_sex_uncertainty_pp, pp_results$b_sex_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_sex_lower[pp_results$expt == n_experiments_per_repeat])
    meta_cond_uncertainty_pp <- c(meta_cond_uncertainty_pp, pp_results$b_cond_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_cond_lower[pp_results$expt == n_experiments_per_repeat])
    meta_sex_cond_uncertainty_pp <- c(meta_sex_cond_uncertainty_pp, pp_results$b_sex_cond_upper[pp_results$expt == n_experiments_per_repeat] - pp_results$b_sex_cond_lower[pp_results$expt == n_experiments_per_repeat])
  }
}
rm(anova_results, glmm_results, bglmm_results, pp_results)

              }
            }
          }
        }
      }
    }
  }
} # end of all for all parameter value loops

meta_base_estimate_glmm_p <- exp(meta_base_estimate_glmm)/(1+exp(meta_base_estimate_glmm))
meta_sex_estimate_glmm_p <- exp(meta_sex_estimate_glmm+meta_base_estimate_glmm)/(1+exp(meta_sex_estimate_glmm+meta_base_estimate_glmm)) - meta_base_estimate_glmm_p
meta_cond_estimate_glmm_p <- exp(meta_cond_estimate_glmm+meta_base_estimate_glmm)/(1+exp(meta_cond_estimate_glmm+meta_base_estimate_glmm)) - meta_base_estimate_glmm_p
meta_sex_cond_estimate_glmm_p <- exp(meta_sex_cond_estimate_glmm + meta_sex_estimate_glmm + meta_cond_estimate_glmm + meta_base_estimate_glmm)/(1+exp(meta_sex_cond_estimate_glmm + meta_sex_estimate_glmm + meta_cond_estimate_glmm + meta_base_estimate_glmm)) - (meta_base_estimate_glmm_p + meta_sex_estimate_glmm_p + meta_cond_estimate_glmm_p)

meta_base_estimate_bglmm_p <- exp(meta_base_estimate_bglmm)/(1+exp(meta_base_estimate_bglmm))
meta_sex_estimate_bglmm_p <- exp(meta_sex_estimate_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_sex_estimate_bglmm+meta_base_estimate_bglmm)) - meta_base_estimate_bglmm_p
meta_cond_estimate_bglmm_p <- exp(meta_cond_estimate_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_cond_estimate_bglmm+meta_base_estimate_bglmm)) - meta_base_estimate_bglmm_p
meta_sex_cond_estimate_bglmm_p <- exp(meta_sex_cond_estimate_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm + meta_base_estimate_bglmm)/(1+exp(meta_sex_cond_estimate_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm + meta_base_estimate_bglmm)) - (meta_base_estimate_bglmm_p + meta_sex_estimate_bglmm_p + meta_cond_estimate_bglmm_p)

meta_base_estimate_pp_p <- exp(meta_base_estimate_pp)/(1+exp(meta_base_estimate_pp))
meta_sex_estimate_pp_p <- exp(meta_sex_estimate_pp+meta_base_estimate_pp)/(1+exp(meta_sex_estimate_pp+meta_base_estimate_pp)) - meta_base_estimate_pp_p
meta_cond_estimate_pp_p <- exp(meta_cond_estimate_pp+meta_base_estimate_pp)/(1+exp(meta_cond_estimate_pp+meta_base_estimate_pp)) - meta_base_estimate_pp_p
meta_sex_cond_estimate_pp_p <- exp(meta_sex_cond_estimate_pp + meta_sex_estimate_pp + meta_cond_estimate_pp + meta_base_estimate_pp)/(1+exp(meta_sex_cond_estimate_pp + meta_sex_estimate_pp + meta_cond_estimate_pp + meta_base_estimate_pp)) - (meta_base_estimate_pp_p + meta_sex_estimate_pp_p + meta_cond_estimate_pp_p)

meta_base_uncertainty_glmm_p <- exp(meta_base_estimate_upper_glmm)/(1+exp(meta_base_estimate_upper_glmm)) - exp(meta_base_estimate_lower_glmm)/(1+exp(meta_base_estimate_lower_glmm))
meta_sex_uncertainty_glmm_p <- exp(meta_sex_estimate_upper_glmm+meta_base_estimate_glmm)/(1+exp(meta_sex_estimate_upper_glmm+meta_base_estimate_glmm)) - exp(meta_sex_estimate_lower_glmm+meta_base_estimate_glmm)/(1+exp(meta_sex_estimate_lower_glmm+meta_base_estimate_glmm))
meta_cond_uncertainty_glmm_p <- exp(meta_cond_estimate_upper_glmm+meta_base_estimate_glmm)/(1+exp(meta_cond_estimate_upper_glmm+meta_base_estimate_glmm)) - exp(meta_cond_estimate_lower_glmm+meta_base_estimate_glmm)/(1+exp(meta_cond_estimate_lower_glmm+meta_base_estimate_glmm))
meta_sex_cond_uncertainty_glmm_p <- exp(meta_sex_cond_estimate_upper_glmm + meta_sex_estimate_glmm + meta_cond_estimate_glmm +meta_base_estimate_glmm)/(1+exp(meta_sex_cond_estimate_upper_glmm + meta_sex_estimate_glmm + meta_cond_estimate_glmm +meta_base_estimate_glmm)) - exp(meta_sex_cond_estimate_lower_glmm + meta_sex_estimate_glmm + meta_cond_estimate_glmm +meta_base_estimate_glmm)/(1+exp(meta_sex_cond_estimate_lower_glmm + meta_sex_estimate_glmm + meta_cond_estimate_glmm +meta_base_estimate_glmm))

meta_base_uncertainty_bglmm_p <- exp(meta_base_estimate_upper_bglmm)/(1+exp(meta_base_estimate_upper_bglmm)) - exp(meta_base_estimate_lower_bglmm)/(1+exp(meta_base_estimate_lower_bglmm))
meta_sex_uncertainty_bglmm_p <- exp(meta_sex_estimate_upper_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_sex_estimate_upper_bglmm+meta_base_estimate_bglmm)) - exp(meta_sex_estimate_lower_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_sex_estimate_lower_bglmm+meta_base_estimate_bglmm))
meta_cond_uncertainty_bglmm_p <- exp(meta_cond_estimate_upper_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_cond_estimate_upper_bglmm+meta_base_estimate_bglmm)) - exp(meta_cond_estimate_lower_bglmm+meta_base_estimate_bglmm)/(1+exp(meta_cond_estimate_lower_bglmm+meta_base_estimate_bglmm))
meta_sex_cond_uncertainty_bglmm_p <- exp(meta_sex_cond_estimate_upper_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm)/(1+exp(meta_sex_cond_estimate_upper_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm)) - exp(meta_sex_cond_estimate_lower_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm)/(1+exp(meta_sex_cond_estimate_lower_bglmm + meta_sex_estimate_bglmm + meta_cond_estimate_bglmm +meta_base_estimate_bglmm))

meta_base_uncertainty_pp_p <- exp(meta_base_estimate_upper_pp)/(1+exp(meta_base_estimate_upper_pp)) - exp(meta_base_estimate_lower_pp)/(1+exp(meta_base_estimate_lower_pp))
meta_sex_uncertainty_pp_p <- exp(meta_sex_estimate_upper_pp+meta_base_estimate_pp)/(1+exp(meta_sex_estimate_upper_pp+meta_base_estimate_pp)) - exp(meta_sex_estimate_lower_pp+meta_base_estimate_pp)/(1+exp(meta_sex_estimate_lower_pp+meta_base_estimate_pp))
meta_cond_uncertainty_pp_p <- exp(meta_cond_estimate_upper_pp+meta_base_estimate_pp)/(1+exp(meta_cond_estimate_upper_pp+meta_base_estimate_pp)) - exp(meta_cond_estimate_lower_pp+meta_base_estimate_pp)/(1+exp(meta_cond_estimate_lower_pp+meta_base_estimate_pp))
meta_sex_cond_uncertainty_pp_p <- exp(meta_sex_cond_estimate_upper_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp)/(1+exp(meta_sex_cond_estimate_upper_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp)) - exp(meta_sex_cond_estimate_lower_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp)/(1+exp(meta_sex_cond_estimate_lower_pp + meta_sex_estimate_pp + meta_cond_estimate_pp +meta_base_estimate_pp))

meta_results <- data.frame(meta_n_repeats,meta_n_experiments_per_repeat, meta_n_participants_per_experiment,
                           meta_n_trials_per_participant, meta_n_people,
                           meta_true_base, meta_true_sex, meta_true_cond, meta_true_sex_cond, 
                           meta_var_base, meta_var_sex, meta_var_cond, meta_var_sex_cond,
                           meta_base_estimate_anova, meta_sex_estimate_anova, meta_cond_estimate_anova, meta_sex_cond_estimate_anova,
                           meta_base_estimate_glmm, meta_sex_estimate_glmm, meta_cond_estimate_glmm, meta_sex_cond_estimate_glmm,
                           meta_base_estimate_glmm_p, meta_sex_estimate_glmm_p, meta_cond_estimate_glmm_p, meta_sex_cond_estimate_glmm_p,
                           meta_base_estimate_bglmm, meta_sex_estimate_bglmm, meta_cond_estimate_bglmm, meta_sex_cond_estimate_bglmm,
                           meta_base_estimate_bglmm_p, meta_sex_estimate_bglmm_p, meta_cond_estimate_bglmm_p, meta_sex_cond_estimate_bglmm_p,
                           meta_base_estimate_pp, meta_sex_estimate_pp, meta_cond_estimate_pp, meta_sex_cond_estimate_pp,
                           meta_base_estimate_pp_p, meta_sex_estimate_pp_p, meta_cond_estimate_pp_p, meta_sex_cond_estimate_pp_p,
                           meta_base_estimate_lower_anova, meta_sex_estimate_lower_anova, meta_cond_estimate_lower_anova, meta_sex_cond_estimate_lower_anova,
                           meta_base_estimate_lower_glmm, meta_sex_estimate_lower_glmm, meta_cond_estimate_lower_glmm, meta_sex_cond_estimate_lower_glmm,
                           meta_base_estimate_lower_bglmm, meta_sex_estimate_lower_bglmm, meta_cond_estimate_lower_bglmm, meta_sex_cond_estimate_lower_bglmm,
                           meta_base_estimate_lower_pp, meta_sex_estimate_lower_pp, meta_cond_estimate_lower_pp, meta_sex_cond_estimate_lower_pp,
                           meta_base_estimate_upper_anova, meta_sex_estimate_upper_anova, meta_cond_estimate_upper_anova, meta_sex_cond_estimate_upper_anova,
                           meta_base_estimate_upper_glmm, meta_sex_estimate_upper_glmm, meta_cond_estimate_upper_glmm, meta_sex_cond_estimate_upper_glmm,
                           meta_base_estimate_upper_bglmm, meta_sex_estimate_upper_bglmm, meta_cond_estimate_upper_bglmm, meta_sex_cond_estimate_upper_bglmm,
                           meta_base_estimate_upper_pp, meta_sex_estimate_upper_pp, meta_cond_estimate_upper_pp, meta_sex_cond_estimate_upper_pp,
                           meta_sex_positive_rate_anova, meta_cond_positive_rate_anova, meta_sex_cond_positive_rate_anova,
                           meta_sex_positive_rate_glmm, meta_cond_positive_rate_glmm, meta_sex_cond_positive_rate_glmm,
                           meta_sex_positive_rate_bglmm, meta_cond_positive_rate_bglmm, meta_sex_cond_positive_rate_bglmm,
                           meta_sex_positive_rate_pp, meta_cond_positive_rate_pp, meta_sex_cond_positive_rate_pp,
                           meta_base_uncertainty_anova, meta_sex_uncertainty_anova, meta_cond_uncertainty_anova, meta_sex_cond_uncertainty_anova,
                           meta_base_uncertainty_glmm, meta_sex_uncertainty_glmm, meta_cond_uncertainty_glmm, meta_sex_cond_uncertainty_glmm,
                           meta_base_uncertainty_glmm_p, meta_sex_uncertainty_glmm_p, meta_cond_uncertainty_glmm_p, meta_sex_cond_uncertainty_glmm_p,
                           meta_base_uncertainty_bglmm, meta_sex_uncertainty_bglmm, meta_cond_uncertainty_bglmm, meta_sex_cond_uncertainty_bglmm,
                           meta_base_uncertainty_bglmm_p, meta_sex_uncertainty_bglmm_p, meta_cond_uncertainty_bglmm_p, meta_sex_cond_uncertainty_bglmm_p,
                           meta_base_uncertainty_pp, meta_sex_uncertainty_pp, meta_cond_uncertainty_pp, meta_sex_cond_uncertainty_pp,
                           meta_base_uncertainty_pp_p, meta_sex_uncertainty_pp_p, meta_cond_uncertainty_pp_p, meta_sex_cond_uncertainty_pp_p)
                           
rm(meta_n_repeats,meta_n_experiments_per_repeat, meta_n_participants_per_experiment,
   meta_n_trials_per_participant, meta_n_people,
   meta_true_base, meta_true_sex, meta_true_cond, meta_true_sex_cond, 
   meta_var_base, meta_var_sex, meta_var_cond, meta_var_sex_cond,
   meta_base_estimate_anova, meta_sex_estimate_anova, meta_cond_estimate_anova, meta_sex_cond_estimate_anova,
   meta_base_estimate_glmm, meta_sex_estimate_glmm, meta_cond_estimate_glmm, meta_sex_cond_estimate_glmm,
   meta_base_estimate_glmm_p, meta_sex_estimate_glmm_p, meta_cond_estimate_glmm_p, meta_sex_cond_estimate_glmm_p,
   meta_base_estimate_bglmm, meta_sex_estimate_bglmm, meta_cond_estimate_bglmm, meta_sex_cond_estimate_bglmm,
   meta_base_estimate_bglmm_p, meta_sex_estimate_bglmm_p, meta_cond_estimate_bglmm_p, meta_sex_cond_estimate_bglmm_p,
   meta_base_estimate_pp, meta_sex_estimate_pp, meta_cond_estimate_pp, meta_sex_cond_estimate_pp,
   meta_base_estimate_pp_p, meta_sex_estimate_pp_p, meta_cond_estimate_pp_p, meta_sex_cond_estimate_pp_p,
   meta_base_estimate_lower_anova, meta_sex_estimate_lower_anova, meta_cond_estimate_lower_anova, meta_sex_cond_estimate_lower_anova,
   meta_base_estimate_lower_glmm, meta_sex_estimate_lower_glmm, meta_cond_estimate_lower_glmm, meta_sex_cond_estimate_lower_glmm,
   meta_base_estimate_lower_bglmm, meta_sex_estimate_lower_bglmm, meta_cond_estimate_lower_bglmm, meta_sex_cond_estimate_lower_bglmm,
   meta_base_estimate_lower_pp, meta_sex_estimate_lower_pp, meta_cond_estimate_lower_pp, meta_sex_cond_estimate_lower_pp,
   meta_base_estimate_upper_anova, meta_sex_estimate_upper_anova, meta_cond_estimate_upper_anova, meta_sex_cond_estimate_upper_anova,
   meta_base_estimate_upper_glmm, meta_sex_estimate_upper_glmm, meta_cond_estimate_upper_glmm, meta_sex_cond_estimate_upper_glmm,
   meta_base_estimate_upper_bglmm, meta_sex_estimate_upper_bglmm, meta_cond_estimate_upper_bglmm, meta_sex_cond_estimate_upper_bglmm,
   meta_base_estimate_upper_pp, meta_sex_estimate_upper_pp, meta_cond_estimate_upper_pp, meta_sex_cond_estimate_upper_pp,
   meta_sex_positive_rate_anova, meta_cond_positive_rate_anova, meta_sex_cond_positive_rate_anova,
   meta_sex_positive_rate_glmm, meta_cond_positive_rate_glmm, meta_sex_cond_positive_rate_glmm,
   meta_sex_positive_rate_bglmm, meta_cond_positive_rate_bglmm, meta_sex_cond_positive_rate_bglmm,
   meta_sex_positive_rate_pp, meta_cond_positive_rate_pp, meta_sex_cond_positive_rate_pp,
   meta_base_uncertainty_anova, meta_sex_uncertainty_anova, meta_cond_uncertainty_anova, meta_sex_cond_uncertainty_anova,
   meta_base_uncertainty_glmm, meta_sex_uncertainty_glmm, meta_cond_uncertainty_glmm, meta_sex_cond_uncertainty_glmm,
   meta_base_uncertainty_glmm_p, meta_sex_uncertainty_glmm_p, meta_cond_uncertainty_glmm_p, meta_sex_cond_uncertainty_glmm_p,
   meta_base_uncertainty_bglmm, meta_sex_uncertainty_bglmm, meta_cond_uncertainty_bglmm, meta_sex_cond_uncertainty_bglmm,
   meta_base_uncertainty_bglmm_p, meta_sex_uncertainty_bglmm_p, meta_cond_uncertainty_bglmm_p, meta_sex_cond_uncertainty_bglmm_p,
   meta_base_uncertainty_pp, meta_sex_uncertainty_pp, meta_cond_uncertainty_pp, meta_sex_cond_uncertainty_pp,
   meta_base_uncertainty_pp_p, meta_sex_uncertainty_pp_p, meta_cond_uncertainty_pp_p, meta_sex_cond_uncertainty_pp_p)


###                 ###
### PLOT THE GRAPHS ###
###                 ###

detach(meta_results)
attach(meta_results)

disp <- 0.02

super_plot <- function(all_points, y, x, disp_mult=0, color="black", xl="x-axis", yl="y-axis", very_first=FALSE, first=FALSE) {
  if (very_first==TRUE) {
    plot(y ~ c(x + disp*disp_mult), col=color, xlab=xl, ylab=yl, ylim=c(min(all_points), max(all_points)), xlim=c(min(x) - 2*disp, max(x) + 2*disp), par(mfrow=c(2,2), mar=c(4.5, 4, 1, 1), cex=1))
  } else if (first == TRUE) {
    plot(y ~ c(x + disp*disp_mult), col=color, xlab=xl, ylab=yl, ylim=c(min(all_points), max(all_points)), xlim=c(min(x) - 2*disp, max(x) + 2*disp))
  } else {
    points(y ~ c(x + disp*disp_mult), col=color, pch=1)
  }
  means <- vector()
  for (i in 1:length(unique(x))) {
    means <- c(means, mean(y[x==unique(x)[i]]))
  }
  points(means~c(unique(x)+disp*disp_mult), col=color, pch=16)
}

colors <- c("black", "red", "green", "blue")

if (plot_sex_cond==TRUE) {
  all_points <- c(meta_sex_cond_positive_rate_anova, meta_sex_cond_positive_rate_glmm, meta_sex_cond_positive_rate_bglmm, meta_sex_cond_positive_rate_pp)
  super_plot(all_points, meta_sex_cond_positive_rate_anova, meta_true_sex_cond, disp_mult=-1, xl="b_sex_cond", yl="positive result rate", very_first=TRUE)
  super_plot(all_points, meta_sex_cond_positive_rate_glmm, meta_true_sex_cond, color=colors[2])
  super_plot(all_points, meta_sex_cond_positive_rate_bglmm, meta_true_sex_cond, color=colors[3], disp_mult=1)
  super_plot(all_points, meta_sex_cond_positive_rate_pp, meta_true_sex_cond, color=colors[4], disp_mult=2)
  
  temp1 <- meta_true_base + meta_true_sex + meta_true_cond + meta_true_sex_cond
  temp2 <- meta_true_base + meta_true_sex + meta_true_cond
  true_vals <- exp(temp1)/(1+exp(temp1)) - exp(temp2)/(1+exp(temp2))
  all_points <- c(true_vals, meta_sex_cond_estimate_anova, meta_sex_cond_estimate_glmm_p, meta_sex_cond_estimate_bglmm_p, meta_sex_cond_estimate_pp_p)
  super_plot(all_points, true_vals, meta_true_sex_cond, col="pink", xl="b_sex_cond", yl="estimates", first=TRUE)
  super_plot(all_points, meta_sex_cond_estimate_anova, meta_true_sex_cond, disp_mult=-2, color=colors[1])
  super_plot(all_points, meta_sex_cond_estimate_glmm_p, meta_true_sex_cond, disp_mult=-1, color=colors[2])
  super_plot(all_points, meta_sex_cond_estimate_bglmm_p, meta_true_sex_cond, disp_mult=1, color=colors[3])
  super_plot(all_points, meta_sex_cond_estimate_pp_p, meta_true_sex_cond, disp_mult=2, color=colors[4])
  
  temp1 <- meta_true_base + meta_true_sex + meta_true_cond  + meta_true_sex_cond
  temp2 <- meta_true_base + meta_true_sex + meta_true_cond
  true_vals <- exp(temp1)/(1+exp(temp1)) - exp(temp2)/(1+exp(temp2))
  all_points <- abs(c(meta_sex_cond_estimate_anova - true_vals, meta_sex_cond_estimate_glmm_p - true_vals, meta_sex_cond_estimate_bglmm_p - true_vals, meta_sex_cond_estimate_pp_p - true_vals))
  super_plot(all_points, (abs(meta_sex_cond_estimate_anova - true_vals)), meta_true_sex_cond, disp_mult=-1, col=colors[1], xl="b_sex_cond", yl="error", first=TRUE)
  super_plot(all_points, (abs(meta_sex_cond_estimate_glmm_p - true_vals)), meta_true_sex_cond, disp_mult=0, color=colors[2])
  super_plot(all_points, (abs(meta_sex_cond_estimate_bglmm_p - true_vals)), meta_true_sex_cond, disp_mult=1, color=colors[3])
  super_plot(all_points, (abs(meta_sex_cond_estimate_pp_p - true_vals)), meta_true_sex_cond, disp_mult=2, color=colors[4])
  
  all_points <- c(meta_sex_cond_uncertainty_anova, meta_sex_cond_uncertainty_glmm_p, meta_sex_cond_uncertainty_bglmm_p, meta_sex_cond_uncertainty_pp_p)
  super_plot(all_points, meta_sex_cond_uncertainty_anova, meta_true_sex_cond, disp_mult=-1, color=colors[1], xl="b_sex_cond", yl="uncertainty", first=TRUE)
  super_plot(all_points, meta_sex_cond_uncertainty_glmm_p, meta_true_sex_cond, disp_mult=0, color=colors[2])
  super_plot(all_points, meta_sex_cond_uncertainty_bglmm_p, meta_true_sex_cond, disp_mult=1, color=colors[3])
  super_plot(all_points, meta_sex_cond_uncertainty_pp_p, meta_true_sex_cond, disp_mult=2, color=colors[4])
  rm(all_points, temp1, temp2)
}

if (plot_var_base == TRUE) {
  all_points <- c(meta_sex_cond_positive_rate_anova, meta_sex_cond_positive_rate_glmm, meta_sex_cond_positive_rate_bglmm, meta_sex_cond_positive_rate_pp)
  super_plot(all_points, meta_sex_cond_positive_rate_anova, meta_var_base, disp_mult=-1, xl="var_base", yl="positive result rate", very_first=TRUE)
  super_plot(all_points, meta_sex_cond_positive_rate_glmm, meta_var_base, color=colors[2])
  super_plot(all_points, meta_sex_cond_positive_rate_bglmm, meta_var_base, color=colors[3], disp_mult=1)
  super_plot(all_points, meta_sex_cond_positive_rate_pp, meta_var_base, color=colors[4], disp_mult=2)
  
  temp1 <- meta_true_base + meta_true_sex + meta_true_cond + meta_true_sex_cond
  temp2 <- meta_true_base + meta_true_sex + meta_true_cond
  true_vals <- exp(temp1)/(1+exp(temp1)) - exp(temp2)/(1+exp(temp2))
  all_points <- c(true_vals, meta_sex_cond_estimate_anova, meta_sex_cond_estimate_glmm_p, meta_sex_cond_estimate_bglmm_p, meta_sex_cond_estimate_pp_p)
  super_plot(all_points, true_vals, meta_var_base, col="pink", xl="var_base", yl="estimates", first=TRUE)
  super_plot(all_points, meta_sex_cond_estimate_anova, meta_var_base, disp_mult=-2, color=colors[1])
  super_plot(all_points, meta_sex_cond_estimate_glmm_p, meta_var_base, disp_mult=-1, color=colors[2])
  super_plot(all_points, meta_sex_cond_estimate_bglmm_p, meta_var_base, disp_mult=1, color=colors[3])
  super_plot(all_points, meta_sex_cond_estimate_pp_p, meta_var_base, disp_mult=2, color=colors[4])
  
  temp1 <- meta_true_base + meta_true_sex + meta_true_cond  + meta_true_sex_cond
  temp2 <- meta_true_base + meta_true_sex + meta_true_cond
  true_vals <- exp(temp1)/(1+exp(temp1)) - exp(temp2)/(1+exp(temp2))
  all_points <- abs(c(meta_sex_cond_estimate_anova - true_vals, meta_sex_cond_estimate_glmm_p - true_vals, meta_sex_cond_estimate_bglmm_p - true_vals, meta_sex_cond_estimate_pp_p - true_vals))
  super_plot(all_points, (abs(meta_sex_cond_estimate_anova - true_vals)), meta_var_base, disp_mult=-1, col=colors[1], xl="var_base", yl="error", first=TRUE)
  super_plot(all_points, (abs(meta_sex_cond_estimate_glmm_p - true_vals)), meta_var_base, disp_mult=0, color=colors[2])
  super_plot(all_points, (abs(meta_sex_cond_estimate_bglmm_p - true_vals)), meta_var_base, disp_mult=1, color=colors[3])
  super_plot(all_points, (abs(meta_sex_cond_estimate_pp_p - true_vals)), meta_var_base, disp_mult=2, color=colors[4])
  
  all_points <- c(meta_sex_cond_uncertainty_anova, meta_sex_cond_uncertainty_glmm_p, meta_sex_cond_uncertainty_bglmm_p, meta_sex_cond_uncertainty_pp_p)
  super_plot(all_points, meta_sex_cond_uncertainty_anova, meta_var_base, disp_mult=-1, color=colors[1], xl="var_base", yl="uncertainty", first=TRUE)
  super_plot(all_points, meta_sex_cond_uncertainty_glmm_p, meta_var_base, disp_mult=0, color=colors[2])
  super_plot(all_points, meta_sex_cond_uncertainty_bglmm_p, meta_var_base, disp_mult=1, color=colors[3])
  super_plot(all_points, meta_sex_cond_uncertainty_pp_p, meta_var_base, disp_mult=2, color=colors[4])
  rm(all_points, temp1, temp2)
}
  
  
