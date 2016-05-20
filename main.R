# imports
library(lme4)
library(rjags)
library(gplots)
source("util.R")
source("simulation.R")
source("analyses.R")

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
  # this function is in analyses.R
  # Now that we have all the data_sets we perform the desired analyses over them.
  # It is in these methods that we start to create the results table. Each entry
  # in this table corresponds to a single analysis on a single data_set.
  do_analyses()
} # end of for each repeat loop

###
### Create the Meta-results table
###
# this function is in util.R
# now we have all our results so we parse them as a single table and collapse
# each repeat simulation (i.e. each run of experiments) into a single entry
save_results_meta()

              }
            }
          }
        }
      }
    }
  }
} # end of all for all parameter value loops

meta_results <- compile_meta_results()

tidy_workspace()


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
  
  
