# contour plotting the meta_results

library(reshape2)

#see mergingData.R file for when meta_results_full was created
meta_results <- meta_results_full
write.table(meta_results, "meta_results.txt", sep="\t")


####
#### Plotting estimates for all five analyses:
####
####
####

#anova estimate
mymatrix1_anova <- data.frame(x = meta_results$meta_true_sex_cond,
                             y = meta_results$meta_var_base,
                             z = meta_results$meta_sex_cond_estimate_anova)



mymatrix_anova <- acast(mymatrix1_anova, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_anova, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="anova estimate")

#glmm estimate
mymatrix1_glmm <- data.frame(x = meta_results$meta_true_sex_cond,
                             y = meta_results$meta_var_base,
                             z = meta_results$meta_sex_cond_estimate_glmm)



mymatrix_glmm <- acast(mymatrix1_glmm, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_glmm, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="glmm estimate")


#bglmm estimate
mymatrix1_bglmm <- data.frame(x = meta_results$meta_true_sex_cond,
                              y = meta_results$meta_var_base,
                              z = meta_results$meta_sex_cond_estimate_bglmm)



mymatrix_bglmm <- acast(mymatrix1_bglmm, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_bglmm, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2),xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="bglmm estimate")


#pp estimate
mymatrix1_pp <- data.frame(x = meta_results$meta_true_sex_cond,
                              y = meta_results$meta_var_base,
                              z = meta_results$meta_sex_cond_estimate_pp)



mymatrix_pp <- acast(mymatrix1_pp, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_pp, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="pp estimate")


#mega estimate
mymatrix1_mega <- data.frame(x = meta_results$meta_true_sex_cond,
                              y = meta_results$meta_var_base,
                              z = meta_results$meta_sex_cond_estimate_mega_bglmm)



mymatrix_mega <- acast(mymatrix1_mega, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_mega, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="mega estimate")



####
#### Plotting positive rate for all five analyses:
####
####
####


#anova positive rate
mymatrix1_anovaP <- data.frame(x = meta_results$meta_true_sex_cond,
                              y = meta_results$meta_var_base,
                              z = meta_results$meta_sex_cond_positive_rate_anova)



mymatrix_anovaP <- acast(mymatrix1_anovaP, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_anovaP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="anova positive rate")

#glmm positive rate
mymatrix1_glmmP <- data.frame(x = meta_results$meta_true_sex_cond,
                             y = meta_results$meta_var_base,
                             z = meta_results$meta_sex_cond_positive_rate_glmm)



mymatrix_glmmP <- acast(mymatrix1_glmmP, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_glmmP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="glmm positive rate")


#bglmm positive
mymatrix1_bglmmP <- data.frame(x = meta_results$meta_true_sex_cond,
                              y = meta_results$meta_var_base,
                              z = meta_results$meta_sex_cond_positive_rate_bglmm)



mymatrix_bglmmP <- acast(mymatrix1_bglmmP, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_bglmmP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="bglmm positive rate")


#pp positive rate
mymatrix1_ppP <- data.frame(x = meta_results$meta_true_sex_cond,
                           y = meta_results$meta_var_base,
                           z = meta_results$meta_sex_cond_positive_rate_pp)



mymatrix_ppP <- acast(mymatrix1_ppP, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_ppP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="pp positive rate")

#mega positive rate
mymatrix1_megaP <- data.frame(x = meta_results$meta_true_sex_cond,
                             y = meta_results$meta_var_base,
                             z = meta_results$meta_sex_cond_positive_rate_bglmm)



mymatrix_megaP <- acast(mymatrix1_megaP, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_megaP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="mega positive rate")


####
#### Plotting uncertainty for all five analyses:
####
####
####

#anova uncertainty
mymatrix1_anova_U <- data.frame(x = meta_results$meta_true_sex_cond,
                               y = meta_results$meta_var_base,
                               z = meta_results$meta_sex_cond_uncertainty_anova)



mymatrix_anova_U <- acast(mymatrix1_anova_U, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_anova_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="anova uncertainty")


#glmm uncertainty
mymatrix1_glmm_U <- data.frame(x = meta_results$meta_true_sex_cond,
                              y = meta_results$meta_var_base,
                              z = meta_results$meta_sex_cond_uncertainty_glmm)



mymatrix_glmm_U <- acast(mymatrix1_glmm_U, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_glmm_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="glmm uncertainty")


#bglmm uncertainty
mymatrix1_bglmm_U <- data.frame(x = meta_results$meta_true_sex_cond,
                               y = meta_results$meta_var_base,
                               z = meta_results$meta_sex_cond_uncertainty_bglmm)



mymatrix_bglmm_U <- acast(mymatrix1_bglmm_U, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_bglmm_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="bglmm uncertainty")


#pp uncertainty
mymatrix1_pp_U <- data.frame(x = meta_results$meta_true_sex_cond,
                            y = meta_results$meta_var_base,
                            z = meta_results$meta_sex_cond_uncertainty_pp)



mymatrix_pp_U <- acast(mymatrix1_pp_U, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_pp_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="pp uncertainty")


#mega uncertainty
mymatrix1_mega_U <- data.frame(x = meta_results$meta_true_sex_cond,
                              y = meta_results$meta_var_base,
                              z = meta_results$meta_sex_cond_uncertainty_mega_bglmm)



mymatrix_mega_U <- acast(mymatrix1_mega_U, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_mega_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="mega uncertainty")


mfrow=c(1,2)
