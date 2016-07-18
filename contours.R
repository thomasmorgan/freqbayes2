library(reshape2)

x <- combined_results$meta_true_sex_cond
y <- combined_results$meta_var_base

do_plot <- function(z, m) {
  matrix <- data.frame(x=x, y=y, z=z)
  matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
  filled.contour(matrix2,
                 x=c(0, 0.02, 0.05, 0.07, 0.10, 0.12),
                 y=c(0, 0.5, 1, 1.5, 2),
                 xlim = c(0, 0.12), ylim = c(0, 2), zlim=c(-0.02, 0.15),
                 color.palette = terrain.colors,
                 xlab="true", ylab="var",
                 main=m)
}

do_plot(z=combined_results$meta_sex_cond_estimate_anova, m="anova estimate")
do_plot(z=combined_results$meta_sex_cond_estimate_glmm_p, m="glmm estimate")
do_plot(z=combined_results$meta_sex_cond_estimate_bglmm_p, m="bglmm estimate")
do_plot(z=combined_results$meta_sex_cond_estimate_pp_p, m="pp estimate")
do_plot(z=combined_results$meta_sex_cond_estimate_mega_bglmm_p, m="mega estimate")

do_plot <- function(z, m) {
  matrix <- data.frame(x=x, y=y, z=z)
  matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
  filled.contour(matrix2,
                 x=c(0, 0.02, 0.05, 0.07, 0.10, 0.12),
                 y=c(0, 0.5, 1, 1.5, 2),
                 xlim = c(0, 0.12), ylim = c(0, 2), zlim=c(0, 1),
                 color.palette = terrain.colors,
                 xlab="true", ylab="var",
                 main=m)
}

do_plot(z=combined_results$meta_sex_cond_positive_rate_anova, m="anova positive_rate")
do_plot(z=combined_results$meta_sex_cond_positive_rate_glmm, m="glmm positive_rate")
do_plot(z=combined_results$meta_sex_cond_positive_rate_bglmm, m="bglmm positive_rate")
do_plot(z=combined_results$meta_sex_cond_positive_rate_pp, m="pp positive_rate")
do_plot(z=combined_results$meta_sex_cond_positive_rate_mega_bglmm, m="mega positive_rate")

do_plot <- function(z, m) {
  matrix <- data.frame(x=x, y=y, z=z)
  matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
  filled.contour(matrix2,
                 x=c(0, 0.02, 0.05, 0.07, 0.10, 0.12),
                 y=c(0, 0.5, 1, 1.5, 2),
                 xlim = c(0, 0.12), ylim = c(0, 2), zlim=c(0, 1),
                 color.palette = terrain.colors,
                 xlab="true", ylab="var",
                 main=m)
}

do_plot(z=combined_results$meta_sex_cond_uncertainty_anova, m="anova uncertainty")
do_plot(z=combined_results$meta_sex_cond_uncertainty_glmm_p, m="glmm uncertainty")
do_plot(z=combined_results$meta_sex_cond_uncertainty_bglmm_p, m="bglmm uncertainty")
do_plot(z=combined_results$meta_sex_cond_uncertainty_pp_p, m="pp uncertainty")
do_plot(z=combined_results$meta_sex_cond_uncertainty_mega_bglmm_p, m="mega uncertainty")


do_plot <- function(z, m) {
  matrix <- data.frame(x=x, y=y, z=z)
  matrix2 <- acast(matrix, x~y, value.var="z", fun.aggregate=mean)
  filled.contour(matrix2,
                 x=c(0, 0.02, 0.05, 0.07, 0.10, 0.12),
                 y=c(0, 0.5, 1, 1.5, 2),
                 xlim = c(0, 0.12), ylim = c(0, 2), zlim=c(-0.1, 0.1),
                 color.palette = terrain.colors,
                 xlab="true", ylab="var",
                 main=m)
}

do_plot(z=combined_results$meta_sex_cond_estimate_anova-(exp(x)/(1+exp(x))-0.5), m="anova error")
do_plot(z=combined_results$meta_sex_cond_estimate_glmm_p-(exp(x)/(1+exp(x))-0.5), m="glmm error")
do_plot(z=combined_results$meta_sex_cond_estimate_bglmm_p-(exp(x)/(1+exp(x))-0.5), m="bglmm error")
do_plot(z=combined_results$meta_sex_cond_estimate_pp_p-(exp(x)/(1+exp(x))-0.5), m="pp error")
do_plot(z=combined_results$meta_sex_cond_estimate_mega_bglmm_p-(exp(x)/(1+exp(x))-0.5), m="mega error")



