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

plot_graphs <- function() {
  
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
}