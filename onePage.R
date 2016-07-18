#trying to save many plots in one pdf

pdf("uncertaintyPlots.pdf")
par(mfrow = c(2,1))

filled.contour(mymatrix_mega_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="mega uncertainty")
filled.contour(mymatrix_pp_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="pp uncertainty")
filled.contour(mymatrix_bglmm_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="bglmm uncertainty")
filled.contour(mymatrix_glmm_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="glmm uncertainty")
filled.contour(mymatrix_anova_U, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="anova uncertainty")


dev.off()

#positive plots

pdf("positive plots.pdf")
par(mfrow = c(2,1))

filled.contour(mymatrix_megaP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="mega positive rate")
filled.contour(mymatrix_ppP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="pp positive rate")
filled.contour(mymatrix_bglmmP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2) ,xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="bglmm positive rate")
filled.contour(mymatrix_glmmP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="glmm positive rate")
filled.contour(mymatrix_anovaP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="anova positive rate")

dev.off()


#estimate plots

pdf("estimate plots.pdf")
par(mfrow = c(1,2))

filled.contour(mymatrix_anova, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="anova estimate")
filled.contour(mymatrix_glmm, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="glmm estimate")
filled.contour(mymatrix_bglmm, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2),xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="bglmm estimate")
filled.contour(mymatrix_pp, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="pp estimate")
filled.contour(mymatrix_mega, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="mega estimate")

dev.off()

summary(meta_results_full$meta_sex_cond_estimate_anova)
summary(meta_results_full$meta_sex_cond_estimate_glmm)
