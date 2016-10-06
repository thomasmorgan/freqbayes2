# trying to plot freq bayes dataset graph

library(reshape2)
library(ggplot2)


saved_0_2 <- read.delim("saved_0_2.txt")

plot_dataset_0_2 <- saved_0_2[,c("expt","analysis_type","b_sex_cond_lower","b_sex_cond_upper","b_sex_cond_med")]


#transform everything to same scale as anovas
plot_dataset_0_2$estimate <- ifelse((plot_dataset_0_2$analysis_type == "anova"), plot_dataset_0_2$b_sex_cond_med, (exp(plot_dataset_0_2$b_sex_cond_med)/(1+exp(plot_dataset_0_2$b_sex_cond_med)))-0.5)
plot_dataset_0_2$estimate_upper <- ifelse((plot_dataset_0_2$analysis_type == "anova"), plot_dataset_0_2$b_sex_cond_upper, (exp(plot_dataset_0_2$b_sex_cond_upper)/(1+exp(plot_dataset_0_2$b_sex_cond_upper)))-0.5)
plot_dataset_0_2$estimate_lower <- ifelse((plot_dataset_0_2$analysis_type == "anova"), plot_dataset_0_2$b_sex_cond_lower, (exp(plot_dataset_0_2$b_sex_cond_lower)/(1+exp(plot_dataset_0_2$b_sex_cond_lower)))-0.5)



##making subsets - no longer needed##
#just_anova <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type=="anova")
#just_glmm <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type=="glmm")
#just_bglmm <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type=="bglmm")
#just_pp <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type =="pp")

#anova on a different scale..? 
limits <- aes(ymax = plot_dataset_0_2$estimate_upper, ymin = plot_dataset_0_2$estimate_lower)
APlot <- ggplot(plot_dataset_0_2, aes(x=expt, y=estimate, group =analysis_type, col=analysis_type))
APlot + geom_point(data = plot_dataset_0_2, size = 1.5) + 
  geom_errorbar(limits, width = 0.08) +
  ylab("estimate")










