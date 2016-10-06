# trying to plot freq bayes dataset graph

library(reshape2)
library(ggplot2)


saved_0_0 <- read.delim("0_0_saved_results.txt")
plot_dataset_0_0 <- saved_0_0[,c("expt","analysis_type","b_sex_cond_lower","b_sex_cond_upper","b_sex_cond_med")]


#transform everything to same scale as anovas
plot_dataset_0_0$estimate <- ifelse((plot_dataset_0_0$analysis_type == "anova"), plot_dataset_0_0$b_sex_cond_med, (exp(plot_dataset_0_0$b_sex_cond_med)/(1+exp(plot_dataset_0_0$b_sex_cond_med)))-0.5)
plot_dataset_0_0$estimate_upper <- ifelse((plot_dataset_0_0$analysis_type == "anova"), plot_dataset_0_0$b_sex_cond_upper, (exp(plot_dataset_0_0$b_sex_cond_upper)/(1+exp(plot_dataset_0_0$b_sex_cond_upper)))-0.5)
plot_dataset_0_0$estimate_lower <- ifelse((plot_dataset_0_0$analysis_type == "anova"), plot_dataset_0_0$b_sex_cond_lower, (exp(plot_dataset_0_0$b_sex_cond_lower)/(1+exp(plot_dataset_0_0$b_sex_cond_lower)))-0.5)



#plot on same axis
limits <- aes(ymax = plot_dataset_0_0$estimate_upper, ymin = plot_dataset_0_0$estimate_lower)
APlot <- ggplot(plot_dataset_0_0, aes(x=expt, y=estimate, group =analysis_type, col=analysis_type))
APlot + geom_point(data = plot_dataset_0_0, size = 2) + 
  geom_errorbar(limits, width = 1) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=1, show.legend=FALSE)
ylab("estimate")


### making subsets -in case plot by separate graphs? no longer needed##
#just_anova <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type=="anova")
#just_glmm <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type=="glmm")
#just_bglmm <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type=="bglmm")
#just_pp <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type =="pp")

