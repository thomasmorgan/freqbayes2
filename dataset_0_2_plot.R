# trying to plot freq bayes dataset graph

library(reshape2)
library(ggplot2)


saved_0_2 <- read.delim("saved_0_2.txt")

plot_dataset_0_2 <- saved_0_2[,c("expt","analysis_type","b_sex_cond_lower","b_sex_cond_upper","b_sex_cond_med")]
plot_dataset_0_2$analysis_type <- as.factor(plot_dataset_0_2$analysis_type)
#plot_dataset_0_2$expt <- as.factor(plot_dataset_0_2$expt)

#making subsets

#just_anova <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type=="anova")
#just_glmm <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type=="glmm")
#just_bglmm <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type=="bglmm")
#just_pp <- subset(plot_dataset_0_2, plot_dataset_0_2$analysis_type =="pp")

#does weird removing of rows
limits <- aes(ymax = plot_dataset_0_2$b_sex_cond_upper, ymin = plot_dataset_0_2$b_sex_cond_lower)
APlot <- ggplot(data = plot_dataset_0_2, aes(expt, b_sex_cond_med, colour = analysis_type))
APlot + geom_point(data = plot_dataset_0_2, size = 1.5) + 
  geom_errorbar(limits, width = 0.08) +
  ylab("estimate") +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits=c(1,60)) 

#try adding up layers a la http://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph
 
limits <- aes(ymax = plot_dataset_0_2$b_sex_cond_upper, ymin = plot_dataset_0_2$b_sex_cond_lower)
APlot <- ggplot(data = plot_dataset_0_2, aes(expt)) +
  geom_point(aes(y= anova, colour="anova")) +
  geom_point(aes(y= glmm, colour="glmm"))
APlot  
