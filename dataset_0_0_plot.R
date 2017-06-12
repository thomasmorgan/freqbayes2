# trying to plot freq bayes dataset graph

library(reshape2)
library(ggplot2)


saved_0_0 <- read.delim("0_0_saved_results.txt")
plot_dataset_0_0 <- saved_0_0[,c("expt","analysis_type","b_sex_cond_lower","b_sex_cond_upper","b_sex_cond_med")]


#transform everything to same scale as anovas
plot_dataset_0_0$estimate <- ifelse((plot_dataset_0_0$analysis_type == "anova"), plot_dataset_0_0$b_sex_cond_med, (exp(plot_dataset_0_0$b_sex_cond_med)/(1+exp(plot_dataset_0_0$b_sex_cond_med)))-0.5)
plot_dataset_0_0$estimate_upper <- ifelse((plot_dataset_0_0$analysis_type == "anova"), plot_dataset_0_0$b_sex_cond_upper, (exp(plot_dataset_0_0$b_sex_cond_upper)/(1+exp(plot_dataset_0_0$b_sex_cond_upper)))-0.5)
plot_dataset_0_0$estimate_lower <- ifelse((plot_dataset_0_0$analysis_type == "anova"), plot_dataset_0_0$b_sex_cond_lower, (exp(plot_dataset_0_0$b_sex_cond_lower)/(1+exp(plot_dataset_0_0$b_sex_cond_lower)))-0.5)

#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#ColourBrewer palette diverging: BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral,
#ColourBrewer palette qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3,

#plot on same axis
limits <- aes(ymax = plot_dataset_0_0$estimate_upper, ymin = plot_dataset_0_0$estimate_lower)
APlot <- ggplot(plot_dataset_0_0, aes(x=expt, y=estimate, group =analysis_type, col=analysis_type))
APlot + geom_point(data = plot_dataset_0_0, size = 1.2, position = position_dodge(width = 0)) + 
  scale_color_manual(values=c("cyan4", "darkgoldenrod1", "black", "darkorchid1")) +
  geom_errorbar(limits, width = 6, position = position_dodge(width = 0)) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=1, show.legend=FALSE) +
  labs(color="Analysis Type")+
  xlab("Data set") +
  ylab("Analysis estimate")+
  theme_linedraw() + theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0)))

#used dev.off when got error in call.graphics invalid graphics state
#dev.off()

### Want to do this for pre-VIVA corrections 20.1.17 ###
### making subsets -in case plot by separate graphs? no longer needed##
just_anova <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type=="anova")
just_glmm <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type=="glmm")
just_bglmm <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type=="bglmm")
just_pp <- subset(plot_dataset_0_0, plot_dataset_0_0$analysis_type =="pp")

#plot on diff axes: anova
limits <- aes(ymax = just_anova$estimate_upper, ymin = just_anova$estimate_lower)
AnovPlot <- ggplot(just_anova, aes(x=expt, y=estimate))
AnovPlot + geom_point(data = just_anova, size = 1) + 
  geom_errorbar(limits, width = 6) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=1, show.legend=FALSE) +
  xlab("Dataset no.") +
  ylab("Estimate") +
  labs(title="ANOVA Estimates") +
  scale_y_continuous(limits=c(-0.3,0.3)) +
  theme_linedraw() + theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0)))

#plot on diff axes: glmm
limits <- aes(ymax = just_glmm$estimate_upper, ymin = just_glmm$estimate_lower)
AnovPlot <- ggplot(just_glmm, aes(x=expt, y=estimate))
AnovPlot + geom_point(data = just_glmm, size = 1) + 
  geom_errorbar(limits, width = 6) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=1, show.legend=FALSE) +
  xlab("Dataset no.") +
  ylab("Estimate") +
  labs(title="GLMM Estimates") +
  scale_y_continuous(limits=c(-0.3,0.3)) +
  theme_linedraw() + theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0)))

#plot on diff axes: BGLMM
limits <- aes(ymax = just_bglmm$estimate_upper, ymin = just_bglmm$estimate_lower)
AnovPlot <- ggplot(just_bglmm, aes(x=expt, y=estimate))
AnovPlot + geom_point(data = just_bglmm, size = 1) + 
  geom_errorbar(limits, width = 6) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=1, show.legend=FALSE) +
  xlab("Dataset no.") +
  ylab("Estimate") +
  labs(title="BGLMM Estimates") +
  scale_y_continuous(limits=c(-0.3,0.3)) +
  theme_linedraw() + theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0)))

#plot on diff axes: pp
limits <- aes(ymax = just_pp$estimate_upper, ymin = just_pp$estimate_lower)
AnovPlot <- ggplot(just_pp, aes(x=expt, y=estimate))
AnovPlot + geom_point(data = just_pp, size = 1) + 
  geom_errorbar(limits, width = 6) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=1, show.legend=FALSE) +
  xlab("Dataset no.") +
  ylab("Estimate") +
  labs(title="PP Estimates") +
  scale_y_continuous(limits=c(-0.3,0.3)) +
  theme_linedraw() + theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0)))

