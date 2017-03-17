
## can we access no.false positives from the meta data? using old metaResultsPlotting.R file...

library(reshape2)

#####our latest results are in the file 18_09_16 NOT meta_results.txt####
meta_results <- read.delim("meta_results_18_09_16.txt")

#see mergingData.R file for when meta_results_full was created
#meta_results <- meta_results_full
#write.table(meta_results, "meta_results.txt", sep="\t")

#just true value = 0? 

ZeroEffect <- meta_results[meta_results$meta_true_sex_cond==0,]

hist(ZeroEffect$meta_sex_cond_positive_rate_anova)
hist(ZeroEffect$meta_sex_cond_positive_rate_pp)

ZeroEffect_PositiveRate <- subset(ZeroEffect, select = c("meta_var_base",
                                                         "meta_sex_cond_positive_rate_anova",
                                                         "meta_sex_cond_positive_rate_glmm", 
                                                         "meta_sex_cond_positive_rate_bglmm",
                                                         "meta_sex_cond_positive_rate_pp",
                                                         "meta_sex_cond_positive_rate_mega_bglmm"))

means <- c(mean(ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_anova), mean(ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_glmm), mean(ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_bglmm), mean(ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_pp), mean(ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_mega_bglmm))
print(means)

variance <- ZeroEffect_PositiveRate$meta_var_base
Anova_PosRate <- ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_anova

posRateMeanAnovaPlot <- ggplot(ZeroEffect_PositiveRate, aes(variance, Anova_PosRate)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,0.25)) 
posRateMeanAnovaPlot

ZeroEffect_PositiveRate_reshaping <- data.frame(ZeroEffect_PositiveRate[1], stack(ZeroEffect_PositiveRate[2:6]))
ZeroEffect_PositiveRate <- ZeroEffect_PositiveRate_reshaping
colnames(ZeroEffect_PositiveRate)[3] <- "AnalysisType"
colnames(ZeroEffect_PositiveRate)[2] <- "PositiveRate"
colnames(ZeroEffect_PositiveRate)[1] <- "Variance"

factor((ZeroEffect_PositiveRate$AnalysisType))
levels(ZeroEffect_PositiveRate$AnalysisType)


levels(ZeroEffect_PositiveRate$AnalysisType)[levels(ZeroEffect_PositiveRate$AnalysisType)=="meta_sex_cond_positive_rate_anova"] <- "Anova"
levels(ZeroEffect_PositiveRate$AnalysisType)[levels(ZeroEffect_PositiveRate$AnalysisType)=="meta_sex_cond_positive_rate_glmm"] <- "glmm"
levels(ZeroEffect_PositiveRate$AnalysisType)[levels(ZeroEffect_PositiveRate$AnalysisType)=="meta_sex_cond_positive_rate_bglmm"] <- "bglmm"
levels(ZeroEffect_PositiveRate$AnalysisType)[levels(ZeroEffect_PositiveRate$AnalysisType)=="meta_sex_cond_positive_rate_pp"] <- "pp"
levels(ZeroEffect_PositiveRate$AnalysisType)[levels(ZeroEffect_PositiveRate$AnalysisType)=="meta_sex_cond_positive_rate_mega_bglmm"] <- "megaBglmm"

posRateMeanPlot <- ggplot(ZeroEffect_PositiveRate, aes(Variance, PositiveRate, colour=AnalysisType)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,0.1)) 
posRateMeanPlot

table(ZeroEffect_PositiveRate$PositiveRate, ZeroEffect_PositiveRate$AnalysisType)


#anova positive rate at all effects (plot in the paper)
mymatrix1_anovaP <- data.frame(x = meta_results$meta_true_sex_cond,
                               y = meta_results$meta_var_base,
                               z = meta_results$meta_sex_cond_positive_rate_anova)



mymatrix_anovaP <- acast(mymatrix1_anovaP, x~y, value.var="z", fun.aggregate=mean)

filled.contour(mymatrix_anovaP, x=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), y=c(0, 0.5, 1, 1.5, 2), xlim = c(0, 0.5), ylim = c(0, 2), color.palette = terrain.colors, xlab="true", ylab="var", main="anova positive rate")

ZeroEffect_PositiveRate$noFalsePos_anova <- ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_anova*60
ZeroEffect_PositiveRate$noFalsePos_glmm <- ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_glmm*60
ZeroEffect_PositiveRate$noFalsePos_bglmm <- ZeroEffect_PositiveRate$meta_sex_cond_positive_rate_bglmm*60

PosRates <- subset(ZeroEffect_PositiveRate, select = c("meta_var_base",
                                                      "meta_sex_cond_positive_rate_pp",
                                                      "meta_sex_cond_positive_rate_mega_bglmm",
                                                      "noFalsePos_anova",
                                                      "noFalsePos_glmm",
                                                      "noFalsePos_bglmm"))

?melt
?reshape
FalsePosReshape <- reshape(PosRates, 
                 varying = list(c("meta_sex_cond_positive_rate_pp","meta_sex_cond_positive_rate_mega_bglmm","noFalsePos_anova","noFalsePos_glmm","noFalsePos_bglmm")),
                 timevar = "analysis type",
                 v.names = c("falsePos"), 
                 direction = "long")

FalsePos <- FalsePosReshape
FalsePos$trialNo <- ifelse(FalsePos$`analysis type`<3, 1, 60)
colnames(FalsePos)[2]<- "analysisType"
FalsePos$analysisType[FalsePos$analysisType==1] <- "PP"
FalsePos$analysisType[FalsePos$analysisType==2] <- "megaBglmm"
FalsePos$analysisType[FalsePos$analysisType==3] <- "anova"
FalsePos$analysisType[FalsePos$analysisType==4] <- "glmm"
FalsePos$analysisType[FalsePos$analysisType==5] <- "bglmm"


#Now binomial model as Tom suggested: 

library(rethinking)

trialNo <- FalsePos$trialNo
analysisType <- FalsePos$analysisType
falsePos <- FalsePos$falsePos

fpModel <- map2stan(
  alist(falsePos ~ dbinom(60, p),
        logit(p) <- a + b_at*analysisType,
        a ~ dnorm(0,10),
        c(b_at) ~ dnorm(0,4)
  ),
  data=FalsePos, warmup=1000, iter=3000, chains=1, cores=1 )


