#Machine learning and statistical models
library(dplyr, warn.conflicts = FALSE)
#libraries used for analysis
library(sjPlot)
library(ggplot2)
library(gridExtra)
library(party)
library(lme4)
library(rpart)
library(caTools)
library(MLmetrics)
library(questionr)
library(caret)
library(plotly)
library(ggmosaic)
library(rbokeh)
#dataset Russian as a second language (L2)
x <- read.csv('aspfl2.csv')
x$asp <- as.factor(x$asp)
x$stem <- as.factor(x$stem)
x$marker <- as.factor(x$marker)
x$lang <- as.factor(x$lang)
x$prof <- as.factor(x$prof)
xtabs(~stem + marker + asp, x)

stemmihl <- ggplotly(stemhl)
saveRDS(stemmil2,"stemmil2.rds")
saveRDS(stemmihl,"stemmihl.rds")



#dataset Russian as a heritage language (HL)
y <- read.csv('asphl2.csv')
y$asp <- as.factor(y$asp)
y$stem <- as.factor(y$stem)
y$marker <- as.factor(y$marker)
y$lang <- as.factor(y$lang)
y$prof <- as.factor(y$prof)
saveRDS(y,"aspektiaineisto_hl.rds")

#Decision tree visualization, L2 RUSSIAN
vidtree <- ctree(asp ~ stem + marker, data = x)
plot(vidtree)

#Decision tree visualization, HL RUSSIAN
vidtree <- ctree(asp ~ stem + marker, data = y)
plot(vidtree)
xtabs(~marker + stem + asp, x)
#Mixed effects logistic regression, proficiency and native language as random effects, RUSSIAN L2
#stem and marker as fixed effect
vidglm_l2 <- glmer(asp ~ stem + marker + (1|lang) + (1|prof), data = x, family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
int_l2 <- glmer(asp ~ stem + marker + (1|lang) + (1|prof), data = x, family = binomial)
summary(vidglm_l2)
plot_model(vidglm_l2, type = 're')
xtabs(~ lang + asp, data = x)
summary(vidglm_l2)
odds.ratio(vidglm_l2)
slopes <- glmer(asp ~ stem + marker + (1+stem|lang) + (1+marker|lang) + (1|prof), data = x, family = binomial)
#Mixed effects logistic regression, proficiency and native language as random effects, RUSSIAN HL
vidglm_hl <- glmer(asp ~ stem + marker + (1|lang)  + (1|prof), data = y, family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
xtabs(~ lang + asp, data = y)
summary(vidglm_hl)
plot_models(vidglm_l2,vidglm_hl, grid = TRUE)

summary(slopes)


#Visualize logistics regression and random effects

x2<-x[!(x$lang=="ser" | x$lang=="nor"|x$lang=="na"|x$lang=="kor"|x$lang=="jap"|x$lang=="chi"|x$prof=="Other"|x$prof=="A1"|x$prof=="C1"|x$prof=="C2"),]
y2<-y[!(y$lang=="chi"|y$marker=="suffix"|y$prof=="Other"|y$prof=="A1"|y$prof=="C1"|y$prof=="C2"|y$prof==""),]
y3 <- y[!y$marker=="suffix",]

vidglm_hl <- glmer(asp ~ stem + marker + (1|lang)  + (1|prof), data = y3, family = binomial)
vidglm_l2 <- glmer(asp ~ stem + marker + (1|lang) + (1|prof), data = x, family = binomial)
hllog <- plot_model(vidglm_hl,title = "HL")
l2log <- plot_model(vidglm_l2, title = "L2")

vidglm_hl_prof <- glmer(asp ~ stem + marker + (1|prof), data = y2, family = binomial)
vidglm_hl_lang <- glmer(asp ~ stem + marker + (1|lang), data = y2, family = binomial)
vidglm_l2_lang <- glmer(asp ~ stem + marker + (1|lang), data = x2, family = binomial)
vidglm_l2_prof <- glmer(asp ~ stem + marker + (1|prof), data = x2, family = binomial)
install.packages('TMB', type = 'source')


l2 <- plot_model(vidglm_l2_lang, type = 're', title = "L2")
hl <-  plot_model(vidglm_hl_lang, type = 're', title = "HL")

prof_l2 <- plot_model(vidglm_l2_prof, type = 're', title = "L2")
prof_hl <-  plot_model(vidglm_hl_prof, type = 're', title = "HL")
grid.arrange(l2log,hllog,l2,hl, prof_l2,prof_hl)
saveRDS(grid.arrange(l2log,hllog,l2,hl, prof_l2,prof_hl),"kaikkioddsit.rds")
griddi
ggplotly(l2log)

xtabs(~lang, y)
xtabs(~prof, x)


#Machine learning model, CART, RUSSIAN L2
set.seed(123)
split = sample.split(x$asp, SplitRatio = 0.75)
training_set = subset(x, split == TRUE)
test_set = subset(x, split == FALSE)
classifier = rpart(formula = asp ~ stem + marker,data = training_set)
y_pred = predict(classifier, newdata = test_set[-3], type = 'class')
acc <- table(test_set[,3],y_pred)


sum(diag(acc))/sum(acc)*100

#Machine learning model, log reg, L2
set.seed(123)
split = sample.split(x$asp, SplitRatio = 0.75)
training_set = subset(x, split == TRUE)
test_set = subset(x, split == FALSE)
classifier = glm(formula = asp ~ stem + marker,data = training_set, family = 'binomial')
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
acc <- table(test_set[,3],y_pred)
sum(diag(acc))/sum(acc)*100
Accuracy(y_pred,test_set$asp)
Recall(test_set[,3],prob_pred)

head(test_set)

#Machine learning model, CART, RUSSIAN HL
set.seed(123)
split = sample.split(y$asp, SplitRatio = 0.75)
training_set = subset(y, split == TRUE)
test_set = subset(y, split == FALSE)
classifier = rpart(formula = asp ~ stem + marker,data = training_set)
y_pred = predict(classifier, newdata = test_set[-3], type = 'class')
acc <- table(test_set[,3],y_pred)
sum(diag(acc))/sum(acc)*100

#Machine learning model, log reg, HL
set.seed(123)
split = sample.split(y$asp, SplitRatio = 0.75)
training_set = subset(y, split == TRUE)
test_set = subset(y, split == FALSE)
classifier = glm(formula = asp ~ stem + marker,data = training_set, family = 'binomial')
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
acc <- table(test_set[,3],y_pred)
sum(diag(acc))/sum(acc)*100

#Machine learning metrics
Recall(y_pred,test_set[,3])
Precision(y_pred,test_set[,3])
F1_Score(y_pred,test_set[,3])