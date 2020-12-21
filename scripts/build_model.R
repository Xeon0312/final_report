library(lme4)
library(brms)
library(tidybayes)
library(caret)
library(ROCR)

ces2019_web <- read.csv("./input/ces2019_web_cleaned.csv")
gss_2017 <- read.csv("./input/gss.csv")

ces2019_libral <- ces2019_web

ces2019_libral$cps19_votechoice<-ifelse(ces2019_libral$cps19_votechoice=="Liberal Party","Liberal Party",F)

ces2019_cons <- ces2019_web

ces2019_cons$cps19_votechoice<-ifelse(ces2019_cons$cps19_votechoice=="Conservative Party",T,F)

ces2019_quebec <- ces2019_web

ces2019_quebec$cps19_votechoice<-ifelse(ces2019_quebec$cps19_votechoice=="Bloc Québécois",T,F)

ces2019_green <- ces2019_web

ces2019_green$cps19_votechoice<-ifelse(ces2019_green$cps19_votechoice=="Green Party",T,F)

ces2019_ndp <- ces2019_web

ces2019_ndp$cps19_votechoice<-ifelse(ces2019_ndp$cps19_votechoice=="ndp",T,F)

ces2019_peo <- ces2019_web

ces2019_peo$cps19_votechoice<-ifelse(ces2019_peo$cps19_votechoice=="People's Party",T,F)

# Liberal Party
# Bulid multi-level regression
brms_model_libral <- brm(cps19_votechoice ~ agegroup + sex + province + education,
                      data = ces2019_libral,
                      file = "./outputs/model/brms_model_libral",
                      family = bernoulli())

prob_libral<-predict(brms_model_libral,type=c('response'))
vote_libral<-ifelse(prob_libral>=0.5,T,F)
survey_data_with_pred<-cbind(ces2019_libral,vote_libral)
survey_data_with_pred$Estimate<- as.factor(survey_data_with_pred$Estimate)
survey_data_with_pred$cps19_votechoice<- as.factor(survey_data_with_pred$cps19_votechoice)

## Use confusion matrix to see our model's accrary
confusion_matrix<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[2]
accuary<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[3]$overall['Accuracy']
confusion_matrix
accuary



# Conservative Party 
# Bulid multi-level regression
brms_model_cons <- brm(cps19_votechoice ~ agegroup + sex + province + education,
                         data = ces2019_cons,
                         file = "./outputs/model/brms_model_cons",
                         family = bernoulli())

prob_cons<-predict(brms_model_cons,type=c('response'))
vote_cons<-ifelse(prob_cons>=0.5,T,F)
survey_data_with_pred<-cbind(ces2019_cons,vote_cons)
survey_data_with_pred$Estimate<- as.factor(survey_data_with_pred$Estimate)
survey_data_with_pred$cps19_votechoice<- as.factor(survey_data_with_pred$cps19_votechoice)

## Use confusion matrix to see our model's accrary
confusion_matrix<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[2]
accuary<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[3]$overall['Accuracy']
confusion_matrix
accuary

# Bloc Québécois
# Bulid multi-level regression
brms_model_quebec <- brm(cps19_votechoice ~ agegroup + sex + province + education,
                       data = ces2019_quebec,
                       file = "./outputs/model/brms_model_quebec",
                       family = bernoulli())

prob_quebec<-predict(brms_model_quebec,type=c('response'))
vote_quebec<-ifelse(prob_quebec>=0.16,T,F)
survey_data_with_pred<-cbind(ces2019_quebec,vote_quebec)
survey_data_with_pred$Estimate<- as.factor(survey_data_with_pred$Estimate)
survey_data_with_pred$cps19_votechoice<- as.factor(survey_data_with_pred$cps19_votechoice)

## Use confusion matrix to see our model's accrary
confusion_matrix<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[2]
accuary<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[3]$overall['Accuracy']
confusion_matrix
accuary

# Green Party
# Bulid multi-level regression
brms_model_green <- brm(cps19_votechoice ~ agegroup + sex + province + education,
                       data = ces2019_green,
                       file = "./outputs/model/brms_model_green",
                       family = bernoulli())

prob_green<-predict(brms_model_green,type=c('response'))
vote_green<-ifelse(prob_green>=0.16,T,F)
survey_data_with_pred<-cbind(ces2019_green,vote_green)
survey_data_with_pred$Estimate<- as.factor(survey_data_with_pred$Estimate)
survey_data_with_pred$cps19_votechoice<- as.factor(survey_data_with_pred$cps19_votechoice)

## Use confusion matrix to see our model's accrary
confusion_matrix<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[2]
accuary<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[3]$overall['Accuracy']
confusion_matrix
accuary

# ndp
# Bulid multi-level regression
brms_model_ndp <- brm(cps19_votechoice ~ agegroup + sex + province + education,
                        data = ces2019_ndp,
                        file = "./outputs/model/brms_model_ndp",
                        family = bernoulli())

prob_ndp<-predict(brms_model_ndp,type=c('response'))
vote_ndp<-ifelse(prob_ndp>=0.5,T,F)
survey_data_with_pred<-cbind(ces2019_ndp,vote_ndp)
survey_data_with_pred$Estimate<- as.factor(survey_data_with_pred$Estimate)
survey_data_with_pred$cps19_votechoice<- as.factor(survey_data_with_pred$cps19_votechoice)

## Use confusion matrix to see our model's accrary
confusion_matrix<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[2]
accuary<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[3]$overall['Accuracy']
confusion_matrix
accuary

# People's Party
# Bulid multi-level regression
brms_model_peo <- brm(cps19_votechoice ~ agegroup + sex + province + education,
                      data = ces2019_peo,
                      file = "./outputs/model/brms_model_peo",
                      family = bernoulli())

prob_peo<-predict(brms_model_peo,type=c('response'))
vote_peo<-ifelse(prob_peo>=0.16,T,F)
survey_data_with_pred<-cbind(ces2019_peo,vote_peo)
survey_data_with_pred$Estimate<- as.factor(survey_data_with_pred$Estimate)
survey_data_with_pred$cps19_votechoice<- as.factor(survey_data_with_pred$cps19_votechoice)

## Use confusion matrix to see our model's accrary
confusion_matrix<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[2]
accuary<-confusionMatrix(survey_data_with_pred$Estimate,survey_data_with_pred$cps19_votechoice)[3]$overall['Accuracy']
confusion_matrix
accuary