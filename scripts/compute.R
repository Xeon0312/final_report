library(lme4)
library(brms)
library(tidybayes)
library(caret)
library(ROCR)
library(tidyverse)

brms_model_libral <- read_rds("./outputs/model/brms_model_libral.rds")
brms_model_cons <- read_rds("./outputs/model/brms_model_cons.rds")
brms_model_quebec <- read_rds("./outputs/model/brms_model_quebec.rds")
brms_model_green <- read_rds("./outputs/model/brms_model_green.rds")
brms_model_ndp <- read_rds("./outputs/model/brms_model_ndp.rds")
brms_model_peo <- read_rds("./outputs/model/brms_model_peo.rds")
ces2019_web <- read.csv("./input/ces2019_web_cleaned.csv")
gss_2017 <- read.csv("./input/gss.csv")


# Predict
predict_L = brms_model_libral %>% 
  add_predicted_draws(newdata = gss_2017)
results_L = predict_L %>% 
  rename(vote_predictions = .prediction) %>%
  mutate(vote_predictions_prop = vote_predictions)%>%
  group_by(province, .draw) %>% 
  summarise(vote_predictions = sum(vote_predictions_prop)) %>% 
  group_by(province) %>% 
  summarise(mean = mean(vote_predictions),
            lower = quantile(vote_predictions, 0.025),
            upper = quantile(vote_predictions, 0.975))

rm(predict_L)

predict_C = brms_model_cons %>% 
  add_predicted_draws(newdata = gss_2017)
results_C = predict_C %>% 
  rename(vote_predictions = .prediction) %>%
  mutate(vote_predictions_prop = vote_predictions)%>%
  group_by(province, .draw) %>% 
  summarise(vote_predictions = sum(vote_predictions_prop)) %>% 
  group_by(province) %>% 
  summarise(mean = mean(vote_predictions),
            lower = quantile(vote_predictions, 0.025),
            upper = quantile(vote_predictions, 0.975))

rm(predict_C)

predict_Q = brms_model_quebec %>% 
  add_predicted_draws(newdata = gss_2017)
results_Q = predict_Q %>% 
  rename(vote_predictions = .prediction) %>%
  mutate(vote_predictions_prop = vote_predictions)%>%
  group_by(province, .draw) %>% 
  summarise(vote_predictions = sum(vote_predictions_prop)) %>% 
  group_by(province) %>% 
  summarise(mean = mean(vote_predictions),
            lower = quantile(vote_predictions, 0.025),
            upper = quantile(vote_predictions, 0.975))

rm(predict_Q)

predict_G = brms_model_green %>% 
  add_predicted_draws(newdata = gss_2017)
results_G = predict_G %>% 
  rename(vote_predictions = .prediction) %>%
  mutate(vote_predictions_prop = vote_predictions)%>%
  group_by(province, .draw) %>% 
  summarise(vote_predictions = sum(vote_predictions_prop)) %>% 
  group_by(province) %>% 
  summarise(mean = mean(vote_predictions),
            lower = quantile(vote_predictions, 0.025),
            upper = quantile(vote_predictions, 0.975))

rm(predict_G)

predict_N = brms_model_ndp %>% 
  add_predicted_draws(newdata = gss_2017)
results_N = predict_N %>% 
  rename(vote_predictions = .prediction) %>%
  mutate(vote_predictions_prop = vote_predictions)%>%
  group_by(province, .draw) %>% 
  summarise(vote_predictions = sum(vote_predictions_prop)) %>% 
  group_by(province) %>% 
  summarise(mean = mean(vote_predictions),
            lower = quantile(vote_predictions, 0.025),
            upper = quantile(vote_predictions, 0.975))

rm(predict_N)

predict_P = brms_model_peo %>% 
  add_predicted_draws(newdata = gss_2017)
results_P = predict_P %>% 
  rename(vote_predictions = .prediction) %>%
  mutate(vote_predictions_prop = vote_predictions)%>%
  group_by(province, .draw) %>% 
  summarise(vote_predictions = sum(vote_predictions_prop)) %>% 
  group_by(province) %>% 
  summarise(mean = mean(vote_predictions),
            lower = quantile(vote_predictions, 0.025),
            upper = quantile(vote_predictions, 0.975))

rm(predict_P)

write_csv(results_P, "./outputs/results/results_P.csv")
write_csv(results_C, "./outputs/results/results_C.csv")
write_csv(results_G, "./outputs/results/results_G.csv")
write_csv(results_L, "./outputs/results/results_L.csv")
write_csv(results_N, "./outputs/results/results_N.csv")
write_csv(results_Q, "./outputs/results/results_Q.csv")
