library(cesR)
library(labelled)
library(tidyverse)

# call 2019 CES online survey
get_ces("ces2019_web")
# convert values to factor type
ces2019_web <- to_factor(ces2019_web)

ces2019_web_cleaned<-ces2019_web %>% filter((cps19_votechoice!="Don't know/ Prefer not to answer") &
                                            (cps19_votechoice!="Another party (please specify)") 
)
ces2019_web_cleaned$cps19_yob = as.numeric(as.character(ces2019_web_cleaned$cps19_yob))
ces2019_web_cleaned <- ces2019_web_cleaned %>% 
  mutate(agegroup = case_when((2019 - cps19_yob) <=20 ~ '20 or less',
                              (2019 - cps19_yob) >20  & (2019 - cps19_yob) <= 30 ~ '21 to 30',
                              (2019 - cps19_yob) >30  & (2019 - cps19_yob) <= 40 ~ '31 to 40',
                              (2019 - cps19_yob) >40  & (2019 - cps19_yob) <= 50 ~ '41 to 50',
                              (2019 - cps19_yob) >50  & (2019 - cps19_yob) <= 60 ~ '51 to 60',
                              (2019 - cps19_yob) >60  & (2019 - cps19_yob) <= 70 ~ '61 to 70',
                              (2019 - cps19_yob) >70  & (2019 - cps19_yob) <= 80 ~ '71 to 80',
                              (2019 - cps19_yob) >80 ~ 'above 80'
  )) 

ces2019_web_cleaned <- ces2019_web_cleaned %>% select(cps19_citizenship,
                                              agegroup,
                                              cps19_gender,
                                              cps19_province,
                                              cps19_education,
                                              cps19_v_likely,
                                              cps19_votechoice) 

ces2019_web_cleaned<-rename(ces2019_web_cleaned,citizenship_status=cps19_citizenship)
ces2019_web_cleaned<-rename(ces2019_web_cleaned,sex=cps19_gender)
ces2019_web_cleaned<-rename(ces2019_web_cleaned,province=cps19_province)
ces2019_web_cleaned<-rename(ces2019_web_cleaned,education=cps19_education)
ces2019_web_cleaned = ces2019_web_cleaned %>% na.omit()
## education
less_high<-c("No schooling","Some elementary school","Completed elementary school","Some secondary/ high school")
dazhuan<-c("Some technical, community college, CEGEP, College Classique","Completed technical, community college, CEGEP, College Classique")
above<-c("Master's degree","Professional degree or doctorate")
high_school_grad<-c("ged or alternative credential","regular high school diploma")
col_not_grad<-c("some college, but less than 1 year",
                "1 or more years of college credit, no degree")
ces2019_web_cleaned_1 <- ces2019_web_cleaned %>% 
  mutate(educd2 = case_when(education =="Don't know/ Prefer not to answer" ~ "NA",
                            education =="Bachelor's degree" ~ "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)",
                            education =="Completed secondary/ high school" ~ "High school diploma or a high school equivalency certificate",
                            education =="Some university" ~ "University certificate or diploma below the bachelor's level",
                            education %in% dazhuan~"College, CEGEP or other non-university certificate or di...",
                            education %in% less_high~"Less than high school diploma or its equivalent",
                            education %in% above~"University certificate, diploma or degree above the bach..."
  )) 

### drop educd & rename educd2
ces2019_web_cleaned_1$educd2 = as.factor(ces2019_web_cleaned_1$educd2)
ces2019_web_cleaned_1$education <- NA

ces2019_web_cleaned_1 <- ces2019_web_cleaned_1 %>% 
  mutate(vote = case_when(cps19_votechoice =="Bloc Québécois" ~ "Bolc Quebecois",
                          cps19_votechoice =="Conservative Party" ~ "Conservative Party",
                          cps19_votechoice =="Green Party" ~ "Green Party",
                          cps19_votechoice =="Liberal Party" ~ "Liberal Party",
                          cps19_votechoice =="People's Party"  ~ "People's Party",
                          cps19_votechoice =="ndp"  ~ "NDP"
  )) 

ces2019_web_cleaned_1 <- ces2019_web_cleaned_1 %>% select(citizenship_status,
                                                      agegroup,
                                                      sex,
                                                      province,
                                                      educd2,
                                                      cps19_v_likely,
                                                      cps19_votechoice) 
ces2019_web_cleaned_1 <- rename(ces2019_web_cleaned_1,education=educd2)

ces2019_web_cleaned_1$sex<-ifelse(ces2019_web_cleaned_1$sex=="A woman","Female","Male")

ces2019_web_cleaned_1$cps19_votechoice = as.character(ces2019_web_cleaned_1$cps19_votechoice)
ces2019_web_cleaned_1 = ces2019_web_cleaned_1 %>% filter(cps19_votechoice != "Another party (please specify)")

write_csv(ces2019_web_cleaned_1, "./input/ces2019_web_cleaned.csv")

ces2019_web_cleaned_1$citizenship_status = as.factor(ces2019_web_cleaned_1$citizenship_status)
ces2019_web_cleaned_1$agegroup = as.factor(ces2019_web_cleaned_1$agegroup)
ces2019_web_cleaned_1$sex = as.factor(ces2019_web_cleaned_1$sex)
ces2019_web_cleaned_1$province = as.factor(ces2019_web_cleaned_1$province)
ces2019_web_cleaned_1$education = as.factor(ces2019_web_cleaned_1$education)
ces2019_web_cleaned_1$cps19_votechoice = as.factor(ces2019_web_cleaned_1$cps19_votechoice)
ces2019_web_cleaned_1$vote = as.factor(ces2019_web_cleaned_1$vote)

model1 = lm(cps19_votechoice ~ citizenship_status + agegroup + sex + province + education,ces2019_web_cleaned_1)
ces2019_web = ces2019_web %>% na.omit()
model_states <- brm(vote ~ agegroup + sex + province + education,
                    data = ces2019_web_cleaned_1,
                    family = "categorical",
                    file = "./outputs/model/brms_model_states8",
                    chains=6
)
fit_lmer <- lmer(cps19_votechoice ~ 1+ agegroup + (1 + agegroup | sex), data = ces2019_web)


