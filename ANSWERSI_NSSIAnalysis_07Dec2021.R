#### Load packages ####
library(MASS)
library(psych)
library(ggpubr)
library(lubridate)
library(gtsummary)
library(sandwich)
library(lmtest)
library(broom)
library(lavaan)
library(semPlot)
library(snow)
library(semptools)
library(tidyverse)

#### Load Data ####
answers <- read_csv("c:/users/atala/sync/research/projects/to publish/answers study/phase I survey/ANSWERSI_Data_Scored.csv") %>% 
  filter(Age<26) %>% 
  mutate(
    "ID" = factor(ID),
    "Sex" = factor(Sex, levels = c("Male","Female")),
    "Race" = factor(Race,levels = c("White","Black","Asian","Native","Multiracial")),
    "Ethnicity" = factor(Ethnicity, levels = c("Non-Hispanic","Hispanic")),
    "isi_cat" = factor(case_when(isi_total<8 ~ "No Insomnia",
                                 isi_total>7 & isi_total < 15 ~ "Mild Insomnia",
                                 isi_total>14 & isi_total < 22 ~ "Moderate Insomnia",
                                 isi_total>21 ~ "Severe Insomnia"),
                       levels = c("No Insomnia","Mild Insomnia",'Moderate Insomnia',"Severe Insomnia")),
    "isi_cat2" = factor(case_when(isi_total<15 ~ "No Insomnia",
                                  isi_total>=15 ~ "Insomnia"),
                        levels = c("No Insomnia","Insomnia")),
    "ddnsi_cat" = factor(case_when(ddnsi_total<=10 ~ "No Nightmare Disorder",
                                   ddnsi_total>10 ~ "Nightmare Disorder"),
                         levels = c("No Nightmare Disorder","Nightmare Disorder")),
    "ddnsi_cat2" = as.numeric(ddnsi_cat)-1,
    "psqi_cat" = factor(case_when(psqi_total<=5~"Good Sleep Quality",
                                  psqi_total>5~ "Poor Sleep Quality"),
                        levels = c("Good Sleep Quality","Poor Sleep Quality")),
    "LifetimeNSSI" = case_when(NSSI_3mo=="Yes" | NSSI_lf=="Yes" ~ 1,
                               NSSI_lf=="No" ~ 0),
    "RecentNSSI1" = case_when(LifetimeNSSI==0~0,
                              LifetimeNSSI==1 & NSSI_3mo=="No" ~ 0,
                              LifetimeNSSI==1 & NSSI_3mo=="Yes" ~ 1),
    "RecentNSSI" = case_when(LifetimeNSSI==0~NA_real_,
                             LifetimeNSSI==1 & NSSI_3mo=="No" ~ 0,
                             LifetimeNSSI==1 & NSSI_3mo=="Yes" ~ 1),
    "LifetimeSI" = case_when(SI_lf_severity<2&SI_3mo_severity<2~0,
                             SI_lf_severity>1|SI_3mo_severity>1~1),
    "RecentSI1" = case_when(LifetimeSI==0~0,
                            LifetimeSI==1&SI_3mo_severity<2~0,
                            LifetimeSI==1&SI_3mo_severity>1~1),
    "RecentSI" = case_when(LifetimeSI==0~NA_real_,
                           LifetimeSI==1&SI_3mo_severity<2~0,
                           LifetimeSI==1&SI_3mo_severity>1~1),
    "LifetimeSA" = case_when(LifetimeSI==0 & SA_lf=="No" & SA_3mo=="No" ~ NA_real_,
                             LifetimeSI==1 & SA_lf=="No" & SA_3mo=="No" ~ 0,
                             LifetimeSI==1 & SA_lf=="Yes" | SA_3mo=="Yes" ~ 1),
    "RecentSA" = case_when(LifetimeSA==0 ~ NA_real_,
                           LifetimeSA==1 & SA_3mo=="No" ~ 0,
                           LifetimeSA==1 & SA_3mo=="Yes" ~ 1),
    "RecentSA1" = case_when(LifetimeSA==0 | LifetimeSA==NA_real_ ~ 0,
                            LifetimeSA==1 & SA_3mo=="No" ~ 0,
                            LifetimeSA==1 & SA_3mo=="Yes" ~ 1),
    "SIcat" = factor(case_when(LifetimeSI==0 ~ "None",
                               LifetimeSI==1 & RecentSI==0 ~ "Lifetime", 
                               LifetimeSI==1 & RecentSI==1 ~ "Recent"),
                     levels = c("None",'Lifetime',"Recent")),
    "SAcat" = factor(case_when(LifetimeSA==0 ~ "None",
                               LifetimeSA==1 & RecentSA==0 ~ "Lifetime", 
                               LifetimeSA==1 & RecentSA==1 ~ "Recent"),
                     levels = c("None",'Lifetime',"Recent")),
    "NSSIcat" = factor(case_when(LifetimeNSSI==0 ~ "None",
                                 LifetimeNSSI==1 & RecentNSSI==0 ~ "Lifetime", 
                                 LifetimeNSSI==1 & RecentNSSI==1 ~ "Recent"),
                       levels = c("None",'Lifetime',"Recent")),
    "subs_caff_1" = factor(subs_caff_1,c("Never","Once a month or less","Once a week or less",
                                         "A few times a week","Daily","Multiple times daily")),
    "subs_tob_1" = factor(subs_tob_1,c("Never Smoker","Former Smoker","Occasional Smoker","Daily Smoker")),
    "subs_tob_3" = factor(subs_tob_3,c("Never","Rarely","Often")),
    "subs_alc_1" = factor(subs_alc_1,
                          c("Never","Once a month or less","Once a week or less","A few times a week","Daily","Multiple times daily"),
                          c("Never","Once a week or less","Once a week or less",
                            "More than once a week","More than once a week","More than once a week")),
    "subs_alc_3" = factor(subs_alc_3,c("Never","Rarely","Often")),
    "subs_can_1" = factor(subs_can_1,
                          c("Never","Once a month or less","Once a week or less","A few times a week","Daily","Multiple times daily"),
                          c("Never","Once a week or less","Once a week or less",
                            "More than once a week","More than once a week","More than once a week")),
    "subs_can_3" = factor(subs_can_3,c("Never","Rarely","Often")),
    "BedPartner" = factor(BedPartner,c("No partner","Partner in other room",
                                       "Partner in other bed","Partner in same bed")),
    "WD_TSTh" = round(WD_TST/60,2),"WE_TSTh" = round(WE_TST/60,2),
    "WD_TWT" = WD_TWT/60, "WE_TWT" = WE_TWT/60,
    "MSFsc2" = MSFsc2*24,
    "MSFsc1" = MSFsc1*24,
    "SocialJetlag" = SocialJetlag/60,
    "absSocialJetlag" = abs(SocialJetlag)
    )

##### Begin Analyses #####

#### Table 1 ####
tbl1.p1 <- answers %>% dplyr::select(Sex,Race,Ethnicity,psqi_total,cesd_total,gad_total,
                          subs_tob_1,subs_alc_1,subs_can_1,LifetimeNSSI) %>%
  tbl_summary(statistic= list(all_continuous() ~ "{mean} ({sd})",
                              all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ c(1,2)),
              missing = "no",
              by=c("LifetimeNSSI")) %>% 
  add_p(test = list(all_continuous() ~ "t.test",all_categorical() ~ "chisq.test"),
        pvalue_fun = function(x) style_pvalue(x, digits=3))
tbl1.p2 <- answers %>% dplyr::select(Sex,Race,Ethnicity,psqi_total,cesd_total,gad_total,
                                     subs_tob_1,subs_alc_1,subs_can_1,RecentNSSI) %>%
  tbl_summary(statistic= list(all_continuous() ~ "{mean} ({sd})",
                              all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ c(1,2)),
              missing = "no",
              by=c("RecentNSSI")) %>% 
  add_p(test = list(all_continuous() ~ "t.test",all_categorical() ~ "chisq.test"),
        pvalue_fun = function(x) style_pvalue(x, digits=3))
tbl1 <- tbl_merge(tab_spanner = c("Lifetime NSSI","Recent NSSI"),list(tbl1.p1,tbl1.p2))

#### Step 1: Individual sleep variables on lifetime and recent NSSI unadjusted and adjusted ####
outcomes <- c("LifetimeNSSI","RecentNSSI")
predictors <- c("WD_TSTh","WD_TWT","WD_SE","WD_SQ",
                "WE_TSTh","WE_TWT","WE_SE","WE_SQ",
                "MSFsc2","SocialJetlag","absSocialJetlag",
                "brisc_total","isi_total","ddnsi_cat")
dataset <- list(answers,filter(answers,is.na(RecentNSSI)==F))
models <- c("","+Sex+Race+Ethnicity+cesd_total+gad_total+subs_alc_1+subs_can_1")
modelnames <- c("Unadjusted","Adjusted")
step1.models <- tibble()
for(idx in 1:length(outcomes)){
  for(jdx in 1:length(predictors)){
    for(kdx in 1:length(models)){
      nssi.m <- glm(as.formula(paste(outcomes[idx],"~",predictors[jdx],models[kdx],sep="")),dataset[[idx]],family="poisson")
      nssi.coef <- coeftest(nssi.m,vcov. = sandwich(nssi.m)) %>% 
        tidy(conf.int=T) %>% filter(str_detect(term,predictors[jdx])==T) %>% 
        transmute("Outcome" = outcomes[idx],
                  "Model" = modelnames[kdx],
                  "Predictors" = term,
                  "PRR" = round(exp(estimate),2),
                  "Lower" = round(exp(conf.low),2),
                  "Upper" = round(exp(conf.high),2),
                  "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
                  "pvalue" = round(p.value,4))
      step1.models <- bind_rows(step1.models,nssi.coef)
    }
  }
}

# Write Table S1
# write_csv(step1.models,"c:/users/atala/sync/research/projects/to publish/answers study/phase I survey/nssi/tables1.csv")

#### Step 2: Variable selection ####
## Lifetime NSSI
nssi.lf.data <- answers %>%
  dplyr::select("LifetimeNSSI",
                "WD_TSTh","WD_TWT","WD_SE","WD_SQ",
                "WE_TSTh","WE_TWT","WE_SE","WE_SQ",
                "MSFsc2","SocialJetlag","absSocialJetlag",
                "brisc_total","isi_total","ddnsi_cat")
nssi.lf.step <- stepAIC(glm(LifetimeNSSI ~ ., nssi.lf.data,family="poisson"),trace=F,direction = "both")
nssi.lf.step.sandwich <- sandwich(nssi.lf.step)
nssi.lf.step.coef <- coeftest(nssi.lf.step,nssi.lf.step.sandwich)
nssi.lf.step.conf <- confint(nssi.lf.step.coef)
nssi.lf.step.final <- bind_cols(
  tidy(nssi.lf.step.coef),tibble("Lower" = nssi.lf.step.conf[,1],"Upper" = nssi.lf.step.conf[,2])
  ) %>%   transmute("Model" = "Model 1",
                    "Predictor" = term,
                    "PR" = round(exp(estimate),2),
                    "Lower" = round(exp(Lower),2),
                    "Upper" = round(exp(Upper),2),
                    "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
                    "pvalue" = p.value)
nssi.lf.predictors <- nssi.lf.step.final$Predictor
# When WE_TSTh is eliminated, MSFsc2 is no longer significant, so both are removed.

## 3mo NSSI
nssi.3mo.data <- answers %>% filter(LifetimeNSSI==1) %>%
  dplyr::select("RecentNSSI",
                "WD_TSTh","WD_TWT","WD_SE","WD_SQ",
                "WE_TSTh","WE_TWT","WE_SE","WE_SQ",
                "MSFsc2","SocialJetlag","absSocialJetlag",
                "brisc_total","isi_total","ddnsi_cat")
nssi.3mo.step <- stepAIC(glm(RecentNSSI ~ ., nssi.3mo.data,family="poisson"),trace=F,direction = "both")
nssi.3mo.step.sandwich <- sandwich(nssi.3mo.step)
nssi.3mo.step.coef <- coeftest(nssi.3mo.step,nssi.3mo.step.sandwich)
nssi.3mo.step.conf <- confint(nssi.3mo.step.coef)
nssi.3mo.step.final <- bind_cols(
  tidy(nssi.3mo.step.coef),tibble("Lower" = nssi.3mo.step.conf[,1],"Upper" = nssi.3mo.step.conf[,2])
) %>%   transmute("Model" = "Model 1",
                  "Predictor" = term,
                  "PR" = round(exp(estimate),2),
                  "Lower" = round(exp(Lower),2),
                  "Upper" = round(exp(Upper),2),
                  "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
                  "pvalue" = p.value)
nssi.3mo.predictors <- nssi.3mo.step.final$Predictor

#### Step 3: Modeling Significant Sleep Predictors ####
outcomes <- c("LifetimeNSSI","RecentNSSI")
predictors <- list(
  "LifetimeNSSI" = c("~WE_SE+brisc_total+ddnsi_cat",
                     "~WE_SE+brisc_total+ddnsi_cat+Sex+Race+Ethnicity+cesd_total+gad_total+subs_alc_1+subs_can_1",
                     "~WE_SE+brisc_total+ddnsi_cat+upps_positiveurgency+upps_negativeurgency+upps_lackpersevere+upps_lackpremeditate+upps_sensationseek",
                     "~WE_SE+brisc_total+ddnsi_cat+inq_perceivedburden+inq_thwartedbelong"),
  "RecentNSSI" = c("~absSocialJetlag",
                   "~absSocialJetlag+Sex+Race+Ethnicity+cesd_total+gad_total+subs_alc_1+subs_can_1",
                   "~absSocialJetlag+upps_positiveurgency+upps_negativeurgency+upps_lackpersevere+upps_lackpremeditate+upps_sensationseek",
                   "~absSocialJetlag+inq_perceivedburden+inq_thwartedbelong"))
modelnames <- c("Unadjusted","Adjusted","Impulsivity","IPTS")
modeltable <- tibble()
for(outcome in 1:length(outcomes)){
  for(model in 1:length(modelnames)){
    nssi.m <- glm(as.formula(paste(outcomes[outcome],predictors[[outcome]][model],sep="")),dataset[[outcome]],family="poisson")
    nssi.coef <- coeftest(nssi.m,vcov. = sandwich(nssi.m)) %>% 
      tidy(conf.int=T) %>%
      transmute("Outcome" = outcomes[outcome],
                "Model" = modelnames[model],
                "Predictors" = term,
                "PRR" = round(exp(estimate),2),
                "Lower" = round(exp(conf.low),2),
                "Upper" = round(exp(conf.high),2),
                "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
                "pvalue" = round(p.value,4))
    modeltable <- bind_rows(modeltable,nssi.coef)
  }
}
# Write Table 2
# write_csv(modeltable, "c:/users/atala/sync/research/projects/to publish/answers study/phase I survey/nssi/table2.csv")

#### Step 4: Mediation analyses for impulsivity and IPTS ####
## Lifetime Impulsivity
sleep.impulse.life.model <- '# Models
                        LifetimeNSSI ~ b1*upps_lackpremeditate + b2*upps_negativeurgency + c*(WE_SE+brisc_total+ddnsi_cat2)
                        upps_lackpremeditate ~ a1*(WE_SE+brisc_total+ddnsi_cat2)
                        upps_negativeurgency ~ a2*(WE_SE+brisc_total+ddnsi_cat2)
                      # indirect effects
                        indirect_LPre := a1 * b1
                        indirect_NUrg := a2 * b2
                      # total effect
                        total := c + (a1*b1)
                        direct:= c
                      '
set.seed(1685)
sleep.impulse.life.med <- sem(sleep.impulse.life.model,data = answers,estimator = "DWLS",
                         se="bootstrap",bootstrap = 2000,parallel="snow",ncpus=3)
summary(sleep.impulse.life.med,fit.measures=T,ci=T,estimates=T,standardized=T)                   
## Lifetime IPTS
sleep.ipts.life.model <- '# Models
                        LifetimeNSSI ~ b1*inq_perceivedburden + c*(WE_SE+brisc_total+ddnsi_cat2)
                        inq_perceivedburden ~ a1*(WE_SE+brisc_total+ddnsi_cat2)
                      # indirect effects
                        indirect_PB := a1 * b1
                      # total effect
                        total := c + (a1*b1)
                        direct:= c
                      '
set.seed(8645)
sleep.ipts.life.med <- sem(sleep.ipts.life.model,data = answers,estimator = "DWLS",
                          se="bootstrap",bootstrap = 2000,parallel="snow",ncpus=3)
summary(sleep.ipts.life.med,fit.measures=T,ci=T,estimates=T,standardized=T)  
## Recent Impulsivity
sleep.impulse.recent.model <- '# Models
                        RecentNSSI ~ b1*upps_negativeurgency + c*absSocialJetlag
                        upps_negativeurgency ~ a1*absSocialJetlag
                      # indirect effects
                        indirect_NUrg := a1 * b1
                      # total effect
                        total := c + (a1*b1)
                        direct:= c
                      '
set.seed(4563)
sleep.impulse.recent.med <- sem(sleep.impulse.recent.model,data = filter(answers, LifetimeNSSI==1),
                                          estimator = "DWLS",se="bootstrap",bootstrap = 2000,parallel="snow",ncpus=3)
summary(sleep.impulse.recent.med,fit.measures=T,ci=T,estimates=T,standardized=T)                   
## Recent IPTS
sleep.ipts.recent.model <- '# Models
                        RecentNSSI ~ b1*inq_perceivedburden + c*absSocialJetlag
                        inq_perceivedburden ~ a1*absSocialJetlag
                        # indirect effects
                        indirect_PB := a1 * b1
                        # total effect
                        total := c + (a1*b1)
                        direct:= c
                        '
set.seed(8645)
sleep.ipts.recent.med <- sem(sleep.ipts.recent.model,data = filter(answers, LifetimeNSSI==1),
                                     estimator = "DWLS",se="bootstrap",bootstrap = 2000,parallel="snow",ncpus=3)
summary(sleep.ipts.recent.med,fit.measures=T,ci=T,estimates=T,standardized=T)  

## Tabulate differences by significant sleep variables
tbl3.p1 <- answers %>% dplyr::select("LifetimeNSSI","WE_SE","brisc_total","ddnsi_cat") %>% 
  tbl_summary(statistic= list(all_continuous() ~ "{mean} ({sd})",
                              all_categorical() ~ "{n} ({p}%)"),
              missing = "no",
              by=c("LifetimeNSSI")) %>% 
  add_p(test = list(all_continuous() ~ "t.test",
                    all_categorical() ~ "chisq.test"),
        pvalue_fun = function(x) style_pvalue(x, digits=3))

tbl3.p2 <- answers %>% dplyr::select("RecentNSSI","absSocialJetlag") %>% 
  tbl_summary(statistic= list(all_continuous() ~ "{mean} ({sd})",
                              all_categorical() ~ "{n} ({p}%)"),
              missing = "no",
              by=c("RecentNSSI")) %>% 
  add_p(test = list(all_continuous() ~ "t.test",
                    all_categorical() ~ "chisq.test"),
        pvalue_fun = function(x) style_pvalue(x, digits=3))
tbl3 <- tbl_stack(group_header = c("Lifetime NSSI","Recent NSSI"),list(tbl3.p1,tbl3.p2))

# Figure 1
fig1 <- ggarrange(nrow=2,ncol=1,labels = c("","D"),heights = c(3,1),
          ggarrange(nrow=3,ncol=1,labels=c("A","B","C"),common.legend = T,
                    ggdensity(answers,x="WE_SE",fill="NSSIcat",palette="Dark2")+
                      xlab("Weekend Sleep Efficiency (%)")+ylab("% Response")+
                      labs(fill="Lifetime Non-Suicidal Self-Injury   ")+
                      theme(legend.position="top")+scale_y_continuous(labels = scales::percent),
                    ggdensity(answers,x="brisc_total",fill="NSSIcat",palette="Dark2")+
                      xlab("Perceived Control of Sleep")+ylab("% Response")+
                      theme(legend.position="none")+scale_y_continuous(labels = scales::percent),
                    ggdensity(answers,x="ddnsi_total",fill="NSSIcat",palette="Dark2")+
                      xlab("Nightmare Severity")+ylab("% Response")+
                      theme(legend.position="none")+scale_y_continuous(labels = scales::percent)),
          ggdensity(answers,x="absSocialJetlag",fill="NSSIcat",palette="Dark2")+
            xlab("Absolute Social Jetlag (h)")+ylab("% Response")+
            labs(fill="Recent Non-Suicidal Self-Injury")+scale_y_continuous(labels = scales::percent))

# tiff("c:/users/atala/sync/research/projects/to publish/answers study/phase i survey/NSSI/fig1.tiff",
#      res=300,height=2500,width=1800)
# fig1
# dev.off()
