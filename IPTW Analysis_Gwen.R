###### Inverse Probability Treatment Weighting and Marginal Structural Model ######

#### Create the denominator of IPW for treatment (IPTW)

# Data prep (same steps as in PS_Analysis)
aggregate_pm_census_cdc_test_beds$face_masks<- ifelse(aggregate_pm_census_cdc_test_beds$Face_Masks_Required_in_Public=="Yes", 1, 0)
aggregate_pm_census_cdc_test_beds[is.na(aggregate_pm_census_cdc_test_beds$face_masks),122]<-0 

median(aggregate_pm_census_cdc_test_beds$mean_pm25)
aggregate_pm_census_cdc_test_beds$pm25 <- cut(aggregate_pm_census_cdc_test_beds$mean_pm25, breaks = c(-Inf, median(aggregate_pm_census_cdc_test_beds$mean_pm25), +Inf), labels = c(0,1))

# Estimate propensity score (same as in PS_Analysis)
fitps <- glm(pm25 ~ face_masks + Tests_per_100K
             + perc_unins, data=aggregate_pm_census_cdc_test_beds, family="binomial")
summary(fitps)
aggregate_pm_census_cdc_test_beds$ps <- predict(fitps, aggregate_pm_census_cdc_test_beds, type="response")


#### Create the numerator of IPTW for stablizied weight

fitpn <- glm(pm25 ~ 1, data=aggregate_pm_census_cdc_test_beds, family="binomial")
summary(fitpn)
aggregate_pm_census_cdc_test_beds$pn <- predict(fitpn, data=aggregate_pm_census_cdc_test_beds, type="response")


#### Calculate the weights for the exposed/treated and the unexposed/untreated

aggregate_pm_census_cdc_test_beds <- aggregate_pm_census_cdc_test_beds %>% 
  mutate(uw_t = ifelse(pm25 == 1,
                       yes = 1/ps,
                       no = 1/(1-ps)),
         sw_t = ifelse(pm25 == 1,
                       yes = pn/ps,
                       no = (1-pn)/(1-ps)))

# Mean values of unstabilized and stabilized IPW
summary(aggregate_pm_census_cdc_test_beds$uw_t) #mean =~ 2
summary(aggregate_pm_census_cdc_test_beds$sw_t) #mean =~ 1


#### Marginal structural model with IPTW estimating the marginal mean differences for the exposure/treatment on the outcome

## QUESTION: When we do MSM, do we include other covariates??
# Or do we just include other covariates in PS?

fituw <- glm(Deaths~ pm25,data=aggregate_pm_census_cdc_test_beds,
             weights = uw_t)
summary(fituw)
cbind(beta = coef(fituw), confint(fituw))

fitsw <- glm(Deaths~ pm25,data=aggregate_pm_census_cdc_test_beds,
             weights = sw_t)
summary(fitsw)
cbind(beta = coef(fitsw), confint(fitsw))


#### Robust standard error estimators

library("geepack")
library("broom")

msm.uw <- geeglm(
  Deaths~ pm25,
  data=aggregate_pm_census_cdc_test_beds,
  weights = uw_t,
  id = NAME.x,
  corstr = "independence"
)
summary(msm.uw)
broom::tidy(x = msm.uw, conf.int = TRUE)


msm.sw <- geeglm(
  Deaths~ pm25,
  data=aggregate_pm_census_cdc_test_beds,
  weights = sw_t,
  id = NAME.x,
  corstr = "independence"
)
summary(msm.sw)
broom::tidy(x = msm.sw, conf.int = TRUE)


