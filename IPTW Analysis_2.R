###### Inverse Probability Treatment Weighting and Marginal Structural Model ######

#### Create the denominator of IPW for treatment (IPTW)

# Data prep 
aggregate_pm_census_cdc_test_beds$face_masks<- ifelse(aggregate_pm_census_cdc_test_beds$Face_Masks_Required_in_Public=="Yes", 1, 0)
aggregate_pm_census_cdc_test_beds[is.na(aggregate_pm_census_cdc_test_beds$face_masks),122]<-0 

median(aggregate_pm_census_cdc_test_beds$mean_pm25)
aggregate_pm_census_cdc_test_beds$pm25 <- cut(aggregate_pm_census_cdc_test_beds$mean_pm25, breaks = c(-Inf, median(aggregate_pm_census_cdc_test_beds$mean_pm25), +Inf), labels = c(0,1))

# Estimate propensity score (INCLUDING ALL COVARIATES)
fitps <- glm(pm25 ~ face_masks + Tests_per_100K
             + perc_unins 
             + q_popdensity
             + poverty + log(medianhousevalue)
             + log(medhouseholdincome) + pct_owner_occ
             + education + pct_blk + hispanic
             + older_pecent + prime_pecent + mid_pecent 
             + date_since_social + date_since
             + beds/population
             + obese + smoke
             + mean_summer_temp + mean_winter_temp + mean_summer_rm + mean_winter_rm
             #+ (1|Province_State)
             + offset(log(population)), data=aggregate_pm_census_cdc_test_beds, family="quasibinomial")
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
summary(aggregate_pm_census_cdc_test_beds$uw_t) #mean =~ 28
summary(aggregate_pm_census_cdc_test_beds$sw_t) #mean =~ 14


#### Marginal structural model with IPTW estimating the marginal mean differences for the exposure/treatment on the outcome

fituw <- glm(Deaths~ pm25,data=aggregate_pm_census_cdc_test_beds,
             weights = uw_t)
summary(fituw)
cbind(beta = coef(fituw), confint(fituw))

fitsw <- glm(Deaths~ pm25,data=aggregate_pm_census_cdc_test_beds,
             weights = sw_t)
summary(fitsw)
cbind(beta = coef(fitsw), confint(fitsw))


####

# Estimate propensity score (NOT INCLUDING COVARIATES)
fitps <- glm(pm25 ~ face_masks + Tests_per_100K
             + perc_unins, data=aggregate_pm_census_cdc_test_beds, family="quasibinomial")
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
summary(aggregate_pm_census_cdc_test_beds$uw_t) #mean =~ 28
summary(aggregate_pm_census_cdc_test_beds$sw_t) #mean =~ 14


#### Marginal structural model with IPTW estimating the marginal mean differences for the exposure/treatment on the outcome

fituw <- glm(Deaths~ pm25,data=aggregate_pm_census_cdc_test_beds,
             weights = uw_t)
summary(fituw)
cbind(beta = coef(fituw), confint(fituw))

fitsw <- glm(Deaths~ pm25,data=aggregate_pm_census_cdc_test_beds,
             weights = sw_t)
summary(fitsw)
cbind(beta = coef(fitsw), confint(fitsw))
