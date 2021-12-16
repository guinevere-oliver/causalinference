library(tidyverse)

#load in dataset 
#aggregate_pm_census_cdc_test_beds<- readRDS("G:/My Drive/Fall 2021\EHS 566 Causal Inference Methods in Public Health Research/causalinference-main/aggregate_pm_census_cdc_test_beds_P6.RDS")

aggregate_pm_census_cdc_test_beds$face_masks<- ifelse(aggregate_pm_census_cdc_test_beds$Face_Masks_Required_in_Public=="Yes", 1, 0)
aggregate_pm_census_cdc_test_beds[is.na(aggregate_pm_census_cdc_test_beds$face_masks),122]<-0 

median(aggregate_pm_census_cdc_test_beds$mean_pm25)
aggregate_pm_census_cdc_test_beds$pm25 <- cut(aggregate_pm_census_cdc_test_beds$mean_pm25, breaks = c(-Inf, median(aggregate_pm_census_cdc_test_beds$mean_pm25), +Inf), labels = c(0, 1))

#Estimate propensity score
fitps <- glm(pm25 ~ face_masks + Tests_per_100K
             + perc_unins, data=aggregate_pm_census_cdc_test_beds, family=binomial())
summary(fitps)
aggregate_pm_census_cdc_test_beds$ps <- predict(fitps, aggregate_pm_census_cdc_test_beds, type="response")

summary(aggregate_pm_census_cdc_test_beds$ps[aggregate_pm_census_cdc_test_beds$Deaths==0])
summary(aggregate_pm_census_cdc_test_beds$ps[aggregate_pm_census_cdc_test_beds$Deaths==1])

#Outcome regression 
fit<-glm(data=aggregate_pm_census_cdc_test_beds, Deaths ~ pm25 + ps
         + factor(q_popdensity)
         + scale(poverty) + scale(log(median_house_value))
         + scale(log(median_household_income)) + scale(owner_occupied) 
         + scale(no_grad) + scale(blk_pct) + scale(hispanic_pct)
         + scale(older_pecent) + scale(prime_pecent) + scale(mid_pecent) 
         + scale(date_since_social) + scale(date_since)
         + scale(beds/population) 
         + scale(obese) + scale(smoke)
         + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
         + offset(log(population)))

##Deaths ~ pm25 + ps + factor(q_popdensity) + poverty + log(median_house_value) + log(median_household_income) + owner_occupied + no_grad + blk_pct + hispanic_pct + older_pecent + prime_pecent + mid_pecent + date_since_social + date_since + beds/population + obese + smoke + mean_summer_temp + mean_winter_temp + mean_summer_rm + mean_winter_rm + offset(log(population)))
summary(fit)
cbind(coef(fit),confint(fit))


#pm25       24.604412   14.1960027  35.012821

#Matching
library(MatchIt)
set.seed(2021)

#class(aggregate_pm_census_cdc_test_beds$pm25)

m.out <- matchit(pm25 ~ face_masks + Tests_per_100K + perc_unins, data=aggregate_pm_census_cdc_test_beds,method= "nearest", ratio=1, distance= "logit", caliper=0.30, replace= F)

summary(m.out)
summary(m.out, standardize = TRUE) 


g.matches <- get_matches(m.out, data =aggregate_pm_census_cdc_test_beds,distance = "prop.score")

dim(g.matches) #multiple rows per matched unit
head(g.matches)

#check the distribution of PS by exposure in the matched data
summary(g.matches$ps[g.matches$pm25==0])
summary(g.matches$ps[g.matches$pm25==1])

#Plot the estimated propensity score
ggplot(g.matches, aes(x = ps, fill = factor(pm25))) + geom_density(alpha = 0.2) +
  xlab('Probability of High PM2.5 Concentration') +
  ggtitle('Propensity Score Distribution by PM2.5 Group') +
  scale_fill_discrete('') +
  theme(legend.position = 'bottom', legend.direction = 'vertical')


#Outcome regression using PS-matched data 

fitmat <- glm(Deaths ~ pm25 + ps
              + factor(q_popdensity)
              + scale(poverty) + scale(log(median_house_value))
              + scale(log(median_household_income)) + scale(owner_occupied) 
              + scale(no_grad) + scale(blk_pct) + scale(hispanic_pct)
              + scale(older_pecent) + scale(prime_pecent) + scale(mid_pecent) 
              + scale(date_since_social) + scale(date_since)
              + scale(beds/population) 
              + scale(obese) + scale(smoke)
              + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
              + offset(log(population)), data=g.matches)
summary(fitmat)
cbind(beta = coef(fitmat), confint(fitmat))

#pm251         29.1487073   17.3146614  40.982753


###### Stratified by propensity score ######
#library(psych)
# Calculation of deciles
#aggregate_pm_census_cdc_test_beds$ps.dec <- cut(aggregate_pm_census_cdc_test_beds$ps, breaks=c(quantile(aggregate_pm_census_cdc_test_beds$ps, probs=seq(0,1,0.1))), labels=seq(1:10), include.lowest=TRUE)

#describeBy(aggregate_pm_census_cdc_test_beds$ps, list(aggregate_pm_census_cdc_test_beds$ps.dec, aggregate_pm_census_cdc_test_beds$pm25))

#agg_eff<-as.data.frame(matrix(nrow=10,ncol=2))
#for (deciles in c(1:10)) {
  #fit<-glm(data=aggregate_pm_census_cdc_test_beds[which(aggregate_pm_census_cdc_test_beds$ps.dec==deciles),],Deaths ~ pm25)
  #agg_eff[deciles,1]<-coef(fit)[2]                      #Estimate
  #agg_eff[deciles,2]<-summary(fit)$coefficients[, 2][2] #SE
  
}


