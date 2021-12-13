library(dplyr)
library(stringr)
library(RCurl)
library(httr)
library(readr)
library(lubridate)

date_of_study <- "06-18-2020"
# Historical data
covid_hist <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv')
covid_us_hist <- subset(covid_hist, Country_Region == "US" & is.na(FIPS) == F)

# Import outcome data from JHU CSSE
covid <- read.csv(paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/', date_of_study, '.csv'))
covid_us <- subset(covid, Country_Region == "US")[, 1:12]
covid_us <- rbind(covid_us, subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS)) & Confirmed == 0 & Deaths == 0 & is.na(FIPS) == F))
covid_us$FIPS <- str_pad(covid_us$FIPS, 5, pad = "0")

# Import exposure PM2.5 data
county_pm <- read.csv('https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/county_pm25.csv')

county_temp <- read.csv('https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/temp_seasonal_county.csv')
# Import census, brfss, testing, mortality, hosptial beds data as potential confounders
county_census <- read.csv('https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/census_county_interpolated.csv')
#county_brfss <- read.csv(text = getURL("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv"), skip = 1)
GET("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv", 
    write_disk("county_brfss.csv", overwrite = TRUE))
county_brfss <- read.csv("county_brfss.csv", skip = 1)
county_brfss <- county_brfss[, c('fipscode', 'v011_rawvalue', 'v009_rawvalue')]
names(county_brfss) <- c('fips', 'obese', 'smoke')
county_brfss$fips <- str_pad(county_brfss$fips, 5, pad = "0")

state_test <- read.csv('https://api.covidtracking.com/v1/states/daily.csv')
state_test <- subset(state_test, date == paste0(substring(str_remove_all(date_of_study, "-"), 5, 8),substring(str_remove_all(date_of_study, "-"), 1, 4)))[, - 38]
statecode <- read.csv('https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/statecode.csv')

hospitals <- read.csv('https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D')
hospitals$BEDS[hospitals$BEDS < 0] <- NA

county_base_mortality <- read.table(('https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/county_base_mortality.txt'), sep = "", header = TRUE)
county_old_mortality <- read.table(('https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/county_old_mortality.txt'), sep = "", header = TRUE)
county_014_mortality <- read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/county_014_mortality.txt", sep = "", header = TRUE)
county_1544_mortality <- read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/county_1544_mortality.txt", sep = "", header = TRUE)
county_4564_mortality <- read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/county_4564_mortality.txt", sep = "", header = TRUE)

colnames(county_old_mortality)[4] <- c("older_Population")
colnames(county_014_mortality)[4] <- c("014_Population")
colnames(county_1544_mortality)[4] <- c("1544_Population")
colnames(county_4564_mortality)[4] <- c("4564_Population")

county_base_mortality <- merge(county_base_mortality,county_old_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)
county_base_mortality <- merge(county_base_mortality,county_014_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)
county_base_mortality <- merge(county_base_mortality,county_1544_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)
county_base_mortality <- merge(county_base_mortality,county_4564_mortality[, c(2, 4)], by = "County.Code", all.x = TRUE)

county_base_mortality$older_pecent <- county_base_mortality$older_Population / county_base_mortality$Population
county_base_mortality$"young_pecent" <- county_base_mortality$"014_Population" / county_base_mortality$Population
county_base_mortality$"prime_pecent" <- county_base_mortality$"1544_Population" / county_base_mortality$Population
county_base_mortality$"mid_pecent" <- county_base_mortality$"4564_Population" / county_base_mortality$Population
county_base_mortality$"older_pecent"[is.na(county_base_mortality$"older_pecent")] <- 0
county_base_mortality$"prime_pecent"[is.na(county_base_mortality$"prime_pecent")] <- 0
county_base_mortality$"mid_pecent"[is.na(county_base_mortality$"mid_pecent")] <- 0
county_base_mortality$"young_pecent"[is.na(county_base_mortality$"young_pecent")] <- 0

# Import NCHS Urban-Rural Classification Scheme for Counties
NCHSURCodes2013 <- read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/NCHSURCodes2013.csv")
NCHSURCodes2013$FIPS <- str_pad(NCHSURCodes2013$FIPS, 5, pad = "0")

# Import FB survey on covid-like sympton data
script <- ("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/main/src/client/delphi_epidata.R")
eval(parse(text = script))

# Import social distancing measure data
state_policy <- read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/updated_data/Data/state_policy0410.csv")
colnames(state_policy)[6] <- "stay_at_home"

# merging data
state_test <- merge(state_test, statecode, by.x = "state" , by.y = "Code")
state_test <- merge(state_test, state_policy[, c(1, 6)], by = "State")
state_test$date_since_social <- as.numeric(as.Date(Sys.Date()) - as.Date((strptime(state_test$stay_at_home, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_social) == TRUE, ]$date_since_social <- 0

# pm2.5 average over 17 years
county_pm_aggregated <- county_pm %>% 
    group_by(fips) %>% 
    summarise(mean_pm25 = mean(pm25))

# temperature and relative humidity average over 17 years
county_temp_aggregated <- county_temp %>% 
  group_by(fips) %>% 
  summarise(mean_winter_temp = mean(winter_tmmx),
            mean_summer_temp = mean(summer_tmmx),
            mean_winter_rm = mean(winter_rmax),
            mean_summer_rm = mean(summer_rmax))

county_pm_aggregated <- merge(county_pm_aggregated,
                              county_temp_aggregated,
                              by = "fips",
                              all.x = TRUE)

county_hospitals_aggregated <- hospitals %>%
  group_by(COUNTYFIPS) %>%
  summarise(beds = sum(BEDS, na.rm = TRUE))
county_hospitals_aggregated$COUNTYFIPS <- str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0")

county_census_aggregated2 <- subset(county_census, year == 2016)

county_census_aggregated2$q_popdensity <- 1
######### CHANGED VARIABLE NAMES AND ADDED NA.RM = TRUE ##############
quantile_popdensity <- quantile(county_census_aggregated2$population_density, c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE)
county_census_aggregated2$q_popdensity[county_census_aggregated2$population_density <= quantile_popdensity[1]] <- 1
county_census_aggregated2$q_popdensity[county_census_aggregated2$population_density > quantile_popdensity[1] &
                                         county_census_aggregated2$population_density <= quantile_popdensity[2]] <- 2
county_census_aggregated2$q_popdensity[county_census_aggregated2$population_density > quantile_popdensity[2] &
                                         county_census_aggregated2$population_density <= quantile_popdensity[3]] <- 3
county_census_aggregated2$q_popdensity[county_census_aggregated2$population_density > quantile_popdensity[3] &
                                         county_census_aggregated2$population_density <= quantile_popdensity[4]] <- 4
county_census_aggregated2$q_popdensity[county_census_aggregated2$population_density > quantile_popdensity[4]] <- 5

county_census_aggregated2$fips <- str_pad(county_census_aggregated2$fips, 5, pad = "0")
county_census_aggregated2 <- merge(county_census_aggregated2,county_brfss,
                                   by = "fips",
                                   all.x = TRUE)

county_pm_aggregated$fips <- str_pad(county_pm_aggregated$fips, 5, pad = "0")
aggregate_pm <- merge(county_pm_aggregated,covid_us,
                      by.x = "fips",
                      by.y = "FIPS")

aggregate_pm_census <- merge(aggregate_pm,
                             county_census_aggregated2,
                             by.x = "fips",
                             by.y = "fips")

county_base_mortality$County.Code <- str_pad(county_base_mortality$County.Code, 5, pad = "0")
aggregate_pm_census_cdc <- merge(aggregate_pm_census,
                                 county_base_mortality[, c("County.Code", 
                                                           "Population",
                                                           "older_pecent",
                                                           "young_pecent",
                                                           "prime_pecent",
                                                           "mid_pecent")],
                                 by.x = "fips",
                                 by.y = "County.Code",
                                 all.x = TRUE)

aggregate_pm_census_cdc <- aggregate_pm_census_cdc[is.na(aggregate_pm_census_cdc$fips) == F, ]

aggregate_pm_census_cdc_test <- merge(aggregate_pm_census_cdc,
                                      state_test[, !(names(state_test) %in% c("fips"))],
                                      by.x = "Province_State",
                                      by.y = "State")

aggregate_pm_census_cdc_test_beds <- merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,
                                           by.x = "fips",
                                           by.y = "COUNTYFIPS",
                                           all.x = TRUE)
aggregate_pm_census_cdc_test_beds$beds[is.na(aggregate_pm_census_cdc_test_beds$beds)] <- 0

# Import outcome data from JHU CSSE, calculate the timing of the 1st confirmed case for each county
date_of_all <- format(seq(as.Date("2020-03-22"), 
                          as.Date(strptime(date_of_study, "%m-%d-%Y")), 
                          by = "days"),
                      "%m-%d-%Y")
covid_us_daily_confirmed <- lapply(date_of_all,
                                   function(date_of_all) {
                                     covid_daily <- read.csv((paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
                                                                                date_of_all,
                                                                                ".csv")))
                                     covid_daily <- covid_daily[!duplicated(covid_daily$FIPS), 1:12]
                                     return(subset(covid_daily, Country_Region == "US" & is.na(FIPS) != TRUE & Confirmed > 0))
                                  }
                                  )

covid_us_new_confirmed <- list()
covid_us_new_confirmed[1] <- covid_us_daily_confirmed[1]
covid_us_new_confirmed[[1]]$date_since <- length(covid_us_daily_confirmed) 

covid_us_new_confirmed[2:length(date_of_all)] <- lapply(2:(length(covid_us_daily_confirmed)),
                                                        function(i) {
                                                          covid_us_new_confirmed <- subset(covid_us_daily_confirmed[[i]],
                                                                                           !(FIPS %in% unlist(sapply(1:(i - 1),
                                                                                                                     function(k)covid_us_daily_confirmed[[k]]$FIPS))))
                                                          if (nrow(covid_us_new_confirmed) > 0) {
                                                            covid_us_new_confirmed$date_since <- length(covid_us_daily_confirmed) - i + 1
                                                            return(covid_us_new_confirmed)
                                                          } else {return(NA)}
                                                        })

covid_us_new_confirmed.df <- do.call("rbind", 
                                     covid_us_new_confirmed)[, c("FIPS", "date_since")]
covid_us_new_confirmed.df$FIPS <- str_pad(covid_us_new_confirmed.df$FIPS, 5, pad = "0")
aggregate_pm_census_cdc_test_beds <- merge(aggregate_pm_census_cdc_test_beds,
                                           covid_us_new_confirmed.df,
                                           by.x = "fips",
                                           by.y = "FIPS", 
                                           all.x = TRUE)
aggregate_pm_census_cdc_test_beds$date_since[is.na(aggregate_pm_census_cdc_test_beds$date_since)] <- 0

aggregate_pm_census_cdc_test_beds <- merge(aggregate_pm_census_cdc_test_beds,
                                           NCHSURCodes2013[, c(1, 7)],
                                           by.x = "fips",
                                           by.y = "FIPS", 
                                           all.x = TRUE)

# Combine five boroughs of NYC
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2 == "New York City", ]$population <-
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "New York City" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$population
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2 == "New York City", ]$beds <-
  subset(aggregate_pm_census_cdc_test_beds,Admin2 == "New York City" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$beds

######## CHANGED VARIABLE NAMES, REMOVED EDUCATION ##########
vars <- c("mean_pm25", "poverty", "median_house_value", "median_household_income", "owner_occupied",
          "blk_pct", "hispanic_pct", "no_grad", "older_pecent", "prime_pecent", "mid_pecent", "obese", "smoke",
          "mean_summer_temp", "mean_summer_rm", "mean_winter_temp", "mean_winter_rm")
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2 == "New York City",][, vars] <-
  sapply(vars, function(var) {
    (subset(aggregate_pm_census_cdc_test_beds, Admin2=="New York City" & Province_State=="New York")[, var] *
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "New York City" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")[, var] * 
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")[, var] * 
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")[, var] *
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")[, var] *
       subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$population) / (
         subset(aggregate_pm_census_cdc_test_beds, Admin2 == "New York City" & Province_State == "New York")$population + 
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Bronx" & Province_State == "New York")$population +
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Kings" & Province_State == "New York")$population + 
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Queens" & Province_State == "New York")$population +
           subset(aggregate_pm_census_cdc_test_beds, Admin2 == "Richmond" & Province_State == "New York")$population)
    }
    )
aggregate_pm_census_cdc_test_beds <- subset(aggregate_pm_census_cdc_test_beds,
                                            !(Admin2 == "Bronx" & Province_State == "New York") &
                                              !(Admin2 == "Kings" & Province_State == "New York") &
                                              !(Admin2 == "Queens" & Province_State == "New York") &
                                              !(Admin2 == "Richmond" & Province_State == "New York"))

# Download ACS data
acs <- read.csv("G:/My Drive/Fall 2021/EHS 566 Causal Inference Methods in Public Health Research/causalinference-main/ACSST5Y2019/ACSST5Y2019.S2701_data_with_overlays_2021-12-03T093915.csv", header=TRUE)[-1,]
# Subset columns
acs_data = subset(acs, select=c("GEO_ID","S2701_C04_001E","NAME","S2701_C01_001E"))
# Calculate percent uninsured
acs_data$uninsured <- as.numeric(acs_data$S2701_C04_001E)
acs_data$total_pop <- as.numeric(acs_data$S2701_C01_001E)
acs_data$perc_unins <- acs_data$uninsured/acs_data$total_pop
# Match GEO_ID to fips
acs_data$fips <- substr(acs_data$GEO_ID,10,14)

# Merge ACS with aggregate_pm_census_cdc_test_beds
aggregate_pm_census_cdc_test_beds <- merge(aggregate_pm_census_cdc_test_beds, acs_data, by="fips", all.x=TRUE)

######################
# Testing data 
######################

testing_state="https://raw.githubusercontent.com/guinevere-oliver/causalinference/main/united_states_covid19_cases_deaths_and_testing_by_state.csv"
testing_data<- read_csv(url(testing_state), skip = 2)
testing_data<- as.data.frame(testing_data, check.names=FALSE)

#Subset & data cleaning ---------------
rownames(testing_data)<- testing_data$`State/Territory`
testing_data_df<- testing_data[c(1,18,19)] #"State/Territory"  "# Tests per 100K" "Total # Tests"
testing_data_df<- testing_data_df[-1]
testing_data_df<- as.data.frame(lapply(testing_data_df,as.numeric), check.names = FALSE)
rownames(testing_data_df)<- testing_data$`State/Territory` #State.Territory #change this part 
testing_data_df$Province_State<- rownames(testing_data_df)
testing_data_df$Province_State[38]<- "New York"

#merging testing data with aggregate data ---------------
dim(aggregate_pm_census_cdc_test_beds) #3101  114
aggregate_pm_census_cdc_test_beds<-merge(aggregate_pm_census_cdc_test_beds,testing_data_df, by.x ="Province_State", by.y = "Province_State", all.x = TRUE  )
#3101 116 now 


######################
# Masking data 
######################

# Download masking data from Github
# Call masking data (CHANGE PATH) 
masking <- read.csv("G:/My Drive/Fall 2021/EHS 566 Causal Inference Methods in Public Health Research/causalinference-main/Masking_data.csv")
# Subset to before July 2020

masking$date_new<- as_date(masking$date, format ="%m/%d/%Y")
masking_june<-masking%>% filter(masking$date_new < "2020-07-01")
range(masking_june$date_new) #Range: "2020-04-10" "2020-06-30"
# Remove unnecessary columns
masking_june <- masking_june[,-c(5:6,8:11)]

# Summarize masking_june by frequency of Face_Masks_Required_in_Public
tallied <- masking_june %>% group_by(State_Tribe_Territory,County_Name,FIPS_State,FIPS_County,Face_Masks_Required_in_Public) %>% 
  summarize(frequency = n()) %>% 
  ungroup()

# Filter down to the most common Face_Masks_Required_in_Public for each county
summ <- tallied %>% 
  group_by(State_Tribe_Territory,County_Name,FIPS_State,FIPS_County) %>% 
  filter(frequency == max(frequency)) %>%
  ungroup()
# Make df
summ <- as.data.frame(summ)

# Merge summ with aggregate data
aggregate_pm_census_cdc_test_beds<-merge(aggregate_pm_census_cdc_test_beds, summ, by.x ="NAME.x", by.y = "County_Name", all.x = TRUE  )

aggregate_pm_census_cdc_test_beds <- rename(aggregate_pm_census_cdc_test_beds, Tests_per_100K = `# Tests per 100K`)