### 0. load the tidyverse package

library(tidyverse)

### 0a. set working directory

"G:/My Drive/Fall 2021/EPH 505 Biostatistics in Public Health/Teaching Fellow" %>% setwd
getwd()

### 0b. read-in data from file

nhanes = "NHANES_DemoBMX.csv" %>% read.csv
#1. Subset (filter)

nhanes = nhanes %>% filter(ridageyr>=18) %>% filter(ridageyr<=40)

#nhanes = nhanes %>% filter(ridageyr>=18 & ridageyr<=40)

nhanes %>% nrow %>% print

#Printing (Second arguments via dot)
nhanes %>% select(ridageyr) %>% min %>% paste("minimum age is",.) %>% print

##wrong approach wo period
#nhanes %>% select(ridageyr) %>% min %>% paste("minimum age is") %>% print

nhanes %>% select(ridageyr) %>% max %>% paste("maximum age is",.) %>% print
nhanes %>% select(ridageyr) %>% count %>% paste("number of observations is",.) %>% print

#3. Manipulation (mutate)
nhanes %>% dim
nhanes %>% mutate(tall=bmxht>=170) %>% dim
nhanes %>% mutate(tall=bmxht>=170) %>% names

#4. Conditioning (drop_na)
nhanes %>% mutate(tall=bmxht>=170) %>% select(tall, riagendr) %>% drop_na %>% dim

#5. Tabularizing (tabyl)
#install.packages("janitor")
library(janitor)
nhanes %>% mutate(tall=bmxht>=170) %>% select(tall, riagendr) %>% drop_na %>% tabyl(tall, riagendr)

#6. Summarizing (group_by, summarize, and tibbles)
nhanes %>% mutate(tall=bmxht>=170) %>% select(tall, riagendr) %>% drop_na %>% group_by(tall, riagendr) %>% summarize(N=n())

