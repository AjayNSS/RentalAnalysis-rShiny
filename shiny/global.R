library(shiny)
library(tidyverse)
library(shinydashboard)
library(sf)
library(ggplot2)

dfRental <- readRDS("../data/dfRentalPropertiesSubset.rds")

# 1.Work Data - Exclude any rows with rents more than 6000. These are outliers
dfRental <- dfRental %>% 
  filter(LeasePerMonth <= 6000) 

#get distinct Zip
dfRentalZip <- dfRental %>% 
   distinct(ZipCode) %>% 
   arrange(ZipCode)




#dataframe for distinct Year greater than 2016
dfRentalYears <- dfRental %>% 
  distinct(ListDateYear) %>% 
  filter(ListDateYear > 2016) %>% 
  arrange(ListDateYear)

dfZipCodeCityArea  <-  read.csv('../data/ZipCodeCityArea.csv')

#Create DF for datatable Pct Increase in Rent

PerctIncreaseInRentByZip_Global <- dfRental %>% 
  group_by(ZipCode, ListDateYear) %>% 
  summarise(mean_rent=mean(LeasePerMonth), .groups = 'drop')  %>% 
  group_by(ZipCode) %>% 
  mutate(lag = lag(mean_rent)) %>%
  mutate(pct_change = (mean_rent - lag) / lag * 100 )

#Find % increase in rent by combining City
PerctIncreaseInRentByZipSub_Global <- merge(x=PerctIncreaseInRentByZip_Global,y=dfZipCodeCityArea) %>% 
  arrange(desc(ListDateYear), desc(pct_change)) 
  


PerctIncreaseInRentByZipSub_Global$mean_rent <- round(PerctIncreaseInRentByZipSub_Global$mean_rent,2)
PerctIncreaseInRentByZipSub_Global$lag <- round(PerctIncreaseInRentByZipSub_Global$lag,2)

PerctIncreaseInRentByZipSub_Global$pct_change <- round(PerctIncreaseInRentByZipSub_Global$pct_change,2)

#global Df for Tab4 plot
dfOverAllRentCompT4_global <- dfRental %>% 
  filter (TotalBedrooms == 3, ListDateYear > 2016) %>% 
  group_by(ZipCode, ListDateYear) %>% 
  summarise(mean_rent=mean(LeasePerMonth), .groups = 'drop')  %>% 
  arrange(ZipCode, ListDateYear) %>% 
  pivot_wider(names_from = ZipCode, values_from = mean_rent) %>% 
  select_if(~ !any(is.na(.))) %>% 
  arrange(ListDateYear) %>% 
  pivot_longer(cols = -ListDateYear)

dfOverAllRentCompT4_global <- merge(x=PerctIncreaseInRentByZip_Global,y=dfZipCodeCityArea) 

#dfOverAllRentCompT4_global$value <- round(dfOverAllRentCompT4_global$value,2)


# 
PerctIncreaseInRentByZipSub_Global_2022 <- PerctIncreaseInRentByZipSub_Global %>% 
filter(ListDateYear==2022, ) %>% 
  arrange(desc(pct_change) ) %>% 
  drop_na()

PerctIncreaseInRentByZipSub_Global_2022$mean_rent <- round(PerctIncreaseInRentByZipSub_Global_2022$mean_rent,2)
PerctIncreaseInRentByZipSub_Global_2022$lag <- round(PerctIncreaseInRentByZipSub_Global_2022$lag,2)

#Get Distinct Area
dfRentalArea <- PerctIncreaseInRentByZipSub_Global %>% 
  distinct(Area) %>% 
  arrange(Area)

#DF for % change id mean rent (2022-2017)
target <- c(2017, 2022)
PctChangeOverYears_5Yr <- dfRental  %>% 
  filter (TotalBedrooms == 3) %>% 
  group_by(ZipCode, ListDateYear) %>% 
  summarise(mean_rent=mean(LeasePerMonth),
            .groups = 'drop')  %>% 
  arrange(ZipCode, ListDateYear) %>% 
  filter( ListDateYear %in% target) %>% 
  group_by(ZipCode) %>%
  filter(n() == 2) %>% 
  mutate(lag = lag(mean_rent)) %>% 
  filter( !is.na(lag) | lag != "") %>% 
  mutate(pct_change = (mean_rent - lag) / lag * 100)

  PctChangeOverYears_5YrCityArea <- merge(x=PctChangeOverYears_5Yr,y=dfZipCodeCityArea)
  
  PctChangeOverYears_5YrCityArea$mean_rent <- round(PctChangeOverYears_5YrCityArea$mean_rent,2)
  PctChangeOverYears_5YrCityArea$lag <- round(PctChangeOverYears_5YrCityArea$lag,2)
  PctChangeOverYears_5YrCityArea$pct_change <- round(PctChangeOverYears_5YrCityArea$pct_change,2)
  
  