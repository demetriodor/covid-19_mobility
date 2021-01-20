library (tidyverse)
library(zoo)
library(scales)
library(countrycode)


# ### Get Google Mobility data --------------------------------------------
### Get Google Mobility data
#g<- read.csv('./data/Global_Mobility_Report (4).csv') # from a previously downloaded file
g<- read.csv('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv') # directly from the internet
head(g)

countries = c("Argentina","Australia","Austria","Belarus", "Belgium","Brazil", "Bulgaria","Canada", "Chile", "Colombia", 
              "Croatia","Czechia","Denmark","Ecuador", "Egypt", "Estonia",
              "Finland","France","Germany","Greece","Hong Kong","Hungary","India", "Indonesia", "Ireland","Israel", "Italy", "Japan",
              "Latvia","Lebanon", "Lithuania","Luxembourg","Malta","Malaysia", "Mexico", "Netherlands","New Zealand", "Nigeria","North Macedonia", "Norway",
              "Pakistan", "Poland","Portugal",
              "Romania","Russia","Saudi Arabia", "Serbia", "Singapore", "Slovakia","Slovenia", "South Africa", "South Korea", 
              "Spain","Sweden", "Switzerland","Taiwan", "Thailand", "Turkey", "Ukraine","United Kingdom", "United States", "Venezuela", "Vietnam")

g.sub <- g %>%
  mutate(date = as.Date(date)) %>%
  filter (country_region %in% countries) %>%
  filter (sub_region_1=='' & metro_area=='') %>%
  droplevels() %>%
  arrange (country_region, date) %>%
  group_by (country_region) %>%
  mutate (`Retail and recreation`= retail_and_recreation_percent_change_from_baseline,
          `Grocery shops and markets` = grocery_and_pharmacy_percent_change_from_baseline,
          `Parks and outdoor areas` = parks_percent_change_from_baseline, 
          `Public transport hubs` = transit_stations_percent_change_from_baseline,
          `Places of work` = workplaces_percent_change_from_baseline, 
          `Residential places` = residential_percent_change_from_baseline,
          id = paste0(country_region_code, '.', date)
          ) %>%
  select (id, country_region, country_region_code, date,`Retail and recreation`, `Grocery shops and markets`, `Parks and outdoor areas`, `Public transport hubs`, `Places of work`, `Residential places`)

write_csv(g.sub, './data/g.csv')
save(g.sub, file = './data/g.RData')


# Get the COVID-19 policy data -------------------------------------------
d<-read.csv('https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv')
head(d)

ds <- d %>%
  mutate (date = as.Date(as.character(Date), format = '%Y%m%d'),
          workplace = rescale (C2_Workplace.closing, to=c(0,1)),
          stayhome = rescale (C6_Stay.at.home.requirements, to=c(0,1)),
          transport = rescale (C5_Close.public.transport, to=c(0,1)),
          schools = rescale (C1_School.closing, to=c(0,1)),
          gathers = rescale (C4_Restrictions.on.gatherings, to=c(0,1)),
          events = rescale (C3_Cancel.public.events, to=c(0,1)),
          masks = rescale (H6_Facial.Coverings, to=c(0,1)),
          myindex_ns = workplace + stayhome + transport + schools + 0.5 * gathers + 0.5 * events + 0.5 * masks,
          myindex = rescale (myindex_ns, to=c(0,100)),
          country_region_code = countrycode(CountryCode, origin = 'iso3c', destination = 'iso2c'),
          id = paste0(country_region_code, '.', date)
  ) %>%
  filter (RegionName =='', date>'2020-02-14') %>%
  select (id, date, CountryCode, country_region_code, workplace, stayhome, transport, schools, gathers, events, masks, myindex)

ds <- ds %>%
  group_by(CountryCode) %>%
  mutate(workplace_d = workplace - lag(workplace),
         stayhome_d = stayhome - lag(stayhome),
         transport_d = transport - lag(transport),
         schools_d = schools - lag(schools),
         gathers_d = gathers - lag(gathers),
         events_d = events - lag(events),
         masks_d = masks - lag(masks),
         myindex_d = myindex - lag(myindex)
  )
head(ds)

write.csv(ds, './data/policy_data.csv')
save(ds, file='./data/policy_data.Rdata')


### Merge the mobility and policy data and save
g.sub <- left_join (g.sub, ds%>%select(- date, -country_region_code), by='id')

#summary(g.sub)
#g.sub[which(is.na(g.sub$workplace)==T),]

write_csv(g.sub, './data/g.csv')
save(g.sub, file = './data/g.RData')

