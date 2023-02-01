
#### NIS - FLU
#################
########### State X Age
#################

states0 <- states()

# Influenza Vaccination Coverage for All Ages (6+ Months)
# https://data.cdc.gov/Flu-Vaccinations/Influenza-Vaccination-Coverage-for-All-Ages-6-Mont/vh55-3he6
fluv_coverage <- read.socrata("https://data.cdc.gov/resource/vh55-3he6.json")

fluv_coverage$year[fluv_coverage$year_season == "2009-10"] <- 2009.5 #           
fluv_coverage$year[fluv_coverage$year_season == "2010-11"] <- 2010.5 #
fluv_coverage$year[fluv_coverage$year_season == "2011-12"] <- 2011.5 #
fluv_coverage$year[fluv_coverage$year_season == "2012-13"] <- 2012.5 #
fluv_coverage$year[fluv_coverage$year_season == "2013-14"] <- 2013.5 #
fluv_coverage$year[fluv_coverage$year_season == "2014-15"] <- 2014.5 #
fluv_coverage$year[fluv_coverage$year_season == "2015-16"] <- 2015.5 #
fluv_coverage$year[fluv_coverage$year_season == "2016-17"] <- 2016.5 #
fluv_coverage$year[fluv_coverage$year_season == "2017-18"] <- 2017.5 #
fluv_coverage$year[fluv_coverage$year_season == "2018-19"] <- 2018.5 #
fluv_coverage$year[fluv_coverage$year_season == "2019-20"] <- 2019.5 #
fluv_coverage$year[fluv_coverage$year_season == "2020-21"] <- 2020.5 #
fluv_coverage$year[fluv_coverage$year_season == "2021-22"] <- 2021.5 #
fluv_coverage$year <- as.numeric(fluv_coverage$year)

fluv_coverage$elect_year[fluv_coverage$year > 2020] <- 2020
fluv_coverage$elect_year[fluv_coverage$year < 2020] <- 2016
fluv_coverage$elect_year[fluv_coverage$year < 2016] <- 2012
fluv_coverage$elect_year[fluv_coverage$year < 2012] <- 2008

fluv_coverage$est    <- as.numeric(fluv_coverage$coverage_estimate)
fluv_coverage$est_ll <- as.numeric(gsub(' to .*', '', fluv_coverage$X_95_ci))

# done in sections as there is a ‡ sign for some UL which is removed first
fluv_coverage$est_ul0 <- (gsub('.*to ', '', fluv_coverage$X_95_ci)) # there's a footnote marker, so this does not come out correct all the time
fluv_coverage$est_ul1 <- (gsub('‡', '', fluv_coverage$est_ul0)) 
fluv_coverage$est_ul <- as.numeric(fluv_coverage$est_ul1)

fluv_coverage$raceth[fluv_coverage$dimension == "Asian, Non-Hispanic"] <- "Asian, NH" #
fluv_coverage$raceth[fluv_coverage$dimension == "Hispanic"] <- "Hispanic" #
fluv_coverage$raceth[fluv_coverage$dimension == "White, Non-Hispanic"] <- "White, NH" #
fluv_coverage$raceth[fluv_coverage$dimension == "Other or Multiple Races, Non-Hispanic"] <- "Other"
fluv_coverage$raceth[fluv_coverage$dimension == "American Indian or Alaska Native, Non-Hispanic"] <- "AI/AN, NH" #
fluv_coverage$raceth[fluv_coverage$dimension == "Black, Non-Hispanic"] <- "Black, NH" #
fluv_coverage$raceth[fluv_coverage$dimension == "≥18 Years"] <- "Overall, 18+" #
fluv_coverage$raceth[fluv_coverage$dimension_type == "Age"] <- "All"

fluv_coverage$dimension[fluv_coverage$dimension == "≥65 Years"] <- "65+ Years" #
fluv_coverage$dimension[fluv_coverage$dimension == "≥18 Years"] <- "Overall, 18+" #
fluv_coverage$age_group[fluv_coverage$dimension == "Overall, 18+"] <- "Overall, 18+" #

fluv_coverage$data_source <- "NIS-Flu & BRFSS"
fluv_coverage$infection <- "flu"

fluv_coverage$state_code <- states0$STUSPS[match(fluv_coverage$geography, states0$NAME)]


# select state level data
fluv_coverage %>%
  filter(geography_type == "States/Local Areas" & dimension_type == "Age") %>%
  filter(month == "5")  %>%
  filter(vaccine == "Seasonal Influenza") %>%
  filter(dimension == "6 Months - 4 Years" | dimension == "5-12 Years" |
           dimension == "13-17 Years" | dimension == "18-49 Years" | dimension == "50-64 Years" |
           dimension == "65+ Years" ) %>%
  mutate(age_group = ifelse(dimension_type == "Age", dimension, "All")) %>%
  mutate(age_group = factor(age_group, levels = c("6 Months - 4 Years", "5-12 Years", "13-17 Years", 
                                                  "18-49 Years", "50-64 Years", "65+ Years", "All")))  -> flust

# overall 2020-2021, 2021-2022 only
fluv_coverage %>%
  filter(age_group ==  "Overall, 18+" ) %>%
  filter(month == "5")  %>%
  filter(vaccine == "Seasonal Influenza") %>%
  filter(year > 2020) -> flust_overall

# region level
fluv_coverage %>%
  filter(geography_type == "HHS Regions/National" & dimension_type == "Age" & 
           geography != "United States") %>%
  filter(month == "5")  %>%
  filter(vaccine == "Seasonal Influenza") %>%
  filter(dimension == "6 Months - 4 Years" | dimension == "5-12 Years" |
           dimension == "13-17 Years" | dimension == "18-49 Years" | dimension == "50-64 Years" |
           dimension == "65+ Years" ) %>%
  mutate(age_group = ifelse(dimension_type == "Age", dimension, "All")) %>%
  mutate(age_group = factor(age_group, levels = c("6 Months - 4 Years", "5-12 Years", "13-17 Years", 
                                                  "18-49 Years", "50-64 Years", "65+ Years", "All"))) -> flurg


# flust %>%
#   group_by(dimension_type, dimension) %>%
#   summarise(n=n()) %>%
#   spread(dimension, n) %>%
#   knitr::kable()


#################
####### COVID-19
#################

# National Immunization Survey Adult COVID Module (NIS-ACM): Vaccination Status and Intent by Demographics
# https://data.cdc.gov/Vaccinations/National-Immunization-Survey-Adult-COVID-Module-NI/iwxc-qftf
nisacm_cv <- read.socrata("https://data.cdc.gov/resource/iwxc-qftf.json")

### COVID-19 X state
nisacm_cv %>%
  filter(geography != "National") -> statev

statev$month[statev$time_period == "April 22 – May 29"] <- "May"
statev$month[statev$time_period == "May 30 – June 26"] <- "June"
statev$month[statev$time_period == "June 27 – July 31"] <- "July"
statev$month[statev$time_period == "August 1 – August 28"] <- "August"
statev$month[statev$time_period == "August 29 – September 25"] <- "September"
statev$month[statev$time_period == "September 26 – October 30"] <- "October"
statev$month[statev$time_period == "October 31 – November 27"] <- "November"
statev$month[statev$time_period == "November 28 – December 31"] <- "December"
statev$month[statev$time_period == "January 2 – January 29"] <- "January"
statev$month[statev$time_period == "January 30 – February 26"] <- "February"
statev$month[statev$time_period == "February 27 – March 26 "] <- "March"

statev$month[statev$time_period == "March 27 – April 30 "] <- "April"
statev$month[statev$time_period == "May 1 – May 28"] <- "May"
statev$month[statev$time_period == "May 29 – June 25"] <- "June"
statev$month[statev$time_period == "June 26 – July 30"] <- "July"
statev$month[statev$time_period == "July 31 – August 27"] <- "August"
statev$month[statev$time_period == "August 28 – September 30"] <- "September"
statev$month[statev$time_period == "October 1 – October 29"] <- "October"

statev$month <- factor(statev$month, levels = c("May", "June", "July", "August",
                                                "September", "October", "November", "December", "January", "February", "March", "April"))

# alternative way the data have been read:
# statev$month[statev$time_period == "April 22 â€“ May 29"] <- "May"
# statev$month[statev$time_period == "May 30 â€“ June 26"] <- "June"
# statev$month[statev$time_period == "June 27 â€“ July 31"] <- "July"
# statev$month[statev$time_period == "August 1 â€“ August 28"] <- "August"
# statev$month[statev$time_period == "August 29 â€“ September 25"] <- "September"
# statev$month[statev$time_period == "September 26 â€“ October 30"] <- "October"
# statev$month[statev$time_period == "October 31 â€“ November 27"] <- "November"
# statev$month[statev$time_period == "November 28 â€“ December 31"] <- "December"
# statev$month[statev$time_period == "January 2 â€“ January 29"] <- "January"
# statev$month[statev$time_period == "January 30 â€“ February 26"] <- "February"
# 
# statev$month <- factor(statev$month, levels = c("May", "June", "July", "August", 
#                                                 "September", "October", "November", 
#                                                 "December", "January", "February"))

statev$year <- as.numeric(statev$time_year)


statev$est    <- as.numeric(statev$estimate)
statev$est_ll <- as.numeric(gsub('-.*', '', statev$coninf_95))
statev$est_ul <- as.numeric(gsub('.*-', '', statev$coninf_95))

statev$state <- statev$geography
# only state overalls used
statev$state[statev$geography == "NY-City of New York" |statev$geography ==  "NY-Rest of State"] <- "NA"
statev$state[statev$geography == "TX-City of Houston" | statev$geography == "TX-Rest of State" | statev$geography == "TX-Bexar County"] <- "NA"
statev$state[statev$geography == "PA-Philadelphia County" | statev$geography == "PA-Rest of State"] <- "NA"
statev$state[statev$geography == "IL-City of Chicago" | statev$geography == "IL-City of Chicago"] <- "NA"

statev$state_code <- states0$STUSPS[match( statev$geography, states0$NAME)]

statev$age_group[statev$group_category == "Overall"] <- "Overall, 18+" #
statev$age_group[statev$group_category == "18–49 yrs"] <- "18-49 Years" #
statev$age_group[statev$group_category == "50–64 yrs"] <- "50-64 Years" #
statev$age_group[statev$group_category == "65+ yrs"] <- "65+ Years" #

# statev$raceth <- ifelse("Race/Ethnicity (7 level)" statev$group_category,  )
# statev$raceth[statev$group_category == "Hispanic/Latino"] <- "Hispanic" #
# statev$raceth[statev$group_category == "Multi"] <- "Other or multi, NH" #
# statev$raceth[statev$group_category == "Overall"] <- "Overall, 18+" #

statev %>%
  filter(indicator_category == "Vaccinated") %>%
  filter( group_name == "Age" | group_name == "All Adults Age 18+") %>%
  # filter(time_period == "August 1–August 28") %>% # turn this off if you want all times
  mutate(year = as.numeric(2021)) %>%
  mutate(pseudo_num = est/100* as.numeric(sample_size)) %>%
  mutate(infection = "COVID-19")  %>%
  mutate(data_source = "NIS-ACM") %>%
  select(year, sample_size, pseudo_num, group_category, data_source, est, est_ll, est_ul, infection, age_group, state, state_code, time_period ) %>%
  mutate(est_ll2 = est - 100*(1.96 * sqrt(est/100 * (1-est/100)/as.numeric(sample_size))))  %>%
  mutate(est_ul2 = est + 100*(1.96 * sqrt(est/100 * (1-est/100)/as.numeric(sample_size))))  -> statev_age

# ## long form  
# statev_age %>%
#   bind_rows(., flust_age) %>%
#   left_join(elect, by="state_code") %>%
#   mutate(age_group = factor(age_group, levels = c("6 Months - 4 Years", "5-12 Years", "13-17 Years",
#                                                   "18-49 Years", "50-64 Years", "65+ Years",
#                                                   "Overall, 18+"))) -> stv_age
# 
# ## wide form
# statev_age %>%
#   inner_join(flust_age, by = c("state", "state_code", "age_group")) %>%
#   left_join(elect, by="state_code") %>%
#   mutate(age_group = factor(age_group, levels = c("6 Months - 4 Years", "5-12 Years", "13-17 Years",
#                                                   "18-49 Years", "50-64 Years", "65+ Years",
#                                                   "Overall, 18+"))) -> stv_age2

########################################################################
# National Immunization Survey Child COVID Module (NIS-CCM): Vaccination Status and Intent by Demographics 
# https://data.cdc.gov/Vaccinations/National-Immunization-Survey-Child-COVID-Module-NI/gr26-95h2
# nisccm_cv <- read.socrata("https://data.cdc.gov/resource/gr26-95h2.json")
# print("expect 445")
# nrow(nisccm_cv)
# # save(c19v_intdemo, file=paste(here("cdc_data_bup/c19v_intdemo.Rdata")))
# THIS IS ONLY AT NATIONAL LEVEL

#### CDC COVID DATA - SURVEILLANCE

# COVID-19 Vaccinations in the United States,Jurisdiction
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc
c19v_all <- read.socrata("https://data.cdc.gov/resource/unsk-b7fc.json")

c19v_all %>%
  mutate(date = ymd(date)) %>%
  mutate(state_code = location) %>%
  mutate(elect_year = ifelse(year(date)>2019, 2020, ifelse(year(date)<2020, 2016, NA ))) %>%
  group_by(month = month(date), year = year(date), location) %>%
  slice(which.max(day(date))) %>%
  ungroup() %>%
  select(date, state_code, month, elect_year, administered_dose1_pop_pct, administered_dose1_recip_5pluspop_pct,
         administered_dose1_recip_2, administered_dose1_recip_4, administered_dose1_recip_6) %>%
  left_join(elect, by=c("state_code", "elect_year")) %>%
  filter(!is.na(year)) %>%
  mutate(est = as.numeric(administered_dose1_pop_pct)) %>%
  mutate(est_5plus = as.numeric(administered_dose1_recip_5pluspop_pct)) %>%
  mutate(est_12plus = as.numeric(administered_dose1_recip_2)) %>%
  mutate(est_65plus = as.numeric(administered_dose1_recip_6)) -> cv19_bymonth
  

