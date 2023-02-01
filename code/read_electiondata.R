
#############################
####### Presidential election
#############################

elect0 <- fread(here("data/1976-2020-president.csv"), header=TRUE)

## writein == false will respond to votes for the democrat main candidate 
## 2016 maryland and arizona had writein candidates marked for the Democrat candidate (Clinton). 
## The candidatevotes when writein == FALSE corresponds to the official count for Clinton for both states and the writein votes are not included here

## all 2020 D.C. results are writein, manually corrected
elect0$writein[elect0$state_po=="DC" & elect0$year==2020 & elect0$party_simplified == "DEMOCRAT"] <- FALSE

elect0 %>%
  filter(year > 2007 & (party_simplified == "DEMOCRAT") & writein == FALSE) %>%
  mutate(vote_share = candidatevotes / totalvotes) %>%
  rename(state_code = state_po) %>%
  mutate(rank_share = rank(vote_share)) %>%
  mutate(elect_year = year) -> elect

# get election by HHS region 

elect$geography[elect$state_code == "ME" | elect$state_code == "MA" | elect$state_code == "CT" | elect$state_code == "NH" |
                  elect$state_code == "RI" | elect$state_code == "VT"] <- "Region 1"

elect$geography[elect$state_code == "NJ" | elect$state_code == "NY"] <- "Region 2"

elect$geography[elect$state_code == "DE" | elect$state_code == "DC" | elect$state_code == "MD" | elect$state_code == "PA" |
                  elect$state_code == "VA" | elect$state_code == "WV"] <- "Region 3"

elect$geography[elect$state_code == "AL" | elect$state_code == "FL" | elect$state_code == "GA" | elect$state_code == "KY" |
                  elect$state_code == "MS" | elect$state_code == "NC" | elect$state_code == "SC" | elect$state_code == "TN"] <- "Region 4"

elect$geography[elect$state_code == "IL" | elect$state_code == "IN" | elect$state_code == "MI" | elect$state_code == "MN" |
                  elect$state_code == "OH" | elect$state_code == "WI"] <- "Region 5"

elect$geography[elect$state_code == "AR" | elect$state_code == "LA" | elect$state_code == "NM" | elect$state_code == "OK" |
                  elect$state_code == "TX"] <- "Region 6"

elect$geography[elect$state_code == "IA" | elect$state_code == "KS" | elect$state_code == "MO" | elect$state_code == "NE"] <- "Region 7"

elect$geography[elect$state_code == "CO" | elect$state_code == "MT" | elect$state_code == "ND" | elect$state_code == "SD" |
                  elect$state_code == "UT" | elect$state_code == "WY"] <- "Region 8"

elect$geography[elect$state_code == "AZ" | elect$state_code == "CA" | elect$state_code == "HI" | elect$state_code == "NV"] <- "Region 9"

elect$geography[elect$state_code == "AK" | elect$state_code == "ID" | elect$state_code == "OR" | elect$state_code == "WA"] <- "Region 10"

elect %>%
  group_by(geography) %>%
  summarise(vote_share_hhs = sum(candidatevotes) / sum(totalvotes)) %>%
  ungroup() -> elect_hhs


 




