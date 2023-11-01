## Packages

library(reactable)
library(tidyverse)
library(here)
library(dbplyr)
library(readr)
library(bigrquery)
library(DBI)
library(janitor)
library(icesTAF)
library(pbapply)
library(tippy)
library(tsibble)
library(lubridate)
library(zoo)
library(sf)
library(scales)

## EPD data downloaded from NHS BSA - combine all monthly datasets into one DF - not included in GitHub Download
## create separate dfs for Primary data sets 
# PriOX <- list.files(path='C:/../Oxybate', full.names = TRUE) %>%
#   lapply(read_csv) %>%
#   bind_rows
# PriPit <- list.files(path='C:/../Pitolisant', full.names = TRUE) %>%
#   lapply(read_csv) %>%
#   bind_rows

PriOX <- read.csv(here("data/PriOX.csv"))
PriPit <- read.csv(here("data/PriPit.csv"))

## Update ICB names test

Gp_data <- read_csv(here("data/gp-reg-pat-prac-map.csv"))
ICBnames <- read.csv(here("data/ICBnames.csv"))

PriOX <- left_join(PriOX,
                   Gp_data,
                   by = "PRACTICE_CODE")

PriPit <- left_join(PriPit,
                    Gp_data,
                    by = "PRACTICE_CODE")

## Tidy up dataframes - removing excess columns + changing names

PriOX[ , c('POSTCODE',
           'UNIDENTIFIED',
           'BNF_CHAPTER_PLUS_CODE',
           'ACTUAL_COST',
           'REGIONAL_OFFICE_CODE',
           'ADDRESS_4',
           'ADDRESS_2',
           'ADDRESS_3',
           'ADQUSAGE',
           'PCO_CODE',
           'PCO_NAME',
           'ADDRESS_1',
           'STP_NAME',
           'ICB_NAME.x',
           'ICB_CODE.x',
           'PRACTICE_NAME',
           'AREA_TEAM_NAME',
           'REGIONAL_OFFICE_NAME',
           'QUANTITY')] <- list(NULL)

# Create separate dataframe for ICB non-matches
OxybateNoICB <- PriOX %>%
  filter(is.na(ICB_NAME.y)) %>%
  select(-ICB_NAME.x, -ICB_CODE.x)

OxybateNoICB <- OxybateNoICB %>%
  left_join(ICBnames, by = c("STP_CODE.x" = "stp_code")) %>%
  mutate(ICB_NAME = coalesce(ICB_NAME, ICB_NAME.y),
         ICB_CODE = coalesce(ICB_CODE, ICB_CODE.y)) %>%
  select(-STP_CODE.y, -ICB_NAME.y, -ICB_CODE.y)

PriOX <- PriOX %>%
  rename("ICB_CODE" = "ICB_CODE.y",
         "ICB_NAME" = "ICB_NAME.y")

# Combine the result with rows not meeting the NA condition
PriOX <- bind_rows(OxybateNoICB, PriOX %>% filter(!is.na(ICB_NAME)))

# filter out remaining practices not matched to ICB
# 3916/4010 months included 
PriOX <- PriOX %>%
  filter(!is.na(ICB_NAME))

## Pitolisant

PriPit[ , c('POSTCODE',
            'UNIDENTIFIED',
            'BNF_CHAPTER_PLUS_CODE',
            'ACTUAL_COST',
            'REGIONAL_OFFICE_CODE',
            'ADDRESS_4',
            'ADDRESS_2',
            'ADDRESS_3',
            'ADQUSAGE',
            'PCO_CODE',
            'PCO_NAME',
            'ADDRESS_1',
            'STP_NAME',
            'ICB_NAME.x',
            'ICB_CODE.x',
            'PRACTICE_NAME',
            'AREA_TEAM_NAME',
            'REGIONAL_OFFICE_NAME',
            'QUANTITY')] <- list(NULL)

### No longer necessary - not sure how this has resulted in duplicate columns
# PriPit <- PriPit %>%
#   rename("ICB_CODE" = "ICB_CODE.y",
#          "ICB_NAME" = "ICB_NAME.y")


### Adjust dataframes to match ICB codes to GP practices which practice codes don't make GP mapping data
### total 4 year quantities

# PriOXQuantities <- PriOX %>%
#   group_by(YEAR_MONTH, ICB_NAME) %>%
#   summarize(TOTAL_QUANTITY = sum(TOTAL_QUANTITY))
# 
# PriOX4Year <- PriOXQuantities %>%
#   group_by(ICB_NAME) %>%
#   summarize(TOTAL_QUANTITY = sum(TOTAL_QUANTITY))
# 
# ## Convert to DDDs
# 
# PriOX4Final <- PriOX4Year %>%
#   mutate(DDDs = (TOTAL_QUANTITY / 180) * 12)


## Manual edit of missing ICB names (i.e. with 0 issuance) - csv below
# write.csv(PriOX4Final, here("data/PriOx4year.csv", row.names = TRUE))

PriOX4Final <- read.csv(here("data/PriOx4year.csv"))
Pri4yearPit <- read.csv(here("data/PriPit4year.csv"))

#### 4 year plot

PriOX4Final <- PriOX4Final %>%
  rename("ICB22NM" = "ICB_NAME",
         "total_quantity" = "TOTAL_QUANTITY",
         "volume_ddd" = "DDDs")

PriOX4Final <- left_join(ICS,
                         PriOX4Final,
                         by = "ICB22NM")

PriOX4YearPlot <- ggplot(data = ICS) +
  ggtitle("(a)") +
  geom_sf(aes(fill = PriOX4Final$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "DDDs",
                      high = "red",  
                      low = "yellow",
                      na.value = "#F2F0F0",
                      limits = c(5,115000),
                      breaks = c(2500, 15000, 80000),
                      labels = c(2500, 15000, 80000),
                      trans = scales::trans_new("log",
                                                transform = function(x) log(x + 1000),
                                                inverse = function(x) exp(x) - 1000)
  ) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1, 0.8)
  )
## 4 Year pitolisant Map

PriPit4Final <- left_join(ICS,
                          Pri4yearPit,
                          by = "ICB22NM")
PriPit4YearPlot <- ggplot(data = ICS) +
  ggtitle("(b)") +
  geom_sf(aes(fill = PriPit4Final$DDDs)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "DDDs",
                      high = "red",  
                      low = "yellow",
                      na.value = "#F2F0F0",
                      limits = c(5,115000),
                      breaks = c(2500, 15000, 80000),
                      labels = c(2500, 15000, 80000),
                      trans = scales::trans_new("log",
                                                transform = function(x) log(x + 1000),
                                                inverse = function(x) exp(x) - 1000)
  ) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1, 0.8)
  )
