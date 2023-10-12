## Very much still in draft
## Set wd

setwd('C:/..')

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

## Combine downloaded data into one df
# Combined DFs are available in this pull request 
# PriOX <- list.files(path='C:/../Oxybate', full.names = TRUE) %>% 
#   lapply(read_csv) %>% 
#   bind_rows
# PriPit <- list.files(path='C:/../Pitolisant', full.names = TRUE) %>% 
#   lapply(read_csv) %>% 
#   bind_rows

## Update ICB names test using up to date BSA gp registration metadata

Gp_data <- read_csv("C:/../gp-reg-pat-prac-map.csv")

PriOX <- left_join(PriOX,
                   Gp_data,
                   by = "PRACTICE_CODE")

PriPit <- left_join(PriPit,
                    Gp_data,
                    by = "PRACTICE_CODE")

## Tidy up dataframes - removing excess columns + changing names - purely for aescetics, not critical step

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

PriOX <- PriOX %>%
  rename("ICB_CODE" = "ICB_CODE.y",
         "ICB_NAME" = "ICB_NAME.y")

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

PriPit <- PriPit %>%
  rename("ICB_CODE" = "ICB_CODE.y",
         "ICB_NAME" = "ICB_NAME.y")

### Adjust dataframes to match ICB codes to GP practices which practice codes don't match GP mapping data
## doesn't work check with someone else
# result_df <- PriOX %>%
#   left_join(Gp_data, by = "STP_CODE") %>%
#   mutate(
#     ICB_NAME = coalesce(ICB_NAME.x, ICB_NAME.y),
#     ICB_CODE = coalesce(ICB_CODE.x, ICB_CODE.y),
#     PRACTICE_CODE = coalesce(PRACTICE_CODE.x, PRACTICE_CODE.y)
#   ) %>%
#   select(-ICB_NAME.x, -ICB_CODE.x, -ICB_NAME.y, -ICB_CODE.y, - 
#            PRACTICE_CODE.x, -PRACTICE_CODE.y)

## temporary remove NA rows

PriOXtest <- PriOX %>%
  filter(!is.na(ICB_NAME))

### total 4 year quantities

PriOXQuantities <- PriOXtest %>%
  group_by(YEAR_MONTH, ICB_NAME) %>%
  summarize(TOTAL_QUANTITY = sum(TOTAL_QUANTITY))

PriOX4Year <- PriOXQuantities %>%
  group_by(ICB_NAME) %>%
  summarize(TOTAL_QUANTITY = sum(TOTAL_QUANTITY))

## Convert to DDDs

PriOX4Final <- PriOX4Year %>%
  mutate(DDDs = (TOTAL_QUANTITY / 180) * 12)


## Manual edit of missing ICB names (i.e. with 0 issuance) as can't figure out how to do in R
write.csv(PriOX4Final, "C:/../PriOx4test.csv", row.names = TRUE)

PriOX4Final <- read.csv("C:/../PriOx4test.csv")

### Primary Care Oxybate 2019

PriOx2019 <- PriOXQuantities %>%
  filter(YEAR_MONTH >= "201901" & YEAR_MONTH <= "201912")

PriOx2019 <- PriOx2019 %>%
  group_by(ICB_NAME) %>%
  summarize(TOTAL_QUANTITY = sum(TOTAL_QUANTITY))

#Convert to DDDs
PriOx2019 <- PriOx2019 %>%
  mutate(DDDs = (TOTAL_QUANTITY / 180) * 12)

PriOx2019 <- PriOx2019 %>%
  rename("ICB22NM" = "ICB_NAME")

## Manual edit for ICB names

write.csv(PriOx2019, "C:/../PriOx2019.csv")

## ReRead edit file for plot

Pri2019Ox <- read.csv("C:/../PriOx2019.csv")

Pri2019Ox <- left_join(ICS,
                       Pri2019Ox,
                       by = "ICB22NM")

### Primary Care Oxybate 2022

PriOx2022 <- PriOXQuantities %>%
  filter(YEAR_MONTH >= "202201" & YEAR_MONTH <= "202212")

PriOx2022 <- PriOx2022 %>%
  group_by(ICB_NAME) %>%
  summarize(TOTAL_QUANTITY = sum(TOTAL_QUANTITY))

#Convert to DDDs
PriOx2022 <- PriOx2022 %>%
  mutate(DDDs = (TOTAL_QUANTITY / 180) * 12)

PriOx2022 <- PriOx2022 %>%
  rename("ICB22NM" = "ICB_NAME")

## Manual edit for ICB names

write.csv(PriOx2022, "C:/../PriOx2022.csv")

## ReRead edit file for plot

Pri2022Ox <- read.csv("C:/../PriOx2022.csv")

Pri2022Ox <- left_join(ICS,
                       Pri2022Ox,
                       by = "ICB22NM")
#### Pitolisant

### generate DDDs
# Pitolisant18 <- PriPit %>%
#   filter(BNF_DESCRIPTION == "Pitolisant 18mg tablets")
# 
# Pitolisant18 <- Pitolisant18 %>%
#   mutate(DDDs = TOTAL_QUANTITY * 1)
# 
# Pitolisant45 <- PriPit %>%
#   filter(BNF_DESCRIPTION == "Pitolisant 4.5mg tablets")
# 
# Pitolisant45 <- Pitolisant45 %>%
#   mutate((DDDs = TOTAL_QUANTITY * 4.5) / 18)
# 
# Pitolisant45 <- Pitolisant45 %>%
#   rename("DDDs" = "(DDDs = TOTAL_QUANTITY * 4.5)/18")
# 
# PitolisantDDDs <- rbind(Pitolisant18, Pitolisant45)

## temporary remove NA rows

PitolisantDDDs <- PitolisantDDDs %>%
  filter(!is.na(ICB_NAME))

## 4 year Pitolisant quantities
PriPitQuantities <- PitolisantDDDs %>%
  group_by(YEAR_MONTH, ICB_NAME) %>%
  summarize(DDDs = sum(DDDs))

PriPit4Year <- PriPitQuantities %>%
  group_by(ICB_NAME) %>%
  summarize(DDDs = sum(DDDs))

write.csv(PriPit4Year, "C:/../PriPit4year.csv")

Pri4yearPit <- read.csv("C:/../PriPit4year.csv")

### Pitolisant 2019

PriPit2019 <- PitolisantDDDs %>%
  filter(YEAR_MONTH >= "201901" & YEAR_MONTH <= "201912")

PriPit2019 <- PriPit2019 %>%
  group_by(ICB_NAME) %>%
  summarize(DDDs = sum(DDDs))

PriPit2019 <- PriPit2019 %>%
  rename("ICB22NM" = "ICB_NAME")

## Manual edit for ICB names - include ICBs with 0 issuance

write.csv(PriPit2019, "C:/../PriPit2019.csv")

## ReRead edit file for plot

Pri2019Pit <- read.csv("C:/../PriPit2019.csv")

Pri2019Pit <- left_join(ICS,
                        Pri2019Pit,
                        by = "ICB22NM")

### Pitolisant 2022

PriPit2022 <- PitolisantDDDs %>%
  filter(YEAR_MONTH >= "202201" & YEAR_MONTH <= "202212")

PriPit2022 <- PriPit2022 %>%
  group_by(ICB_NAME) %>%
  summarize(DDDs = sum(DDDs))

PriPit2022 <- PriPit2022 %>%
  rename("ICB22NM" = "ICB_NAME")

## Manual edit for ICB names

write.csv(PriPit2022, "C:/../PriPit2022.csv")

## ReRead edit file for plot

Pri2022Pit <- read.csv("C:/../PriPit2022.csv")

Pri2022Pit <- left_join(ICS,
                        Pri2022Pit,
                        by = "ICB22NM")

### Yearly plots - NOT USED

# PriPit2019Plot <- ggplot(data = ICS) +
#   ggtitle("Primary Care Pitolisant 2019") +
#   geom_sf(aes(fill = Pri2019Pit$DDDs)) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient(name = "DDDs",
#                       high = "red",  
#                       low = "yellow",
#                       na.value = "#F2F0F0",
#                       limits = c(5,2500),
#                       breaks = c(250, 1000, 2000),
#                       labels = c(250, 1000, 2000),
#                       trans = scales::trans_new("log",
#                                                 transform = function(x) log(x + 1000),
#                                                 inverse = function(x) exp(x) - 1000)
#   ) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     legend.position = c(0.1, 0.8)
#   )
# 
# PriPit2022Plot <- ggplot(data = ICS) +
#   ggtitle("Primary Care Pitolisant 2022") +
#   geom_sf(aes(fill = Pri2022Pit$DDDs)) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient(name = "DDDs",
#                       high = "red",  
#                       low = "yellow",
#                       na.value = "#F2F0F0",
#                       limits = c(5,7500),
#                       breaks = c(800, 2500, 6500),
#                       labels = c(800, 2500, 6500),
#                       trans = scales::trans_new("log",
#                                                 transform = function(x) log(x + 1000),
#                                                 inverse = function(x) exp(x) - 1000)
#   ) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     legend.position = c(0.1, 0.8)
#   )
# 
# PriOX2019Plot <- ggplot(data = ICS) +
#   ggtitle("Primary Care Sodium Oxybate 2019") +
#   geom_sf(aes(fill = Pri2019Ox$DDDs)) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient(name = "DDDs",
#                       high = "red",  
#                       low = "yellow",
#                       na.value = "#F2F0F0",
#                       limits = c(5,12000),
#                       breaks = c(1000, 3000, 8000),
#                       labels = c(1000, 3000, 8000),
#                       trans = scales::trans_new("log",
#                                                 transform = function(x) log(x + 1000),
#                                                 inverse = function(x) exp(x) - 1000)
#   ) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     legend.position = c(0.1, 0.8)
#   )
# 
# 
# 
# PriOX2022Plot <- ggplot(data = ICS) +
#   ggtitle("Primary Care Sodium Oxybate 2022") +
#   geom_sf(aes(fill = Pri2022Ox$DDDs)) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient(name = "DDDs",
#                       high = "red",  
#                       low = "yellow",
#                       na.value = "#F2F0F0",
#                       limits = c(5,12000),
#                       breaks = c(1000, 3000, 8000),
#                       labels = c(1000, 3000, 8000),
#                       trans = scales::trans_new("log",
#                                                 transform = function(x) log(x + 1000),
#                                                 inverse = function(x) exp(x) - 1000)
#   ) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     legend.position = c(0.1, 0.8)
#   )

#### 4 year plot

## Primary care issuance Oxybate

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
                      limits = c(5,50000),
                      breaks = c(1000, 8000, 30000),
                      labels = c(1000, 8000, 30000),
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

## primary care issuance pitolisant

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
                      limits = c(5,15000),
                      breaks = c(1000, 4000, 10000),
                      labels = c(1000, 4000, 10000),
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
