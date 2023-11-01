### Combined code updated for GitHub upload
## Mostly small aesthetics edits to pre-existing code from DataLab team


# Utilise right packages

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

### Initial creation of required data frames /variables

## Define vector with selected SNOMED codes for narcolepsy Treatments
## Updated codelist includes correct SNOMED codes etc for Pitolisant + Solriamfetol, + removed other drugs not being investigated
codelist <- read_csv(here("data/updatecodelist.csv"),
                     # convert to strings (they are stored as strings in SCMD table)
                     col_types = cols(id = col_character()))

# pull snomed codes here into a new object for later use
codes <- codelist %>% 
  pull(id)


# Load ETR data
df_etr <- readr::read_csv(here::here("data/etr_tidy.csv")) %>%
  select(c("ods_code", "ods_name", "region_code", "stp_code"))

# load older ETR data and find any additional codes here not in current list
df_etr_historic <- readr::read_csv(here::here("data/etr.csv"), 
                                   col_names = FALSE) %>%
  select("ods_code" = "X1", "ods_name" = "X2", "region_code" = "X3", "stp_code" = "X4")

df_etr_intersect <- intersect(df_etr$ods_code, df_etr_historic$ods_code)

df_etr_historic <- df_etr_historic %>% 
  dplyr::filter(!ods_code %in% df_etr_intersect)

df_etr <- rbind(df_etr, df_etr_historic)


# Load stp to regions data
stp_to_region_map <- read_csv(here::here("data/gp-reg-pat-prac-map.csv")) %>%
  group_by(STP_CODE, STP_NAME) %>%
  summarise(COMM_REGION_NAME = first(COMM_REGION_NAME),
            COMM_REGION_CODE = first(COMM_REGION_CODE)) %>%
  janitor::clean_names()

# check which STPs are in lookup table
stp_count <- df_etr %>% 
  group_by(stp_code) %>%
  summarise(n = n(),
            ods_code = first(ods_code),
            ods_name = first(ods_name))

stp_count <- left_join(stp_count, 
                       stp_to_region_map, 
                       by = "stp_code")
# Sustainability and Transformation Partnerships (STPs) 
df_etr %>% 
  left_join(stp_to_region_map, by = "stp_code") %>% 
  select(ods_name, stp_name, comm_region_name) %>% 
  mutate(stp_name = fct_explicit_na(stp_name),
         comm_region_name = fct_explicit_na(comm_region_name)) 
# reactable::reactable(filterable = TRUE,
#                      columns = list(ods_name = reactable::colDef(name = "Name", 
#                                                                  minWidth = 200),
#                                     stp_name = reactable::colDef(name = "STP", 
#                                                                  minWidth = 150),
#                                     comm_region_name = reactable::colDef(name = "Region", 
#                                                                          minWidth = 70)),
#                      style = list(fontSize = "12px"),
#                      highlight = TRUE)

# Read csv created above using bigquery database; 
## df_complete_scmd includes all scmd available for Oxybate, Pitolisant, Solriamfetol
# edit excludes 1month of RM3 - northern care alliance NHS foundation trust, with 1 month -315000
df_scmd <- read_csv(here("data/df_complete_scmd.csv"),
                    col_types = cols(vmp_snomed_code = col_character()))




# Tidy tidy tidy data
df_scmd_names <- df_scmd %>% 
  dplyr::left_join(dplyr::select(df_etr, ods_code, ods_name, stp_code), by = "ods_code") %>% 
  # some data cleaning as scmd uses some ods codes that are not up to date
  mutate(stp_code = as.character(stp_code),
         stp_code = case_when(
           ods_code == "RQ6" ~ "QYG", # cheshire + merseyside
           ods_code %in% c("RNL", "RE9", "RLN") ~ "QHM", # cumbria
           ods_code %in% c("RM2", "RW3") ~ "QOP", # Mcr
           ods_code == "RGQ" ~ "QJG", # Suffolk and North East Essex
           ods_code == "RJF" ~ "QJ2", # Derbyshire
           ods_code == "RR1" ~ "QHL", # Birmingham
           ods_code == "R1J" ~ "QR1", # gloucestershire (trust present in data but wrong/old code)
           ods_code == "R1E" ~ "QNC", # Staffs
           ods_code == "TAD" ~ "QWO", # W Yorks
           ods_code == "TAJ" ~ "QUA", # Black country
           ods_code == "TAH" ~ "QF7", # South Yorkshire & Bassetlow
           ods_code == "TAF" ~ "QMJ", # North Central London
           TRUE ~ stp_code)
  )
check_missing <- select(df_scmd_names,c("ods_code", "ods_name", "stp_code")) %>%
  distinct(.keep_all = TRUE)

check_missing <- check_missing[order(check_missing[["ods_name"]]), ]

## filter out aberrant result RM3
## See discussion in paper for rationale 
df_scmd_names$total_quantity <- ifelse(df_scmd_names$total_quantity < -20000, 0, df_scmd_names$total_quantity)

# Read the DMD info csv file 
## Updated DMD info includes relevant codes/quantities for Solriamfetol 
df_dmd_info <- read_csv(here("data/UpdateDMDInfo.csv"),
                        col_types = cols(vmp_snomed_code = col_character(),
                                         form_cd = col_character(),
                                         vtmid = col_character()))

# Define tibble with mg_per_ddd for join later
ddds <- select(codelist, c('nm', 'ddd')) %>% 
  drop_na('ddd')

# get additional DDDs sourced elsewhere
df_scmd_mg <- df_scmd_names %>% 
  left_join(df_dmd_info, by = c("vmp_snomed_code", "vmp_product_name")) %>% 
  left_join(ddds, by = c("vmp_product_name" = "nm"))  

#covert volumes
df_scmd_ddd <- df_scmd_mg %>% 
  mutate(volume_singles = total_quantity / udfs) %>% 
  mutate(volume_mg_strength = volume_singles * if_else(is.na(strnt_dnmtr_val),
                                                       true = strnt_nmrtr_val, 
                                                       false = strnt_nmrtr_val * (udfs / strnt_dnmtr_val)),
         volume_ddd = volume_mg_strength / ddd) %>% 
  arrange(year_month, ods_code, vmp_snomed_code) %>% 
  # Temp fix for unit mismatch in mg to microgram
  mutate(volume_ddd = case_when(vmp_snomed_code == "395522003" ~ ((total_quantity * strnt_nmrtr_val) / ddd) / 1000,
                                TRUE ~ volume_ddd))

##################################################################

## Volumes per region ##

df_scmd_all <- df_scmd_ddd %>%
  left_join(stp_to_region_map, by = "stp_code")

###### Rolling 3 monthly average

# national volume_ddd 
national_AllVols <- df_scmd_all %>%
  group_by(year_month, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd))

### + calculate 3 month rolling average

Rolling_3month <- national_AllVols %>%
  group_by(vtmnm) %>%
  arrange(year_month) %>%
  mutate(Rolling_3month = rollapply(volume_ddd, width = 3, FUN = mean, fill = NA, align = "right"))

# Plotting

temp_ggplot_cgpt2 <- ggplot() +
  geom_line(data = Rolling_3month, 
            aes(x = as.Date(year_month), 
                y = Rolling_3month, 
                colour = vtmnm),
            linewidth = 0.5,
            alpha = 0.8) +
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%b '%y", 
               limits = as.Date(c("2019-03-01", "2022-12-01")), 
               expand = c(0, 0)) +
  scale_colour_manual(values = c("orange", "purple","#CCCC33")) +
  labs(x = NULL, y = "Defined Daily Dose",
       colour = NULL) +
  theme_bw() +
  theme(text = element_text(size = 8))

plotly::ggplotly(temp_ggplot_cgpt2,
                 tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)


################
## Regional + STP volumes

## Combined Reg/STP volumes

## Volumes per region ##

df_scmd_all <- df_scmd_ddd %>%
  left_join(stp_to_region_map, by = "stp_code")

temp_ggplot_RegAll <- df_scmd_all %>% 
  select(year_month, ods_code, vtmnm, volume_ddd, comm_region_name) %>% 
  mutate(comm_region_name = fct_explicit_na(comm_region_name)) %>% 
  group_by(comm_region_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(comm_region_name) %>%
  mutate(prop_use = volume_ddd / sum(volume_ddd, na.rm = TRUE),
         pos = cumsum(volume_ddd) - volume_ddd/2,
         total = sum(volume_ddd, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(comm_region_name = fct_reorder(comm_region_name, total)) %>% 
  ggplot(aes(comm_region_name)) +
  geom_bar(aes(y = volume_ddd,
               fill = vtmnm,
               text = paste0("<b>Region:</b> ", comm_region_name, "<br>",
                             "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
                             "<b>Medication:</b> ", vtmnm, "<br>",
                             "<b>", vtmnm , " volume in ddd (%):</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), ")"
               )
  ), 
  stat='identity',
  position = position_dodge()
  ) +
  scale_fill_manual(values = c("#E85811","#660066","#CCCC33")) +
  labs(subtitle = NULL,
       x = NULL,
       y = NULL,
       fill = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 10)) +
  coord_flip()

# temp_ggplot
plotly::ggplotly(temp_ggplot_RegAll,
                 tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

### Volumes per STP


###### All 3 drugs, 2019 + 2022

df_scmd_all <- df_scmd_ddd %>%
  left_join(stp_to_region_map, by = "stp_code")

# Total time period

total_volume_all <- df_scmd_all %>%
  summarise(total_volume_all = sum(volume_ddd, na.rm = TRUE)) %>%
  pull(total_volume_all)

total_volume_all19 <- df_scmd_all %>%
  filter(year_month >= as.Date("2019-01-01") & year_month <= as.Date("2019-12-01")) %>%
  summarise(total_volume_all19 = sum(volume_ddd, na.rm = TRUE)) %>%
  pull(total_volume_all19)

total_volume_all22 <- df_scmd_all %>%
  filter(year_month >= as.Date("2022-01-01") & year_month <= as.Date("2022-12-01")) %>%
  summarise(total_volume_all22 = sum(volume_ddd, na.rm = TRUE)) %>%
  pull(total_volume_all22)

# Calculate proportion for each stp_name
df_scmd_ddd_perSTPall <- df_scmd_all %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>%
  mutate(stp_name = fct_explicit_na(stp_name)) %>%
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(
    prop_use = (volume_ddd / total_volume_all) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = total_volume_all) %>%
  ungroup() %>%
  group_by(stp_name) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))) %>%
  ungroup() %>%
  mutate(stp_name = fct_reorder(stp_name, combined_prop_use, .desc = TRUE)
  )

temp_ggplot_perSTPall <- df_scmd_ddd_perSTPall %>%
  group_by(stp_name) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(df_scmd_ddd_perSTPall, by = "stp_name") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(stp_name, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", stp_name, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066", "#CCCC33")) +
  labs(
    subtitle = NULL,
    x = NULL,
    y = "Proportion of Total Volume (%)",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 12),
        legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 7.5)) +
  coord_flip()

plotly::ggplotly(temp_ggplot_perSTPall, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)


####################

# 2019
df_scmd_ddd_perSTPall19 <- df_scmd_all %>%
  filter(year_month >= as.Date("2019-01-01") & year_month <= as.Date("2019-12-01")) %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>%
  mutate(stp_name = fct_explicit_na(stp_name)) %>%
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(
    prop_use = (volume_ddd / total_volume_all19) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = total_volume_all19
  ) %>%
  ungroup() %>%
  group_by(stp_name) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))
  ) %>%
  ungroup() %>%
  mutate(
    stp_name = fct_reorder(stp_name, combined_prop_use, .desc = TRUE)
  )

temp_ggplot_perSTPall19 <- df_scmd_ddd_perSTPall19 %>%
  group_by(stp_name) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(df_scmd_ddd_perSTPall19, by = "stp_name") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(stp_name, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", stp_name, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066", "#CCCC33")) +
  labs(
    subtitle = NULL,
    x = NULL,
    y = "Proportion of Total Volume (%)",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 12),
        legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 7.5)) +
  coord_flip()

plotly::ggplotly(temp_ggplot_perSTPall19, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

# 2022
df_scmd_ddd_perSTPall22 <- df_scmd_all %>%
  filter(year_month >= as.Date("2022-01-01") & year_month <= as.Date("2022-12-01")) %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>%
  mutate(stp_name = fct_explicit_na(stp_name)) %>%
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(
    prop_use = (volume_ddd / total_volume_all22) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = total_volume_all22
  ) %>%
  ungroup() %>%
  group_by(stp_name) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))
  ) %>%
  ungroup() %>%
  mutate(
    stp_name = fct_reorder(stp_name, combined_prop_use, .desc = TRUE)
  )

temp_ggplot_perSTPall22 <- df_scmd_ddd_perSTPall22 %>%
  group_by(stp_name) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(df_scmd_ddd_perSTPall22, by = "stp_name") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(stp_name, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", stp_name, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066", "#CCCC33")) +
  labs(
    subtitle = NULL,
    x = NULL,
    y = "Proportion of Total Volume (%)",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 12),
        legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 7.5)) +
  coord_flip()

plotly::ggplotly(temp_ggplot_perSTPall22, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

#######
##### Misc
#### Create dataframes for yearly STP data for mapping etc

## Oxybate 2019
df_OxySTP_2019 <- df_scmd_ddd_oxybate %>% 
  filter(year_month >= as.Date("2019-01-01") & year_month <= as.Date("2019-12-31")) %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>% 
  mutate(stp_name = fct_explicit_na(stp_name)) %>% 
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(prop_use = volume_ddd / sum(volume_ddd, na.rm = TRUE),
         pos = cumsum(volume_ddd) - volume_ddd/2,
         total = sum(volume_ddd, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(rank = dense_rank(-total),
         stp_name = fct_reorder(stp_name, -rank))

## To export to CSV:
## Edit as needed for each data frame generated below
## write.csv(df_OxySTP_2019, "here/data/oxybate2019.csv", row.names = FALSE)

## Oxybate 2022
df_OxySTP_2022 <- df_scmd_ddd_oxybate %>% 
  filter(year_month >= as.Date("2022-01-01") & year_month <= as.Date("2022-12-31")) %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>% 
  mutate(stp_name = fct_explicit_na(stp_name)) %>% 
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(prop_use = volume_ddd / sum(volume_ddd, na.rm = TRUE),
         pos = cumsum(volume_ddd) - volume_ddd/2,
         total = sum(volume_ddd, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(rank = dense_rank(-total),
         stp_name = fct_reorder(stp_name, -rank))

## Pitolisant 2019

df_pit19 <- df_scmd_ddd_pit %>% 
  filter(year_month >= as.Date("2019-01-01") & year_month <= as.Date("2019-12-31")) %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>% 
  mutate(stp_name = fct_explicit_na(stp_name)) %>% 
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(prop_use = volume_ddd / sum(volume_ddd, na.rm = TRUE),
         pos = cumsum(volume_ddd) - volume_ddd/2,
         total = sum(volume_ddd, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(rank = dense_rank(-total),
         stp_name = fct_reorder(stp_name, -rank))

## Pitolisant 2022

df_pit22 <- df_scmd_ddd_pit %>% 
  filter(year_month >= as.Date("2022-01-01") & year_month <= as.Date("2022-12-31")) %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>% 
  mutate(stp_name = fct_explicit_na(stp_name)) %>% 
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(prop_use = volume_ddd / sum(volume_ddd, na.rm = TRUE),
         pos = cumsum(volume_ddd) - volume_ddd/2,
         total = sum(volume_ddd, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(rank = dense_rank(-total),
         stp_name = fct_reorder(stp_name, -rank))

## Solriamfetol 2022

### 2022 year data

df_sol22 <- df_scmd_ddd_solriamfetol %>% 
  filter(year_month >= as.Date("2022-01-01") & year_month <= as.Date("2022-12-31")) %>%
  select(year_month, ods_code, vtmnm, volume_ddd, stp_name) %>% 
  mutate(stp_name = fct_explicit_na(stp_name)) %>% 
  group_by(stp_name, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(stp_name) %>%
  mutate(prop_use = volume_ddd / sum(volume_ddd, na.rm = TRUE),
         pos = cumsum(volume_ddd) - volume_ddd/2,
         total = sum(volume_ddd, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(rank = dense_rank(-total),
         stp_name = fct_reorder(stp_name, -rank))

#######

## count unique trusts in final dataset

n_distinct(df_scmd_all$ods_name)
## 54

n_distinct(df_scmd_all$stp_code)
## 29

### extract mapped STP + trusts

df_trusts <- df_scmd_all[, c("ods_name", "stp_name")]

## total - 4823821

sum(df_scmd_all$total_quantity)



