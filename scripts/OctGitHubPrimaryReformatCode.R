### Reformat Primary Care data to use with code already written for secondary care
## use in conjunction withe script "PrimaryCare Code.R" + "Final Code.R"

## Sodium Oxybate

## Update ICB names with most uptodate GP practice data

Gp_data <- read_csv(here("data/gp-reg-pat-prac-map.csv"))

PriOX <- left_join(PriOX,
                   Gp_data,
                   by = "PRACTICE_CODE")

# Remove uneccessary colums + rename columns

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

# PriOX <- PriOX %>%
#   rename("ICB_CODE" = "ICB_CODE.y",
#          "ICB_NAME" = "ICB_NAME.y")

## Filter out  Practices which don't have STP/ICB code due to incorrect practice codes in EPD 
# 4010 months -> 3862 (148 months prescribing data lost - 3.69%)
PriOX <- PriOX %>%
  filter(!is.na(ICB_NAME))

## Amendments to df to bring in line with code already written for SCMD:
# add dmd_info
# use same labels for dfs as per scmd now for simplicity

## read PriOX + PriPit.csv here

PriOX <- PriOX %>%
  rename("bnf_code" = "BNF_CODE",
         "vtmnm" = "CHEMICAL_SUBSTANCE_BNF_DESCR",
         "year_month" = "YEAR_MONTH",
         "total_quantity" = "TOTAL_QUANTITY")

PriOX_mg <- PriOX %>% 
  left_join(df_dmd_info, by = c("vtmnm")) %>% ## NB Only works for oxybate as only one formulation
  left_join(ddds, by = c("vmp_product_name" = "nm"))

## Convert total_quanity into DDDs
PriOX_ddd <- PriOX_mg %>% 
  mutate(volume_singles = total_quantity / udfs) %>% 
  mutate(volume_mg_strength = volume_singles * if_else(is.na(strnt_dnmtr_val),
                                                       true = strnt_nmrtr_val, 
                                                       false = strnt_nmrtr_val * (udfs / strnt_dnmtr_val)),
         volume_ddd = volume_mg_strength / ddd) %>% 
  arrange(year_month, ICB_CODE, vmp_snomed_code) %>% 
  mutate(volume_ddd = case_when(vmp_snomed_code == "395522003" ~ ((total_quantity * strnt_nmrtr_val) / ddd) / 1000,
                                TRUE ~ volume_ddd))

## Tidy up previous mistake
PriOX_ddd <- PriOX_ddd %>%
  rename("stp_code" = "STP_CODE.y")

PriOX_ddd <- PriOX_ddd %>%
  left_join(stp_to_region_map, by = "stp_code")

## reformat date in Primary care data
PriOX_ddd$year_month <- sub("(.{4})(.)", "\\1-\\2", PriOX_ddd$year_month)
PriOX_ddd$year_month <- paste0(PriOX_ddd$year_month, "-01")


## Create Figure 1 (Oxybate only for now) = 3 monthly rolling average
PriOX_RollVols <- PriOX_ddd %>%
  group_by(year_month, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd))

PriOx_3month <- PriOX_RollVols %>%
  group_by(vtmnm) %>%
  arrange(year_month) %>%
  mutate(Rolling_3month = rollapply(volume_ddd, width = 3, FUN = mean, fill = NA, align = "right"))

############ PITOLISANT #######################
### Reformat Pitolisant Data as above

PriPit <- left_join(PriPit,
                    Gp_data,
                    by = "PRACTICE_CODE")

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

##  Filter out  Practices which don't have STP/ICB code due to incorrect practice codes in EPD
# 975 -> 951 = (24 months lost = 2.46%)
PriPit <- PriPit %>%
  filter(!is.na(ICB_NAME))

PriPit <- PriPit %>%
  mutate(CHEMICAL_SUBSTANCE_BNF_DESCR = "Pitolisant")

##

PriPit <- PriPit %>%
  rename("bnf_code" = "BNF_CODE",
         "vtmnm" = "CHEMICAL_SUBSTANCE_BNF_DESCR",
         "year_month" = "YEAR_MONTH",
         "total_quantity" = "TOTAL_QUANTITY",
         "stp_code" = "STP_CODE.y",
         "vmp_product_name" = "BNF_DESCRIPTION")

## Substitute Wakix for Pitolisant
PriPit$vmp_product_name <- gsub("Wakix 18mg tablets", "Pitolisant 18mg tablets", PriPit$vmp_product_name)

PriPit_mg <- PriPit %>% 
  left_join(df_dmd_info, by = c("vmp_product_name")) %>%
  left_join(ddds, by = c("vmp_product_name" = "nm")) 

## Tidy up the mess
PriPit_mg <- PriPit_mg %>%
  select(-bnf_code.y, -vtmnm.y, -STP_CODE.x) %>%
  rename("bnf_code" = "bnf_code.x",
         "vtmnm" = "vtmnm.x")

## Convert total_quanity into DDDs
PriPit_ddd <- PriPit_mg %>% 
  mutate(volume_singles = total_quantity / udfs) %>% 
  mutate(volume_mg_strength = volume_singles * if_else(is.na(strnt_dnmtr_val),
                                                       true = strnt_nmrtr_val, 
                                                       false = strnt_nmrtr_val * (udfs / strnt_dnmtr_val)),
         volume_ddd = volume_mg_strength / ddd) %>% 
  arrange(year_month, ICB_CODE, vmp_snomed_code)

PriPit_ddd <- PriPit_ddd %>%
  left_join(stp_to_region_map, by = "stp_code")

## reformat date in Primary care data
PriPit_ddd$year_month <- sub("(.{4})(.)", "\\1-\\2", PriPit_ddd$year_month)
PriPit_ddd$year_month <- paste0(PriPit_ddd$year_month, "-01")


## just to check has worked
PriPit_RollVols <- PriPit_ddd %>%
  group_by(year_month, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd))

PriPit_3month <- PriPit_RollVols %>%
  group_by(vtmnm) %>%
  arrange(year_month) %>%
  mutate(Rolling_3month = rollapply(volume_ddd, width = 3, FUN = mean, fill = NA, align = "right"))

## Combine both datasets to make combined figure

PriPitOx3 <- rbind(PriOx_3month, PriPit_3month)

temp_PriComb <- ggplot() +
  geom_line(data = PriPitOx3, 
            aes(x = as.Date(year_month), 
                y = Rolling_3month, 
                colour = vtmnm),
            linewidth = 0.5,
            alpha = 0.8) +
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%b '%y", 
               limits = as.Date(c("2019-03-01", "2022-12-01")), 
               expand = c(0, 0)) +
  scale_colour_manual(values = c("purple", "orange")) +
  labs(x = NULL, y = "Defined Daily Dose",
       colour = NULL) +
  theme_bw() +
  theme(text = element_text(size = 8))

plotly::ggplotly(temp_PriComb,
                 tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

##########################################

### Create combined dataframe for remaining plots - essentially further tidying up of messy dataframes made
## theres a significantly faster way of doing this i just realised afterwards - new df with only relevant columns pulled.
# PriOXMatched <- PriOX_ddd %>%
#    select(-STP_CODE.x, -BNF_DESCRIPTION, -bnf_code.y)
# 
# PriOXMatched <- PriOXMatched %>%
#  rename("bnf_code" = "bnf_code.x")
# 
# PriOXMatched <- PriOXMatched %>%
#    select(order(colnames(PriOXMatched)))
# 
# PriPitMatched <- PriPit_ddd %>%
#    select(order(colnames(PriPit_ddd)))
# 
# epd_all <- rbind(PriPitMatched, PriOXMatched)

# write.csv(epd_all, "C:/../epdall.csv", row.names = FALSE)

epd_all <- read.csv(here("data/epdall.csv"))

######

# Volumes per region

epd_regions <- epd_all %>% 
  select(year_month, stp_code, vtmnm, volume_ddd, comm_region_name) %>% 
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
  scale_fill_manual(values = c("#E85811","#660066")) +
  labs(subtitle = NULL,
       x = NULL,
       y = "Volume of DDDs",
       fill = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 10)) +
  coord_flip()

plotly::ggplotly(epd_regions,
                 tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

### ICB Yearly volume plots primary care

# Total volumes prescribed in primary care each year

EPDTotalICB19 <- epd_all %>%
  filter(year_month >= as.Date("2019-01-01") & year_month <= as.Date("2019-12-01")) %>%
  summarise(EPDTotalICB19 = sum(volume_ddd, na.rm = TRUE)) %>%
  pull(EPDTotalICB19)

EPDTotalICB22 <- epd_all %>%
  filter(year_month >= as.Date("2022-01-01") & year_month <= as.Date("2022-12-01")) %>%
  summarise(EPDTotalICB22 = sum(volume_ddd, na.rm = TRUE)) %>%
  pull(EPDTotalICB22)

epdtotal_volume <- epd_all %>%
  summarise(epdtotal_volume = sum(volume_ddd, na.rm = TRUE)) %>%
  pull(epdtotal_volume)


## Calculate proportions
## 4 year period

epdICBsall <- epd_all %>%
  select(year_month, ICB_CODE, vtmnm, volume_ddd, ICB_NAME) %>%
  mutate(ICB_NAME = fct_explicit_na(ICB_NAME)) %>%
  group_by(ICB_NAME, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(ICB_NAME) %>%
  mutate(
    prop_use = (volume_ddd / epdtotal_volume) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = epdtotal_volume) %>%
  ungroup() %>%
  group_by(ICB_NAME) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))) %>%
  ungroup() %>%
  mutate(ICB_NAME = fct_reorder(ICB_NAME, combined_prop_use, .desc = TRUE)
  )

## 4 Year prescribing per ISB - not needed currently
#
# temp_epdICBsall <- epdICBsall %>%
#   group_by(ICB_NAME) %>%
#   summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
#   top_n(15, total_prop) %>%
#   inner_join(epdICBsall, by = "stp_name") %>%
#   arrange(desc(combined_prop_use)) %>%
#   ggplot(aes(fct_reorder(ICB_NAME, combined_prop_use))) +
#   geom_bar(
#     aes(
#       y = prop_use,
#       fill = vtmnm,
#       text = paste0(
#         "<b>STP:</b> ", ICB_NAME, "<br>",
#         "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
#         "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
#       )
#     ),
#     stat = 'identity'
#   ) +
#   scale_fill_manual(values = c("#E85811", "#660066", "#CCCC33")) +
#   labs(
#     subtitle = NULL,
#     x = NULL,
#     y = "Proportion of Total Volume (%)",
#     fill = NULL
#   ) +
#   scale_y_continuous(labels = scales::comma) +
#   theme(text = element_text(size = 12),
#         legend.position = "bottom") +
#   theme(axis.text.x = element_text(size = 7.5)) +
#   coord_flip()

plotly::ggplotly(temp_epdICBsall, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

## Primary care Sodium Oxybate + Pitolisant ICB ranked prescribing 2019
epdICBs19 <- epd_all %>%
  filter(year_month >= as.Date("2019-01-01") & year_month <= as.Date("2019-12-01")) %>%
  select(year_month, ICB_CODE, vtmnm, volume_ddd, ICB_NAME) %>%
  mutate(ICB_NAME = fct_explicit_na(ICB_NAME)) %>%
  group_by(ICB_NAME, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(ICB_NAME) %>%
  mutate(
    prop_use = (volume_ddd / EPDTotalICB19) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = EPDTotalICB19) %>%
  ungroup() %>%
  group_by(ICB_NAME) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))) %>%
  ungroup() %>%
  mutate(ICB_NAME = fct_reorder(ICB_NAME, combined_prop_use, .desc = TRUE)
  )

# plot data
temp_ICBs19 <- epdICBs19 %>%
  group_by(ICB_NAME) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(epdICBs19, by = "ICB_NAME") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(ICB_NAME, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", ICB_NAME, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066")) +
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

plotly::ggplotly(temp_ICBs19, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

## 2022
epdICBs22 <- epd_all %>%
  filter(year_month >= as.Date("2022-01-01") & year_month <= as.Date("2022-12-01")) %>%
  select(year_month, ICB_CODE, vtmnm, volume_ddd, ICB_NAME) %>%
  mutate(ICB_NAME = fct_explicit_na(ICB_NAME)) %>%
  group_by(ICB_NAME, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(ICB_NAME) %>%
  mutate(
    prop_use = (volume_ddd / EPDTotalICB22) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = EPDTotalICB22) %>%
  ungroup() %>%
  group_by(ICB_NAME) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))) %>%
  ungroup() %>%
  mutate(ICB_NAME = fct_reorder(ICB_NAME, combined_prop_use, .desc = TRUE)
  )

# plot data
temp_ICBs22 <- epdICBs22 %>%
  group_by(ICB_NAME) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(epdICBs22, by = "ICB_NAME") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(ICB_NAME, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", ICB_NAME, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066")) +
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

plotly::ggplotly(temp_ICBs22, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

################# Combined ICB Primary + SCMD data ###################

## change long names for simplicity

scmdICBsall <- df_scmd_ddd_perSTPall
scmdICBs19 <- df_scmd_ddd_perSTPall19
scmdICBs22 <- df_scmd_ddd_perSTPall22

## Edit SCMD data such that it utilises ICB names rather than STP

scmdICBsall <- scmdICBsall %>%
  left_join(select(stp_to_region_map, stp_name, stp_code), by = "stp_name")

# Import ICB names from primary care dataset
ICBnames <- read.csv(here("data/ICBnames.csv"))
# Add ICB Names
scmdICBsall <- scmdICBsall %>%
  left_join(select(ICBnames, stp_code, ICB_NAME), by = "stp_code")

#remove stp_column to match epd_all

scmdICBsall <- scmdICBsall %>%
  select(-stp_name, -stp_code)

# combined into one dataframe

epdscmdICB <- rbind(epdICBsall, scmdICBsall)

# Remove old proportions/rankings

epdscmdICB <- epdscmdICB %>%
  select(-prop_use, -pos, -total, -combined_prop_use, -rank)

epdICB4 <- epdscmdICB %>%
  select(ICB_NAME, vtmnm, volume_ddd) %>%
  mutate(ICB_NAME = fct_explicit_na(ICB_NAME)) %>%
  group_by(ICB_NAME, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(ICB_NAME) %>%
  mutate(
    prop_use = (volume_ddd / (epdtotal_volume+total_volume_all)) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = (epdtotal_volume+total_volume_all)) %>%
  ungroup() %>%
  group_by(ICB_NAME) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))) %>%
  ungroup() %>%
  mutate(ICB_NAME = fct_reorder(ICB_NAME, combined_prop_use, .desc = TRUE)
  )

## Combined 4 year ICB prescribing.

temp_epdICB4<- epdICB4 %>%
  group_by(ICB_NAME) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(epdICB4, by = "ICB_NAME") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(ICB_NAME, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", ICB_NAME, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066","#CCCC44" )) +
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

plotly::ggplotly(temp_epdICB4, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

#### Create plots for ICB Primary and Secondary ICB data 2019 + 2022
# Create scmd 19 + 22 dataframes with ICBs

scmdICBs19 <- scmdICBs19 %>%
  left_join(select(stp_to_region_map, stp_name, stp_code), by = "stp_name")

scmdICBs19 <- scmdICBs19 %>%
  left_join(select(ICBnames, stp_code, ICB_NAME), by = "stp_code")

scmdICBs19 <- scmdICBs19 %>%
  select(-stp_name, -stp_code)

## 22
scmdICBs22 <- scmdICBs22 %>%
  left_join(select(stp_to_region_map, stp_name, stp_code), by = "stp_name")

scmdICBs22 <- scmdICBs22 %>%
  left_join(select(ICBnames, stp_code, ICB_NAME), by = "stp_code")

scmdICBs22 <- scmdICBs22 %>%
  select(-stp_name, -stp_code)

## Combined yearly datasets
epdscmdICB19 <- rbind(epdICBs19, scmdICBs19)
epdscmdICB22 <- rbind(epdICBs22, scmdICBs22)

# remove old rankings
epdscmdICB19 <- epdscmdICB19 %>%
  select(-prop_use, -pos, -total, -combined_prop_use, -rank)

epdscmdICB22 <- epdscmdICB22 %>%
  select(-prop_use, -pos, -total, -combined_prop_use, -rank)

## Plot for 2019 Primary + Secondary Care issuance

epdscmdICB19 <- epdscmdICB19 %>%
  select(ICB_NAME, vtmnm, volume_ddd) %>%
  mutate(ICB_NAME = fct_explicit_na(ICB_NAME)) %>%
  group_by(ICB_NAME, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(ICB_NAME) %>%
  mutate(
    prop_use = (volume_ddd / (EPDTotalICB19+total_volume_all19)) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = (EPDTotalICB19+total_volume_all19)) %>%
  ungroup() %>%
  group_by(ICB_NAME) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))) %>%
  ungroup() %>%
  mutate(ICB_NAME = fct_reorder(ICB_NAME, combined_prop_use, .desc = TRUE)
  )

temp_epdscmdICB19 <- epdscmdICB19 %>%
  group_by(ICB_NAME) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(epdscmdICB19, by = "ICB_NAME") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(ICB_NAME, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", ICB_NAME, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066")) +
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

plotly::ggplotly(temp_epdscmdICB19, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

## plot for 2022 Primary + Secondary care issuance

epdscmdICB22 <- epdscmdICB22 %>%
  select(ICB_NAME, vtmnm, volume_ddd) %>%
  mutate(ICB_NAME = fct_explicit_na(ICB_NAME)) %>%
  group_by(ICB_NAME, vtmnm) %>%
  summarise(volume_ddd = sum(volume_ddd, na.rm = TRUE)) %>%
  group_by(ICB_NAME) %>%
  mutate(
    prop_use = (volume_ddd / (EPDTotalICB22+total_volume_all22)) * 100,
    pos = cumsum(volume_ddd) - volume_ddd / 2,
    total = (EPDTotalICB22+total_volume_all22)) %>%
  ungroup() %>%
  group_by(ICB_NAME) %>%
  mutate(
    combined_prop_use = sum(prop_use),
    rank = dense_rank(desc(combined_prop_use))) %>%
  ungroup() %>%
  mutate(ICB_NAME = fct_reorder(ICB_NAME, combined_prop_use, .desc = TRUE)
  )

temp_epdscmdICB22 <- epdscmdICB22 %>%
  group_by(ICB_NAME) %>%
  summarise(total_prop = sum(prop_use, na.rm = TRUE)) %>%
  top_n(15, total_prop) %>%
  inner_join(epdscmdICB22, by = "ICB_NAME") %>%
  arrange(desc(combined_prop_use)) %>%
  ggplot(aes(fct_reorder(ICB_NAME, combined_prop_use))) +
  geom_bar(
    aes(
      y = prop_use,
      fill = vtmnm,
      text = paste0(
        "<b>STP:</b> ", ICB_NAME, "<br>",
        "<b>Total volume in ddd:</b> ", round(total, 0), "<br>",
        "<b>", vtmnm , " volume in ddd:</b> ", round(volume_ddd, 0), " (", scales::percent(prop_use, accuracy = 0.1), "%)"
      )
    ),
    stat = 'identity'
  ) +
  scale_fill_manual(values = c("#E85811", "#660066", "#CCCC44")) +
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

plotly::ggplotly(temp_epdscmdICB22, tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)


########### Combined 4 year regional comparison
## Re-add region info to epdscmdICB dataframe (i.e. combined volumes df)

epdscmdRegion <- epdscmdICB %>%
  left_join(select(ICBnames, ICB_NAME, stp_code), by = "ICB_NAME")

epdscmdRegion <- epdscmdRegion %>%
  left_join(select(stp_to_region_map, stp_code, comm_region_name), by = "stp_code")

## plot 4 year regional volume comparison

epdscmd_regions4 <- epdscmdRegion %>% 
  select(stp_code, vtmnm, volume_ddd, comm_region_name) %>% 
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
  scale_fill_manual(values = c("#E85811","#660066","#CCCC44")) +
  labs(subtitle = NULL,
       x = NULL,
       y = "Volume of DDDs",
       fill = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 10)) +
  coord_flip()

plotly::ggplotly(epdscmd_regions4,
                 tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)

##### Combined figure for Primary and Secondary 3month plot

Primary3month <- PriPitOx3 %>%
  mutate(group = 1)

Primary3month <- Primary3month %>%
  mutate(vtmnm = ifelse(vtmnm == "Pitolisant hydrochloride", "Pitolisant", vtmnm))

Primary3month$year_month <- as.Date(Primary3month$year_month)

Secondary3month <- Rolling_3month %>%
  mutate(group = 2)

PrimarySecondary3month <- rbind(Primary3month, Secondary3month)

#####

Prisec3month <- ggplot(PrimarySecondary3month, aes(x = as.Date(year_month))) +
  geom_line(aes(y = Rolling_3month, 
                colour = interaction(group, vtmnm, sep = "ary ")),
            linewidth = 0.5,
            alpha = 0.8) +
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%b '%y", 
               limits = as.Date(c("2019-03-01", "2022-12-01")), 
               expand = c(0, 0)) +
  scale_colour_manual(values = c("red","orange","green", "purple", "#CCCC44")) +
  labs(x = NULL, y = "Defined Daily Dose",
       colour = NULL) +
  theme_bw() +
  theme(text = element_text(size = 8))

plotly::ggplotly(Prisec3month,
                 tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE)


###### Testing 

# Create separate dataframe for ICB non-matches
OxybateNoICB <- PriOX %>%
  filter(is.na(ICB_NAME.y)) %>%
  select(-ICB_NAME.x, -ICB_CODE.x)

OxybateNoICB <- OxybateNoICB %>%
  left_join(ICBnames, by = c("STP_CODE.x" = "stp_code")) %>%
  mutate(ICB_NAME = coalesce(ICB_NAME, ICB_NAME.y),
         ICB_CODE = coalesce(ICB_CODE, ICB_CODE.y)) %>%
  select(-STP_CODE.y, -ICB_NAME.y, -ICB_CODE.y)

# filter out remaining practices not matched to ICB
# 3916/4010 months included -> 94 months exclude ~2.34%
PriOX <- PriOX %>%
  filter(!is.na(ICB_NAME))

### Misc Values

PracticesPrescribing <- n_distinct(epd_all$PRACTICE_CODE)
UniqueICBs <- n_distinct(epd_all$ICB_NAME)