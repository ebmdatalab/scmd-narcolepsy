# ETR LOOKUP ----
# Get column names from pdf document "etr.pdf" available at:
# https://files.digital.nhs.uk/assets/ods/current/etr.zip
col_names <- c("Organisation Code",
               "Name",
               "National Grouping",
               "High Level Health Geography",
               "Address Line 1",
               "Address Line 2",
               "Address Line 3",
               "Address Line 4",
               "Address Line 5",
               "Postcode",
               "Open Date",
               "Close Date",
               "DROP_1",
               "DROP_2",
               "DROP_3",
               "DROP_4",
               "DROP_5",
               "Contact Telephone Number",
               "DROP_6",
               "DROP_7",
               "DROP_8",
               "Amended Record Indicator",
               "DROP_9",
               "GOR Code",
               "DROP_10",
               "DROP_11",
               "DROP_12")

# Tidy variable names
col_names <- janitor::make_clean_names(col_names)

# Get data from web
temp <- tempfile()
download.file("https://files.digital.nhs.uk/assets/ods/current/etr.zip", temp)

# Unzip and tidy data
data <- readr::read_csv(unz(temp, "etr.csv"), 
                        col_names = col_names) |>
        dplyr::select(-dplyr::starts_with("drop"), -contact_telephone_number) |>
        dplyr::mutate(name = stringr::str_to_title(name),
                      name = stringr::str_replace(name, "Nhs", "NHS"),
                      name = stringr::str_replace(name, "And", "and"),
                      open_date = as.Date(as.character(open_date), format = '%Y%m%d'),
                      close_date = as.Date(as.character(close_date), format = '%Y%m%d')) %>% 
  dplyr::rename(region_code = national_grouping, 
                stp_code = high_level_health_geography,
                ods_code = organisation_code,
                ods_name = name) %>% 
  # Some data cleaning as SCMD uses some ODS codes that are not up to date
  dplyr::mutate(stp_code = dplyr::case_when(
    ods_code == "RQ6" ~ "QYG",
    ods_code %in% c("RNL", "RE9", "RLN") ~ "QHM",
    ods_code %in% c("RM2", "RW3") ~ "QOP",
    ods_code == "RGQ" ~ "QJG",
    ods_code == "RJF" ~ "QJ2",
    ods_code == "RR1" ~ "QHL",
    TRUE ~ stp_code))

# Write tidy data
readr::write_csv(data, here::here("data/etr_tidy.csv"))


# gp-reg-pat-prac-map ----
# Is this the file I'm looking for?
# readr::read_csv(url("https://files.digital.nhs.uk/BA/206EF1/gp-reg-pat-prac-map.csv"))
