create_med_use_table <- function(data) {
  
  # Select variable names (just a quick check that the correct variables are in data)
  # This needs to be tested better
  data <- data %>% 
    select(stp_name, vtmnm, volume_ddd, prop_use, pos, total, rank)
  
  # Define data for table ----
  data_temp <- data %>% 
    mutate(prop_use = round(prop_use, 3)) %>% 
    replace_na(list(volume_ddd = 0, 
                    prop_use = 0)) %>% 
    pivot_wider(id_cols = c(stp_name, total, rank),
                names_from = vtmnm, 
                values_from = c(volume_ddd, prop_use), 
                values_fill = 0) %>% 
    select(stp_name, starts_with("prop"), total)
  
  # Create variable ranging from 0 to 1 needed for consistent formatting
  data_tab <- data_temp %>% 
    mutate(prop_scale = seq(from = 0, 
                            to = 1, 
                            length = nrow(data_temp)))
  
  # Define colour palette for conditional formatting ---
  green_pal <- function(x) rgb(colorRamp(c("#effbf7", "#31cf96"))(x), maxColorValue = 255)
  
  # Extract list of column names from data for defining conditional formatting
  # Here we extract all variable names starting with "prop_use"
  prop_use_var_list <- select(data_tab, starts_with("prop_use_")) %>% names()
  
  # Define generic function for conditional formatting
  # Note, this only works when the data that is used for the table is names "data_tab"
  col_style_fun <- function(value) {
    normalized <- (value - min(data_tab$prop_scale)) / (max(data_tab$prop_scale) - min(data_tab$prop_scale))
    color <- green_pal(normalized)
    list(background = color)
  }
  
  # Create list with default column formatting for one column
  # Add a placeholder for the name of each column, this will be replaced with the
  # correct names in a for loop later
  col_def_list_prop_1 <- list(reactable::colDef(name = "NAME_PLACEHOLDER",
                                                style = col_style_fun,
                                                minWidth = 100,
                                                format = colFormat(percent = TRUE, 
                                                                   digits = 1)))
  
  # Replicate conditional formatting colour definition for the numeric cols with % data
  # and name each list with one column name
  col_def_list_prop_all <- rep(col_def_list_prop_1, length(prop_use_var_list))
  names(col_def_list_prop_all) <- prop_use_var_list
  
  # Regex to extract the product name only from the column name
  prop_use_var_list_products <- stringr::str_extract(string = prop_use_var_list, 
                                                     pattern = "(?<=prop_use_)[:alpha:]+")
  
  # For loop to fill the placeholder name with the correct product name
  for (product in seq_along(col_def_list_prop_all)) {
    
    col_def_list_prop_all[[product]]$name <- prop_use_var_list_products[product]
    
  }
  
  # Now create column definitions for all other columns in the table
  # Create list for STP col
  col_def_list_stp <- list(colDef(name = "STP",
                                  minWidth = 300))
  names(col_def_list_stp) <- "stp_name"
  
  # Create list for total ddd col
  col_def_list_total_ddd <- list(colDef(name = "Total (ddd)", 
                                        minWidth = 65, 
                                        format = colFormat(digits = 0)))
  names(col_def_list_total_ddd) <- "total"
  
  # Create list for prop scale used for formatting only so dont show in table
  col_def_list_prop_scale <- list(colDef(show = FALSE))
  names(col_def_list_prop_scale) <- "prop_scale"
  
  # Combine column definitions
  col_def_list_all_vars <- c(col_def_list_stp, 
                             col_def_list_prop_all, 
                             col_def_list_total_ddd, 
                             col_def_list_prop_scale)
  
  # Create table
  reactable(data_tab, 
            filterable = TRUE,
            columns = col_def_list_all_vars,
            style = list(fontSize = "12px"),
            highlight = TRUE)
  
}