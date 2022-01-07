#' Create SQL code for looking up ingredients
#'
#' @param ingredient_list Vector defining list of ingredients (must be 2 or more)
#' @param wildcards Logical, specifying whether to include 'wildcards' (%) as a 
#' prefix and suffix of each ingredient
#' @param return String, either class 'str' or 'sql' with the SQL query
#'
#' @return 
#' @export
#'
#' @examples
create_ingredient_lookup_sql <- function(ingredient_list, wildcards = TRUE, return = c("sql", "str")) {
  
  # Check return argument
  return <- match.arg(return)
  
  # Define beginning of SQL query 
  sql_query_base <- c("SELECT *
FROM (SELECT 'vmp' AS type, vmp.id, bnf_code, vmp.nm, ing.nm AS ingredient, ddd.ddd 
FROM dmd.vmp
INNER JOIN dmd.vpi AS vpi ON vmp.id = vpi.vmp 
INNER JOIN dmd.ing as ing ON ing.id = vpi.ing 
LEFT JOIN dmd.ddd on vmp.id = ddd.vpid
ORDER BY type, nm)
")
  
  # Write SQL for wildcards
  if (wildcards) {
    
    if (length(ingredient_list) < 2) {
      stop("ingredient_list must contain 2 or more ingredients")
    }
    
    wild_ingredient_list <- paste0("%", ingredient_list, "%")
    

    
    # Create empty list
    sql_query_or_wild <- list()
    
    # For loop, first element using WHERE, then OR
    for (ingredient in seq_along(wild_ingredient_list)) {
      
      if (ingredient == 1) {
        
        sql_query_where_wild <- paste0("WHERE ", "`ingredient` ", "LIKE ", 
                                       "'", wild_ingredient_list[1], "'\n")
        
      } else {
        
        sql_query_or_wild[ingredient - 1] <- paste0("   ", "OR ", "`ingredient` ", "LIKE ", 
                                                         "'", wild_ingredient_list[ingredient], "'\n")
        
      }
    }
    
    # Bring everything together using paste
    sql_query_or_wild_collapsed <- paste0(sql_query_or_wild, collapse = "")
    sql_query_where_or_wild <- paste0(sql_query_where_wild, sql_query_or_wild_collapsed)
    
    # Define return SQL statement
    return_sql_str <- paste0(sql_query_base, sql_query_where_or_wild)
    
  }
  
  # Write SQL for exact matches
  if (!wildcards) {
    
    sql_query_in <- paste0("WHERE ", "`ingredient` ", "IN ", 
                           paste0("(", paste0("'", ingredient_list, "'", collapse = ", "), ")"), "\n")
    
    return_sql_str <- paste0(sql_query_base, sql_query_in)
    
  }
 
  if (return == "str") {
    
    return_sql_str
    
  } else if (return == "sql") {
    # SQL class needed when working with dbplyr
    dplyr::sql(return_sql_str)
    
  }
}
