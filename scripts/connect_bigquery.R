# See https://bigrquery.r-dbi.org/ for information about authentication and authorization
# Establish database connection to Secondary Care Medicines Data (SCMD) 
conn_ebm_scmd <- DBI::dbConnect(bigrquery::bigquery(),
                                project = "ebmdatalab",
                                dataset = "scmd")

conn_ebm_dmd <- DBI::dbConnect(bigrquery::bigquery(),
                               project = "ebmdatalab",
                               dataset = "dmd")

conn_ebm_hscic <- DBI::dbConnect(bigrquery::bigquery(),
                                 project = "ebmdatalab",
                                 dataset = "hscic")

# List tables of each dataset
# DBI::dbListTables(conn_ebm_scmd)
# DBI::dbListTables(conn_ebm_dmd)
# DBI::dbListTables(conn_ebm_hscic)
