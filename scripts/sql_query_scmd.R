# Define SQL query for Secondary Care Medicines Data (SCMD) data 
# Select variables and calculate quantity of medicines grouped by
# year_month, ods_code, vmp_snomed_code, vmp_snomed_name
sql_query_scmd <- dbplyr::sql("SELECT
                               year_month,
                               ods_code,
                               vmp_snomed_code,
                               vmp_product_name,

                               SUM(total_quanity_in_vmp_unit) AS total_quantity
                               
                               FROM ebmdatalab.scmd.scmd
                                   
                               GROUP BY
                               year_month,
                               ods_code,
                               vmp_snomed_code,
                               vmp_product_name")
