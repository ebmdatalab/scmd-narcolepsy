# Define SQL query for ddd (Daily Defined Dose) https://isd.digital.nhs.uk/trud 

sql_query_ddd <- dbplyr::sql("SELECT
                              CAST(VPID AS STRING) AS vmp_snomed_code,
                              CAST(BNF AS STRING) AS bnf,
                              ATC AS atc,
                              DDD AS ddd,
                              CAST(DDD_UOMCD AS STRING) AS ddd_uomcd,
                              FROM ebmdatalab.dmd.ddd")
