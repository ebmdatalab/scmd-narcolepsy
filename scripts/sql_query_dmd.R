# Define SQL query for Dictionary of Medicines and Devices (dm+d) information
# Rename some variables to match names across different queries (e.g., vmp_snomed_code)
sql_query_dmd_info <- dbplyr::sql("SELECT
                                   CAST(a.id AS STRING) AS vmp_snomed_code,
                                   a.nm AS vmp_product_name,
                                   a.vtm AS vtmid,
                                   j.nm AS vtmnm,
                                   b.form AS form_cd,
                                   c.descr AS form_descr,
                                   a.df_ind AS df_ind_cd,
                                   d.descr AS df_descr,
                                   a.udfs,
                                   e.descr AS udfs_descr,
                                   f.descr AS unit_dose_descr,
                                   g.strnt_nmrtr_val,
                                   h.descr AS strnt_nmrtr_uom,
                                   g.strnt_dnmtr_val,
                                   i.descr AS strnt_dnmtr_descr,
                                   a.bnf_code,
                                   k.presentation AS bnf_presentation
                                   
                                   FROM ebmdatalab.dmd.vmp AS a

                                   LEFT JOIN ebmdatalab.dmd.dform AS b
                                   ON a.id = b.vmp

                                   LEFT JOIN ebmdatalab.dmd.form AS c
                                   ON b.form = c.cd

                                   LEFT JOIN ebmdatalab.dmd.dfindicator AS d
                                   ON a.df_ind = d.cd

                                   LEFT JOIN ebmdatalab.dmd.unitofmeasure AS e
                                   ON a.udfs_uom = e.cd

                                   LEFT JOIN ebmdatalab.dmd.unitofmeasure AS f
                                   ON a.unit_dose_uom = f.cd

                                   LEFT JOIN ebmdatalab.dmd.vpi AS g
                                   ON a.id = g.vmp

                                   LEFT JOIN ebmdatalab.dmd.unitofmeasure AS h
                                   ON g.strnt_nmrtr_uom = h.cd

                                   LEFT JOIN ebmdatalab.dmd.unitofmeasure AS i
                                   ON g.strnt_dnmtr_uom = i.cd

                                   LEFT JOIN ebmdatalab.dmd.vtm AS j
                                   ON a.vtm = j.id

                                   LEFT JOIN ebmdatalab.hscic.bnf AS k
                                   ON a.bnf_code = k.presentation_code")
