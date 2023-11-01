# scmd-narcolepsy
Investigation of medicines used in Primary (English Prescribing Data) care & secondary (Secondary Care Medicines Data) care to treat narcolepsy

Description of R scripts:
  - 'scripts/OctGitHubFinal.R' - Data cleaning + analysis of secondary care data (SCMD). Largely adapated from initial code provided by DataLab team
  - 'scripts/OctGitMapping.R' - Incoporation of SCMD with geographic data to plot volumes of DDDs per ICB on map of ICBs + using separate data frames
  - 'scripts/OctGitPrimaryCare.R' - Data Cleaning + creating maps for primary care (English prescribing data) mapped to ICB regions
  - 'scripts/OctGitHubPrimaryReformatCode.R' - Data Cleaning/Reformatting primary care data to facilitate create equivalent plots to secondary care plots (National 3 monthly rolling average, volumes per geographic region, Top ICBs prescribing)

Description of Datasets:
Raw data (.csv):
  - "epdall" - Primary care ("English Prescribing Data") data for all prescriptions of Sodium Oxybate, Pitolisant, Solriamfetol issued between Jan 2019 - Dec 2022. Excluding prescriptions which could not be mapped to ICBs due to incorrect codes
  - "df_complete_scmd" - Secondary care ("Secondary Care Medicines Data") data for all prescriptions of Sodium Oxybate, Pitolisant and Solriamfetol. Extended from initial data provided by DataLab, to include Jan 2019 - Dec 2022
  - "PriOx" - Data on all primary care prescriptions of Sodium Oxybate (2019-2022), unedited from EPD
  - "PriPit" - Data on all primary care prescriptions of Pitolisant (2019-2022), unedited from EPD

Extra datasets made for specific plots (typically due to requiring a manual edit of the dataset):
  - "MappingOxybate" - Secondary care data for Sodium Oxbate 2019-2022, volumes of DDDs per ICB
  - "Mapping Pitolisant" - Secondary care data for Pitolisant 2019-2022, volumes of DDDs per ICB
  - "Sol2022" - Secondary Care data for Solriamfetol (2022), volumes of DDDs per ICB.
  - "PriOx4year" - Primary Care data for Sodium Oxyabte (2019-2022), volumes of DDD per ICB
  - "PriPit4year" - Primary Care data for Pitolisant (2019-2022), volumes of DDD per ICB

Descriptive Data - i.e. pharmacy info, trust/region/ICB codes, geographical information (.csv):
  - "updatecodelist" - refined codelist containing drug codes for Pitolisant, Sodium Oxybate, Solriamfetol
  - "Narcolepsymeds" - full list of stock data for drugs involved in narcolepsy
  - "etr_tidy" + "etr_csv" - Provides ODS + regional codes to map trusts to ICBs/geographic regions
  - "UpdateDMDInfo" - Pharmaceutical stock and strength information for Pitolisant, Sodium Oxybate, Solriamfetol
  - "gp-reg-pat-prac-map" - Additional codes used to map trusts to ICBs/Regions
  - "GpPracticeInfo" - July '23 list of NHS England GP practices, used to map primary care data to ICBs + Regions
  - "ICBnames" - List of NHS England ICBs


  
