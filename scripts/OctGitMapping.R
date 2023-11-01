install.packages('tidyverse')
install.packages('sf')

library('tidyverse')
library('sf')
library('scales')

## ONS provides ICS geographical data as a GEOsjon on Open Geography portal (https://geoportal.statistics.gov.uk/)
ICS <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_July_2022_EN_BFC_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')

#check geographical data works
plot(st_geometry(ICS))

## import correct Oxybate data mapped to ICB '22 names
# ICS data from ONS uses 2021 onward ICS names which don't match with SCMD ICS/STP names
# Manually corrected in file below

MapData <- read.csv(here("data/MappingOxybate.csv"))

Oxybate4year <- left_join(ICS, MapData, by = "ICB22NM")

## Final Edit for plot

OxybateMap <-
  ggplot(data = ICS) +
  ggtitle("(c)") +
  geom_sf(aes(fill = Oxybate4year$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "DDDs",
                      high = "red",  
                      low = "yellow",
                      na.value = "#F2F0F0",
                      limits = c(12, 115000),
                      breaks = c(2500, 15000, 80000),
                      labels = c(2500, 15000, 80000),
                      trans = scales::trans_new("log",
                                                transform = function(x) log(x + 1000),
                                                inverse = function(x) exp(x) - 1000)
  ) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1, 0.8)
  )

###### Pitolisant #######

PitolisantMapData <- read.csv(here("data/Mapping Pitolisant.csv"))

Pitolisant4year <- left_join(ICS, PitolisantMapData, by = "ICB22NM")

## plot 

PitolisantMap <- 
  ggplot(data = ICS) +
  ggtitle("(d)") +
  geom_sf(aes(fill = Pitolisant4year$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "DDDs",
                      high = "red",  
                      low = "yellow",
                      na.value = "#F2F0F0",
                      limits = c(12, 115000),
                      breaks = c(2500, 15000, 80000),
                      labels = c(2500, 15000, 80000),
                      trans = scales::trans_new("log",
                                                transform = function(x) log(x + 1000),
                                                inverse = function(x) exp(x) - 1000)
  ) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1, 0.8)
  )

####### Solriamfetol ######

Solriamfetol <- read.csv(here("here/Sol2022.csv"))
Solriamfetol <- left_join(ICS, Solriamfetol, by = "ICB22NM")

SolriamfetolMap <- ggplot(data = ICS) +
  ggtitle("(e)") +
  geom_sf(aes(fill = Solriamfetol$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "DDDs",
                      high = "red",  
                      low = "yellow",
                      na.value = "#F2F0F0",
                      limits = c(12, 5000),
                      breaks = c(250, 1000, 3000),
                      labels = c(250, 1000, 3000),
                      trans = scales::trans_new("log",
                                                transform = function(x) log(x + 100),
                                                inverse = function(x) exp(x) - 100)
  ) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1, 0.8)
  )

###### Yearly data sets #######
# Can be duplicated for each drug
### 2019
#import Oxybate 2019 data

# df_oxy2019 <- read.csv(here("data/Oxy2019.csv", header = TRUE))

# Left join with geographic data for plots

# Oxybate2019 <- left_join(ICS, df_oxy2019, by = "ICB22NM")

## plot data for year 2019

# ggplot(data = ICS) +
#   geom_sf(aes(fill = Oxybate2019$volume_ddd)) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient(name = "Sodium Oxybate(DDDs)", 
#                       high = "red",
#                       low = "yellow",
#                       na.value = "#f2f0f0",
#                       limits = c(12, 25000))+ 
#   theme_bw() + 
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     legend.position = c(0.1,0.8)
#   ) 
# 
# ### 2022
# 
# df_oxy2022 <- read.csv(here("data/Oxy2022.csv", header = TRUE))
# 
# # Left join with geo data
# 
# Oxybate2022 <- left_join(ICS, df_oxy2022, by = "ICB22NM")

## plot 2022 data

# ggplot(data = ICS) +
#   geom_sf(aes(fill = Oxybate2022$volume_ddd)) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient(name = "Sodium Oxybate(DDDs)", 
#                       high = "red",
#                       low = "yellow",
#                       na.value = "#f2f0f0",
#                       limits = c(12, 30000)) + 
#   theme_bw() + 
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     legend.position = c(0.1,0.8)
#   ) 

