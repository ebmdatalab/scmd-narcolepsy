install.packages('tidyverse')
install.packages('sf')

library('tidyverse')
library('sf')

## ONS provides ICS geographical data as a GEOsjon on Open Geography portal (https://geoportal.statistics.gov.uk/)
ICS <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_July_2022_EN_BFC_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')

#check geographical data works
plot(st_geometry(ICS))

## import correct Oxybate data mapped to ICB '22 names
# ICS data from ONS uses 2021 onward ICS names which don't match with SCMD ICS/STP names
# Manually corrected in file below

MapData <- read.csv("C:/Users/fredv/Dropbox/Work/NHNN/Prescribing Data for Wake Promoting agents/MyGraphs/Mapping/MappingOxybate.csv")

Oxybate4year <- left_join(ICS, MapData, by = "ICB22NM")

#test 1
ggplot(data = ICS) +
  geom_sf(aes(fill = OxybateMap$volume_ddd)) +
  scale_fill_viridis_c(option = 'plasma', trans = 'sqrt')

## test 2
# Limits to ensure NA values appear correctly 

ggplot(data = ICS) +
  geom_sf(aes(fill = OxybateMap$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "Sodium Oxybate(DDDs)", 
                      high = "red",
                      low = "yellow",
                      na.value = "#f2f0f0",
                      limits = factor(12, 99000)
                      ) + 
  theme_bw() + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1,0.8),
  )


##Final plot - Jan'19 - Dec '22
ggplot(data = ICS) +
  ggtitle("Regional Sodium Oxybate Prescribing 2019-2022") +
  geom_sf(aes(fill = Oxybate4year$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "Sodium Oxybate(DDDs)", 
                      high = "red",
                      low = "yellow",
                      na.value = "#f2f0f0",
                      limits = c(12, 99000)) + 
  theme_bw() + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1,0.8)
  )

## Yearly data sets

### 2019
#import Oxybate 2019 data

df_oxy2019 <- read.csv("C:/Users/fredv/Dropbox/Work/NHNN/Prescribing Data for Wake Promoting agents/GraphsFinal/Yearly Datasets/Oxy2019.csv", header = TRUE)

# Left join with geographic data for plots

Oxybate2019 <- left_join(ICS, df_oxy2019, by = "ICB22NM")

## plot data for year 2019

ggplot(data = ICS) +
  geom_sf(aes(fill = Oxybate2019$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "Sodium Oxybate(DDDs)", 
                      high = "red",
                      low = "yellow",
                      na.value = "#f2f0f0",
                      limits = c(12, 25000)) + 
  theme_bw() + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1,0.8)
  ) 

### 2022

df_oxy2022 <- read.csv("C:/Users/fredv/Dropbox/Work/NHNN/Prescribing Data for Wake Promoting agents/GraphsFinal/Yearly Datasets/Oxy2022.csv", header = TRUE)

# Left join with geo data

Oxybate2022 <- left_join(ICS, df_oxy2022, by = "ICB22NM")

## plot 2022 data

ggplot(data = ICS) +
  geom_sf(aes(fill = Oxybate2022$volume_ddd)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(name = "Sodium Oxybate(DDDs)", 
                      high = "red",
                      low = "yellow",
                      na.value = "#f2f0f0",
                      limits = c(12, 30000)) + 
  theme_bw() + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.1,0.8)
  ) 