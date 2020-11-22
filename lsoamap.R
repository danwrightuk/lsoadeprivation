library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)
library(rgdal)
library(dplyr)
library(plotly)
library(mapproj)
library(viridis)
library(broom)
library(tmap)
set.seed(420)
my_spdf <- readOGR(dsn="C:/Users/Danie/Desktop/LSOA2011", layer="Lower_Layer_Super_Output_Area__December_2011__EW_BSC_V2")
spdf_fort <- fortify(my_spdf, region="LSOA11CD")
setwd("C:/Users/Danie/Desktop/lsoadeprivation/")
IMD <- read.csv(file = 'C:/Users/Danie/Desktop/lsoadeprivation/IMD2019.csv', header=TRUE)
colnames(IMD)[which(names(IMD) == "ï..LSOA.code..2011.")] <- "LSOA11CD"
wiltsIMD <- dplyr::filter(IMD, Local.Authority.District.name..2019. %in% c("Wiltshire"))
mergeWLT <- merge(spdf_fort,wiltshire, by="id")
finalWLT <- mergeWLT[order(mergeWLT$order),]

wilts <- ggplot(data=finalWLT, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Index.of.Multiple.Deprivation..IMD..Decile),
               colour = alpha("black", 1/2), size = 0.3)+
  theme_void()+
  labs(
    title = "IMD Decile",
    subtitle = "",
    caption = "World"
  ) +
  scale_fill_viridis(na.value="white", direction=1, n=9, name="Pop Difference", guide = guide_colorbar( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm")) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )  +
  coord_map() #Adds Mercator Projection

fig4 <- ggplotly(wilts)
fig4

darloIMD <- dplyr::filter(IMD, Local.Authority.District.name..2019. %in% c("Darlington"))
NCLIMD <- dplyr::filter(IMD, Local.Authority.District.name..2019. %in% c("Newcastle upon Tyne"))

lsoas <- st_read(dsn ="C:/Users/Danie/Desktop/LSOA2011/Lower_Layer_Super_Output_Area__December_2011__EW_BSC_V2.shp")
lsoaswilts <- merge(lsoas, wiltsIMD, by="LSOA11CD")
lsoasdarlo <- merge(lsoas, darloIMD, by="LSOA11CD")
lsoasNCL <- merge(lsoas, NCLIMD, by="LSOA11CD")
tmap_mode("view")
tm_shape(lsoaswilts) +
  tm_polygons("Index.of.Multiple.Deprivation..IMD..Decile", palette = "viridis", alpha=0.4, n=5, title = "IMD Decile") +
  tm_basemap(leaflet::providers$CartoDB)

tmap_mode("view")
tm_shape(lsoasNCL) +
  tm_polygons("Index.of.Multiple.Deprivation..IMD..Decile", palette = "viridis", alpha=0.4, n=5, title = "IMD Decile") +
  tm_basemap(leaflet::providers$CartoDB)
