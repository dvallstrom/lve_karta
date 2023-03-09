
library(sf)
library(tidyverse)
library(PxWebApiData)
library(cowplot)

region_lista <- c("00","01","03","04","05","06","07","08","09","10","12","13","14","17","18","19","20","21","22","23","24","25")
snilist <- c("01-03","05-09","10-12","13-15","16-17","16","17","18","19-21","22","23","24-25","26-27","28","29-30","31","32","33","35","36-39","41","42","43","45","46","47","49-51","52","53","55","56","58","59","60","61","62","63","68","69","70","71+72","73","74","75","77","78","79","80","81","82","85","86","87","88","90","91","92","93","94","95","96")

nv0109 <- ApiData("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NV/NV0109/NV0109L/RegionalBasf07",
                  Region = region_lista,
                  SNI2007 = snilist,
                  Tid = c("2019","2020"),
                  ContentsCode = c("NV01091E", "NV01092E", "NV01095E", "NV01096E")
)

nv0109t<- nv0109[2]$dataset |> 
  pivot_wider(
    names_from = c(ContentsCode,Tid),
    values_from = value) |> 
    as_tibble()

nv0109klar <-nv0109t |> 
  select(-NAstatus) |> 
  drop_na() |> 
  group_by(Region) |> 
  summarise(
    Förädlingsvärde = sum(NV01095E_2020)/sum(NV01095E_2019),
    AntalVerksamheter = sum(NV01091E_2020)/sum(NV01091E_2019),
    Anställda = sum(NV01092E_2020)/sum(NV01092E_2019),
    Nettoomsättning = sum(NV01096E_2020)/sum(NV01096E_2019),
  ) |> 
  pivot_longer(!Region, names_to = "variabel", values_to = "Value")

mapswe <- st_read("E:/r-work/Kartor/Lan_Sweref99TM_region.shp")

mapsweden <- mapswe |> 
  left_join(nv0109klar, by =join_by(LnKod==Region), multiple="all")


ggplot(mapsweden)+
  geom_sf(aes(fill = Value))+
  labs(fill = "Utveckling")+
  facet_wrap(~variabel)+
  theme_map()

ggsave("karta.pdf")

