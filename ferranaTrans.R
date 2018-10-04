library(janitor)
library(rio)
library(tidyverse)

ferranao <- import("~/Documentos/ferranaData/ferranao.xlsx")
ferranao %>% 
  mutate(Partido = case_when(Partido == "MDB/PMDB" ~ "MDB",
          Partido == "PMDB" ~ "MDB",
          Partido == "PFL" ~ "PFL/DEM",
          Partido == 'DEM' ~ "PFL/DEM",
          Partido == "PSD" & ANO < 1987 ~ "PSD1",
          Partido == "PSD" & ANO > 1988 & ANO < 2010 ~ "PSD2",
          Partido == "PSD" & ANO > 2011 ~ "PSD3",
          Partido == "PTB" & ANO < 1980 ~ "PTB1",
          Partido == "PTB" & ANO > 1981 ~ "PTB2",
         TRUE ~ as.character(Partido))) %>% 
  tabyl(Partido)
  

ferranao$Partido <- as.factor(ferranao$Partido)
