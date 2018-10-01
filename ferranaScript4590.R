library(rio)
library(tidyverse)

pathToFile <- "Área de trabalho/teste.csv"
dados <- import(pathToFile)

ferrana <- dados %>% 
  group_by(V5, V6, V3) %>% 
  summarise(NCAD = length(V6)) %>% 
  mutate(NCAD = NCAD)

export(ferrana, "ferrana4590.xlsx")
