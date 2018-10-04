library(janitor)
library(rio)
library(tidyverse)
library(plyr)

### Para dados de 1945 a 1990.
pathToFile <- "Documentos/ferranaData/dados4590.csv"
dados <- import(pathToFile)

ferrana <- dados %>% 
  group_by(V5, V6, V3) %>% 
  summarise(NCAD = length(V6)) %>% 
  mutate(NCAD = NCAD)

export(ferrana, "ferrana4590.xlsx")

ferrana <- import("ferrana.xlsx")

### Eleições de 1994
pathToFile <- "Documentos/ferranaData/dados1994.xls"
dados <- import(pathToFile)
dados$ANO <- 1994

dados <- dados %>% 
  filter(CARGO == "Deputado Federal" & SITUAÇÃO == "Eleito" |
           CARGO == "Deputado Federal" & SITUAÇÃO == "Média") %>% 
  select(UF, PARTIDO, ANO)

ferrana94 <- dados %>% 
  group_by(UF, ANO, PARTIDO) %>% 
  summarise(NCAD = length(PARTIDO)) %>% 
  mutate(NCAD = NCAD)

ferrana94 <- dplyr::rename(ferrana94, Partido = PARTIDO)

export(ferrana, "ferrana94.xlsx")

### Eleições de 1998
pathToFile <- "Documentos/ferranaData/dados1998.csv"
dados <- import(pathToFile)
dados$ANO <- 1998

dados <- dados %>% 
  select(UF, ANO, Partido)

ferrana98 <- dados %>% 
  group_by(UF, Partido, ANO) %>% 
  summarise(NCAD = length(Partido)) %>% 
  mutate(NCAD = NCAD)

export(ferrana, "ferrana1998.xlsx")

### Eleições de 2002
pathToFile <-  "Documentos/ferranaData/dados2002.csv"
dados <- import(pathToFile)
dados$ANO <- 2002

dados <- dados %>% 
  select(UF, ANO, Partido)

ferrana02 <- dados %>% 
  group_by(UF, Partido, ANO) %>% 
  summarise(NCAD = length(Partido)) %>% 
  mutate(NCAD = NCAD)

export(ferrana, "ferrana2002.xlsx")

#### 2006 do Pestana

ferrana06 <- import("Área de trabalho/ferranacad2006.xlsx")

### Eleições de 2010
pathToFile <- "Documentos/ferranaData/dados2010.csv"
dados <- import(pathToFile)
dados$ANO <- 2010

ferrana2010 <- dados %>% 
  select(UF, Partido, ANO, NCAD) %>% 
  filter(NCAD > 0)

export(ferrana, "ferrana2010.xlsx")

### Eleições de 2014
pathToFile <- "Documentos/ferranaData/dados2014.csv" 
dados <- import(pathToFile)
dados$ANO <- 2014

ferrana14 <- dados %>% 
  group_by(UF, Partido, ANO) %>% 
  summarise(NCAD = length(Partido)) %>% 
  mutate(NCAD = NCAD)

export(ferrana, "ferrana2014.xlsx")

ferranao <- rbind.fill(ferrana, ferrana02, ferrana06, ferrana14, ferrana2010,
                       ferrana94, ferrana98)

export(ferranao, "~/Documentos/ferranaData/ferranao.xlsx")

