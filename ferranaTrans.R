library(janitor)
library(rio)
library(tidyverse)

ferranao <- import("~/Documentos/ferranaData/ferranao.xlsx")
ferranao <- ferranao %>% 
  mutate(Partido = case_when(Partido == "MDB/PMDB" ~ "MDB",
          Partido == "PMDB" ~ "MDB",
          Partido == "PFL" ~ "PFL/DEM",
          Partido == 'DEM' ~ "PFL/DEM",
          Partido == "PSD" & ANO < 1987 ~ "PSD1",
          Partido == "PSD" & ANO > 1988 & ANO < 2010 ~ "PSD2",
          Partido == "PSD" & ANO > 2011 ~ "PSD3",
          Partido == "PTB" & ANO < 1980 ~ "PTB1",
          Partido == "PTB" & ANO > 1981 ~ "PTB2",
          Partido == "PC do B" | Partido == "PC do B" | Partido == "PCdoB" | Partido == "PC DO B" ~ "PCDOB",
          Partido == "PTdoB" | Partido == "PT do B" | Partido == "PT DO B" ~ "PTDOB",
         TRUE ~ as.character(Partido)))
  

ferranao$Partido <- as.factor(ferranao$Partido)
ferranao$UF <- as.factor(ferranao$UF)

# Variável World Cup. O ideal seria botar o número de fases avançadas pelo Brasil,
# mas nas fases de 50, 54, etc, não existiam oitavas, pelo número de participantes. 
# Logo, o efeito ficaria alterado (já que um 4 em 50 equivaleria a um 5 em 2002, por exemplo).

ferranao <- ferranao %>% 
  mutate(WC = case_when(ANO == 1950 ~ 1,
                        ANO == 1954 ~ 0,
                        ANO == 1958 ~ 1,
                        ANO == 1962 ~ 1,
                        ANO == 1966 ~ 0,
                        ANO == 1970 ~ 1,
                        ANO == 1974 ~ 0,
                        ANO == 1978 ~ 0,
                        ANO == 1982 ~ 0,
                        ANO == 1986 ~ 0,
                        ANO == 1990 ~ 0,
                        ANO == 1994 ~ 1,
                        ANO == 1998 ~ 1,
                        ANO == 2002 ~ 1,
                        ANO == 2006 ~ 0,
                        ANO == 2010 ~ 0,
                        ANO == 2014 ~ 0,
                        ANO == 1945 | ANO == 1947 ~ 0, #Como em 45 e 47 não teve copa, efeito 0 pra não influenciar
                        TRUE ~ as.numeric(Partido))) 
<<<<<<< HEAD

ferranao <- ferranao %>% 
  dplyr::rename(PARTIDO = Partido)

=======
>>>>>>> c3889229b082ce4e4a7d07cf6ab8bdff47a96c27
