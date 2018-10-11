library(janitor)
library(rio)
library(tidyverse)

ferranao <- import("~/Documentos/ferranaData/ferranao.xlsx")
ferranao <- ferranao %>% 
  mutate(partido = case_when(partido == "MDB/PMDB" ~ "MDB",
          partido == "PMDB" ~ "MDB",
          partido == "PFL" ~ "PFL/DEM",
          partido == 'DEM' ~ "PFL/DEM",
          partido == "PSD" & ano < 1987 ~ "PSD1",
          partido == "PSD" & ano > 1988 & ano < 2010 ~ "PSD2",
          partido == "PSD" & ano > 2011 ~ "PSD3",
          partido == "PTB" & ano < 1980 ~ "PTB1",
          partido == "PTB" & ano > 1981 ~ "PTB2",
          partido == "PC do B" | partido == "PC do B" | partido == "PCdoB" | partido == "PC DO B" ~ "PCDOB",
          partido == "PTdoB" | partido == "PT do B" | partido == "PT DO B" ~ "PTDOB",
          # Alterando os partidos anteriores à ditadura
          partido == "PTN" & ano < 1958 ~ "PTN1", 
          partido == "PTN" & ano > 2010 ~ "PTN2",
          partido == "PST" & ano < 1958 ~ "PST1",
          partido == "PST" & ano > 1994 ~ "PST2",
          partido == "PRP" & ano < 1966 ~ "PRP1",
          partido == "PRP" & ano > 1990 ~ "PRP2",
          partido == "PR" & ano < 1966 ~ "PR1",
          partido == "PR" & ano > 2006 ~ "PR2",
          partido == "PPS" & ano < 1950 ~ "PPS1",
          partido == "PPS" & ano > 1994 ~ "PPS2",
          partido == "PL" & ano < 1966 ~ "PL1",
          partido == "PL" & ano > 1982 ~ "PL2",
          partido == "PDC" & ano < 1966 ~ "PDC1",
          partido == "PDC" & ano > 1982 ~ "PDC2",
         TRUE ~ as.character(partido)))
  

ferranao$partido <- as.factor(ferranao$partido)
ferranao$UF <- as.factor(ferranao$UF)

ferranao <- clean_names(ferranao)
# Variável World Cup. O ideal seria botar o número de fases avançadas pelo Brasil,
# mas nas fases de 50, 54, etc, não existiam oitavas, pelo número de participantes. 
# Logo, o efeito ficaria alterado (já que um 4 em 50 equivaleria a um 5 em 2002, por exemplo).

ferranao <- ferranao %>% 
  mutate(wc = case_when(ano == 1950 ~ 1,
                        ano == 1954 ~ 0,
                        ano == 1958 ~ 1,
                        ano == 1962 ~ 1,
                        ano == 1966 ~ 0,
                        ano == 1970 ~ 1,
                        ano == 1974 ~ 0,
                        ano == 1978 ~ 0,
                        ano == 1982 ~ 0,
                        ano == 1986 ~ 0,
                        ano == 1990 ~ 0,
                        ano == 1994 ~ 1,
                        ano == 1998 ~ 1,
                        ano == 2002 ~ 1,
                        ano == 2006 ~ 0,
                        ano == 2010 ~ 0,
                        ano == 2014 ~ 0,
                        ano == 1945 | ano == 1947 ~ 0, #Como em 45 e 47 não teve copa, efeito 0 pra não influenciar
                        TRUE ~ as.numeric(partido))) 



ferranao <- ferranao %>% 
  mutate(pib_aa = case_when(ano == 1994 ~ 4.7, ano == 1998 ~ 3.4,
                            ano == 2002 ~ 1.3, ano == 2006 ~ 3.2,
                            ano == 2010 ~ -0.6, ano == 2014 ~ 2.3,
                            ano == 1990 ~ -4.3, ano == 1986 ~ 7.5,
                            ano == 1982 ~ 0.8, ano == 1978 ~ 5,
                            ano == 1974 ~ 8.2, ano == 1970 ~ 10.4,
                            ano == 1966 ~ 6.7, ano == 1962 ~ 6.6,
                            ano == 1958 ~ 10.8, ano == 1954 ~ 7.8,
                            ano == 1950 ~ 6.8))

ferranao <- ferranao %>% 
  mutate(desemp = case_when(ano == 1982 ~ 6.7, ano == 1986 ~ 3.6,
                            ano == 1990 ~ 4.3, ano == 1994 ~ 5.1,
                            ano == 1998 ~ 7.6, ano == 2002 ~ 7.1,
                            ano == 2006 ~ 10, ano == 2010 ~ 6.2, 
                            ano == 2014 ~ 4.9))


ferranao <- ferranao %>%
	mutate(govop = case_when(ano == 1990 & partido == "PRN" |
				 partido == "PFL/DEM" ~ 1,
			 	 ano == 1994 & partido == "PFL/DEM" |
			         partido == "MDB" | partido == "PSDB" |
				 partido == "PPB" ~ 1,
			         ano == 1998 & partido == "PSDB" |
				 partido == "PFL/DEM" | partido == "MDB" |
				 partido == "PTB2" | partido == "PPB" ~ 1,
			         ano == 2002 & partido == "PSDB" |
				 partido == "MDB" | partido == "PPB" ~ 1,
			         ano == 2006 & partido == "PT" | 
			         partido == "PL" | partido == "PCDOB" | 
				   partido == "PSB" | partido == "PTB" | #Verificar PSB
				   partido == "MDB" | partido == "PT" ~ 1, 
				 ano == 2010 & partido == "PT" | partido == "PR" | #Verificar PR
				 partido == "PCDOB" | partido == "PSB" | #Verificar PSB
				   partido == "MDB" | patido == "PP" | 
				   partido == "PDT" | partido == "PRB" ~ 1, 
				 ano == 2014 & partido == "PT" | partido == "MDB" |
				   partido == "PR" | partido == "PCDOB" | partido == "PDT" |
				   partido == "PP" | partido == "PRB" ~ 1, 
				TRUE ~ as.numeric(govop)))
 