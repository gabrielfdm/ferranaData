library(tidyverse)
library(rio)
library(readbulk)
library(janitor)

# Abrindo em bulk todos os arquivos 

ferrana90 <- read_bulk(subdirectories = "1990", extension = ".txt", fun = rio::import)
ferrana94 <- read_bulk(subdirectories = "1994",extension = ".txt", fun = rio::import)
ferrana98 <- read_bulk(subdirectories = "1998",extension = ".txt", fun = rio::import)
ferrana02 <- read_bulk(subdirectories = "2002",extension = ".txt", fun = rio::import)
ferrana06 <- read_bulk(subdirectories = "2006",extension = ".txt", fun = rio::import)
ferrana10 <- read_bulk(subdirectories = "2010",extension = ".txt", fun = rio::import)
ferrana14 <- read_bulk(subdirectories = "2014",extension = ".txt", fun = rio::import)
ferrana18 <- read_bulk(subdirectories = "2018",extension = ".csv", fun = rio::import)

# Selecionando as variáveis utilizadas e nomeando-as, de 1990 até 2014 

ferrana90 <- ferrana90 %>%
  select(-c("V1", "V2", "V4", "V5", "V7", "V8", "V9", "V10", "V12", "V14", "V23", 
            "V15", "V16", "V17", "V18", "V24", "Subdirectory", "File")) %>% 
  filter(V13 == "DEPUTADO FEDERAL") %>% 
  filter(V17 != "INAPTO") %>% 
  select(-c("V17")) %>% 
  colnames(ferrana90) <- c("ano", "uf", "candidato", "cargo", "situacao", "npartido", "partido", "nomepartido", "colig", "nvoto")
  
ferrana94 <- ferrana94 %>% 
  select(-c("V1", "V2", "V4", "V5", "V7", "V8", "V9", "V10", "V11", "V12", "V15",
            "V18", "V20", "Subdirectory", "File", "V13", "V17", "V19", "V21", "V28", "V26")) %>%
  filter(V16 == "DEPUTADO FEDERAL")

names(ferrana94) <- names(ferrana90)

ferrana98 <- ferrana98 %>% 
  select(-c("V1", "V2", "V4", "V5", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V15", "V17", "V19",
            "V18", "V21", "V26", "V28", "Subdirectory", "File")) %>%
  filter(V16 == "DEPUTADO FEDERAL") %>% 
  filter(V20 != "INAPTO") %>% 
  select(-c("V20"))

names(ferrana98) <- names(ferrana94)

ferrana02 <- ferrana02 %>% 
  select(-c("V1", "V2", "V4", "V5", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V15", "V17", "V19",
            "V18", "V21", "V26", "V27", "Subdirectory", "File")) %>%
  filter(V16 == "DEPUTADO FEDERAL") %>% 
  filter(V20 != "INAPTO") %>% 
  select(-c("V20"))

names(ferrana02) <- names(ferrana98)

ferrana06 <- ferrana06 %>% 
  select(-c("V1", "V2", "V4", "V5", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V15", "V17", "V19",
           "V18", "V21", "V26", "V27", "Subdirectory", "File")) %>%
  filter(V16 == "DEPUTADO FEDERAL") %>% 
  filter(V20 != "INAPTO") %>% 
  select(-c("V20"))

names(ferrana06) <- names(ferrana02) 

ferrana10 <- ferrana10 %>% 
  select(-c("V1", "V2", "V4", "V5", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V15", "V17", "V19",
            "V18", "V21", "V26", "V27", "Subdirectory", "File")) %>%
  filter(V16 == "DEPUTADO FEDERAL") %>% 
  filter(V20 != "INAPTO") %>% 
  select(-c("V20"))

names(ferrana10) <- names(ferrana06)

ferrana14 <- ferrana14 %>% 
  select(-c("V1", "V2", "V4", "V5", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V15", "V17", "V19",
            "V18", "V21", "V26", "V27", "V30", "Subdirectory", "File")) %>%
  filter(V16 == "DEPUTADO FEDERAL") %>% 
  filter(V20 != "INAPTO") %>% 
  select(-c("V20"))

names(ferrana14) <- names(ferrana10)

# Corrigindo e selecionando as variáveis de 2018

ferrana18 <- ferrana18 %>% 
  select(-c("DT_GERACAO", "HH_GERACAO", "CD_TIPO_ELEICAO", "NM_TIPO_ELEICAO", "NR_TURNO", 
  "CD_ELEICAO", "DS_ELEICAO", "DT_ELEICAO", "SG_UE", "NM_UE", "CD_MUNICIPIO", 
  "NM_MUNICIPIO", "NR_ZONA", "CD_CARGO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO", 
  "NM_SOCIAL_CANDIDATO", "CD_SITUACAO_CANDIDATURA", "TP_AGREMIACAO", 
  "SQ_COLIGACAO", "NM_COLIGACAO", "CD_SIT_TOT_TURNO", "ST_VOTO_EM_TRANSITO", "NR_CANDIDATO", "Subdirectory", "File",
  "NR_CANDIDATO", "DS_SITUACAO_CANDIDATURA", "CD_DETALHE_SITUACAO_CAND", "TP_ABRANGENCIA")) %>%
  filter(DS_CARGO == "Deputado Federal") %>% 
  select(-c("DS_DETALHE_SITUACAO_CAND"))

# Renomeando e reposicionando as variáveis de 2018

ferrana18 <- ferrana18 %>% 
  select(ANO_ELEICAO, SG_UF, NM_CANDIDATO, DS_CARGO, DS_SIT_TOT_TURNO, 
         NR_PARTIDO, SG_PARTIDO, NM_PARTIDO, DS_COMPOSICAO_COLIGACAO, QT_VOTOS_NOMINAIS) 

names(ferrana18) <- names(ferrana14)

# Juntando todos os dataframes por observação
ferrana <- do.call("rbind", list(ferrana90, ferrana94, ferrana98, ferrana02, 
                                 ferrana06, ferrana10, ferrana14, ferrana18))

glimpse(ferrana)

# Corrigindo alguns inputs 

ferrana <- ferrana %>% 
  mutate(situacao = case_when(situacao == "M\xc9DIA" ~ "MEDIA",
                              situacao == "N\xc3O ELEITO" ~ "NAO ELEITO",
                              situacao == "ELEITO POR M\xc9DIA" ~ "ELEITO POR MEDIA",
                              TRUE ~ as.character(situacao)))

ferrana <- ferrana %>% 
  mutate(partido = case_when(partido == "SOLIDARIEDADE" ~ "SD",
                             partido == "PMDB" ~ "MDB",
                             partido == "PC do B" ~ "PCdoB",
                             partido == "PC DO B" ~ "PCdoB",
                             partido == "PT do B" ~ "PTdoB",
                             TRUE ~ as.character(partido))) 

ferrana <- ferrana %>% 
  mutate(cargo = case_when(cargo == "Deputado Federal" ~ "DEPUTADO FEDERAL",
                           TRUE ~ as.character(cargo)))

# Colocando as variáveis na categoria correta...

ferrana$uf <- as.factor(ferrana$uf)
ferrana$partido <- as.factor(ferrana$partido)
ferrana$ano <- as.factor(ferrana$ano)
ferrana$situacao <- as.factor(ferrana$situacao)
ferrana$cargo <- as.factor(ferrana$cargo)
ferrana$npartido <- as.factor(ferrana$npartido)
ferrana$nomepartido <- as.factor(ferrana$nomepartido)

# Inserindo pib_aa e desemp

ferrana <- ferrana %>% 
  mutate(pib_aa = case_when(ano == 1994 ~ 4.7, ano == 1998 ~ 3.4,
                            ano == 2002 ~ 1.3, ano == 2006 ~ 3.2,
                            ano == 2010 ~ -0.6, ano == 2014 ~ 2.3,
                            ano == 1990 ~ -4.3, ano == 2018 ~ 1.0))

ferrana <- ferrana %>% 
  mutate(desemp = case_when(ano == 1990 ~ 4.3, ano == 1994 ~ 5.1,
                            ano == 1998 ~ 7.6, ano == 2002 ~ 7.1,
                            ano == 2006 ~ 10, ano == 2010 ~ 6.2, 
                            ano == 2014 ~ 4.9, ano == 2018 ~ 11.9))

ferrana <- ferrana %>% 
  mutate(pbpet = case_when(ano == 1990 ~ 24.51750, ano == 1994 ~ 17.18333,
                           ano == 1998 ~ 14.41583, ano == 2002 ~ 26.11583,
                           ano == 2006 ~ 66.05250, ano == 2010 ~ 79.40083,
                           ano == 2014 ~ 93.13417, ano == 2018 ~ 62.17))

# Agora , correção de PFL pra PFL/DEM e inserção de GOVOP

ferrana <- ferrana %>% 
  mutate(partido = case_when(partido == "DEM" ~ "PFL/DEM",
                             partido == "PFL" ~ "PFL/DEM",
                             TRUE ~ as.character(partido)))


# Para inserir GovOp é preciso saber a coligação antes, e o governo ainda não está formado.
ferrana %>%
  mutate(govop = case_when(ano == 1990 & partido == "PRN" ~1,
                           ano == 1990 & partido == "PFL/DEM" ~ 1,
                           ano == 1994 & partido == "PFL/DEM" ~ 1,
                           ano == 1994 & partido == "MDB" ~1,
                           ano == 1994 & partido == "PSDB" ~1,
                           ano == 1994 & partido == "PPB" ~ 1,
                           ano == 1998 & partido == "PSDB" ~1,
                           ano == 1998 & partido == "PFL/DEM" ~ 1,
                           ano == 1998 & partido == "MDB" ~ 1,
                           ano == 1998 & partido == "PTB2" ~1,
                           ano == 1998 & partido == "PPB" ~ 1,
                           ano == 2002 & partido == "PSDB" ~1,
                           ano == 2002 & partido == "MDB" ~1,
                           ano == 2002 & partido == "PPB" ~ 1,
                           ano == 2006 & partido == "PT" ~1,
                           ano == 2006 & partido == "PL2" ~1,
                           ano == 2006 & partido == "PCDOB" ~1, 
                           ano == 2006 & partido == "PSB" ~1,
                           ano == 2006 & partido == "PTB" ~1, 
                           ano == 2006 & partido == "MDB" ~1,
                           ano == 2006 & partido == "PT" ~ 1, 
                           ano == 2010 & partido == "PT" ~1,
                           ano == 2010 & partido == "PR" ~1,
                           ano == 2010 & partido == "PCDOB" ~ 1,
                           ano == 2010 & partido == "PSB" ~1,
                           ano == 2010 & partido == "MDB" ~1,
                           ano == 2010 & partido == "PP" ~1,
                           ano == 2010 & partido == "PDT" ~1,
                           ano == 2010 & partido == "PRB" ~ 1, 
                           ano == 2014 & partido == "PT" ~1,
                           ano == 2014 & partido == "MDB" ~ 1,
                           ano == 2014 & partido == "PR2" ~ 1,
                           ano == 2014 & partido == "PCDOB" ~1,
                           ano == 2014 & partido == "PDT" ~1,
                           ano == 2014 & partido == "PP" ~1,
                           ano == 2014 & partido == "PRB" ~ 1,
                           ano == 2018 & partido == ""
                           TRUE ~ 0))
