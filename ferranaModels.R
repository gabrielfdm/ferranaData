library(tidyverse)
library(plotly)
library(janitor)

ferranapt <- ferranao[which(ferranao$partido == "PT"),]
ferranamdb <- ferranao[which(ferranao$partido == "MDB"),]


mpt1 <- lm(ncad ~ pib_aa + desemp + wc, data = ferranapt)
mpt2 <- lm(ncad ~ pib_aa * desemp, data = ferranapt)

mmdb1 <- lm(ncad ~ pib_aa + desemp, data = ferranamdb)
mmdb2 <- lm(ncad ~ pib_aa * desemp, data = ferranamdb)

ferrana82 <- ferranao %>% 
  filter(ano > 1982)
