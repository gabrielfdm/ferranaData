library(ggplot2)
library(tidyverse)
library(plotly)

ferranaT <- ferranao %>% 
  group_by(ano, partido) %>% 
  summarise(ncadt = sum(ncad)) %>% 
  mutate(ncadt = ncadt)

theme_set(theme_bw())
ggCadAno <- ggplot(ferranaT, aes(x=ferranaT$ano,  colour=partido)) +
  geom_line(aes(y= ferranaT$ncadt)) +
  labs(title="Evolução do nº de cadeiras, por partido, na Câmara", 
       y="Nº de cadeiras", x="Ano da Eleição", 
       caption="Fonte: Tribunal Superior Eleitoral",
       color="Partido") +
  theme(plot.title=element_text(size=14, 
                                hjust=0.5,
                                lineheight=1.2)) +
  scale_x_continuous(breaks=c(1945, 1950, 1954, 1958, 1962, 1966, 1970, 1974, 1978, 1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014),
                     limits=c(1945, 2014))
ggCadAno
ggplotly(ggCadAno)

# Gráfico da evolução das cadeiras do Rio, somando RJ e GB 
ferranaRio <- ferranao %>% 
  filter(uf == "RJ" | uf == "GB") %>% 
  group_by(ano, partido) %>% 
  summarise(ncadt = sum(ncad)) %>% 
  mutate(ncadt = ncadt)

ggRioCad <- ggplot(ferranaRio, aes(x=ferranaRio$ano, colour=partido)) +
  geom_line(aes(y = ferranaRio$ncadt)) + 
  labs(title="Evolução do nº de Cadeiras do Rio de Janeiro, por partido, na Câmara",
       y = "Nº de cadeiras", x = "Ano da Eleição",
       caption = "Fonte: Tribunal Superior Eleitoral",
       color = "Partido") +
  theme(plot.title=element_text(size=14, 
                                hjust=0.5,
                                lineheight=1.2)) +
  scale_x_continuous(breaks=c(1945, 1950, 1954, 1958, 1962, 1966, 1970, 1974, 1978, 1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014),
                     limits=c(1945, 2014))
ggRioCad
ggplotly(ggRioCad)


# Gráficos a partir de 1982 
ferrana82 <- ferranao %>% 
  filter(ano >= 1982) %>% 
  group_by(ano, partido) %>% 
  summarise(ncadt = sum(ncad)) %>% 
  mutate(ncadt = ncadt)

ggCad82 <- ggplot(ferrana82, aes(x=ferrana82$ano, colour=partido)) +
  geom_line(aes(y=ferrana82$ncadt)) +
  labs(title="Evolução do nº de Cadeiras a partir de 1982, por partido, na Câmara",
       y = "Nº de cadeiras", x = "Ano da Eleição",
       caption = "Fonte: Tribunal Superior Eleitoral",
       color = "Partido") +
  theme(plot.title=element_text(size=14, 
                                hjust=0.5,
                                lineheight=1.2)) +
  scale_x_continuous(breaks=c(1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014),
                     limits=c(1982, 2014))
ggCad82
ggplotly(ggCad82)

# Gráficos só do Rio a partir de 1982 
ferranaRio82 <- ferranao %>% 
  filter(ano >= 1982 & uf == "RJ") %>% 
  group_by(ano, partido) %>% 
  summarise(ncadt = sum(ncad)) %>% 
  mutate(ncadt = ncadt)

ggRio82 <- ggplot(ferranaRio82, aes(x=ferranaRio82$ano, colour=partido)) +
  geom_line(aes(y=ferranaRio82$ncadt)) +
  labs(title="Evolução do nº de Cadeiras a partir de 1982, no RJ",
       y = "Nº de cadeiras", x = "Ano da Eleição",
       caption = "Fonte: Tribunal Superior Eleitoral",
       color = "Partido") +
  theme(plot.title=element_text(size=14, 
                                hjust=0.5,
                                lineheight=1.2)) +
  scale_x_continuous(breaks=c(1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014),
                     limits=c(1982, 2014))
ggRio82
ggplotly(ggRio82)


