library(ggplot2)
library(tidyverse)
library(plotly)


ferranaT <- ferranao %>% 
  group_by(ANO, PARTIDO) %>% 
  summarise(NCADT = sum(NCAD)) %>% 
  mutate(NCADT = NCADT)

theme_set(theme_bw())
ggCadAno <- ggplot(ferranaT, aes(x=ferranaT$ANO,  colour=PARTIDO)) +
  geom_line(aes(y= ferranaT$NCADT)) +
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

# Existem outros partidos que existiam antes da ditadura que apareceram, com a mesma sigla, depois, como PR, PL, etc.
# Pelo Plotly dá pra visualizar melhor. Precisamos excluir esses. 

