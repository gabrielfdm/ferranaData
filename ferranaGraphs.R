library(ggplot2)
library(tidyverse)
library(plotly)


ferrana2 <- ferranao %>% 
  group_by(ANO, PARTIDO)

theme_set(theme_bw())
ggCadAno <- ggplot(ferrana2, aes(x=ferrana2$ANO,  colour=PARTIDO, group=UF)) +
  geom_line(aes(y= ferrana2$NCAD)) +
  labs(title="Evolução do nº de cadeiras, por partido, na Câmara", 
       y="Nº de cadeiras", x="Ano da Eleição", 
       caption="Fonte: Tribunal Superior Eleitoral",
       color="Partido") +
  theme(plot.title=element_text(size=14, 
                                hjust=0.5,
                                lineheight=1.2)) +
  scale_x_continuous(breaks=c(1945, 1950, 1954, 1958, 1962, 1966, 1970, 1974, 1978, 1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014),
                     limits=c(1945, 2014))
ggplotly(ggCadAno)


