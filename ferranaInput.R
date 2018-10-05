# Não usar o IPCA, furada! 
#ferranaE <- ferranao %>% 
#  mutate(IPCA = case_when(ano == 1994 ~ 909.29, ano == 1998 ~ 1.25,
#                          ano == 2002 ~ 6.1, ano == 2006 ~ 2.02,
#                          ano == 2010 ~ 3.88, ano == 2014 ~ 4.69,))

ferranaE <- ferranao %>% 
  mutate(pib_aa = case_when(ano == 1994 ~ 4.7, ano == 1998 ~ 3.4,
                            ano == 2002 ~ 1.3, ano == 2006 ~ 3.2,
                            ano == 2010 ~ -0.6, ano == 2014 ~ 2.3,
                            ano == 1990 ~ -4.3, ano == 1986 ~ 7.5,
                            ano == 1982 ~ 0.8, ano == 1978 ~ 5,
                            ano == 1974 ~ 8.2, ano == 1970 ~ 10.4,
                            ano == 1966 ~ 6.7, ano == 1962 ~ 6.6,
                            ano == 1958 ~ 10.8, ano == 1954 ~ 7.8,
                            ano == 1950 ~ 6.8))

ferranaE <- ferranaE %>% 
  mutate(desemp = case_when(ano == 1994 ~ 8.3, ano == 1998 ~ 11.1,
                            ano == 2002 ~ 11.5, ano == 2006 ~ 10,
                            ano == 2010 ~ 6.2, ano == 2014 ~ 4.9))
