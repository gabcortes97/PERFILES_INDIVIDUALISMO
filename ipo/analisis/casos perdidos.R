library(tidyverse)


wvs <- read.csv("ipo/input/Datos/wvs.csv", header = T, sep = ";")

datos <- wvs %>% dplyr::filter(if_any(c(Q27, Q48, Q109, Q182, Q185, 
                                        Q186, Q246, Q247, Q249, Q257, Q150, Q177, Q178, #LCA
                                        Q235, Q236), ~.<0))
  
datos %>% group_by(Q260) %>% summarise(casos= n()) %>% mutate(casos= casos/sum(casos)*100)
wvs %>% group_by(Q260) %>% summarise(casos= n()) %>% mutate(casos= casos/sum(casos)*100)

datos %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                              Q262>=30 & Q262<45 ~ "30 a 44",
                              Q262>=45 & Q262<60 ~ "45 a 59",
                              Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(casos=n()) %>% mutate(casos= casos/sum(casos)*100)

wvs %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                                 Q262>=30 & Q262<45 ~ "30 a 44",
                                 Q262>=45 & Q262<60 ~ "45 a 59",
                                 Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(casos=n()) %>% mutate(casos= casos/sum(casos)*100)

datos %>% filter(Q275!= is.na(Q275)) %>%
  mutate(educ= ifelse(Q275<5, "Baja Educación", "Alta Educación")) %>%
  group_by(educ) %>% summarise(educ=n()) %>% mutate(educ= educ/sum(educ)*100)

wvs %>% filter(Q275!= is.na(Q275)) %>%
  mutate(educ= ifelse(Q275<5, "Baja Educación", "Alta Educación")) %>%
  group_by(educ) %>% summarise(casos=n()) %>% mutate(casos= casos/sum(casos)*100) 
#80% casos pérdidos son personas sin educación superior


datos %>% mutate(id_pol= case_when(Q240<3 & Q240>0~ "Izquierda",
                                Q240==3 | Q240==4 ~"Centro Izquierda",
                                Q240==5 ~ "Centro",
                                Q240==6 | Q240==7  ~"Centro Derecha",
                                Q240>7 ~"Derecha",
                                Q240<0 ~ "Ninguna")) %>%
  group_by(id_pol) %>% summarise(casos=n()) %>% 
  mutate(casos= casos/sum(casos)*100)

wvs %>% mutate(id_pol= case_when(Q240<3 & Q240>0~ "Izquierda",
                                   Q240==3 | Q240==4 ~"Centro Izquierda",
                                   Q240==5 ~ "Centro",
                                   Q240==6 | Q240==7  ~"Centro Derecha",
                                   Q240>7 ~"Derecha",
                                   Q240<0 ~ "Ninguna")) %>%
  group_by(id_pol) %>% summarise(casos=n()) %>% 
  mutate(casos= casos/sum(casos)*100)


datos %>% group_by(G_TOWNSIZE2) %>% summarise(casos=n()) %>%
  mutate(casos= casos/sum(casos)*100)

wvs %>% group_by(G_TOWNSIZE2) %>% summarise(casos=n()) %>%
  mutate(casos= casos/sum(casos)*100)


datos %>% mutate(region= ifelse(H_SETTLEMENT==1, "Santiago", "No-Santiago")) %>%
  group_by(region) %>% summarise(casos=n()) %>% 
  mutate(casos= casos/sum(casos)*100)

wvs %>% mutate(region= ifelse(H_SETTLEMENT==1, "Santiago", "No-Santiago")) %>%
  group_by(region) %>% summarise(casos=n()) %>% 
  mutate(casos= casos/sum(casos)*100)
