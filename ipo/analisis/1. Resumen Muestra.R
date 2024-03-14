library(dplyr)

wvs <- read.csv("ipo/input/Datos/wvs.csv", header = T, sep = ";")

df <- wvs %>% select(c(D_INTERVIEW, H_SETTLEMENT, H_URBRURAL, Q1, Q2, Q8, Q11, Q14,
                       Q27, Q109, Q150, Q178, Q177, Q182, Q185, Q186,
                       Q235, Q236, Q240, Q246, Q247, Q249, Q255,
                       Q256, Q257, Q260, Q262, Q275R, Q287, Q289)) %>%
  mutate(across(everything(),~ifelse(.<0, NA, .)))

df %>% select(c(D_INTERVIEW, H_URBRURAL, Q260, Q262, Q275R, Q287, Q289))

zona <- df %>% group_by(H_URBRURAL) %>% summarise(D_INTERVIEW= n()) %>%
  mutate(Indicador = case_when(H_URBRURAL== 1 ~"Urbano",
                               H_URBRURAL==2 ~"Rural"),
         n= D_INTERVIEW, Porcentaje= round((n/sum(n)*100),1)) %>% select(Indicador, n, Porcentaje)
zona

sexo <- df %>% group_by(Q260) %>% summarise(D_INTERVIEW= n()) %>%
  mutate(Indicador = case_when(Q260== 1 ~"Hombre",
                               Q260==2 ~"Mujer"),
         n= D_INTERVIEW, Porcentaje= round((n/sum(n)*100),1)) %>% select(Indicador, n, Porcentaje)
sexo

edad <- df %>% mutate(Indicador= case_when(Q262<=29 ~ "18 a 29 años",
                                           Q262>29 & Q262<=49 ~ "30 a 49 años",
                                           Q262>49 ~ "Más de 50 años")) %>% na.omit() %>%
  group_by(Indicador) %>% summarise(D_INTERVIEW=n())%>% mutate(n= D_INTERVIEW, Porcentaje= round((n/sum(n)*100),1)) %>% select(Indicador, n, Porcentaje)
edad

educ <- df %>% mutate(Indicador= case_when(Q275R==1 ~ "Básico",
                                           Q275R==2 ~ "Medio",
                                           Q275R==3 ~ "Superior")) %>% na.omit() %>%
  group_by(Indicador) %>% summarise(D_INTERVIEW=n())%>% mutate(n= D_INTERVIEW, Porcentaje= round((n/sum(n)*100),1)) %>% select(Indicador, n, Porcentaje)
educ

rel <- df %>% mutate(Indicador= case_when(Q289==0 ~ "Ninguna",
                                          Q289==1 ~ "Católica",
                                          Q289==2 ~"Evangélica",
                                          Q289>2 ~"Otra")) %>% na.omit() %>%
  group_by(Indicador) %>% summarise(D_INTERVIEW=n())%>% mutate(n= D_INTERVIEW, Porcentaje= round((n/sum(n)*100),1)) %>% select(Indicador, n, Porcentaje)
rel

tab2 <- data.frame(Indicador="N", n= 1000, Porcentaje = 100)
tab2 <- rbind(tab2, sexo, edad, zona, educ, rel)

saveRDS(tab2, "ipo/output/muestra.rds")
