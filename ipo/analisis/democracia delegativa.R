# Librerías

library(tidyverse)
library(psych)


# Cargar datos

wvs <- read.csv("ipo/input/Datos/wvs.csv", header = T, sep = ";")

df <- wvs %>% dplyr::select(c(D_INTERVIEW, N_REGION_ISO,
                              Q27, Q48, Q109, Q182, Q185, 
                              Q186, Q246, Q247, Q249, Q257, Q150, Q177, Q178, #LCA
                              Q45, Q46, Q106, Q131, Q176, Q199, Q222, Q240, # otras variables
                              Q235, Q236, Q238, Q250, # democracia
                              Q260, Q262, Q274, Q275, Q281, Q288, Q289, G_TOWNSIZE2,
                              H_SETTLEMENT)) %>% #demográficos
  mutate(across(c(Q48, Q27, Q109, Q182, Q185, 
                            Q186, Q246, Q247, Q249, Q257, Q150, Q177, Q178,
                            Q45, Q46, Q106, Q131, Q176, Q199, Q222,
                            Q235, Q236, Q238, Q250,
                            Q260, Q262, Q274, Q275, Q281, Q288, Q289),~ifelse(.<0, NA, .))) 

# Análisis descriptivo

df$Q235 <- factor(df$Q235, levels=(c(1,2,3,4)), labels=c("Muy Bueno", "Bueno", "Malo", "Muy Malo"))
df$Q236 <- factor(df$Q236, levels=(c(1,2,3,4)), labels=c("Muy Bueno", "Bueno", "Malo", "Muy Malo"))

lf <- df %>% group_by(Q235) %>% filter(Q235!=is.na(Q235)) %>% summarise(D_INTERVIEW= n()) %>%
  mutate("Líder Fuerte"= round((D_INTERVIEW/sum(D_INTERVIEW)*100),2)) %>% rename("Categoría" = Q235)
ex <- df %>% group_by(Q236) %>% filter(Q236!=is.na(Q235)) %>% summarise(D_INTERVIEW= n()) %>%
  mutate("Expertos"= round((D_INTERVIEW/sum(D_INTERVIEW)*100),2)) %>% rename("Categoría" = Q236)

tabla4 <- merge(lf, ex, by="Categoría") %>% 
  dplyr::select(c("Categoría", "Líder Fuerte", "Expertos")) %>% arrange(Categoría)

tabla4
# El Apoyo a un Líder fuerte es intermedio. 44% cree que es bueno o muy bueno
# El Apoyo a que expertos tomen decisiones es algo mayor: 49% cree que es bueno o muy bueno

df<- df %>% mutate(across(c(Q235, Q236), ~case_when(.=="Muy Bueno" ~ 4,
                                                       .=="Bueno" ~ 3,
                                                       .=="Malo" ~ 2,
                                                       .=="Muy Malo" ~ 1))) %>%
  mutate(dd= (Q235+Q236)/2)


psych::describe(df$dd, na.rm=T) # En promedio, los niveles de apoyo se encuentran en 2.39
# El 50% de los casos muestra niveles de apoyo superiores a 2.5

tab5<- df %>% mutate(dd_cat = case_when(dd<2 ~ "Bajo",
                                        dd>=2 & dd<=3 ~ "Medio",
                                        dd>3 ~ "Alto")) %>%
  group_by(dd_cat) %>% filter(dd_cat!=is.na(dd_cat)) %>% summarise(D_INTERVIEW = n()) %>% 
  mutate(apoyo= round((D_INTERVIEW/sum(D_INTERVIEW)*100),2),
         dd_cat= factor(dd_cat, levels=c("Alto", "Medio", "Bajo"))) %>%
  arrange(dd_cat) %>% dplyr::select("Democracia Delegativa"=dd_cat, Apoyo=apoyo)
tab5 #19% muestra una apoyo alto a la democracia delegativa (sobre 3) 
# 68% un apoyo medio (entre 2 y 3)

tabx <- df %>% filter(dd != is.na(dd)) %>%
  group_by(dd) %>% summarise(D_INTERVIEW= n()) %>% mutate(Frecuencia= round((D_INTERVIEW/sum(D_INTERVIEW)*100),2))
tabx



hist(df$dd)

# Democracia delegativa y género

tab6 <- df %>% group_by(Q260) %>% summarise(Lider= mean(Q235, na.rm=T),
                                            Expertos = mean(Q236, na.rm=T),
                                            Delegativa= mean(dd, na.rm=T)) %>% rename(cat = Q260)
tab6
# El apoyo entre las mujeres es apenas superior

# Democracia delegativa y edad
tab7 <- df %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                                      Q262>=30 & Q262<45 ~ "30 a 44",
                                      Q262>=45 & Q262<60 ~ "45 a 59",
                                      Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(Lider= mean(Q235, na.rm=T),
                               Expertos = mean(Q236, na.rm=T),
                               Delegativa= mean(dd, na.rm=T),
                               desviacion= sd(dd, na.rm=T)) %>% rename(cat = edad)
tab7 # Tampoco hay tanta diferencia por edad. 

# Democracia delegativa y educación

tab8 <- df %>% filter(Q275!= is.na(Q275)) %>%
  mutate(educ= ifelse(Q275<5, "Baja Educación", "Alta Educación")) %>%
  group_by(educ) %>% summarise(Lider= mean(Q235, na.rm=T),
                               Expertos = mean(Q236, na.rm=T),
                               Delegativa= mean(dd, na.rm=T)) %>% rename(cat = educ)
tab8

#Las personas con baja educación muestran un mayor apoyo a los expertos

# Democracia delegativa y regiones

tab10 <- df %>% mutate(region= ifelse(N_REGION_ISO==152014, "Santiago", "Regiones")) %>%
  group_by(region) %>% summarise(Lider= mean(Q235, na.rm=T),
                                 Expertos = mean(Q236, na.rm=T),
                                 Delegativa= mean(dd, na.rm=T)) %>% rename(cat = region)
tab10

tabxx <- df %>% mutate(region= ifelse(H_SETTLEMENT==1, "Santiago", "Regiones")) %>%
  group_by(region) %>% summarise(Lider= mean(Q235, na.rm=T),
                                 Expertos = mean(Q236, na.rm=T),
                                 Delegativa= mean(dd, na.rm=T)) %>% rename(cat = region)
tabxx

tabxx <- df %>% 
  group_by(G_TOWNSIZE2) %>% summarise(Lider= mean(Q235, na.rm=T),
                                 Expertos = mean(Q236, na.rm=T),
                                 Delegativa= mean(dd, na.rm=T))
tabxx

table(df$G_TOWNSIZE2)

# En Regiones se tiende apoyar más una democracia delegativa

# Democracia delegativa y posición política

tab9 <- df %>% mutate(id_pol= case_when(Q240<3 & Q240>0~ "Izquierda",
                                        Q240==3 | Q240==4 ~"Centro Izquierda",
                                        Q240==5 ~ "Centro",
                                        Q240>=6 & Q240<=8  ~"Centro Derecha",
                                        Q240>8 ~"Derecha",
                                        Q240<0 ~ "Ninguna")) %>%
  group_by(id_pol) %>% summarise(Lider= mean(Q235, na.rm=T),
                                 Expertos = mean(Q236, na.rm=T),
                                 Delegativa= mean(dd, na.rm=T),
                                 desviacion= sd(dd, na.rm=T)) %>% rename(cat = id_pol)
tab9

# Democracia delegativa e ingresos subjetivos

tab11 <- df %>% filter(Q288 !=is.na(Q288)) %>% 
  mutate(ingresos= case_when(Q288<4 ~ "Ingresos Bajos",
                             Q288==4 | Q288==5 ~ "Ingresos Medios-Bajos",
                             Q288==6 | Q288==7 ~ "Ingresos Medios-Altos",
                             Q288>=8 ~ "Ingresos Altos")) %>%
  group_by(ingresos) %>% summarise(Lider= mean(Q235, na.rm=T),
                                   Expertos = mean(Q236, na.rm=T),
                                   Delegativa= mean(dd, na.rm=T),
                                   desviacion= sd(dd, na.rm=T)) %>% rename(cat = ingresos)
tab11  

# Democracia delegativa y religión

tab12 <- df %>%
  mutate(religion= case_when(Q289== 0 ~ "Sin religión",
                             Q289== 1 ~ "Católica",
                             Q289==2 | Q289==8 ~ "Evangélica",
                             Q289>2 ~ "Otra")) %>%
  group_by(religion) %>% summarise(Lider= mean(Q235, na.rm=T),
                                   Expertos = mean(Q236, na.rm=T),
                                   Delegativa= mean(dd, na.rm=T),
                                   desviacion= sd(dd, na.rm=T)) %>% rename(cat = religion)
tab12

# Democracia delegativa y democracia

tab13 <- df %>%
  mutate(democracia = ifelse(Q238<=2, "Apoyo", "No apoyo")) %>%
  group_by(democracia) %>% summarise(Lider= mean(Q235, na.rm=T),
                                     Expertos = mean(Q236, na.rm=T),
                                     Delegativa= mean(dd, na.rm=T)) %>% rename(cat = democracia)
tab13

tab_dd <- rbind(tab6, tab7, tab8, tab9, tab10, tab11, tab12, tab13)

# Democracia delegativa y clase social

tab14 <- df %>% mutate(trabajo = case_when(Q281== 0 ~NA,
                                           Q281==1 | Q281==2 ~ "Clase de Servicio",
                                           Q281>=3 & Q281<=5 ~ "Clase Media",
                                           Q281>5 ~ "Clase Trabajadora")) %>%
  filter(trabajo!= is.na(trabajo)) %>%
  group_by(trabajo) %>% summarise(Lider= mean(Q235, na.rm=T),
                                  Expertos = mean(Q236, na.rm=T),
                                  Delegativa= mean(dd, na.rm=T))

tab14


# Guardar

saveRDS(df, "ipo/input/Datos/df.rds")
saveRDS(tabla4, "ipo/output/tabla4.rds")
saveRDS(tabx, "ipo/output/tabx.rds")
