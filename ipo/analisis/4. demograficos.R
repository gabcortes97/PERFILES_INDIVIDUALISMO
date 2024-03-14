library(tidyverse)


lca4 <- readRDS("ipo/output/lca4")
df <- readRDS("ipo/input/Datos/df.rds")
df2 <- readRDS("ipo/input/Datos/df2.rds")

df2 <- df2 %>% mutate(clase= lca4$predclass,
                      probs1 = lca4$posterior[,1]*100,
                      probs2 = lca4$posterior[,2]*100,
                      probs3 = lca4$posterior[,3]*100,
                      probs4 = lca4$posterior[,4]*100)

df2 %>% group_by(clase) %>% summarise(D_INTERVIEW= n()) %>% 
  mutate(D_INTERVIEW/sum(D_INTERVIEW)*100)

subset <- df2 %>% dplyr::select(c(D_INTERVIEW, clase,
                                  probs1, probs2, probs3, probs4))

df <- merge(df, subset, by="D_INTERVIEW")

# Género

df %>% group_by(clase) %>% summarise(mujer= mean(Q260))
t.test(df$Q260) # La clase 4 parece estar más feminizida (56%)

# Edad
t.test(df$Q262) # Entre 43.2 y 45.4
df %>% group_by(clase) %>% summarise(edad= mean(Q262)) 
# La clase 1 y 2 son de mayor edad (46.3 y 47.8). La clase 4 es la más joven 40.3

df %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                                                   Q262>=30 & Q262<45 ~ "30 a 44",
                                                   Q262>=45 & Q262<60 ~ "45 a 59",
                                                   Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(clase=n()) %>% mutate(clase= clase/sum(clase)*100)
# 35% entre 30 a 44; 29% entre 45 a 59; 18% mayores de 60 y 19% menores de 30

df %>% filter(clase==1) %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                                                   Q262>=30 & Q262<45 ~ "30 a 44",
                                                   Q262>=45 & Q262<60 ~ "45 a 59",
                                                   Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(clase=n()) %>% mutate(clase= clase/sum(clase)*100)
# Solo 14% menores de 30

df %>% filter(clase==2) %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                                                   Q262>=30 & Q262<45 ~ "30 a 44",
                                                   Q262>=45 & Q262<60 ~ "45 a 59",
                                                   Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(clase=n()) %>% mutate(clase= clase/sum(clase)*100)
# Solo 12% menores de 30 y 28% mayores de 60%

df %>% filter(clase==2) %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                                                   Q262>=30 & Q262<45 ~ "30 a 44",
                                                   Q262>=45 & Q262<60 ~ "45 a 59",
                                                   Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(clase=n()) %>% mutate(clase= clase/sum(clase)*100)
# Solo 12% menores de 30 y 28% mayores de 60%

df %>% filter(clase==4) %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                                                   Q262>=30 & Q262<45 ~ "30 a 44",
                                                   Q262>=45 & Q262<60 ~ "45 a 59",
                                                   Q262>=60 ~ "Mayores de 60")) %>%
  group_by(edad) %>% summarise(clase=n()) %>% mutate(clase= clase/sum(clase)*100)
# 9% menores de 30 y 28% mayores de 30. 

# Educación

df %>% filter(Q275!= is.na(Q275)) %>%
  mutate(educ= ifelse(Q275<5, "Baja Educación", "Alta Educación")) %>%
  group_by(clase, educ) %>% summarise(educ=n()) %>% mutate(educ= educ/sum(educ)*100)

# Santiago/Regiones

a <- df %>% mutate(region= ifelse(N_REGION_ISO==152014, 1, 0))
t.test(a$region) #Entre 40% y 47%

df %>% mutate(region= ifelse(N_REGION_ISO==152014, "Santiago", "Regiones")) %>%
  group_by(clase, region) %>% summarise(region_=n()) %>% 
  mutate(region_= region_/sum(region_)*100) #62% clase 2 vive fuera de Santiago
 # 50% clase 3 en Santiago
 # 37% clase 4 vive en Santiago

df %>% mutate(region= ifelse(H_SETTLEMENT==1, "Santiago", "No-Santiago")) %>%
  group_by(clase, region) %>% summarise(region_=n()) %>% 
  mutate(region_= region_/sum(region_)*100)

# Posición Política
a <- df %>% mutate(id_pol= ifelse(Q240<=3 & Q240>0, 1, 0))
t.test(a$id_pol) # Entre 4% y 7%
a <- df %>% mutate(id_pol= ifelse(Q240==3 | Q240==4, 1, 0))
t.test(a$id_pol) # Entre 19% y 25%
a <- df %>% mutate(id_pol= ifelse(Q240==5, 1, 0))
t.test(a$id_pol) # Entre 22% y 28%
a <- df %>% mutate(id_pol= ifelse(Q240>=6 & Q240<=8, 1, 0))
t.test(a$id_pol) # Entre 19% y 25%
a <- df %>% mutate(id_pol= ifelse(Q240>8, 1, 0))
t.test(a$id_pol) # Entre 1% y 4%
a <- df %>% mutate(id_pol= ifelse(Q240<0, 1, 0))
t.test(a$id_pol) # Entre 21 y 27% Ninguna

a <- df %>% mutate(id_pol= ifelse(Q240<=4 & Q240>0, 1, 0))
t.test(a$id_pol) # Entre 24% y el 30%
a <- df %>% mutate(id_pol= ifelse(Q240>=6, 1, 0))
t.test(a$id_pol) # Entre 21% y el 28%


idpol <- df %>% mutate(id_pol= case_when(Q240<3 & Q240>0~ "Izquierda",
                                Q240==3 | Q240==4 ~"Centro Izquierda",
                                Q240==5 ~ "Centro",
                                Q240==6 | Q240==7  ~"Centro Derecha",
                                Q240>7 ~"Derecha",
                                Q240<0 ~ "Ninguna")) %>%
  group_by(clase, id_pol) %>% summarise(idpol=n()) %>% 
  mutate(idpol= idpol/sum(idpol)*100)

# Ingresos subjetivos

t.test(df$Q288) # Entre 4.8 y 5

df %>% group_by(clase) %>% summarise(ingresos= mean(Q288, na.rm=T)) 

a <- df %>% mutate(id_pol= ifelse(Q288<4, 1, 0))
t.test(a$id_pol) # Entre 14 y 20% ingresos bajos
a <- df %>% mutate(id_pol= ifelse(Q288==4 | Q288==5, 1, 0))
t.test(a$id_pol) # Entre el 48 y 55% ingresos medios bajos
a <- df %>% mutate(id_pol= ifelse(Q288==6 | Q288==7, 1, 0))
t.test(a$id_pol) # Entre 22% y 28% ingresos medios altos
a <- df %>% mutate(id_pol= ifelse(Q288>=8, 1, 0))
t.test(a$id_pol) # Entre 5 y 9% ingresos altos

df %>% filter(Q288 !=is.na(Q288)) %>% 
  mutate(ingresos= case_when(Q288<4 ~ "Ingresos Bajos",
                             Q288==4 | Q288==5 ~ "Ingresos Medios-Bajos",
                             Q288==6 | Q288==7 ~ "Ingresos Medios-Altos",
                             Q288>=8 ~ "Ingresos Altos")) %>%
  group_by(clase, ingresos) %>% summarise(isub=n()) %>% 
  mutate(isub= isub/sum(isub)*100)


# Religión

a <- df %>% mutate(id_pol= ifelse(Q289==0, 1, 0))
t.test(a$id_pol) # Entre 25 y 32 sin religión
a <- df %>% mutate(id_pol= ifelse(Q289== 1, 1, 0))
t.test(a$id_pol) # Entre el 56% y 63% católicos
a <- df %>% mutate(id_pol= ifelse(Q289==2 | Q289==8, 1, 0))
t.test(a$id_pol) # Entre un 5% y un 8% evángelicos


df %>%
  mutate(religion= case_when(Q289== 0 ~ "Sin religión",
                             Q289== 1 ~ "Católica",
                             Q289==2 | Q289==8 ~ "Evangélica",
                             Q289>2 ~ "Otra")) %>%
  filter(religion!=is.na(religion)) %>%
  group_by(clase, religion) %>% summarise(rel=n()) %>% 
  mutate(rel= rel/sum(rel)*100)

# Tipo de ciudad

a <- df %>% mutate(id_pol= ifelse(G_TOWNSIZE2==1, 1, 0))
t.test(a$id_pol) # Entre 10 y 15% rural
a <- df %>% mutate(id_pol= ifelse(G_TOWNSIZE2==3, 1, 0))
t.test(a$id_pol) # Entre 13 y 19% ciudades de hasta 100.000 hab
a <- df %>% mutate(id_pol= ifelse(G_TOWNSIZE2==4, 1, 0))
t.test(a$id_pol) # Entre 30% y 37% ciudades de hasta 500.000 hab
a <- df %>% mutate(id_pol= ifelse(G_TOWNSIZE2==5, 1, 0))
t.test(a$id_pol) # Entre 34% y 41% ciudades sobre 500.000 hab

df %>% group_by(clase, G_TOWNSIZE2) %>% summarise(zona=n()) %>%
  mutate(zona= zona/sum(zona)*100)

# Hijos

df <- df %>% mutate(hijos= ifelse(Q274==0, 0, 1))
t.test(df$hijos) # Entre el 73% y 80% tienen hijos

df %>% group_by(clase, hijos) %>% summarise(hijos_a =n()) %>% 
  mutate(hijos_a= hijos_a/sum(hijos_a)*100)

# Trabajo Manual

df <- df %>% mutate(t_trabajo= case_when(Q281== 0 ~ NA,
                                         Q281>0 & Q281<6 | Q281==10 ~ "Trabajo No-Manual",
                                         Q281>= 6 & Q281<10 ~ "Trabajo Manual"))
df %>% group_by(t_trabajo) %>% summarise(trabajo= n()) %>% 
  mutate(trabajo= trabajo/sum(trabajo)*100)

a <- df %>% mutate(t_trabajo= ifelse(t_trabajo=="Trabajo No-Manual", 1, 0))
t.test(a$t_trabajo) # Entre el 53% y el 60%

df %>% filter(t_trabajo!= is.na(t_trabajo)) %>% group_by(clase, t_trabajo) %>% summarise(trabajo= n()) %>% 
  mutate(trabajo= trabajo/sum(trabajo)*100)

a <- df %>% filter(Q281!=0) %>% mutate(trabajo= ifelse(Q281==1|Q281==2, 1, 0))
t.test(a$trabajo) # Entre el 20% y el 27%
a <- df %>% filter(Q281!=0) %>% mutate(trabajo= ifelse(Q281>=3 & Q281<=5, 1, 0))
t.test(a$trabajo) # Entre el 30% y 37%
a <- df %>% filter(Q281!=0) %>% mutate(trabajo= ifelse(Q281>5, 1, 0))
t.test(a$trabajo) # Entre el 40 y el 47%


df %>% mutate(trabajo = case_when(Q281== 0 ~NA,
                                  Q281==1 | Q281==2 ~ "Clase de Servicio",
                                  Q281>=3 & Q281<=5 ~ "Clase Media",
                                  Q281>5 ~ "Clase Trabajadora")) %>%
  filter(trabajo!= is.na(trabajo)) %>%
  group_by(clase, trabajo) %>% summarise(clase_social= n()) %>%
  mutate(clase_social= clase_social/sum(clase_social)*100)


saveRDS(df, "ipo/input/Datos/data.rds")

# Tabla Resumen

datos <- readRDS("ipo/input/Datos/datosLM.rds")

datos <- datos %>% mutate(individualismo_autoritario= ifelse(clase=="Individidualismo Autoritario", 1, 0),
                individualismo_conservador= ifelse(clase=="Individualismo Conservador", 1, 0),
                individualismo_liberal= ifelse(clase=="Individualismo Liberal", 1, 0),
                individualismo_agéntico= ifelse(clase=="Individualismo Agéntico", 1, 0))

genero <- datos %>% group_by(genero) %>% summarise(Autoritario=sum(individualismo_autoritario, na.rm=T),
                                        Conservador=sum(individualismo_conservador, na.rm=T),
                                        Liberal=sum(individualismo_liberal, na.rm=T),
                                        Agéntico=sum(individualismo_agéntico, na.rm=T)) %>%
mutate(Autoritario= round((Autoritario/sum(Autoritario)*100), 2),
       Conservador=round((Conservador/sum(Conservador)*100),2),
       Liberal=round((Liberal/sum(Liberal)*100),2),
       Agéntico= round((Agéntico/sum(Agéntico)*100),2)) %>%
  rename(Indicador=genero)

datos <- datos %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                       Q262>=30 & Q262<45 ~ "30 a 44",
                       Q262>=45 & Q262<60 ~ "45 a 59",
                       Q262>=60 ~ "Mayores de 60"))

edad <- datos %>% group_by(edad) %>% summarise(Autoritario=sum(individualismo_autoritario, na.rm=T),
                                                   Conservador=sum(individualismo_conservador, na.rm=T),
                                                   Liberal=sum(individualismo_liberal, na.rm=T),
                                                   Agéntico=sum(individualismo_agéntico, na.rm=T)) %>%
  mutate(Autoritario= round((Autoritario/sum(Autoritario)*100), 2),
         Conservador=round((Conservador/sum(Conservador)*100),2),
         Liberal=round((Liberal/sum(Liberal)*100),2),
         Agéntico= round((Agéntico/sum(Agéntico)*100),2)) %>%
  rename(Indicador=edad)

id_pol <- datos %>% group_by(id_pol) %>% summarise(Autoritario=sum(individualismo_autoritario, na.rm=T),
                                               Conservador=sum(individualismo_conservador, na.rm=T),
                                               Liberal=sum(individualismo_liberal, na.rm=T),
                                               Agéntico=sum(individualismo_agéntico, na.rm=T)) %>%
  mutate(Autoritario= round((Autoritario/sum(Autoritario)*100), 2),
         Conservador=round((Conservador/sum(Conservador)*100),2),
         Liberal=round((Liberal/sum(Liberal)*100),2),
         Agéntico= round((Agéntico/sum(Agéntico)*100),2)) %>%
  rename(Indicador=id_pol)

datos <- datos %>% filter(Q288 !=is.na(Q288)) %>% 
  mutate(ingresos= case_when(Q288<4 ~ "Ingresos Bajos",
                             Q288==4 | Q288==5 ~ "Ingresos Medios-Bajos",
                             Q288==6 | Q288==7 ~ "Ingresos Medios-Altos",
                             Q288>=8 ~ "Ingresos Altos"))

ingresos <- datos %>% group_by(ingresos) %>% summarise(Autoritario=sum(individualismo_autoritario, na.rm=T),
                                                   Conservador=sum(individualismo_conservador, na.rm=T),
                                                   Liberal=sum(individualismo_liberal, na.rm=T),
                                                   Agéntico=sum(individualismo_agéntico, na.rm=T)) %>%
  mutate(Autoritario= round((Autoritario/sum(Autoritario)*100), 2),
         Conservador=round((Conservador/sum(Conservador)*100),2),
         Liberal=round((Liberal/sum(Liberal)*100),2),
         Agéntico= round((Agéntico/sum(Agéntico)*100),2)) %>%
  rename(Indicador=ingresos)

religion <- datos %>% group_by(religion) %>% filter(religion!=is.na(religion)) %>%
  summarise(Autoritario=sum(individualismo_autoritario, na.rm=T),
                                                       Conservador=sum(individualismo_conservador, na.rm=T),
                                                       Liberal=sum(individualismo_liberal, na.rm=T),
                                                       Agéntico=sum(individualismo_agéntico, na.rm=T)) %>%
  mutate(Autoritario= round((Autoritario/sum(Autoritario)*100), 2),
         Conservador=round((Conservador/sum(Conservador)*100),2),
         Liberal=round((Liberal/sum(Liberal)*100),2),
         Agéntico= round((Agéntico/sum(Agéntico)*100),2)) %>%
  rename(Indicador=religion)

trabajo <- datos %>% group_by(trabajo) %>% summarise(Autoritario=sum(individualismo_autoritario, na.rm=T),
                                                       Conservador=sum(individualismo_conservador, na.rm=T),
                                                       Liberal=sum(individualismo_liberal, na.rm=T),
                                                       Agéntico=sum(individualismo_agéntico, na.rm=T)) %>%
  mutate(Autoritario= round((Autoritario/sum(Autoritario)*100), 2),
         Conservador=round((Conservador/sum(Conservador)*100),2),
         Liberal=round((Liberal/sum(Liberal)*100),2),
         Agéntico= round((Agéntico/sum(Agéntico)*100),2)) %>%
  rename(Indicador=trabajo)

trabajo <- datos %>% group_by(trabajo) %>%
  filter(trabajo!=is.na(trabajo)) %>%
  summarise(Autoritario=sum(individualismo_autoritario, na.rm=T),
                                                             Conservador=sum(individualismo_conservador, na.rm=T),
                                                             Liberal=sum(individualismo_liberal, na.rm=T),
                                                             Agéntico=sum(individualismo_agéntico, na.rm=T)) %>%
  mutate(Autoritario= round((Autoritario/sum(Autoritario)*100), 2),
         Conservador=round((Conservador/sum(Conservador)*100),2),
         Liberal=round((Liberal/sum(Liberal)*100),2),
         Agéntico= round((Agéntico/sum(Agéntico)*100),2)) %>%
  rename(Indicador=trabajo)


tab_ind <- rbind(edad, genero, id_pol, ingresos, religion, tipo_ciudad, trabajo)
saveRDS(tab_ind, "ipo/output/tab_ind.rds")

library(kableExtra)

kable(tab_ind, booktabs= TRUE,
      escape = FALSE,
      caption = "Resumen Indicadores Sociodemográficos por Perfiles de Invididualismo", align = "c",
      position= "h", centering= F, format.args = list(decimal.mark=",")) %>%
  row_spec(row=0, align = "c") %>%
  kable_styling(full_width = TRUE) %>% 
  kable_classic(full_width = T, html_font = "Helvetica") %>% 
  pack_rows("Edad", 1, 4) %>%
  pack_rows("Género", 5, 6) %>%
  pack_rows("Identificación Política", 7, 12) %>%
  pack_rows("Ingresos Subjetivos", 13, 16) %>%
  pack_rows("Religion", 17, 20) %>%
  pack_rows("Tipo de Ciudad", 21, 24) %>%
  pack_rows("Clase Social", 25, 27)



######


datos <- readRDS("ipo/input/Datos/data.rds")

datos$Q222 <- factor(datos$Q222, levels = c(1,2,3), labels = c("Vota siempre",
                                                         "Vota a veces",
                                                         "Vota Nunca"))

datos$clase <- factor(datos$clase, levels = c(1,2,3,4), 
                      labels = c("Individualismo Autoritario",
                                 "Individualismo Conservador",
                                 "Individualismo Liberal",
                                 "Individualismo Agéntico"))

datos %>% filter(Q222!=is.na(Q222)) %>%
  group_by(clase, Q222) %>% summarise(prop=n()) %>%
  mutate(prop= prop/sum(prop)*100)  %>%
  ggplot(aes(x=clase, y=prop, fill= Q222)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Participación") +
  scale_fill_manual(values = c("#92C5DE", "#DEDB92", "#DE929A")) +
  theme_minimal() +
  coord_flip()




