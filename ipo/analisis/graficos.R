library(tidyverse)

df <- readRDS("ipo/input/Datos/data.rds")
lca4 <- readRDS("ipo/output/lca4")
lca4

datos <- data.frame(clase= c("Autoritario",
                             "Conservador",
                             "Cívico",
                             "Agéntico"),
                    x= lca4$P,
                    se= lca4$P.se)
datos <- datos %>% mutate(LI= x-1.96*se,
                          LS= x+1.96*se,
                          IC= LS-x)

# Clases de individualismo con intervalos de confianza

g1 <- ggplot(datos, aes(x=clase, y=x)) + 
  geom_col(fill= "#92c5de", colour="#0571b0") +
 geom_errorbar(aes(ymin=LI, ymax=LS), width=0.2) +
  xlab("Clases de Individualismo") + ylab("Proporción") +
  theme_bw()

g1

df$clase <- factor(df$clase, levels = c(1,2,3,4), 
                   labels= c("Autoritario",
                             "Conservador",
                             "Cívico",
                             "Agéntico"))

# Género

df %>% group_by(clase, Q260) %>% summarise(genero= n()) %>%
  mutate(genero= genero/sum(genero)*100) %>%
  ggplot(aes(x=clase, y=genero, fill=factor(Q260))) +
geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(genero,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Género") +
  theme_minimal() +
  coord_flip() +
  scale_fill_discrete(labels= c("Hombre", "Mujer"))

# Edad

df %>% mutate(edad= case_when(Q262<30 ~ "Menores de 30",
                              Q262>=30 & Q262<45 ~ "30 a 44",
                              Q262>=45 & Q262<60 ~ "45 a 59",
                              Q262>=60 ~ "Mayores de 60")) %>%
  group_by(clase, edad) %>% summarise(prop= n()) %>%
  mutate(prop= prop/sum(prop)*100) %>%
  ggplot(aes(x=clase, y=prop, fill=factor(edad,
                                          levels= c("Mayores de 60",
                                                    "45 a 59",
                                                    "30 a 44",
                                                    "Menores de 30")))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Edad") +
  theme_minimal() +
  coord_flip()

# Región Metropolitana

df %>% mutate(region= ifelse(H_SETTLEMENT==1, "Metropolitana", "Otras")) %>%
  group_by(clase, region) %>% summarise(prop=n()) %>% 
  mutate(prop= prop/sum(prop)*100) %>%
  ggplot(aes(x=clase, y=prop, fill=factor(region))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Region") +
  theme_minimal() +
  coord_flip()

# Identificación Política

df %>% mutate(id_pol= case_when(Q240<3 & Q240>0~ "Izquierda",
                                Q240==3 | Q240==4 ~"Centro Izquierda",
                                Q240==5 ~ "Centro",
                                Q240>=6 & Q240<=8  ~"Centro Derecha",
                                Q240>8 ~"Derecha",
                                Q240<0 ~ "Ninguna")) %>%
  group_by(clase, id_pol) %>% summarise(prop=n()) %>% 
  mutate(prop= prop/sum(prop)*100) %>%
  ggplot(aes(x=clase, y=prop, fill=factor(id_pol,
                                          levels = c("Ninguna",
                                                     "Derecha",
                                                     "Centro Derecha",
                                                     "Centro",
                                                     "Centro Izquierda",
                                                     "Izquierda")))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Id. Política") +
  theme_minimal() +
  coord_flip()

# Ingresos Subjetivos

df %>% filter(Q288 !=is.na(Q288)) %>% 
  mutate(ingresos= case_when(Q288<4 ~ "Ingresos Bajos",
                             Q288==4 | Q288==5 ~ "Ingresos Medios-Bajos",
                             Q288==6 | Q288==7 ~ "Ingresos Medios-Altos",
                             Q288>=8 ~ "Ingresos Altos")) %>%
  group_by(clase, ingresos) %>% summarise(prop=n()) %>% 
  mutate(prop= prop/sum(prop)*100) %>%
  ggplot(aes(x=clase, y=prop, fill=factor(ingresos,
                                          levels = c("Ingresos Altos",
                                                     "Ingresos Medios-Altos",
                                                     "Ingresos Medios-Bajos",
                                                     "Ingresos Bajos")))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Ingresos Subjetivos") +
  theme_minimal() +
  coord_flip()

#Religión

df %>%
  mutate(religion= case_when(Q289== 0 ~ "Sin religión",
                             Q289== 1 ~ "Católica",
                             Q289==2 ~ "Evangélica",
                             Q289>2 ~ "Otra")) %>%
  filter(religion!=is.na(religion)) %>%
  group_by(clase, religion) %>% summarise(prop=n()) %>% 
  mutate(prop= prop/sum(prop)*100) %>%
  ggplot(aes(x=clase, y=prop, fill=factor(religion,
                                          levels = c("Otra",
                                                     "Sin religión",
                                                     "Evangélica",
                                                     "Católica")))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Religión") +
  theme_minimal() +
  coord_flip()

# Tipo de Ciudad

df %>% group_by(clase, G_TOWNSIZE2) %>% summarise(prop=n()) %>%
  mutate(prop= prop/sum(prop)*100) %>%
  ggplot(aes(x=clase, y=prop, fill=factor(G_TOWNSIZE2,
                                          levels= c(5,4,3,2,1),
                                          labels= c("Santiago",
                                                    ">100.000 hab",
                                                    "<100.000 hab",
                                                    "<20.000 hab",
                                                    "Rural")))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Tipo de Ciudad") +
  theme_minimal() +
  coord_flip()

# Clase Social

df %>% mutate(trabajo = case_when(Q281== 0 ~NA,
                                  Q281==1 | Q281==2 ~ "Clase de Servicio",
                                  Q281>=3 & Q281<=5 ~ "Clase Media",
                                  Q281>5 ~ "Clase Trabajadora")) %>%
  filter(trabajo!= is.na(trabajo)) %>%
  group_by(clase, trabajo) %>% summarise(prop= n()) %>%
  mutate(prop= prop/sum(prop)*100) %>%
  ggplot(aes(x=clase, y=prop, fill=factor(trabajo,
                                          labels= c("Clase de Servicios",
                                                    "Clases Medias",
                                                    "Clase Trabajadora")))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = round(prop,2)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(x = "Clase", y = "Proporción", fill = "Clase Social") +
  theme_minimal() +
  coord_flip()
  
  

