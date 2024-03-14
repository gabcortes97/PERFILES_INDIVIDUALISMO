library(tidyverse)
library(labelled)

var_label(wvs$Q235) <- NULL
var_label(wvs$Q236) <- NULL

wvs <- wvs %>% mutate(across(c(Q235, Q236), ~case_when(.==1 ~ 4,
																										.==2 ~ 3,
																										.==3 ~ 2,
																										.==4 ~ 1))) %>%
	mutate(dd= (Q235+Q236)/2)

mean(wvs$dd, na.rm=T)

wvs$B_COUNTRY <- as_factor(wvs$B_COUNTRY)

wvs %>% group_by(B_COUNTRY) %>% 
	filter(B_COUNTRY== "Argentina" |
				 B_COUNTRY== "Bolivia"|
				 B_COUNTRY== "Brazil"|
				 B_COUNTRY== "Canada"|
				 B_COUNTRY== "Colombia"|
				 B_COUNTRY== "Chile"|
				 B_COUNTRY== "Ecuador"|
				 B_COUNTRY== "Guatemala"|
				 B_COUNTRY== "Mexico"|
				 B_COUNTRY== "Nicaragua"|
				 B_COUNTRY== "Peru"|
				 B_COUNTRY== "Uruguay"|
				 B_COUNTRY== "United States"|
				 B_COUNTRY== "Venezuela") %>%
	summarise(promedio= mean(dd, na.rm=T)) %>%
	ggplot(aes(x = reorder(B_COUNTRY, -promedio), y = promedio)) +
	geom_bar(stat = "identity", fill="#92c5de") +
	labs(x = "País",
			 y = "Promedio Apoyo a la Democracia Delegativa") +
	geom_text(aes(label = sprintf("%.2f", promedio)), hjust = -0.2, size = 3) +
	coord_flip() +
	theme_classic()

###########

datoscl <- datosl %>% filter(S003==152)

datoscl <- datoscl %>% mutate(across(c(E114, E115), ~case_when(.==1 ~ 4,
																											 .==2 ~ 3,
																											 .==3 ~ 2,
																											 .==4 ~ 1))) %>%
	mutate(dd= (E114+E115)/2)


datoscl %>% filter(S020!=1990) %>%
	group_by(S020) %>% summarise("Democracia Delegativa"=mean(dd, na.rm=T),
																 "Líder Fuerte"= mean(E114, na.rm=T),
																 Expertos= mean(E115, na.rm=T)) %>%
	gather(key = "variable", value = "valor", -S020) %>%
	ggplot(aes(x = as.factor(S020), y = valor, color = variable,
						 group=variable)) +
	geom_point() +
	geom_path() +
	labs(x = "Ola",
			 y = "Nivel de Apoyo") +
	theme_classic() +
	theme(legend.position = "bottom") +
	geom_text(aes(label = sprintf("%.2f", valor)), hjust = -0.2, size = 3) +
	labs(color= "Apoyo a")

