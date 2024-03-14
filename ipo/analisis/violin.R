library(tidyverse)

datos <- readRDS("ipo/input/Datos/datosLM.rds")

ggplot(datos, aes(x = clase, y = dd, fill = clase)) +
  geom_violin(alpha = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar",
               colour = "black", width = 0.2) +
  geom_text(stat = "summary", fun = mean,
            aes(label = paste("x =", round(..y.., 2))),
            vjust = -0.5, size = 2) +
  labs(x = "Clase",
       y = "Apoyo a la Democracia Delegativa") 
