# Cargar librerías

library(tidyverse)
library(psych)
library(poLCA)
library(gridExtra)
library(broom)
library(purrr)

# Cargar base de datos

df <- readRDS("ipo/input/Datos/df.rds")

# Subset y recodicación

df2 <- df %>% dplyr::select(c(D_INTERVIEW, Q27, Q48, Q109, Q182, Q185, 
                              Q186, Q246, Q247, Q249, Q257, Q150, Q177, Q178)) %>%
  mutate(across(c(Q182, Q185, Q186, Q246, Q247, Q249, Q177, Q178, Q48),
                            ~ifelse(.>5, 1, 2)),
                     across(c(Q109), ~ifelse(.>5, 2, 1)),
         Q150= ifelse(Q150==2, 2, 1)) %>%
  na.omit() #713 casos válidos

psych::describe(df2)

#Específicar el modelo
f1 <- as.formula(cbind(Q109, Q177, Q178,
                       Q246, Q247, Q249,
                       Q182, Q185, Q186,
                       Q48, Q27, Q257, 
                       Q150)~1)

# Seleccionar modelos
set.seed(09051931)
LCA1 <- poLCA(f1, data=df2, nclass=1, nrep=10, na.rm=T)
LCA2 <- poLCA(f1, data=df2, nclass=2, nrep=10, na.rm=T)
LCA3 <- poLCA(f1, data=df2, nclass=3, nrep=10, na.rm=T)
LCA4 <- poLCA(f1, data=df2, nclass=4, nrep=10, na.rm=T, maxiter = 5000)
LCA5 <- poLCA(f1, data=df2, nclass=5, nrep=10, na.rm=T, maxiter= 5000)
LCA6 <- poLCA(f1, data=df2, nclass=6, nrep=10, na.rm=T, maxiter=10000)

fits <- list(LCA2, LCA3, LCA4, LCA5, LCA6)
aic <- purrr::map_dbl(fits, ~ .x$"aic")
bic <- purrr::map_dbl(fits, ~ .x$"bic")
lca_fits <- data.frame(n_class = 2:6, aic = aic, bic = bic)

saveRDS(df2, "ipo/input/Datos/df2.rds")
saveRDS(LCA1, "ipo/output/lca1")
saveRDS(LCA4, "ipo/output/lca4")
saveRDS(lca_fits, "ipo/output/fits" )
