# ===============================================
# Taller 
#================================================
# Contenido 
# Parte 1. Ruta de carpeta
# Parte 2. Importacion de data
# Parte 3. Desarrollo de preguntas


# ===============================================
# Parte 1
# ===============================================
ruta <- "C:\\Users\\et396\\Dropbox\\Docencia\\Educate\\Econometria\\S1\\Data"
setwd(ruta)

# ===============================================
# Parte 2
# ===============================================
# Librerias
library(dplyr)
library(readstata13)
library(stargazer)
library(ggplot2)
#library(lm_robust)
library(robust)
library(MASS)

# Importacion de datos
base = read.dta13("Mincer_2021.dta")


# ===============================================
# Parte 3
# ===============================================
base %>% dim()
base %>% summary()
base <- base %>% mutate(rdpto = factor(rDpto) )

# Pregunta 1
# *****************************
m1 <- lm(lnr6 ~ reduca, data = base)
m2 <- lm(lnr6 ~ reduca + rmujer + redad + redadsq + rpareja, data = base)
m3 <- lm(lnr6 ~ reduca + rmujer + redad + redadsq + rpareja + rdpto, data = base)

stargazer(m1, m2, m3, title="Results", type="text")
stargazer(m1, m2, m3, title="Results", type="text", keep = c("reduca", "rmujer"))

# Grafico del Residuos
base$rerror <- m2$resid

(
  ggplot(base, aes(x=rerror)) + geom_density() + theme_bw()
)

(
  ggplot(base, aes(y=rerror)) + geom_boxplot(outliers = FALSE) + theme_bw()
)

# Estimacion de los residuos
base$rerrorsq <- (base$rerror)^2
m2_error <- lm(rerrorsq ~ reduca + rmujer + redad + redadsq + rpareja, data = base)
stargazer(m2_error , 
          title="Results", 
          type="text", 
          keep = c("reduca", "rmujer", "redad", "redadsq", "rpareja"))

# Pregunta 2
# *****************************
m4 <- rlm(lnr6 ~ reduca + rmujer + redad + redadsq + rpareja, data = base)
summary(m4)


# Pregunta 3
# *****************************
stargazer( m4 , 
           title="Results", 
           type="text", 
           keep = c("reduca", "rmujer", "redad", "redadsq", "rpareja"))

# Matriz de varianzas y covarianzas
vcov(m4)

# Calculo de la varianza no lineal de parametros
# Metodo Delta
beta2 <- -(1/(2*m4$coefficients['redadsq']))
beta3 <- (m4$coefficients['redad'])/(2*(m4$coefficients['redadsq']^2))
beta2_sq <- beta2^2
beta3_sq <- beta3^2

var_beta2       = vcov(m4)[4,4]
var_beta3       = vcov(m4)[5,5]
cov_beta2_beta3 = vcov(m4)[4,5]

var_delta = (beta2_sq*var_beta2) + (beta3_sq*var_beta3) + (2*beta2*beta3*cov_beta2_beta3)
var_delta

