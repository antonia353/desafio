
# 10.1 Regresiones lineales -----------------------------------------------


# Cargar paquetes ---------------------------------------------------------

pacman::p_load(sjPlot, 
               tidyverse, 
               srvyr,
               survey)

# Cargar datos ------------------------------------------------------------

datos <- readRDS("input/data/datos_proc.rds")


# Explorar datos ----------------------------------------------------------

names(datos)
head(datos)
sjPlot::view_df(datos,
                encoding = "UTF-8")


# Regresiones lineales ----------------------------------------------------


## Simples -----------------------------------------------------------------

### Modelo nulo -------------------------------------------------------------
#Sin ponderadores
modelo0_sin <- lm(ing_t_t ~ 1,
                  data = datos)
summary(modelo0_sin)

#Con ponderadores
modelo0 <- lm(ing_t_t ~ 1,
              data = datos, 
              weights = fact_cal_esi)
summary(modelo0)

#Comparar
summary(modelo0);summary(modelo0_sin)

### Un predictor continuo -----------------------------------------------------------
modelo1 <- lm(ing_t_t ~ edad,
              data = datos, 
              weights = fact_cal_esi)
summary(modelo1)


### Un predictor categórico ------------------------------------------------------------
modelo2_sin <- lm(ing_t_t ~ sexo,
                  data = datos, 
                  weights = fact_cal_esi)
summary(modelo2_sin)

#Transformar en factor
datos$sexo <-  forcats::as_factor(datos$sexo) #Recuerden que esto debería ir en el código de preparación

modelo2 <- lm(ing_t_t ~ sexo,
              data = datos, 
              weights = fact_cal_esi)

summary(modelo2)

datos$ciuo08 <-  forcats::as_factor(datos$ciuo08) 
datos$est_conyugal <-  forcats::as_factor(datos$est_conyugal)
# ¡No olviden que esto debe ir en el código de procesamiento!

modelo3 <- lm(ing_t_t ~ ciuo08,
              data = datos, 
              weights = fact_cal_esi)


summary(modelo3)

modelo4 <- lm(ing_t_t ~ est_conyugal,
              data = datos, 
              weights = fact_cal_esi)

summary(modelo4)

## Regresión lineal múltiple ------------------------------------------------------------

modelo5 <- lm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
              data = datos, 
              weights = fact_cal_esi)

summary(modelo5)

## Con glm() ---------------------------------------------------------------

modelo5_glm <- glm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
                   family = gaussian(link = "identity"), #Especificamos la regresión lineal
                   data = datos, 
                   weights = fact_cal_esi)
summary(modelo5_glm)


## Con survey (svyglm())--------------------------------------------------------------

#Crear objeto encuesta
esi_design <- as_survey_design(datos, 
                               ids = 1, 
                               weights = fact_cal_esi)

modelo5_survey <- svyglm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
                         family = gaussian(link = "identity"),
                         design = esi_design)
summary(modelo5_survey)



# Información de modelos --------------------------------------------------

#Información general
str(modelo5)
summary(modelo5)

#Coeficientes
modelo5$coefficients
modelo5$coefficients[2]
modelo5$coefficients["edad"]

str(summary(modelo5))

#Estadístico F y R2
summary(modelo5)$fstatistic
summary(modelo5)$r.squared

#Valores predichos
modelo5$fitted.values

get_model_data(modelo5, 
               type = "pred")

get_model_data(modelo5, 
               type = "pred", 
               terms = "sexo")

### Información del modelo con broom ----------------------------------------

print <- broom::augment(modelo5) 
tab_df(print)


# Visualización  ----------------------------------------------------------

#Ejemplo
sjPlot::tab_model(objeto_creado, 
                  show.ci= F/T,  # este argumento muestra los intervalos de confianza
                  show.p = F/T, #Este argumento muestra los valores p
                  show.obs = F/T, # Este argumento muestra las observaciones
                  title = "Título de la tabla a crear",
                  digits = 2, # muestra la cantidad de dígitos que tednrá la tabla
                  p.style = c("numeric", "stars", "numeric_stars", "scientific", "scientific_stars"), #cómo representa el pvalue 
                  encoding = "UTF-8",  # evita errores en caracteres
                  file = "output/figures/reg1_tab.doc") # guarda lo creado automáticamente



## Un modelo ---------------------------------------------------------------

sjPlot::tab_model(modelo0, 
                  show.ci=FALSE,  
                  encoding = "UTF-8", 
                  file = "output/figures/regnc_tab.doc")



## Más de un modelo --------------------------------------------------------

sjPlot::tab_model(list(modelo0, modelo1, modelo2), # los modelos estimados
                  show.ci=FALSE, # no mostrar intervalo de confianza (por defecto lo hace)
                  p.style = "stars", # asteriscos de significación estadística
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"), # etiquetas de modelos o variables dep.
                  string.pred = "Predictores", string.est = "β", # nombre predictores y símbolo beta en tabla
                  encoding =  "UTF-8")


### Personalización ---------------------------------------------------------

sjPlot::plot_model(modelo5, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Estimación de predictores", 
                   vline.color = "purple")



