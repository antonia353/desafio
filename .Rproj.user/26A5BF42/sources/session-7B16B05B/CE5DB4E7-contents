
# Práctico 8: Análisis descriptivo bivariado ------------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(sjmisc,
               sjPlot,
               tidyverse,
               magrittr)

# 2. Cargar datos ---------------------------------------------------------

datos_proc <- readRDS("input/data/datos_proc.rds")


# 3. Explorar datos -------------------------------------------------------

sjPlot::view_df(datos_proc,
                encoding = "UTF-8")


# 4. Visualización --------------------------------------------------------


## a) Gráficos de dispersión -----------------------------------------------

#Con dos variables
datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_scatter(., tot_per, ing_pc)

#Con tercera variable categórica
datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_scatter(., tot_per, ing_pc, sexo)
 
#Incorporar recta de regresión (OLS)

datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_scatter(., tot_per, ing_pc, sexo,
               fit.grps = "lm", grid = TRUE)


## b) Frecuencias agrupadas para variables categóricas ---------------------

#Grafico de barras 
plot_grpfrq(datos_proc$sexo, datos_proc$o4,
            type = "bar", title = "Gráfico de barras")

#Barras con porcentaje y barra de totales
plot_xtab(datos_proc$sexo_edad_tramo, datos_proc$o4, title = "Gráfico de barras")


#Barras horizontales
plot_xtab(datos_proc$o4, datos_proc$sexo_edad_tramo, margin = "row", 
          bar.pos = "stack",
          title = "Gráfico de barras horizontales",
          show.summary = TRUE, coord.flip = TRUE)

#Barras para diversas variables con mismas categorías de respuesta

datos_proc %>% select(ocupacion, o2, o3, o4, o6) %>% 
  plot_stackfrq(., title = "Gráfico de barras proporcional")

#Escalas Likert
datos_proc %>% select("ocupacion","o2", "o3", "o4", "o6") %>% 
  sjPlot::plot_likert(., title = "Gráfico de escalas Likert")

#Proporciones cruzadas 
plot_gpt(datos_proc, o4, o6, sexo,
         shapes = c(15, 21), 
         title = "Gráfico de proporción agrupada")

#Gráfico de puntos 
plot_grpfrq(datos_proc$sexo, datos_proc$o4,
            title = "Gráfico de puntos",
            type = "dot")

#Gráfico de líneas
plot_grpfrq(datos_proc$sexo_edad_tramo, datos_proc$o4,
            title = "Gráfico de línea",
            type = "line")


## c) Variables categóricas y continuas ---------------------------------------


#Gráfico de cajas
plot_grpfrq(datos_proc$tot_per, datos_proc$sexo_edad_tramo,
            title = "Gráfico de caja",
            type = "boxplot")

#Incorporar tercera variable
plot_grpfrq(datos_proc$tot_per, datos_proc$sexo_edad_tramo, intr.var = datos_proc$o4, 
            title = "Gráfico de cajas",
            type = "box")

#Gráfico de violín
plot_grpfrq(datos_proc$tot_per, datos_proc$sexo_edad_tramo,
            title = "Gráfico de violín",
            type = "violin")



## 5. Tablas de contingencia --------------------------------------------------

sjt.xtab(datos_proc$sexo, datos_proc$o4,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8", 
         title = "Tabla de contingencia")

#Incorporar tercera variable
tab_stackfrq(as.data.frame(datos_proc %>% select(sexo, "o4", "o6")),
             value.labels=c('1'='Si', '2'='No'),
             encoding = "UTF-8",
             show.n = TRUE, show.total = T)


## 6. Test de independencia chi cuadrado -------------------------------------

data.frame(as_factor(sample(datos_proc$sexo, replace = TRUE)),
           as_factor(sample(datos_proc$o4, replace = TRUE)),
           as_factor(sample(datos_proc$o6, replace = TRUE))) %>% 
  sjp.chi2(., 
           title = "Gráfico de Chi2",
           axis.labels  = c("Ha trabajado alguna vez", "Busco empleo"))



## 7. Correlaciones -----------------------------------------------------------

datos_proc %>%
  select(ing_pc, tot_per) %>% 
  tab_corr(.,
           triangle = "lower",   
           title = "Tabla de correlación",
           encoding = "UTF-8")



# Test ANOVA --------------------------------------------------------------

sjp.aov1(datos_proc$ing_pc, datos_proc$sexo, title = "Anova")







