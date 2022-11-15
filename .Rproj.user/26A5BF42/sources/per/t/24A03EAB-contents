rm(list = ls())
# Reporte con datos muestrales -----------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               knitr,
               kableExtra,
               srvyr, #para pesos muestrales
               ggpubr, #Para incorporar correlación en scatterplot (grid)
               grid, #Para incorporar correlación en scatterplot
               ggrepel) #Para evitar solapamientos en grafico
               


# Cargar datos ------------------------------------------------------------

data = readRDS("output/data/enut.rds")

# Explorar ----------------------------------------------------------------

names(data)
# id = identificador
# exp = Ponderador
# varstrat = Pseudo-estrato de varianza
# varunit = Pseudo-conglomerado de varianza
# sexo
# quintil (factor)
# serv_nr_ds = servicios de trabajo no remunerado en día de semana (horas)
# sat_econ = satisfacción con situación económica
# sat_trab = satisfacción con trabajo
# sat_can_tl = satisfacción cantidad de tiempo libre
# sat_cal_tl = satisfacción calidad de tiempo libre
# traslado = al trabajo (horas)

# 3. Crear objeto encuesta ------------------------------------------------

objeto_encuesta <- data %>% 
  as_survey_design(ids = id, #el identificador
                   strata = varstrat,#los estratos estimados según varstrat
                   weights = exp) # el ponderador con exp


# Tablas ------------------------------------------------------------------


## formato script --------------------------------------------------------
#univariado
objeto_encuesta %>% 
  group_by(sexo)%>% 
  summarise(porcentaje = survey_mean(vartype = "ci", na.rm = T))%>% 
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) %>% 
  kable(caption = "Distribución de sexo", #Incorporamos título
        format = "pipe", #Especificamos que se muestre en la consola
        col.names = c("Variable", #Definimos los nombres de las columnas
                      "Prc %",
                      "Prc % límite inferior",
                      "Prc % límite superior")) 
#bivariado
objeto_encuesta %>% 
  group_by(sexo) %>% 
  summarise(porcentaje = survey_total(traslado, vartype = "ci", na.rm = T))%>% 
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) %>% 
  kable(caption = "Distribución de las horas de traslado", #Incorporamos título
        format = "pipe", #Especificamos que se muestre en la consola
        col.names = c("Variable", #Definimos los nombres de las columnas
                      "Frecuencia absoluta",
                      "Frec. límite inferior",
                      "Frec. límite superior")) 


## formato Rmd -----------------------------------------------------------
#univariado
objeto_encuesta %>% 
  group_by(sexo)%>% 
  summarise(porcentaje = survey_mean(vartype = "ci", na.rm = T))%>% 
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) %>% 
  kable(caption = "Distribución de sexo y quintil", #Definimos título
      format = "html", #Especificamos que el output se muestre en HTML
      col.names = c("Variable", #Definimos los nombres de las columnas
                    "Prc %",
                    "Prc % límite inferior",
                    "Prc % límite superior"),
      position = "center") %>% #Especificamos que la tabla se muestre al centro
  kable_classic(full_width = F, #Especificamos que el ancho de la tabla no se ajuste al de la página
                html_font = "Cambria") %>% #Definimos fuente
  footnote("Elaboración propia en base a ENUT (2015)", #Especificamos la nota al pie
           general_title = "Fuente: ") #Personalizamos nota al pie

#bivariado
objeto_encuesta %>% 
  group_by(sexo) %>% 
  summarise(porcentaje = survey_total(traslado, vartype = "ci", na.rm = T))%>% 
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) %>% 
  kable(caption = "Distribución de sexo y quintil", #Definimos título
        format = "html", #Especificamos que el output se muestre en HTML
        col.names = c("Variable", #Definimos los nombres de las columnas
                      "Prc %",
                      "Prc % límite inferior",
                      "Prc % límite superior"),
        position = "center") %>% #Especificamos que la tabla se muestre al centro
  kable_classic(full_width = F, #Especificamos que el ancho de la tabla no se ajuste al de la página
                html_font = "Cambria") %>% #Definimos fuente
  footnote("Elaboración propia en base a ENUT (2015)", #Especificamos la nota al pie
           general_title = "Fuente: ") #Personalizamos nota al pie

# asignación a objeto

#univariado

tabla1 <- objeto_encuesta %>% 
  group_by(sexo)%>% 
  summarise(porcentaje = survey_mean(vartype = "ci", na.rm = T))%>% 
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) 

#bivariado 

tabla2 <- objeto_encuesta %>% 
  group_by(sexo) %>% 
  summarise(porcentaje = survey_total(traslado, vartype = "ci", na.rm = T))%>% 
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) 


# Con sjPlot --------------------------------------------------------------

#univariado

tab_df(tabla1,
       title = 'Tabla 1. Distribución de Sexo',
       footnote = 'Elaboración propia en base a ENUT (2015)',
       show.footnote = T,
       encoding = 'latin9')


#bivariado
tab_df(tabla2,
       title = 'Tabla 2. Distribución horas de traslado según sexo',
       footnote = 'Elaboración propia en base a ENUT (2015)',
       show.footnote = T,
       encoding = 'latin9')


## Gráficos con sjPlot -----------------------------------------------------

#univariado
plot_frq(objeto_encuesta$variables$quintil,
         type = c("bar"), 
         title = "Gráfico de: Distribución de quintiles", 
         show.ci = T, 
         coord.flip = T, 
         geom.colors = "violet", 
         weight.by = objeto_encuesta$variables$exp) + 
  theme_classic() #Modificamos el tema

#bivariado
plot_xtab(objeto_encuesta$variables$quintil, objeto_encuesta$variables$sexo, 
          margin = "row", 
          bar.pos = "stack",
          title = "Gráfico de: Distribución de quintiles según sexo", 
          coord.flip = F, 
          weight.by = objeto_encuesta$variables$exp)

# Tablas de contingencia --------------------------------------------------

sjt.xtab(objeto_encuesta$variables$sexo, objeto_encuesta$variables$sat_eq,
         show.col.prc=TRUE,
         show.summary=F, 
         title = "Tabla de contingencia:
sexo y satisfacción con equilibrio trabajo-familia",
         encoding = "UTF-8",
         weight.by = objeto_encuesta$variables$exp)


# Tabla de frecuencias para Likert ----------------------------------------

tab_stackfrq(as.data.frame(objeto_encuesta %>% select(starts_with("sat_"))),
             show.n = TRUE, 
             show.total = T,
             title = "Niveles de satisfacción con el uso del tiempo",  
             weight.by = objeto_encuesta$variables$exp)


