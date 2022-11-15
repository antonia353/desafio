rm(list = ls())
# Análisis univariado -----------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
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

# Análisis ----------------------------------------------------------------


# Gráfico de correlación --------------------------------------------------

plot_scatter(data, 
             serv_nr_ds,
             traslado, 
             title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado")

# Incorporamos recta de regresión

plot_scatter(data, 
             serv_nr_ds,
             traslado, 
             title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado",
             fit.line = "lm") #Incorporamos recta de regresión


# Incorporamos correlación 

#Usamos el paquete grid para crear un objeto con el texto de la correlación
corre = grobTree(textGrob(paste("Pearson's r =", #Creamos un objeto que incorpora el texto "Pearson's r= "
                                round(cor(data$serv_nr_ds, data$traslado), 2)), #Y la correlación entre nuestras variables, redondeada con 2 dígitos
                          x = .80, y = .97), #Especificamos posición del texto
                 gp = gpar(fontsize = 8)) #Indicamos fuente del texto

plot_scatter(data, 
             serv_nr_ds,
             traslado, 
             title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado",
             fit.line = "lm") + #Incorporamos recta de regresión
  annotation_custom(corre) #Incorporamos correlación como texto en el gráfico

#Incorporamos tercera variable (sexo)

plot_scatter(data, 
             serv_nr_ds,
             traslado, 
             sexo,
             title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado",
             fit.line = "lm") #Incorporamos recta de regresión

# Separemos en dos gráficos

plot_scatter(data, 
             serv_nr_ds,
             traslado, 
             sexo, #Definimos tercera variable para agrupar (factor)
             title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado",
             fit.line = "lm", #Incorporamos recta de regresión
             grid = T) #Generamos un gráfico para cada sexo (variable que agrupa)

# Incorporar nuevo tema

plot_scatter(data, 
             serv_nr_ds,
             traslado, 
             sexo, #Definimos tercera variable para agrupar (factor)
             title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado",
             fit.line = "lm", #Incorporamos recta de regresión
             grid = T)+ #Generamos un gráfico para cada sexo (variable que agrupa) 
  theme_classic() #Modificamos el tema

# Incorporamos correlación 

plot_scatter(data, 
             serv_nr_ds,
             traslado, 
             sexo, #Definimos tercera variable para agrupar (factor)
             title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado",
             fit.line = "lm", #Incorporamos recta de regresión
             grid = T)+ #Generamos un gráfico para cada sexo (variable que agrupa) 
  theme_classic() + #Modificamos el tema
  stat_cor(p.accuracy = 0.001, #Incorporamos correlación con ggpubr
           r.accuracy = 0.001,
           label.x.npc = "left", #Especificamos posición horizontal
           label.y.npc = "top") #Especificamos posición vertical



# Tabla de correlación ----------------------------------------------------

tab_corr(data %>% select(serv_nr_ds, traslado),
         triangle = "lower",
         encoding = "UTF-8",
         title = "Correlación entre horas dedicadas al 
trabajo no remunerado y tiempo de traslado")


# Tablas de contingencia --------------------------------------------------

sjt.xtab(data$sexo, data$sat_eq,
         show.col.prc=TRUE,
         show.summary=F, 
         title = "Tabla de contingencia:
sexo y satisfacción con equilibrio trabajo-familia",
         encoding = "UTF-8")


# Tabla de frecuencias para Likert ----------------------------------------

tab_stackfrq(as.data.frame(data %>% select(starts_with("sat_"))),
             show.n = TRUE, 
             show.total = T,
             title = "Niveles de satisfacción con el uso del tiempo")


# Gráfico chi-cuadrado ------------------------------------------------------

data.frame(as_factor(sample(data$sat_econ, replace = TRUE)), #Transformamos variables
           as_factor(sample(data$sat_trab, replace = TRUE)), #para hacerlas compatibles
           as_factor(sample(data$sat_can_tl, replace = TRUE)), #con la función sjp.chi2
           as_factor(sample(data$sat_cal_tl, replace = TRUE)),
           as_factor(sample(data$sat_eq, replace = TRUE)),
           as_factor(sample(data$quintil, replace = TRUE))) %>% 
  sjp.chi2(., 
           title = "Test de Chi2 (p-valores)",
           axis.labels = c("Quintil", #Indicamos etiquetas eje Y
                           "Satisfacción con el trabajo",
                           "Satisfacción equilibrio familia-trabajo",
                           "Satisfacción con situación económica",
                           "Satisfacción cantidad tiempo libre",
                           "Satisfacción calidad tiempo libre")) +
  theme_sjplot2() 


# Test ANOVA --------------------------------------------------------------

sjp.aov1(data$serv_nr_ds, data$sexo, 
         title = "Test ANOVA para diferencias en tiempo de trabajo no remunerado según sexo",
         axis.title = "Tiempo dedicado al trabajo no remunerado")

sjp.aov1(data$serv_nr_ds, data$quintil, 
         title = "Test ANOVA para diferencias en tiempo de trabajo no remunerado según quintil",
         axis.title = "Tiempo dedicado al trabajo no remunerado", #Indicamos título eje X
         axis.labels = c("Primer quintil", #Indicamos etiquetas eje Y
                         "Segundo quintil",
                         "Tercer quintil",
                         "Cuarto quintil",
                         "Quinto quintil")) + 
  theme_sjplot2()

