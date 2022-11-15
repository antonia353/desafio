rm(list = ls())
# Análisis univariado -----------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               ggrepel, #Para evitar solapamientos en grafico
               kableExtra) #Para tablas


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


# Tablas de frecuencias con frq() -----------------------------------------

frq(data$sexo)

# Personalicemos la tabla

frq(data$sexo,
    title = "Distribución de la variable sexo",
    show.na = F,
    out = "viewer",
    encoding = "UTF-8",
    sort.frq = "asc") 

# Exportamos

frq(data$sexo,
    title = "Distribucion de la variable sexo",
    show.na = F,
    encoding = "latin9",
    out = "viewer",
    sort.frq = "asc",
    file = "output/fig/frq_sexo.doc") 

# Con kable

frq(data$sexo,
    show.na = F,
    out = "viewer",
    encoding = "UTF-8",
    sort.frq = "asc") %>% 
  kable(caption = "Distribución de sexo", #Incorporamos título
        format = "pipe", #Especificamos que se muestre en la consola
        col.names = c("Valor", #Definimos los nombres de las columnas
                      "Etiqueta",
                      "F. absoluta",
                      "F. relativa",
                      "F. relativa (valida)",
                      "F. rel. acumulada"))

# Con kable y dplyr

data %>%
  select(sexo, quintil) %>% #Seleccionamos las variables de interés
  frq(show.na = F,
      out = "viewer",
      encoding = "UTF-8",
      sort.frq = "asc") %>% 
  kable(caption = "Distribución de sexo y quintil", #Asignamos título
        format = "pipe", #Especificamos que se muestre en la consola
        col.names = c("Valor",  #Definimos nombres de columnas
                      "Etiqueta",
                      "F. absoluta",
                      "F. relativa",
                      "F. relativa (valida)",
                      "F. rel. acumulada"))

# Con kableExtra

data %>%
  select(sexo, quintil) %>% #Seleccionamos variables
  frq(show.na = F,
      out = "viewer",
      encoding = "UTF-8",
      sort.frq = "asc") %>% 
  kable(caption = "Distribución de sexo y quintil", #Definimos título
        format = "html", #Especificamos que el output se muestre en HTML
        col.names = c("Valor", #Asignamos nombres a las columnas
                      "Etiqueta",
                      "F. absoluta",
                      "F. relativa",
                      "F. relativa (valida)",
                      "F. rel. acumulada"),
        position = "center") %>% #Especificamos que la tabla se muestre al centro
  kable_classic(full_width = F, #Especificamos que el ancho de la tabla no se ajuste al de la página
                html_font = "Cambria") %>% #Definimos fuente
  footnote("Elaboración propia en base a ENUT (2015)", #Especificamos la nota al pie
           general_title = "Fuente: ") #Personalizamos nota al pie

# Con dplyr, kable y kableExtra

frq(data$sexo,
    show.na = F,
    out = "viewer",
    encoding = "UTF-8",
    sort.frq = "asc") %>% 
  as.data.frame() %>% #Transformamos en dataframe para manipular con dplyr
  select(val, frq, raw.prc, cum.prc) %>% #Seleccionamos columnas de interés
  mutate_at(vars(ends_with("prc")), #A las variables que terminen con "prc"
                 ~(paste(., "%"))) %>% #Incorporamos porcentaje con paste()
  kable(caption = "Distribución de sexo", #Incorporamos título
        format = "html", #Especificamos output en HTML
        col.names = c("Sexo", #Definimos el nombre de las columnas
                      "F. absoluta",
                      "F. relativa",
                      "F. rel. acumulada"),
        position = "center") %>% #Especificamos que la tabla se muestre al centro
  kable_classic(full_width = F, #Especificamos que el ancho de la tabla no se ajuste al de la página
                html_font = "Cambria") %>% #Definimos fuente
  footnote("Elaboración propia en base a ENUT (2015)", #Especificamos la nota al pie
           general_title = "Fuente: ") #Personalizamos nota al pie


# Tabla de MTC con descr() ------------------------------------------------

descr(data$traslado)

# Personalicemos la tabla

descr(data$traslado,
      show = c("label", "n", "mean", "sd", "md", "range"))

# Con kable

descr(data$traslado,
      show = c("label", "n", "mean", "sd", "md", "range")) %>% 
  kable(format = "pipe") #Especificamos que la tabla se muestre en la consola

# Presentemos dos variables

data %>% 
  select(traslado, serv_nr_ds) %>% #Seleccionamos más de una variable
  descr(show = c("label", "n", "mean", "sd", "md", "range"))
  
# Exportamos

data %>% 
  select(traslado, serv_nr_ds) %>% #Seleccionamos más de una variable
  descr(show = c("label", "n", "mean", "sd", "md", "range"),
        out = "viewer",
        file = "output/fig/traslado_nr.doc")

# Con dplyr y kable

data %>% 
  select(traslado, serv_nr_ds) %>% #Seleccionamos más de una variable
  descr(show = c("label", "n", "mean", "sd", "md", "range")) %>% 
  kable(format = "pipe", #Especificamos que se muestre en la consola
        caption = "MTC para tiempo de traslado y de trabajo no remunerado", #Asignamos título
        col.names = c("Variable", #Asignamos nombres a las columnas
                      "Etiqueta",
                      "n", "Media",
                      "D. estandar",
                      "Mediana", "Rango"),
        position = "center") #Especificamos que la tabla se muestre al centro de la página

# Con kableExtra

data %>% 
  select(traslado, serv_nr_ds) %>% #Seleccionamos más de una variable
  descr(show = c("label", "n", "mean", "sd", "md", "range")) %>% 
  kable(format = "html", #Especificamos output en HTML
        caption = "MTC para tiempo de traslado y de trabajo no remunerado", #Asignamos título
        col.names = c("Variable", #Asignamos nombres a las columnas
                      "Etiqueta",
                      "n", "Media",
                      "D. estandar",
                      "Mediana", "Rango"),
        position = "center") %>% #Especificamos que la tabla se muestre al centro
  kable_classic(full_width = F, #Especificamos que el ancho de la tabla no se ajuste al de la página
                html_font = "Cambria") %>% #Definimos fuente
  footnote("Elaboración propia en base a ENUT (2015)", #Especificamos la nota al pie
           general_title = "Fuente: ") #Personalizamos nota al pie

# Con dplyr, kable y kable Extra

data %>% 
  select(traslado, serv_nr_ds) %>% #Seleccionamos más de una variable
  descr(show = c("label", "n", "mean", "sd", "md", "range")) %>% 
  as.data.frame() %>% #Transformamos en dataframe para manipular con dplyr
  mutate_at(vars(4, 5), ~(round(., digits = 3))) %>% #Redondeamos la tercera y cuarta variable al tercer decimal
  kable(format = "html", #Especificamos output en HTML
        caption = "MTC para tiempo de traslado y de trabajo no remunerado", #Asignamos título
        col.names = c("Variable", #Especificamos nombres de columnas
                      "Etiqueta",
                      "n", "Media",
                      "D. estandar",
                      "Mediana", "Rango"),
        position = "center") %>% #Especificamos que la tabla se muestre al centro
  kable_classic(full_width = F, #Especificamos que el ancho de la tabla no se ajuste al de la página
                html_font = "Cambria") %>% #Definimos fuente
  footnote("Elaboración propia en base a ENUT (2015)", #Especificamos la nota al pie
           general_title = "Fuente: ") #Personalizamos nota al pie

# Exportar manualmente desde el Viewer
# Export > Save as Image...
# Definir Directory la carpeta output/fig
# Dar un nombre al archivo (por ej., kable) 

# Likert ------------------------------------------------------------------

plot_likert(data %>% select(starts_with("sat_")), #Seleccionamos todas las variables que empiecen con "sat_
                            title = "Niveles de satisfacción con uso del tiempo", #Asignamos título
                            catcount = 5, #Definimos que la escala tiene 5 categorías
                            reverse.scale = T) + #Definimos que la escala de satisfacción se muestre de izq a derecha
  geom_text_repel() # Para no solapar etiquetas



