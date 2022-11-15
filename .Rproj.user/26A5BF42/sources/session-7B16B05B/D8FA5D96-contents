
# Código de procesamiento -------------------------------------------------


# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               haven,
               sjmisc,
               sjlabelled)

# Cargar datos ------------------------------------------------------------

# Deben descargar la base de datos en formato .dta del siguiente link
# https://www.ine.cl/estadisticas/sociales/genero/uso-del-tiempo
# luego, descomprimir y pegar en carpeta output/data

data = read_dta("input/data/BASE_USUARIO_corregida.dta")

# Explorar ----------------------------------------------------------------

find_var(data, "m12") #Tiempo de traslado al trabajo
# m12_1_1 (ida durante semana) y m12_1_2 (vuelta durante semana)

find_var(data, "quintil") #Quintil de ingreso

find_var(data, "cise_5") #CISE para filtrar

find_var(data, "serv_nr_ds") #Total de trabajo no remunerado dia semana

find_var(data, "t12") #Satisfacción con
#t12_1_1: situación económica
#t12_1_2: trabajo
#t12_1_3: cantidad de tiempo libre
#t12_1_4: calidad de tiempo libre
#t12_1_5: equilibrio entre trabajo y vida familiar

find_var(data, "Sexo") #c13_1_1
find_var(data, "pet") #Población en Edad de trabajar

find_var(data, "Var") #VarStrat = pseudo estratos
# VarUnit = pseudo-conglomerados

# Procesamiento de datos --------------------------------------------------

data = data %>% 
  filter(pet == 1 & #Población en edad de trabajar
           cise_5 %in% c(1, 2, 3, 4, 96)) %>% #Personas ocupadas
  select(id = id_persona, #Identificador
         exp = wgt2, #Ponderador de personas cuestionario uso del tiepmo
         varstrat = VarStrat, #Pseudo-estrato de varianza
         varunit = VarUnit, #Pseudo-conglomerado de varianza
         sexo = c13_1_1, #Sexo
         quintil, #Quintil
         traslado_i = m12_1_1, #Tiempo de traslado ida al trabajo
         traslado_r = m12_1_2, #Tiempo de traslado regreso del trabajo
         serv_nr_ds, #Servicios no remunerados en hogar día de semana
         sat_econ = t12_1_1, #Satisfacción con situación económica
         sat_trab = t12_1_2, #Satisfacción con trabajo
         sat_can_tl = t12_1_3, #Satisfacción cantidad tiempo libre
         sat_cal_tl = t12_1_4, #Satisfacción calidad tiempo libre
         sat_eq = t12_1_5) %>% #Satisfacción equilibrio trabajo y familia
  mutate_all(~(as.numeric(.))) %>% 
  mutate_at(vars(starts_with("traslado_|sat_")), ~(car::recode(., "96 = NA"))) %>% 
  mutate_at(vars(starts_with("sat_")), ~(car::recode(.,
                                                     recodes = c("1 = 'Totalmente insatisf.';
                                                                 2 = 'Insatisf.';
                                                                 3 = 'Ni satisf./a ni insatisf.';
                                                                 4 = 'Satisf.';
                                                                 5 = 'Totalmente satisf.';
                                                                 85 = NA"), as.factor = T,
                                                     levels = c('Totalmente insatisf.',
                                                                'Insatisf.',
                                                                'Ni satisf. ni insatisf.',
                                                                'Satisf.',
                                                                'Totalmente satisf.')))) %>% 
  mutate(sexo = car::recode(.$sexo,
                            c("1 = 'Hombre';
                              2 = 'Mujer'"), as.factor = T),
         quintil = forcats::as_factor(.$quintil)) %>% 
  rowwise() %>% 
  mutate(traslado = sum(traslado_i, traslado_r, na.rm = T)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  select(-c(traslado_i, traslado_r)) %>% 
  filter(traslado <= 24)


# Etiquetar variables -----------------------------------------------------

data$id <- set_label(data$id, "Identificador")
data$exp <- set_label(data$exp, "Ponderador")
data$varstrat <- set_label(data$varstrat, "Pseudo-estrato de varianza")
data$varunit <- set_label(data$varunit, "Pseudo-conglomerado de varianza")
data$sexo = set_label(data$sexo, "Sexo")
data$quintil = set_label(data$quintil, "Quintil de ingreso")
data$serv_nr_ds = set_label(data$serv_nr_ds, "Servicios de trabajo no remunerado (semana)")
data$sat_econ = set_label(data$sat_econ, "Satisfacción con situación económica")
data$sat_trab = set_label(data$sat_trab, "Satisfacción con el trabajo")
data$sat_can_tl = set_label(data$sat_can_tl, "Satisfacción con cantidad de tiempo libre")
data$sat_cal_tl = set_label(data$sat_cal_tl, "Satisfacción con calidad de tiempo libre")
data$sat_eq = set_label(data$sat_eq, "Satisfacción con equilibrio trabajo-familia")
data$traslado = set_label(data$traslado, "Tiempo de traslado al trabajo en semana (horas)")


# Exportar data -----------------------------------------------------------

saveRDS(data, "output/data/enut.rds")

