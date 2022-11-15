#Desafio 

#Cargado de paquetes. 
pacman::p_load(tidyverse,
               haven,
               dplyr, 
               car,
               sjmisc,
               sjlabelled,
               sjPlot, 
               readxl,
               magrittr,
               srvyr)

#cargar base 

Data<- read_dta("input/Base de datos Full VI EME (extracto).dta")

view(Data)

#Procesamiento de datos. 


Macrozona<-Data %>% 
  mutate(regionmacro = case_when (region %in% c(1, 2, 3, 15)~ "Macrozona Norte",
                                  region %in% c(4, 5, 6, 7, 8, 16) ~ "Macrozona Central",
                                  region %in% c(9, 14, 10)~ "Macrozona sur",
                                  region %in% c(11, 12)~ "Macrozona Austral",
                                  TRUE ~ NA_character_),) %>% 
  mutate(CISE1 = case_when(CISE %in% c(0)~"cuenta propia",
                           CISE %in% c(1)~ "Empleador",
                           TRUE ~ NA_character_),)
view(Macrozona)

Obj_encuesta<- Macrozona %>% 
  as_survey_design(ids = Enc_rph,
                   weights = Factor_EME)
  
#Número de personas micro emprendedoras. 
Obj_encuesta %>% 
  group_by(regionmacro) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T), 
            total = survey_total(na.rm=T))%>% 
  mutate(per = prop*100)

#Tabla 
tabla<- Obj_encuesta %>% 
  group_by("Macrozona Norte", "Macrozona Central", "Macrozona sur", "Macrozona Austral") %>% 
  summarise(ganancia_prom = survey_mean(ganancia_final_mensual, vartype = "ci", na.rm = T)) %>% 
  ungroup()

head(tabla)


  