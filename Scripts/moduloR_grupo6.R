#cargar paquetes ----
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)

#Importacion de datos -----
balances_2014 <- read.xlsx("Data/proyecto_final/balances_2014.xlsx")
ciiu <- read.xlsx("Data/proyecto_final/ciiu.xlsx")

#Limpieza de datos ----
<<<<<<< HEAD
#imputamos los datos con información faltante 
=======
#impute los NAs
>>>>>>> 4e2793696d9f88d38b27347faf24da4d04bbfd24
balances_2014 <- balances_2014 %>% na.omit()

#exploracion de datos ----
summary(balances_2014)
str(balances_2014)
dim(balances_2014)

# creacion de tibble ----
# seleccion de variables
balances_tbl <- balances_2014 %>% select(nombre_cia, 
                                         situacion,
                                         tipo,
                                         pais,
                                         provincia,
                                         canton,
                                         ciudad,
                                         tamanio,
                                         ciiu4_nivel1,
                                         ciiu4_nivel6, 
                                         v345,
                                         v539,
                                         v498,
                                         v569,
                                         v698) 

#union de tabla
balances_tbl <- balances_tbl %>% left_join(ciiu, by = c("ciiu4_nivel1" = "CODIGO")) %>%
                                 left_join(ciiu, by = c("ciiu4_nivel6" = "CODIGO")) %>%
                                 view()

#renaming variables
balances_tbl <- balances_tbl %>% rename("Activos_Corrientes" = "v345",
                                        "Pasivos_Corrientes" = "v539",
                                        "Activos_NoCorrientes" = "v498",
                                        "Pasivos_NoCorrientes" = "v569",
                                        "Patrimonio" = "v698",
                                        "Actividad_Economica" = "DESCRIPCION.x",
                                        "Nivel1" = "NIVEL.x",
                                        "Subactividad" = "DESCRIPCION.y",
                                        "Nivel6" = "NIVEL.y")

#mutate para crear variables 
balances_tbl <- balances_tbl %>% mutate(Activo = Activos_Corrientes + Activos_NoCorrientes,
                                        Pasivo = Pasivos_Corrientes + Pasivos_NoCorrientes,
                                        Liquidez_corriente = Activos_Corrientes/Pasivos_Corrientes,
                                        Endeundamiento_activo = Pasivo / Activo,
                                        Endeudamiento_patrimonial = Pasivo / Patrimonio,
                                        Endeudamiento_del_activo_fijo = Patrimonio / Activos_NoCorrientes,
                                        Apalancamiento = Activo / Patrimonio) %>%
  view("final")

balances_tbl[sapply(balances_tbl, is.infinite)] <- NA #Reemplazo todos los inf que me dan por la division por 0
balances_tbl <- balances_tbl %>% na.omit() #vuelvo a omitir NAs


empresas <- as_tibble(balances_tbl)

<<<<<<< HEAD
=======
##Respuestas pregunta 2
## Empresas con mayor apalancamiento ----
Top10_A <- empresas %>% select(nombre_cia, 
                                   situacion,
                                   Actividad_Economica, 
                                   pais,
                                   ciudad,
                                   Apalancamiento) %>%
                            group_by(nombre_cia) %>%
                            summarise(Total_Apalancamiento = sum(Apalancamiento)) %>%
                            arrange(desc(Total_Apalancamiento)) %>%
                            head(n=10)

Top10_A %>% ggplot(aes(x = fct_reorder(nombre_cia, Total_Apalancamiento), y = Total_Apalancamiento, fill = nombre_cia)) +
            geom_col() +
            coord_flip() +
            labs(title = "TOP 10 Empresar con Apalancamiento mas alto", x = "Compania", y = "Apalancamiento") +
            theme(legend.position = 'none')

## graficas
##Liquidez y solvencia por situacion y provincia
empresas_plot3 <- empresas %>% group_by(provincia, situacion) %>%
                               summarise(Liquidez_corrienteT = sum(Liquidez_corriente),
                                         Endeudamiento_activoT = sum(Endeundamiento_activo),
                                         Endeudamiento_patrimonialT = sum(Endeudamiento_patrimonial),
                                         Endeudamiento_del_activo_fijoT = sum(Endeudamiento_del_activo_fijo),
                                         ApalancamientoT = sum(Apalancamiento)
                                         )

#por terminar
empresas_plot3 %>% ggplot(aes(x = provincia, y = Liquidez_corrienteT, fill = situacion)) +
                   geom_col() 
                                         
                            

>>>>>>> 4e2793696d9f88d38b27347faf24da4d04bbfd24
