#cargar paquetes ----
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyverse)
library(readxl)

#Importacion de datos -----
balances_2014 <- read.xlsx("Data/proyecto_final/balances_2014.xlsx")
ciiu <- read.xlsx("Data/proyecto_final/ciiu.xlsx")

#Limpieza de datos ----
#reemplazar NA con 0
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
                                         canton,
                                         ciudad,
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

empresas <- as_tibble(balances_tbl)
