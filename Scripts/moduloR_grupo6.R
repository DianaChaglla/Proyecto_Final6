#cargar paquetes ----
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(patchwork)

#Importacion de datos -----
balances_2014 <- read.xlsx("Data/proyecto_final/balances_2014.xlsx")
ciiu <- read.xlsx("Data/proyecto_final/ciiu.xlsx")

#Limpieza de datos ----
#impute los NAs
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
                                        "Nivel6" = "NIVEL.y",
                                        "Status" = "situacion", 
                                        "Compania" = "nombre_cia")

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

##Respuestas pregunta 2
## Empresas con mayor apalancamiento ----
Top10_A <- empresas %>% select(Compania, 
                               Status,
                               Actividad_Economica, 
                               pais,
                               ciudad,
                               Apalancamiento) %>%
  group_by(Compania) %>%
  summarise(Total_Apalancamiento = sum(Apalancamiento)) %>%
  arrange(desc(Total_Apalancamiento)) %>%
  head(n=10)

Top10_A %>% ggplot(aes(x = fct_reorder(Compania, Total_Apalancamiento), y = Total_Apalancamiento, fill = Compania)) +
  geom_col() +
  coord_flip() +
  labs(title = "TOP 10 Empresas con Apalancamiento mas alto", x = "Compania", y = "Apalancamiento") +
  theme(legend.position = 'none')

## graficas

LC <- empresas %>% ggplot(aes(x = provincia, y = Liquidez_corriente, fill = Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Liquidez Corriente por Status y Provincia",
       x = "Provincia", y = "Liquidez Corriente") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))
LC

EA <- empresas %>% ggplot(aes(x = provincia, y = Endeundamiento_activo, fill = Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Endeudamiento del Activo por Status y Provincia",
       x = "Provincia", y = "Endeudamiento del Activo") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))
EA

EP <- empresas %>% ggplot(aes(x = provincia, y = Endeudamiento_patrimonial, fill = Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Endeudamiento Patrimonial por Status y Provincia",
       x = "Provincia", y = "Endeudamiento Patrimonial") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))
EP

EP <- empresas %>% ggplot(aes(x = provincia, y = Endeudamiento_patrimonial, fill = Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Endeudamiento Patrimonial por Status y Provincia",
       x = "Provincia", y = "Endeudamiento Patrimonial") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))
EP

EAF <- empresas %>% ggplot(aes(x = provincia, y = Endeudamiento_del_activo_fijo, fill = Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Endeudamiento del Activo Fijo por Status y Provincia",
       x = "Provincia", y = "Endeudamiento del Activo Fijo") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))
EAF

A <- empresas %>% ggplot(aes(x = provincia, y = Apalancamiento, fill = Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Apalancamiento por Status y Provincia",
       x = "Provincia", y = "Apalancamiento") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=5,angle = 45, hjust = 1))
A

## Natasha Calle ----