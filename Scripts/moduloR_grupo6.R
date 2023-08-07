#Diana Chaglla
#Natasha Calle
#Lissette Pita

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
  arrange(desc(Apalancamiento)) %>%
  head(n=10)

Top10_A %>% ggplot(aes(x = fct_reorder(Compania, Apalancamiento), y = Apalancamiento, fill = Compania)) +
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
balances_tblN <- balances_2014 %>% select(nombre_cia, 
                                          situacion,
                                          tipo,
                                          tamanio,
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

balances_tblN <- balances_tblN %>% left_join(ciiu, by = c("ciiu4_nivel1" = "CODIGO")) %>%
  left_join(ciiu, by = c("ciiu4_nivel6" = "CODIGO")) 

balances_tblN <- balances_tblN %>% rename("Activos_Corrientes" = "v345",
                                          "Pasivos_Corrientes" = "v539",
                                          "Activos_NoCorrientes" = "v498",
                                          "Pasivos_NoCorrientes" = "v569",
                                          "Patrimonio" = "v698",
                                          "Actividad_Economica" = "DESCRIPCION.x",
                                          "Nivel1" = "NIVEL.x",
                                          "Subactividad" = "DESCRIPCION.y",
                                          "Nivel6" = "NIVEL.y",
                                          "Status" = "situacion", 
                                          "Compania" = "nombre_cia",
                                          "Volumen" = "tamanio")

balances_tblN <- balances_tblN %>% mutate(Activo = Activos_Corrientes + Activos_NoCorrientes,
                                          Pasivo = Pasivos_Corrientes + Pasivos_NoCorrientes,
                                          Liquidez_corriente = Activos_Corrientes/Pasivos_Corrientes,
                                          Endeundamiento_activo = Pasivo / Activo,
                                          Endeudamiento_patrimonial = Pasivo / Patrimonio,
                                          Endeudamiento_del_activo_fijo = Patrimonio / Activos_NoCorrientes,
                                          Apalancamiento = Activo / Patrimonio)

balances_tblN[sapply(balances_tblN, is.infinite)] <- NA #Reemplazo todos los inf que me dan por la division por 0
balances_tblN <- balances_tblN %>% na.omit() #vuelvo a omitir NAs

empresasN <- as_tibble(balances_tblN)

# RESPUESTA PREGUNTA 1 ¿EL ENDEUDAMIENTO DEL ACTIVO FUE MAYOR EN EMPRESAS MICRO + PEQUEÑAS VS GRANDES?
# creación de tabla 

endeudamiento_tbl <- balances_tblN %>% select(Compania, 
                                              Volumen,
                                              canton,
                                              Endeudamiento_del_activo_fijo) %>%
  group_by(Volumen)%>%
  summarise(Endeudamiento_del_activo_fijo = sum(Endeudamiento_del_activo_fijo) 
  )

# Respuesta Item 3, consulta 1 - Crear una tabla resumida número de empresas por actividad economica y por cantón 
# Creación de tabla 

Empresas_actividad_economica <- empresasN %>% group_by(Actividad_Economica, canton) %>%
  count() %>% rename("Cantidad" = "n") 

#DISEÑO DEL GRAFICO 
Empresas_actividad_economica %>% ggplot(aes(x = canton, y = Cantidad, fill = Actividad_Economica)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  labs(title = "NÚMERO DE EMPRESAS POR ACTIVIDAD ECONOMICA Y CANTÓN", x= "CANTON", y="CANTIDAD") + 
  theme(legend.position = "bottom", axis.text.y = element_text(size = 5), legend.text = element_text(size = 5)) +
  guides(fill = guide_legend(ncol = 2)) 

#DISEÑO DEL GRAFICO 
endeudamiento_tbl %>% ggplot(aes(x = Volumen, y = Endeudamiento_del_activo_fijo, fill = Volumen)) +
  geom_bar(stat="identity") + 
  labs(title = "NIVEL DE ENDEUDAMIENTO DEL ACTIVO CLASIFICADO POR EL TAMAÑOS DE LA EMPRESA", x= "TAMAÑO DE EMPRESA", y="NIVEL DE ENDEUDAMIENTO") + 
  theme(legend.position = "none") 

## Lissette Pita codigo ----
balances_tblL <- balances_2014 %>% select(nombre_cia, 
                                          situacion,
                                          tipo,
                                          pais,
                                          provincia,
                                          canton,
                                          ciudad,
                                          trab_direc,
                                          trab_admin,
                                          ciiu4_nivel1,
                                          ciiu4_nivel6, 
                                          v345,
                                          v539,
                                          v498,
                                          v569,
                                          v698) 

#renaming variables
balances_tblL <- balances_tblL %>% rename("Activos_Corrientes" = "v345",
                                          "Pasivos_Corrientes" = "v539",
                                          "Activos_NoCorrientes" = "v498",
                                          "Pasivos_NoCorrientes" = "v569",
                                          "Patrimonio" = "v698",
                                          "Status" = "situacion", 
                                          "Compania" = "nombre_cia")

#mutate para crear variables 
balances_tblL <- balances_tblL %>% mutate(Activo = Activos_Corrientes + Activos_NoCorrientes,
                                          Pasivo = Pasivos_Corrientes + Pasivos_NoCorrientes,
                                          Liquidez_corriente = Activos_Corrientes/Pasivos_Corrientes,
                                          Endeundamiento_activo = Pasivo / Activo,
                                          Endeudamiento_patrimonial = Pasivo / Patrimonio,
                                          Endeudamiento_del_activo_fijo = Patrimonio / Activos_NoCorrientes,
                                          Apalancamiento = Activo / Patrimonio) 

balances_tblL[sapply(balances_tblL, is.infinite)] <- NA #Reemplazo todos los inf que me dan por la division por 0
balances_tblL <- balances_tblL %>% na.omit() #vuelvo a omitir NAs


empresasL <- as_tibble(balances_tblL)

# Agrupo y calculo la liquidez corriente para cada compañía
empresas_com <- empresasL %>% 
  group_by(Compania) %>%
  summarise(Liquidez_corriente = sum(Liquidez_corriente, na.rm = TRUE),
            Total_trabajadores_directos = sum(trab_direc),
            Total_trabajadores_administrativos = sum(trab_admin)) 


#Comparo las empresa como trabajadores 
empresas_mayor_60_directos <- empresas_com %>% filter(Total_trabajadores_directos > 60)

empresas_100_800_administrativos <- empresas_com %>%
  filter(Total_trabajadores_administrativos >= 100 & Total_trabajadores_administrativos <= 800)

# Gráfico de barras para la liquidez
ggplot(empresasL, aes(x = tipo, y = Liquidez_corriente)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") + 
  labs(title = "Comparativo de Liquidez por Tipo de Empresa",
       x = "Tipo de Empresa",
       y = "Liquidez Corriente") +
  theme(axis.text.x = element_text(size=5,angle = 45, hjust = 1))

# Gráfico de barras para la solvencia
ggplot(empresasL, aes(x = tipo, y = Endeundamiento_activo)) +
  geom_bar(stat = "identity", position = "dodge", fill = "red") +
  labs(title = "Comparativo de Solvencia por Tipo de Empresa",
       x = "Tipo de Empresa",
       y = "Endeudamiento del Activo") +
  theme(axis.text.x = element_text(size=5,angle = 45, hjust = 1))

#relación entre los indicadores de liquidez y solvencia para cada empresa

ggplot(empresasL, aes(x = Liquidez_corriente, y = Endeundamiento_activo, color = tipo)) +
  geom_point() + labs(title = "Comparativo de Liquidez y Solvencia por Tipo de Empresa",
                      x = "Liquidez Corriente",
                      y = "Endeudamiento del Activo") +
  theme(legend.position = "bottom")


