library(tidyverse)
library(magrittr)
library(ggthemes)

# ------------------
# Data preprocessing
# ------------------

setwd("~/Documents/CIMAT/DataScienceR/proyecto_covid19/")
covid.data = read_csv("data/200518COVID19MEXICO.csv")

entidad.catalogo = read_csv("data/Catalogo_entidades.csv") %>% select(-ABREVIATURA)
municipio.catalogo = read_csv("data/Catalogo_municipios.csv")
origen.catalogo = read_csv("data/Catalogo_origen.csv")
sector.catalogo = read_csv("data/Catalogo_sector.csv")
sexo.catalogo = read_csv("data/Catalogo_sexo.csv")
tipo.paciente.catalogo = read_csv("data/Catalogo_tipo_paciente.csv")
bool.catalogo = read_csv("data/Catalogo_si_no.csv")
nacionalidad.catalogo = read_csv("data/Catalogo_nacionalidad.csv")
resultado.catalogo = read_csv("data/Catalogo_resultado.csv")

# Expand "ENTIDAD_UM"
covid.data = covid.data %>% 
  inner_join(entidad.catalogo, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"), copy = FALSE) %>%
  select(-ENTIDAD_UM) %>%
  rename(ENTIDAD_UM = ENTIDAD_FEDERATIVA)

# Expand "ENTIDAD_NAC"
covid.data = covid.data %>% 
  inner_join(entidad.catalogo, by = c("ENTIDAD_NAC" = "CLAVE_ENTIDAD"), copy = FALSE) %>%
  select(-ENTIDAD_NAC) %>%
  rename(ENTIDAD_NAC = ENTIDAD_FEDERATIVA)

# Expand "MUNICIPIO_RES"
covid.data = covid.data %>% 
  inner_join(municipio.catalogo, by = c("MUNICIPIO_RES" = "CLAVE_MUNICIPIO", "ENTIDAD_RES" = "CLAVE_ENTIDAD"), copy = FALSE) %>%
  select(-MUNICIPIO_RES) %>%
  rename(MUNICIPIO_RES = MUNICIPIO)

# Expand "ENTIDAD_RES"
covid.data = covid.data %>% 
  inner_join(entidad.catalogo, by = c("ENTIDAD_RES" = "CLAVE_ENTIDAD"), copy = FALSE) %>%
  select(-ENTIDAD_RES) %>%
  rename(ENTIDAD_RES = ENTIDAD_FEDERATIVA)

# Expand "ORIGEN"
covid.data = covid.data %>% 
  inner_join(origen.catalogo, by = c("ORIGEN" = "CLAVE"), copy = FALSE) %>%
  select(-ORIGEN) %>%
  rename(ORIGEN = DESCRIPCIÓN)

# Expand "SECTOR"
covid.data = covid.data %>% 
  inner_join(sector.catalogo, by = c("SECTOR" = "CLAVE"), copy = FALSE) %>%
  select(-SECTOR) %>%
  rename(SECTOR = DESCRIPCIÓN)

# Expand "SEXO"
covid.data = covid.data %>% 
  inner_join(sexo.catalogo, by = c("SEXO" = "CLAVE"), copy = FALSE) %>%
  select(-SEXO) %>%
  rename(SEXO = DESCRIPCIÓN)

# Expand "TIPO PACIENTE"
covid.data = covid.data %>% 
  inner_join(tipo.paciente.catalogo, by = c("TIPO_PACIENTE" = "CLAVE"), copy = FALSE) %>%
  select(-TIPO_PACIENTE) %>%
  rename(TIPO_PACIENTE = DESCRIPCIÓN)

# Expand "INTUBADO"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("INTUBADO" = "CLAVE"), copy = FALSE) %>%
  select(-INTUBADO) %>%
  rename(INTUBADO = DESCRIPCIÓN)

# Expand "NEUMONIA"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("NEUMONIA" = "CLAVE"), copy = FALSE) %>%
  select(-NEUMONIA) %>%
  rename(NEUMONIA = DESCRIPCIÓN)

# Expand "NACIONALIDAD"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("NACIONALIDAD" = "CLAVE"), copy = FALSE) %>%
  select(-NACIONALIDAD) %>%
  rename(NACIONALIDAD = DESCRIPCIÓN)

# Expand "EMBARAZO"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("EMBARAZO" = "CLAVE"), copy = FALSE) %>%
  select(-EMBARAZO) %>%
  rename(EMBARAZO = DESCRIPCIÓN)

# Expand "HABLA_LENGUA_INDIG"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("HABLA_LENGUA_INDIG" = "CLAVE"), copy = FALSE) %>%
  select(-HABLA_LENGUA_INDIG) %>%
  rename(HABLA_LENGUA_INDIG = DESCRIPCIÓN)

# Expand "DIABETES"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("DIABETES" = "CLAVE"), copy = FALSE) %>%
  select(-DIABETES) %>%
  rename(DIABETES = DESCRIPCIÓN)

# Expand "EPOC"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("EPOC" = "CLAVE"), copy = FALSE) %>%
  select(-EPOC) %>%
  rename(EPOC = DESCRIPCIÓN)

# Expand "ASMA"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("ASMA" = "CLAVE"), copy = FALSE) %>%
  select(-ASMA) %>%
  rename(ASMA = DESCRIPCIÓN)

# Expand "INMUSUPR"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("INMUSUPR" = "CLAVE"), copy = FALSE) %>%
  select(-INMUSUPR) %>%
  rename(INMUSUPR = DESCRIPCIÓN)

# Expand "HIPERTENSION"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("HIPERTENSION" = "CLAVE"), copy = FALSE) %>%
  select(-HIPERTENSION) %>%
  rename(HIPERTENSION = DESCRIPCIÓN)

# Expand "OTRA_COM"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("OTRA_COM" = "CLAVE"), copy = FALSE) %>%
  select(-OTRA_COM) %>%
  rename(OTRA_COM = DESCRIPCIÓN)

# Expand "CARDIOVASCULAR"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("CARDIOVASCULAR" = "CLAVE"), copy = FALSE) %>%
  select(-CARDIOVASCULAR) %>%
  rename(CARDIOVASCULAR = DESCRIPCIÓN)

# Expand "OBESIDAD"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("OBESIDAD" = "CLAVE"), copy = FALSE) %>%
  select(-OBESIDAD) %>%
  rename(OBESIDAD = DESCRIPCIÓN)

# Expand "RENAL_CRONICA"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("RENAL_CRONICA" = "CLAVE"), copy = FALSE) %>%
  select(-RENAL_CRONICA) %>%
  rename(RENAL_CRONICA = DESCRIPCIÓN)

# Expand "TABAQUISMO"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("TABAQUISMO" = "CLAVE"), copy = FALSE) %>%
  select(-TABAQUISMO) %>%
  rename(TABAQUISMO = DESCRIPCIÓN)

# Expand "OTRO_CASO"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("OTRO_CASO" = "CLAVE"), copy = FALSE) %>%
  select(-OTRO_CASO) %>%
  rename(OTRO_CASO = DESCRIPCIÓN)

# Expand "RESULTADO"
covid.data = covid.data %>% 
  inner_join(resultado.catalogo, by = c("RESULTADO" = "CLAVE"), copy = FALSE) %>%
  select(-RESULTADO) %>%
  rename(RESULTADO = DESCRIPCIÓN)

# Expand "MIGRANTE"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("MIGRANTE" = "CLAVE"), copy = FALSE) %>%
  select(-MIGRANTE) %>%
  rename(MIGRANTE = DESCRIPCIÓN)

# Expand "UCI"
covid.data = covid.data %>% 
  inner_join(bool.catalogo, by = c("UCI" = "CLAVE"), copy = FALSE) %>%
  select(-UCI) %>%
  rename(UCI = DESCRIPCIÓN)
  
# ------------------
# Exploratory Data Analysis
# ------------------

# Distribución EDAD sobre todos los datos
covid.data %>% ggplot(aes(x=EDAD)) +
  geom_histogram(aes(y=..density..),
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "Edad") +
  theme_light()


# Distribución ENTIDAD_RES sobre todos los datos
covid.data %>% ggplot(aes(x = ENTIDAD_RES)) +
  geom_bar(stat = "count", colour="black", fill="steelblue") +
  labs(x = "Entidad Residencia") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Histograma SEXO sobre todos los datos
covid.data %>% ggplot(aes(x = SEXO)) +
  geom_bar(stat = "count", colour="black", fill="steelblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.8) +
  labs(x = "Sexo") +
  theme_light()


# Histograma RESULTADO sobre todos los datos
covid.data %>% ggplot(aes(x = RESULTADO)) +
  geom_bar(stat = "count", colour="black", fill="steelblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.8) +
  labs(x = "Resultado") +
  theme_light()


# Histograma TIPO_PACIENTE sobre todos los datos
covid.data %>% ggplot(aes(x = TIPO_PACIENTE)) +
  geom_bar(stat = "count", colour="black", fill="steelblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.8) +
  labs(x = "Tipo de paciente") +
  theme_light()

# Histograma TIPO_PACIENTE por cada tipo de RESULTADO sobre todos los datos
covid.data %>% ggplot(aes(x = RESULTADO, fill = factor(TIPO_PACIENTE))) +
  geom_bar(stat="count", position=position_dodge()) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.1, position = position_dodge(0.9)) +
  #facet_grid(.~SEXO) +
  labs(x = "Resultado")
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #theme_light()

# Histograma EDAD para Positivo SARS-CoV-2 y Hospitalizado
covid.data %>% 
  filter(RESULTADO == "Positivo SARS-CoV-2" & TIPO_PACIENTE == "HOSPITALIZADO") %>% 
  ggplot(aes(x=EDAD)) +
  geom_histogram(aes(y=..density..),
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "Edad") +
  theme_light()

# Histograma EDAD para Positivo SARS-CoV-2 y Hospitalizado
covid.data %>% 
  filter(RESULTADO == "Positivo SARS-CoV-2" & TIPO_PACIENTE == "HOSPITALIZADO") %>%
  ggplot(aes(x = INTUBADO)) +
  geom_bar(stat = "count", colour="black", fill="steelblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.8) +
  labs(x = "INTUBADO") +
  theme_light()
