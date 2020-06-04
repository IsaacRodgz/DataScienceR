library(tidyverse)
library(tidymodels)
library(magrittr)
library(ggthemes)
library(stargazer)
library(mclust)
library(factoextra)

# ------------------
# Data preprocessing
# ------------------

setwd("~/Documents/CIMAT/DataScienceR/proyecto_covid19/")
covid.data = read_csv("data/200531COVID19MEXICO.csv")

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

# Transform variables

covid.data = covid.data %>%
  mutate(DIABETES = ifelse(DIABETES == "SI", 1.0, 0.0)) %>%
  mutate(EPOC = ifelse(EPOC == "SI", 1.0, 0.0)) %>%
  mutate(ASMA = ifelse(ASMA == "SI", 1.0, 0.0)) %>%
  mutate(INMUSUPR = ifelse(INMUSUPR == "SI", 1.0, 0.0)) %>%
  mutate(HIPERTENSION = ifelse(HIPERTENSION == "SI", 1.0, 0.0)) %>%
  mutate(OTRA_COM = ifelse(OTRA_COM == "SI", 1, 0)) %>%
  mutate(CARDIOVASCULAR = ifelse(CARDIOVASCULAR == "SI", 1.0, 0.0)) %>%
  mutate(OBESIDAD = ifelse(OBESIDAD == "SI", 1, 0)) %>%
  mutate(RENAL_CRONICA = ifelse(RENAL_CRONICA == "SI", 1.0, 0.0)) %>%
  mutate(TABAQUISMO = ifelse(TABAQUISMO == "SI", 1.0, 0.0)) %>%
  mutate(OTRO_CASO = ifelse(OTRO_CASO == "SI", 1.0, 0.0)) %>%
  mutate(INTUBADO = ifelse(OTRO_CASO == "SI", 1.0, 0.0)) %>%
  mutate(NEUMONIA = ifelse(OTRO_CASO == "SI", 1.0, 0.0))

# Crea variable binaria FALLECIDO que indica si el paciente tiene fecha de defunción

covid.data = covid.data %>%
  mutate(FALLECIDO = if_else(is.na(FECHA_DEF), "0", "1"))

# -------------------------
# Exploratory Data Analysis
# -------------------------

# Distribución EDAD sobre todos los datos
covid.data %>% ggplot(aes(x=EDAD)) +
  geom_histogram(aes(y=..density..),
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "Edad") +
  theme_light()

# Distribución EDAD de acuerdo al grupo SEXO sobre todos los datos
covid.data %>% ggplot(aes(x=EDAD, fill = SEXO)) +
  geom_histogram(color="#e9ecef",
                 binwidth=5,
                 alpha=0.5,
                 position = 'identity') +
  theme_light() +
  labs(fill="")

# Distribución ENTIDAD_RES sobre todos los datos
covid.data %>% ggplot(aes(x = ENTIDAD_RES)) +
  geom_bar(stat = "count", colour="black", fill="steelblue") +
  labs(x = "Entidad Residencia") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribución ENTIDAD_RES sobre todos los datos
covid.data %>%
  filter(!ENTIDAD_RES %in% c('CIUDAD DE MÉXICO', 'MÉXICO')) %>%
  ggplot(aes(x = ENTIDAD_RES)) +
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
  labs(x = "Resultado") +
  labs(fill="Tipo de paciente")
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme_light()

# Histograma EDAD para Positivo SARS-CoV-2 y Hospitalizado
covid.data %>% 
  filter(RESULTADO == "Positivo SARS-CoV-2" & TIPO_PACIENTE == "HOSPITALIZADO") %>% 
  ggplot(aes(x=EDAD)) +
  geom_histogram(aes(y=..density..),
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "Edad") +
  theme_light()

# Distribución EDAD de acuerdo al grupo SEXO sobre todos los datos
covid.data %>%
  filter(RESULTADO == "Positivo SARS-CoV-2" & TIPO_PACIENTE == "HOSPITALIZADO") %>%
  ggplot(aes(x=EDAD, fill = SEXO)) +
  geom_histogram(color="#e9ecef",
                 binwidth=5,
                 alpha=0.5,
                 position = 'identity') +
  theme_light() +
  labs(fill="")

# Histograma INTUBADO para Positivo SARS-CoV-2 y Hospitalizado
covid.data %>% 
  filter(RESULTADO == "Positivo SARS-CoV-2" & TIPO_PACIENTE == "HOSPITALIZADO") %>%
  ggplot(aes(x = INTUBADO)) +
  geom_bar(stat = "count", colour="black", fill="steelblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.8) +
  labs(x = "INTUBADO") +
  theme_light()

# Histograma NEUMONIA para Positivo SARS-CoV-2 y Hospitalizado
covid.data %>% 
  filter(RESULTADO == "Positivo SARS-CoV-2" & TIPO_PACIENTE == "HOSPITALIZADO") %>%
  ggplot(aes(x = NEUMONIA)) +
  geom_bar(stat = "count", colour="black", fill="steelblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.8) +
  labs(x = "NEUMONIA") +
  theme_light()

# Porcentajes comorbilidades para Positivo SARS-CoV-2 y Hospitalizado

covid.data %>% 
  filter(RESULTADO == "Positivo SARS-CoV-2" & TIPO_PACIENTE == "HOSPITALIZADO" & FALLECIDO == "1") %>%
  select(OTRO_CASO) %>%
  data.matrix() %>%
  mean()

comorbilidades_freq <- tribble(
  ~comorbilidad,         ~porcentaje,
  "DIABETES", 0.3,
  "EPOC", 0.038,
  "ASMA", 0.023,
  "INMUSUPR", 0.026,
  "HIPERTENSION", 0.33,
  "OTRAS_COM", 0.042,
  "CARDIOVASCULAR", 0.042,
  "OBESIDAD", 0.24,
  "RENAL_CRONICA", 0.046,
  "TABAQUISMO", 0.086,
  "OTRO_CASO", 0.14
)

comorbilidades_freq %>%
  ggplot(aes(x=reorder(comorbilidad, -porcentaje), y=porcentaje)) +
  geom_bar(stat="identity", colour="black", fill="steelblue", width = 0.3) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Comorbilidad") +
  labs(y = "Porcentaje")

comorbilidades_freq <- tribble(
  ~comorbilidad,         ~porcentaje,
  "DIABETES", 0.38,
  "EPOC", 0.055,
  "ASMA", 0.022,
  "INMUSUPR", 0.031,
  "HIPERTENSION", 0.42,
  "OTRAS_COM", 0.046,
  "CARDIOVASCULAR", 0.056,
  "OBESIDAD", 0.26,
  "RENAL_CRONICA", 0.07,
  "TABAQUISMO", 0.094,
  "OTRO_CASO", 0.11
)

comorbilidades_freq %>%
  ggplot(aes(x=reorder(comorbilidad, -porcentaje), y=porcentaje)) +
  geom_bar(stat="identity", colour="black", fill="steelblue", width = 0.3) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Comorbilidad") +
  labs(y = "Porcentaje")

# --------
# Modeling
# --------

---------------------
# Logistic Regression
---------------------
# Se busca modelar la probabilidad de que un paciente fallezca
# y buscar que variables son más informativas para la modelación

set.seed(42)

# Preprocessing

train_test_split <- initial_split(covid.data)
covid.train <- training(train_test_split)
covid.test <- testing(train_test_split)

covid.recipe = recipe(FALLECIDO ~ EDAD + DIABETES
                      + EPOC + ASMA + INMUSUPR + HIPERTENSION + OTRA_COM +
                        CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO + 
                        OTRO_CASO, data = covid.train)

model.recipe.steps = covid.recipe %>%
  step_mutate(FALLECIDO = as.factor(FALLECIDO))
prepped.recipe = prep(model.recipe.steps, training = covid.train)

covid.train.prep = bake(prepped.recipe, covid.train)
covid.test.prep = bake(prepped.recipe, covid.test)


# Saturated Model

covid.lr.model = logistic_reg(mode = "classification") %>%
  set_engine("glm") %>% 
  fit(FALLECIDO ~ EDAD + DIABETES
      + EPOC + ASMA + INMUSUPR + HIPERTENSION + OTRA_COM +
        CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO + 
        OTRO_CASO, data = covid.train.prep)

# Evaluation metrics

covid.lr.model %>%
  predict(covid.test.prep) %>%
  bind_cols(covid.test.prep) %>%
  metrics(truth = FALLECIDO, estimate = .pred_class)

# Analyze results

summary(covid.lr.model$fit)

# Reduced Model

covid.recipe = recipe(FALLECIDO ~ EDAD + DIABETES +
                        INMUSUPR + HIPERTENSION +
                        OBESIDAD + RENAL_CRONICA + 
                        OTRO_CASO, data = covid.train)

model.recipe.steps = covid.recipe %>%
  step_mutate(FALLECIDO = as.factor(FALLECIDO))
prepped.recipe = prep(model.recipe.steps, training = covid.train)

covid.train.prep = bake(prepped.recipe, covid.train)
covid.test.prep = bake(prepped.recipe, covid.test)


# Model

covid.lr.model = logistic_reg(mode = "classification") %>%
  set_engine("glm") %>% 
  fit(FALLECIDO ~ EDAD + DIABETES +
        INMUSUPR + HIPERTENSION +
        OBESIDAD + RENAL_CRONICA + 
        OTRO_CASO, data = covid.train.prep)

# Evaluation metrics

covid.lr.model %>%
  predict(covid.test.prep) %>%
  bind_cols(covid.test.prep) %>%
  metrics(truth = FALLECIDO, estimate = .pred_class)

# Analyze results

summary(covid.lr.model$fit)

#-----
# PCA 
#-----

pr = covid.data %>%
  select(DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
         OTRA_COM, CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA,
         TABAQUISMO) %>%
  prcomp()

plot(pr, type = "l")
plot(cumsum(pr$sdev[1:11]^2)/sum(pr$sdev^2), xlab = "Num of PC", ylab = "Explained variance", type = "b")
summary(pr)

# PC1
plot(pr$rotation[,1])
text(pr$rotation[,1], labels=names(pr$rotation[,1]), cex=0.75, pos=1, xpd=NA)

# PC2
plot(pr$rotation[,2])
text(pr$rotation[,2], labels=names(pr$rotation[,2]), cex=0.75, pos=1, xpd=NA)

fviz_pca_var(pr,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Data PCA

covid.data.pca = pr$x[,1:2]

# K-Means clustering on PCA

n <- nrow(covid.data.pca)
wss <- rep(0, 8)
wss[1] <- (n-1)*sum(sapply(covid.data.pca, var))

for (i in 2:8)
  wss[i] <- sum(kmeans(covid.data.pca, centers=i)$withinss)

plot(1:8, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

km <- kmeans(covid.data.pca, centers=6)
km.labels <- km$cluster

# Visualize clustering k means with PCA
plot(covid.data.pca, col=km.labels)
