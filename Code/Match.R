### Title: WLS and PSM 
### Author: Álvaro Pérez
### Last update: April 09, 2025

# ******************************************************************************
# ----- I. Libraries -----
# ******************************************************************************

library(dplyr)
library(tidyverse)
library(ggplot2)
library(MatchIt)
library(stargazer)
library(readxl)
install.packages("lmtest")
library(lmtest)
library(car)
library(nnet)
install.packages("cobalt") # Tablas de balance
library(cobalt)
install.packages("cem")
library(cem)
install.packages("tcltk")
library(tcltk)
install.packages("rbounds") #Prueba de sensibilidad para matching
library(rbounds)
install.packages("lfe")
library(lfe)
library(broom)
install.packages("extrafont")
library(extrafont)

### Directory ###

setwd("/Users/alvaroperezlopez/Desktop/TESIS")

# ******************************************************************************
# ----- II. Data ------
# ******************************************************************************

gift <- read_xlsx("/Users/alvaroperezlopez/Desktop/TESIS/Data/Clientelism_gift.xlsx")

gift$REGALOS <- as.numeric(as.character(gift$REGALOS))
gift$INDIGENA <- as.numeric(as.character(gift$INDIGENA))
gift$EDAD <- as.numeric(as.character(gift$EDAD))
gift$VOTED_WHICHPARTY <- as.numeric(as.character(gift$VOTED_WHICHPARTY))

### Rename variables 
gift <- gift %>% rename("margin"= MARGIN_MUNIC,
                        "edu" = EDUCATION,
                        "type" = TIPO,
                        "age" = EDAD,
                        "sex" = SEXO,
                        "gift" = REGALOS, 
                        "p_id" = `PARTY ID1`,
                        "eth" = INDIGENA)
gift <- gift %>% rename("gift_whichparty"= REGALO_WHICHPARTY,
                        "vote_whichparty"= VOTED_WHICHPARTY)

# ******************************************************************************
# ----- III. Ponderator -----
# ******************************************************************************

n <- nrow(gift)
sum_ponderador <- sum(gift$PONDERADOR)
gift$sum_ponderador <- sum(gift$PONDERADOR)

# Crear una nueva columna con el ponderador normalizado
gift$ponderador_norm <- gift$PONDERADOR * (n / sum_ponderador)

# Para aquellos que recibieron regalo 
sum_gift <- sum(gift$PONDERADOR[gift$gift==1],na.rm = T)
giftshare_pon <- sum_gift/unique(gift$sum_ponderador)*100

### Revisando heteroscedasticidad 
modelo <- lm(vote_incum_gob ~ gift + margin + edu + age + sex + p_id + eth, data = gift)
plot(modelo$fitted.values, rstandard(modelo),
     xlab = "Valores Ajustados", ylab = "Residuos Estandarizados",
     main = "Residuos vs. Valores Ajustados")
abline(h = 0, col = "red")
## Prueba de Bresuch-Pagan
bptest(modelo)
## Test de Varianza No Constante 
ncvTest(modelo)
# SI HAY HETEROSCEDASTICIDAD

### Revisando linealidad con educación 
ggplot(gift, aes(x = edu, y = vote_incum_pres)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(x = "Nivel educativo", y = "Vote_incum_pres", 
       title = "Relación entre nivel educativo y vote_incum_pres")

# ******************************************************************************
# ----- IV. WLS and Logit incumbent -----
# ******************************************************************************

#¿Independientemente de quién da regalos, el hecho de recibirlos hará que apoye al incumbent? 
pres_wls <- lm(vote_incum_pres ~ gift + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR), 
               data = gift, weights = ponderador_norm)
summary(pres_wls)

gov_wls <- lm(vote_incum_gob ~ gift + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR), 
              data = gift, weights = ponderador_norm)
summary(gov_wls)

mun_wls <- lm(vote_incum_mun ~ gift + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR), 
              data = gift, weights = ponderador_norm)
summary(mun_wls)

# Tabla con los modelos 
stargazer(
  pres_wls, gov_wls, mun_wls,
  title = "Modelos WLS: Voto Incumbente",
  label = "tab:wls_incumbentes",
  dep.var.labels = c("Presidente", "Gobernador", "Municipal"),
  covariate.labels = c( "Regalo", "Margen victoria", "Educación", "Edad", "Sexo (mujer)", "PartyID_null", "PartyID_PAN", "PartyID_PRI","PartyID_PRD","PartyID_MRN","PartyID_otro", "Tipo de municipio", "ln(Población)"), 
  keep.stat    = c("n","rsq","adj.rsq"),
  no.space     = TRUE,
  float        = TRUE,
  out          = "wls_incumbentes.tex"
)

### Logit ###
# No incluí party id 
pres_logit <- glm(vote_incum_pres ~ gift + margin + as.numeric(edu) + age + sex + as.numeric(type) + `ln(pob)` + factor(YEAR),
                  data = gift, family = binomial)
summary(pres_logit)

gov_logit <-  glm(vote_incum_gob ~ gift + margin + as.numeric(edu) + age + sex + as.numeric(type) + `ln(pob)` + factor(YEAR),
                  data = gift, family = binomial)
summary(gov_logit)

mun_logit <-  glm(vote_incum_pres ~ gift + margin + as.numeric(edu) + age + sex + as.numeric(type) + `ln(pob)` + factor(YEAR),
                  data = gift, family = binomial)
summary(mun_logit)

# Tabla con los modelos 
stargazer(
  pres_logit, gov_logit, mun_logit,
  title = "Modelos Logit: Voto Incumbente",
  label = "tab:logit_incumbentes",
  dep.var.labels = c("Presidente", "Gobernador", "Municipal"),
  covariate.labels = c( "Regalo", "Margen victoria", "Educación", "Edad", "Sexo", "Tipo de municipio", "ln(Población)"), 
  keep.stat    = c("n","aic"),
  no.space     = TRUE,
  float        = TRUE,
  out          = "logit_incumbentes.tex"
)

# ******************************************************************************
# ----- V. WLS, FE and Logit by party -----
# ******************************************************************************

## Separando por partido 

gift <- gift %>% 
  mutate(
    vote_wichparty_clean = ifelse(vote_whichparty == 0, NA_character_, as.character(vote_whichparty)),
    vote_PAN  = if_else(vote_wichparty_clean == "1", 1L, 0L),
    vote_PRI  = if_else((YEAR == 2024 & vote_wichparty_clean == "1") | vote_wichparty_clean == "2", 1L, 0L),
    vote_PRD  = if_else((YEAR == 2024 & vote_wichparty_clean == "1") | vote_wichparty_clean == "3", 1L, 0L),
    vote_MRN  = if_else(vote_wichparty_clean == "4", 1L, 0L),
    vote_otros= if_else(vote_wichparty_clean == "5", 1L, 0L),
    
    gift_PAN  = if_else(gift_whichparty  == 1, 1L, 0L),
    gift_PRI  = if_else((YEAR == 2024 & gift_whichparty == 1) | gift_whichparty == 2, 1L, 0L),
    gift_PRD  = if_else((YEAR == 2024 & gift_whichparty == 1) | gift_whichparty == 3, 1L, 0L),
    gift_MRN  = if_else(gift_whichparty  == 4, 1L, 0L),
    gift_otros= if_else(gift_whichparty  == 5, 1L, 0L),
    
    vote_left = if_else((YEAR >= 2000 & YEAR <= 2015 & vote_whichparty == 3) |
                          (YEAR >= 2018 & YEAR <= 2024 & vote_whichparty == 4), 1L, 0L),
    gift_left = if_else((YEAR >= 2000 & YEAR <= 2015 & gift_whichparty == 3) |
                          (YEAR >= 2018 & YEAR <= 2024 & gift_whichparty == 4), 1L, 0L)
  )

## WLS
PAN_wls <- lm(vote_PAN ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
               data = gift, weights = ponderador_norm)
summary(PAN_wls)

PRI_wls <- lm(vote_PRI ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
               data = gift, weights = ponderador_norm)
summary(PRI_wls)

left_wls <- lm(vote_left ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
               data = gift, weights = ponderador_norm)
summary(left_wls)

# Tabla de resultados
wls_models <- stargazer(PAN_wls, PRI_wls, left_wls, 
          type = "text",
          out = "wls_vote.tex",
          title = "Resultados de los Modelos WLS",
          column.labels = c("PAN", "PRI", "Izquierda"),
          digits = 3,
          keep.stat = c("n","rsq","adj.rsq")
          )

## Logit
PAN_logit <- glm(vote_PAN ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                 data = gift,
                 family = binomial(link = "logit"))
summary(PAN_logit)
exp(coef(PAN_logit))

PRI_logit <- glm(vote_PRI ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                 data = gift,
                 family = binomial(link = "logit"))
summary(PRI_logit)
exp(coef(PRI_logit))

left_logit <- glm(vote_left ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                  data = gift,
                  family = binomial(link = "logit"))
summary(left_logit)
exp(coef(PAN_logit))

# Tabla de resultados 
logit_models <- stargazer(PAN_logit, PRI_logit, left_logit, 
          type = "text",
          title = "Resultados de los Modelos Logit",
          column.labels = c("PAN", "PRI", "Izquierda"),
          digits = 3,
          keep.stat = c("n","aic"))

### Fixed Effects 

PAN_fe <- felm(
  vote_PAN ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)`
  | YEAR,                                    # aquí defines el efecto fijo de año
  data    = gift,
  weights = gift$ponderador_norm
)
summary(PAN_fe)

PRI_fe <- felm(
  vote_PRI ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)`
  | YEAR,
  data    = gift,
  weights = gift$ponderador_norm
)
summary(PRI_fe)

left_fe <- felm(
  vote_left ~ gift_PAN + gift_PRI + gift_left + margin + edu + age + sex + p_id + type + `ln(pob)`
  | YEAR,
  data    = gift,
  weights = gift$ponderador_norm
)
summary(left_fe)

# ******************************************************************************
# ----- VI. Incumbent (local) coincidence -----
# ******************************************************************************
gift$match_pan <- ifelse(gift$gift_PAN == 1 & gift$INCUMB_MUNIC == 1, 1, 0)
PAN_logit_match <- glm(vote_PAN ~ match_pan + match_pri + match_prd + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                       data = gift,
                       family = binomial(link = "logit"))
summary(PAN_logit_match)

gift$match_pri <- ifelse(gift$gift_PRI == 1 & gift$INCUMB_MUNIC == 2, 1, 0)
PRI_logit_match <- glm(vote_PRI ~ match_pan + match_pri + match_prd + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                       data = gift,
                       family = binomial(link = "logit"))
summary(PRI_logit_match)
# Solo salio positivo y significativo para el PRI
gift$match_prd <- ifelse(gift$gift_PRD == 1 & gift$INCUMB_MUNIC == 3, 1, 0)
PRD_logit_match <- glm(vote_PRD ~ match_pan + match_pri + match_prd + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                       data = gift,
                       family = binomial(link = "logit"))
summary(PRD_logit_match)

gift$match_mrn <- ifelse(gift$gift_MRN == 1 & gift$INCUMB_MUNIC == 4, 1, 0)
MRN_logit_match <- glm(vote_MRN ~ match_pan + match_pri + match_mrn + margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                       data = gift,
                       family = binomial(link = "logit"))
summary(MRN_logit_match)

# ******************************************************************************
# ----- VII. PSM clientelism ----
# ******************************************************************************

# Modo correcto de las variables: as factor 
gift$edu <- as.factor(gift$edu)
gift$type <- as.factor(gift$type)
gift$sex <- as.factor(gift$sex)
gift$p_id <- as.factor(gift$p_id)

### Puntaje de propensión ###


# gift_psm elimina NA's
#gift_psm <- na.omit(gift[, c("gift", "margin", "edu", "age", "sex", "p_id", "type", "ln(pob)", "YEAR","ponderador_norm")])
vars <- c("gift", "margin", "edu", "age", "sex", "p_id", "type", "ln(pob)", "YEAR", "ponderador_norm")
gift_psm <- gift[complete.cases(gift[, vars]), ]
ps_model <- glm(gift ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                data = gift_psm, 
                family = binomial(link = "logit"))
gift_psm$pscore <- ps_model$fitted.values

### Common support ###
#Evalúa la distribución de las puntuaciones en ambos grupos

common_support <- ggplot(gift_psm, aes(x = pscore, fill = factor(gift))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Recibió regalo", x = "Puntuación de propensión", y = "Densidad",
       title = "Solapamiento de los puntajes de propensión")+
  theme_minimal()
common_support

### Matching ###

matchit_model <- matchit(gift ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                         data = gift_psm, 
                         method = "nearest",
                         distance = gift_psm$pscore,
                         s.weights = gift_psm$ponderador_norm,
                         ratio = 1,
                         replace = T)

# Extrae la muestra emparejada
matched_data <- match.data(matchit_model)

### Balance de covariables ###

# Ver el balance:
bal.tab(matchit_model)
# ESS:Effective Sample Size que considera ponderadores
# Graficar el love plot:
love.plot(matchit_model, threshold = 0.1)

### Plot ###

matched_data_plot <- ggplot(matched_data, aes(x = pscore, fill = factor(gift))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Tratamiento", x = "Puntuación de Propensión", y = "Densidad",
       title = "Distribución de los Puntajes de Propensión")+
  theme_minimal()
matched_data_plot

### Efecto del tratamiento ###

treatment_effect <- glm(VOTED ~ gift + pscore, 
                        data = matched_data, weights=weights, family = binomial(link = "logit"))
summary(treatment_effect)

## ATT por anio 

effects_by_year <- matched_data %>%
  group_by(YEAR) %>% 
  do(tidy(
    glm(vote_incum_gob ~ gift,
        data    = .,
        weights = weights,
        family  = binomial(link="logit"))
  )) %>%
  filter(term == "gift") %>%
  select(YEAR, estimate, std.error, p.value) %>%
  mutate(
    OR    = exp(estimate),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )
effects_by_year

# Plot
ggplot(effects_by_year, aes(x = as.integer(YEAR), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error),
                width = 0.2) +
  labs(x = "Año", y = "Log-odds de gift",
       title = "ATT de gift sobre VOTED, por año") +
  theme_minimal()

# Modelo lineal LPM
effects_by_year_lpm <- matched_data %>%
  group_by(YEAR) %>%
  do(tidy(
    lm(VOTED ~ gift,
       data    = .,
       weights = weights)
  )) %>%
  filter(term == "gift") %>%
  select(YEAR, estimate, std.error, p.value)
effects_by_year_lpm

## Por nivel de incumbent
effect_pres <- glm(vote_incum_pres ~ gift + pscore, 
                   data = matched_data, weights = weights, family = binomial(link="logit"))
summary(effect_pres)

effect_gov <- glm(vote_incum_gob ~ gift + pscore, 
                  data = matched_data, weights = weights, family = binomial(link="logit"))
summary(effect_gov)

effect_mun <- glm(vote_incum_mun ~ gift + pscore, 
                  data = matched_data, weights = weights, family = binomial(link="logit"))
summary(effect_mun)

## Por partido 
effect_pan <- glm(vote_PAN ~ gift + pscore, 
                  data = matched_data, weights= weights, family = binomial(link="logit"))
summary(effect_pan)

effect_pri <- glm(vote_PRI ~ gift + pscore, 
                  data = matched_data, weights= weights, family = binomial(link="logit"))
summary(effect_pri)

effect_prd <- glm(vote_PRD ~ gift + pscore, 
                  data = matched_data, weights= weights, family = binomial(link="logit"))
summary(effect_prd)

effect_mrn <- glm(vote_MRN ~ gift + + pscore, 
                  data = matched_data, weights= weights, family = binomial(link="logit"))
summary(effect_mrn)

effect_left <- glm(vote_left ~ gift + pscore, 
                   data = matched_data, weights= weights, family = binomial(link="logit"))
summary(effect_left)


# Tabla de resultados 

stargazer(
  treatment_effect,
  type        = "latex",
  out         = "psm_effects.tex",
  title       = "Efectos del Propensity Score Matching",
  label       = "tab:psm_effects",
  column.labels = c(
    "Global", "Inc. Pres.", "Inc. Gob.", "Inc. Mun."
  ),
  keep        = c("(Intercept)", "gift"),     # sólo intercepto y gift
  keep.stat   = c("n", "ll", "aic"),          # N, logLik y AIC
  notes       = "Signif. codes: * p<0.1; ** p<0.05; *** p<0.01",
  notes.align = "l",
  no.space    = TRUE,
  float       = TRUE,
  digits      = 3,
  sanitize.rownames.function = identity
)

# ******************************************************************************
# ----- VIII. PSM know of clientelism -----
# ******************************************************************************

### Puntaje de propensión ###

# Cambiar el tratamiento a factor 

gift$knowofclientelism <- as.factor(gift$knowofclientelism)

vars2 <- c("knowofclientelism","margin","edu","age","sex","p_id","type","ln(pob)","YEAR","ponderador_norm")
know_psm <- gift[complete.cases(gift[, vars2]), ]

# Logit para el nuevo tratamiento
ps_model2 <- glm(knowofclientelism ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                 data = know_psm, 
                 family = binomial(link = "logit"))
know_psm$pscore2 <- ps_model2$fitted.values

# Plot de solapamiento

ggplot(know_psm, aes(x = pscore2, fill = knowofclientelism)) +
  geom_density(alpha = .5) +
  labs(fill = "Sabe clientelismo", x = "Puntuación de propensión")

### Matching ###

# MatchIt

matchit_model2 <- matchit(knowofclientelism ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                          data     = know_psm,
                          method   = "nearest",
                          distance = know_psm$pscore2,
                          s.weights= know_psm$ponderador_norm,
                          ratio    = 1,
                          replace  = TRUE)

matched_data2 <- match.data(matchit_model2)

### Balance de covariables ###
bal.tab(matchit_model2)
love.plot(matchit_model2, data = know_psm, threshold = 0.1)

## Plot ##
matched_data_plot2 <- ggplot(matched_data2, aes(x = pscore2, fill = factor(knowofclientelism))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Tratamiento", x = "Puntuación de Propensión", y = "Densidad",
       title = "Distribución de los Puntajes de Propensión")+
  theme_minimal()
matched_data_plot2

### Efecto del tratamiento ###

treatment_effect2 <- glm(VOTED ~ knowofclientelism,
                         data   = matched_data2,
                         weights = weights,
                         family = quasibinomial(link = "logit"))
summary(treatment_effect2)

# ******************************************************************************
# ---- IX. PSM by party ----
# ******************************************************************************

### PAN ###

# Filtrando casos completos
vars_pan <- c("gift_PAN","margin","edu","age","sex","p_id","type","ln(pob)","YEAR","ponderador_norm")
gift_pan_psm <- gift[complete.cases(gift[, vars_pan]), ]
gift_pan_psm <- filter(gift_pan_psm, age >= 18)

# Puntaje de Propensión para gift_PAN
ps_pan <- glm(gift_PAN ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
               data   = gift_pan_psm,
               family = binomial(link="logit") )
gift_pan_psm$ps_pan <- ps_pan$fitted.values

# gift_PAN sea factor
gift_pan_psm <- gift_pan_psm %>%
  mutate(gift_PAN = factor(gift_PAN, levels = c(0,1),
                           labels = c("No recibe regalo", "Recibe regalo")))
# Plot de solapamiento
ggplot(gift_pan_psm, aes(x = ps_pan, 
                         fill  = gift_PAN, 
                         group = gift_PAN)) +
  geom_density(alpha = 0.5, adjust = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "Regalo del PAN",
       x    = "Puntaje de propensión",
       y    = "Densidad",
       title= "Solapamiento de puntuaciones de propensión\n(PAN)") +
  theme_minimal()

# Matching 1:1 nearest con reemplazo y peso muestral
m_pan <- matchit(gift_PAN ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                  data     = gift_pan_psm,
                  method   = "nearest",
                  distance = gift_pan_psm$ps_pan,
                  s.weights= gift_pan_psm$ponderador_norm,
                  ratio    = 1,
                  replace  = TRUE )

matched_pan <- match.data(m_pan)

# Balance
bal.tab(m_pan, data=gift_pan_psm, un=TRUE)
love.plot(m_pan, data=gift_pan_psm, threshold=0.1)

ggplot(matched_pan, aes(x = ps_pan, fill = factor(gift_PAN))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Tratamiento", x = "Puntuación de Propensión", y = "Densidad",
       title = "Distribución de los Puntajes de Propensión")+
  theme_minimal()

# Efecto de gift_PAN sobre vote_PAN
psm_effect_pan <- glm(vote_PAN ~ gift_PAN,
                  data    = matched_pan,
                  weights = weights,
                  family  = quasibinomial(link="logit"))
summary(psm_effect_pan)

# ******************************************************************************
### PRI ###

# Filtrando casos completos
vars_pri <- c("gift_PRI","margin","edu","age","sex","p_id","type","ln(pob)","YEAR","ponderador_norm")
gift_pri_psm <- gift[complete.cases(gift[, vars_pri]), ]
gift_pri_psm <- filter(gift_pri_psm, age >= 18)

# Puntaje de Propensión para gift_PRI
ps_pri <- glm(gift_PRI ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
              data   = gift_pri_psm,
              family = binomial(link="logit") )
gift_pri_psm$ps_pri <- ps_pri$fitted.values

# gift_PRI sea factor
gift_pri_psm <- gift_pri_psm %>%
  mutate(gift_PRI = factor(gift_PRI, levels = c(0,1),
                           labels = c("No recibe regalo", "Recibe regalo")))
# Plot de solapamiento
ggplot(gift_pri_psm, aes(x = ps_pri, 
                         fill  = gift_PRI, 
                         group = gift_PRI)) +
  geom_density(alpha = 0.5, adjust = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "Regalo del PRI",
       x    = "Puntaje de propensión",
       y    = "Densidad",
       title= "Solapamiento de puntuaciones de propensión\n(PRI)") +
  theme_minimal()

# Matching 1:1 nearest con reemplazo y peso muestral
m_pri <- matchit(gift_PRI ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                 data     = gift_pri_psm,
                 method   = "nearest",
                 distance = gift_pri_psm$ps_pri,
                 s.weights= gift_pri_psm$ponderador_norm,
                 ratio    = 1,
                 replace  = TRUE )

matched_pri <- match.data(m_pri)

# Balance
bal.tab(m_pri, data=gift_pri_psm, un=TRUE)
love.plot(m_pri, data=gift_pri_psm, threshold=0.1)

ggplot(matched_pri, aes(x = ps_pri, fill = factor(gift_PRI))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Tratamiento", x = "Puntuación de Propensión", y = "Densidad",
       title = "Distribución de los Puntajes de Propensión")+
  theme_minimal()

# Efecto de gift_PRI sobre vote_PRI
psm_effect_pri <- glm(vote_PRI ~ gift_PRI,
                      data    = matched_pri,
                      weights = weights,
                      family  = quasibinomial(link="logit"))
summary(psm_effect_pri)

# ******************************************************************************
### Left ###

# Filtrando casos completos
vars_left <- c("gift_left","margin","edu","age","sex","p_id","type","ln(pob)","YEAR","ponderador_norm")
gift_left_psm <- gift[complete.cases(gift[, vars_left]), ]
gift_left_psm <- filter(gift_left_psm, age >= 18)

# Puntaje de Propensión para gift_left
ps_left <- glm(gift_left ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
              data   = gift_left_psm,
              family = binomial(link="logit") )
gift_left_psm$ps_left <- ps_left$fitted.values

# gift_left sea factor
gift_left_psm <- gift_left_psm %>%
  mutate(gift_left = factor(gift_left, levels = c(0,1),
                           labels = c("No recibe regalo", "Recibe regalo")))
# Plot de solapamiento
ggplot(gift_left_psm, aes(x = ps_left, 
                         fill  = gift_left, 
                         group = gift_left)) +
  geom_density(alpha = 0.5, adjust = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "Regalo de la izquierda",
       x    = "Puntaje de propensión",
       y    = "Densidad",
       title= "Solapamiento de puntuaciones de propensión\n(Izquierda)") +
  theme_minimal()

# Matching 1:1 nearest con reemplazo y peso muestral
m_left <- matchit(gift_left ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                 data     = gift_left_psm,
                 method   = "nearest",
                 distance = gift_left_psm$ps_left,
                 s.weights= gift_left_psm$ponderador_norm,
                 ratio    = 1,
                 replace  = TRUE )

matched_left <- match.data(m_left)

# Balance
bal.tab(m_left, data=gift_left_psm, un=TRUE)
love.plot(m_left, data=gift_left_psm, threshold=0.1)

ggplot(matched_left, aes(x = ps_left, fill = factor(gift_left))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Tratamiento", x = "Puntuación de Propensión", y = "Densidad",
       title = "Distribución de los Puntajes de Propensión")+
  theme_minimal()

# Efecto de gift_left sobre vote_left
psm_effect_left <- glm(vote_left ~ gift_left,
                      data    = matched_left,
                      weights = weights,
                      family  = quasibinomial(link="logit"))
summary(psm_effect_left)

# ******************************************************************************

## ATT por anio 

effects_by_year <- matched_left %>%
  group_by(YEAR) %>% 
  do(tidy(
    glm(vote_left ~ gift_left,
        data    = .,
        weights = weights,
        family  = binomial(link="logit"))
  )) %>%
  filter(term == "gift_left") %>%
  select(YEAR, estimate, std.error, p.value) %>%
  mutate(
    OR    = exp(estimate),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )
effects_by_year

# ******************************************************************************
# ---- X. Effect over Party_ID ----
# ******************************************************************************

gift <- gift %>% 
  mutate(
    pid_PAN = if_else(as.character(p_id) == "1", 1L, 0L),
    pid_PRI = if_else(as.character(p_id) == "2", 1L, 0L),
    pid_PRD = if_else(as.character(p_id) == "3", 1L, 0L),
    pid_MRN = if_else(as.character(p_id) == "4", 1L, 0L)
  )

### PAN_ID ###
# 1) Filtrar casos completos *sin* p_id
vars_pid1 <- c("gift_PAN","margin","edu","age","sex","type","ln(pob)","YEAR","ponderador_norm")
gift_pid1_psm <- gift[complete.cases(gift[, vars_pid1]), ] %>%
  filter(age >= 18)

# 2) Estimar el propensity score para gift_PAN (sin incluir p_id)
ps_pid1 <- glm(
  gift_PAN ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR),
  data   = gift_pid1_psm,
  family = binomial(link="logit")
)
gift_pid1_psm$ps_pid1 <- ps_pid1$fitted.values

# 3) Preparar variable de tratamiento como factor (opcional para los plots)
gift_pid1_psm <- gift_pid1_psm %>%
  mutate(gift_PAN = factor(gift_PAN, levels = c(0,1),
                           labels = c("No recibe regalo", "Recibe regalo")))

# 4) Diagnóstico de solapamiento
ggplot(gift_pid1_psm, aes(x = ps_pid1, fill = gift_PAN, group = gift_PAN)) +
  geom_density(alpha = 0.5, adjust = 1) +
  scale_fill_manual(
    values = c("Recibe regalo"= "#006851",
               "No recibe regalo" = "#882c38")
  ) +
  labs(fill  = "Regalo del PAN",
       x     = "Puntaje de propensión",
       y     = "Densidad",
       title = "Solapamiento de puntuaciones de propensión (PAN)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman")
  )

# 5) Matching 1:1 nearest neighbor (sin p_id en la fórmula)
m_pid1 <- matchit(
  gift_PAN ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR),
  data      = gift_pid1_psm,
  method    = "nearest",
  distance  = gift_pid1_psm$ps_pid1,
  s.weights = gift_pid1_psm$ponderador_norm,
  ratio     = 1,
  replace   = TRUE
)

matched_pid1 <- match.data(m_pid1)

# 6) Balance post–matching
bal.tab(m_pid1, data = gift_pid1_psm, un = TRUE)
love.plot(m_pid1, data = gift_pid1_psm, threshold = 0.1)

# 7) Distribución de pscore en la muestra emparejada
ggplot(matched_pid1, aes(x = ps_pid1, fill = gift_PAN, group = gift_PAN)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("Recibe regalo"= "#006851",
               "No recibe regalo" = "#882c38")
  ) +
  labs(fill  = "Regalo del PAN",
       x     = "Puntuación de propensión",
       y     = "Densidad",
       title = "Distribución de puntajes de propensión (PAN) — Muestra emparejada") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman")
  )

# 8) Efecto ATT
psm_effect_pid1 <- glm(pid_PAN ~ gift_PAN,
                      data = matched_pid1,
                      weights = weights,
                      family = binomial(link = "logit"))
summary(psm_effect_pid1)

## Probabilidades 
b0 <- coef(psm_effect_pid1)["(Intercept)"]
b1 <- coef(psm_effect_pid1)["gift_PAN"]
plogis(b0)            # probabilidad sin regalo
plogis(b0 + b1)       # probabilidad con regalo

#
### Sensibilidad a sesgos no observados (Rosenbaum bounds) ###
# Obtén la match.matrix
mm_pid1 <- m_pid1$match.matrix
# Crea un data.frame de índices de pares
pairs_pid1 <- tibble(
  id_treated = as.integer(rownames(mm_pid1)),
  id_control = as.integer(mm_pid1[,1])
)
# Extrae el outcome binario de cada uno
treated_out_pid1 <- matched_pid1[pairs_pid1$id_treated, "pid_PAN"]
control_out_pid1 <- matched_pid1[pairs_pid1$id_control, "pid_PAN"]

# Cuenta los pares discordantes
n10 <- sum(treated_out_pid1 == 1 & control_out_pid1 == 0, na.rm = T)  # tratados ganan  
n01 <- sum(treated_out_pid1 == 0 & control_out_pid1 == 1, na.rm = T)  # controles ganan

res <- binarysens(x = n10, y = n01, Gamma = 1.2, GammaInc = 0.1)
str(res$bounds)
gammas <- seq(1, 2, by = 0.1)

bounds_df <- map_dfr(gammas, function(g) {
  res <- binarysens(x     = n10,
                    y     = n01,
                    Gamma = g,
                    GammaInc = 0.1)   # ajuste interno en 0.1
  # toma la última fila de res$bounds$"Lower bound"
  pbd <- tail(res$bounds[["Lower bound"]], 1)
  tibble(Gamma = g, p_bound = pbd)
})

bounds_df

ggplot(bounds_df, aes(x = Gamma, y = p_bound)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(x = expression(Gamma),
       y = "P-value bound",
       title = "Rosenbaum bounds: sensibilidad del ATT") +
  theme_minimal()

# Función para datos binarios emparejados
binarysens(x = n10, y = n01,
           Gamma    = 1,     # arranca en sin sesgo
           GammaInc = 0.1)   # incremento en 0.1

### E-value
or_est <- exp(coef(psm_effect_pid1)["gift_PAN"])
ci     <- exp(confint(psm_effect_pid1)["gift_PAN", ])
evalues.OR(est = or_est, lo = ci[1], hi = ci[2], true = 1, rare = FALSE)

### PRI_ID ###

# 1) Filtrar casos completos *sin* p_id
vars_pid2 <- c("gift_PRI","margin","edu","age","sex","type","ln(pob)","YEAR","ponderador_norm")
gift_pid2_psm <- gift[complete.cases(gift[, vars_pid2]), ] %>%
  filter(age >= 18)

# 2) Estimar el propensity score para gift_PAN (sin incluir p_id)
ps_pid2 <- glm(
  gift_PRI ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR),
  data   = gift_pid2_psm,
  family = binomial(link="logit")
)
gift_pid2_psm$ps_pid2 <- ps_pid2$fitted.values

# 3) Preparar variable de tratamiento como factor (opcional para los plots)
gift_pid2_psm <- gift_pid2_psm %>%
  mutate(gift_PRI = factor(gift_PRI, levels = c(0,1),
                           labels = c("No recibe regalo", "Recibe regalo")))

# 4) Diagnóstico de solapamiento
ggplot(gift_pid2_psm, aes(x = ps_pid2, fill = gift_PRI, group = gift_PRI)) +
  geom_density(alpha = 0.5, adjust = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(fill  = "Regalo del PRI",
       x     = "Puntaje de propensión",
       y     = "Densidad",
       title = "Solapamiento de puntuaciones de propensión (PRI)") +
  theme_minimal()

# 5) Matching 1:1 nearest neighbor (sin p_id en la fórmula)
m_pid2 <- matchit(
  gift_PRI ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR),
  data      = gift_pid2_psm,
  method    = "nearest",
  distance  = gift_pid2_psm$ps_pid2,
  s.weights = gift_pid2_psm$ponderador_norm,
  ratio     = 1,
  replace   = TRUE
)

matched_pid2 <- match.data(m_pid2)

# 6) Balance post–matching
bal.tab(m_pid2, data = gift_pid2_psm, un = TRUE)
love.plot(m_pid2, data = gift_pid2_psm, threshold = 0.1)

# 7) Distribución de pscore en la muestra emparejada
ggplot(matched_pid2, aes(x = ps_pid2, fill = gift_PRI, group = gift_PRI)) +
  geom_density(alpha = 0.5) +
  labs(fill  = "Regalo del PRI",
       x     = "Puntuación de propensión",
       y     = "Densidad",
       title = "Distribución de puntajes de propensión (PRI) — Muestra emparejada") +
  theme_minimal()

# 8) Efecto ATT
psm_effect_pid2 <- glm(pid_PRI ~ gift_PRI,
                       data = matched_pid2,
                       weights = weights,
                       family = quasibinomial(link = "logit"))
summary(psm_effect_pid2)

#
### Sensibilidad a sesgos no observados (Rosenbaum bounds) ###
# Obtén la match.matrix
mm_pid2 <- m_pid2$match.matrix
# Crea un data.frame de índices de pares
pairs_pid2 <- tibble(
  id_treated = as.integer(rownames(mm_pid2)),
  id_control = as.integer(mm_pid2[,1])
)
# Extrae el outcome binario de cada uno
treated_out_pid2 <- matched_pid2[pairs_pid2$id_treated, "pid_PRI"]
control_out_pid2 <- matched_pid2[pairs_pid2$id_control, "pid_PRI"]

# Cuenta los pares discordantes
n10 <- sum(treated_out_pid2 == 1 & control_out_pid2 == 0, na.rm = T)  # tratados ganan  
n01 <- sum(treated_out_pid2 == 0 & control_out_pid2 == 1, na.rm = T)  # controles ganan

res <- binarysens(x = n10, y = n01, Gamma = 1.2, GammaInc = 0.1)
str(res$bounds)
gammas <- seq(1, 2, by = 0.1)

bounds_df <- map_dfr(gammas, function(g) {
  res <- binarysens(x     = n10,
                    y     = n01,
                    Gamma = g,
                    GammaInc = 0.1)   # ajuste interno en 0.1
  # toma la última fila de res$bounds$"Lower bound"
  pbd <- tail(res$bounds[["Lower bound"]], 1)
  tibble(Gamma = g, p_bound = pbd)
})

bounds_df

ggplot(bounds_df, aes(x = Gamma, y = p_bound)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(x = expression(Gamma),
       y = "P-value bound",
       title = "Rosenbaum bounds: sensibilidad del ATT") +
  theme_minimal()

# Función para datos binarios emparejados
binarysens(x = n10, y = n01,
           Gamma    = 1,     # arranca en sin sesgo
           GammaInc = 0.1)   # incremento en 0.1

### E-value
or_est <- exp(coef(psm_effect_pid2)["gift_PRIRecibe regalo"])
ci     <- exp(confint(psm_effect_pid2)["gift_PRIRecibe regalo", ])
evalues.OR(est = or_est, lo = ci[1], hi = ci[2], true = 1, rare = FALSE)

### PRD_ID ###

# 1) Filtrar casos completos *sin* p_id
vars_pid3 <- c("gift_PRD","margin","edu","age","sex","type","ln(pob)","YEAR","ponderador_norm")
gift_pid3_psm <- gift[complete.cases(gift[, vars_pid3]), ] %>%
  filter(age >= 18)

# 2) Estimar el propensity score para gift_PRD (sin incluir p_id)
ps_pid3 <- glm(
  gift_PRD ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR),
  data   = gift_pid3_psm,
  family = binomial(link="logit")
)
gift_pid3_psm$ps_pid3 <- ps_pid3$fitted.values

# 3) Preparar variable de tratamiento como factor (opcional para los plots)
gift_pid3_psm <- gift_pid3_psm %>%
  mutate(gift_PRD = factor(gift_PRD, levels = c(0,1),
                           labels = c("No recibe regalo", "Recibe regalo")))

# 4) Diagnóstico de solapamiento
ggplot(gift_pid3_psm, aes(x = ps_pid3, fill = gift_PRD, group = gift_PRD)) +
  geom_density(alpha = 0.5, adjust = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(fill  = "Regalo de PRD",
       x     = "Puntaje de propensión",
       y     = "Densidad",
       title = "Solapamiento de puntuaciones de propensión (PRD)") +
  theme_minimal()

# 5) Matching 1:1 nearest neighbor (sin p_id en la fórmula)
m_pid3 <- matchit(
  gift_PRD ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR),
  data      = gift_pid3_psm,
  method    = "nearest",
  distance  = gift_pid3_psm$ps_pid3,
  s.weights = gift_pid3_psm$ponderador_norm,
  ratio     = 1,
  replace   = TRUE
)

matched_pid3 <- match.data(m_pid3)

# 6) Balance post–matching
bal.tab(m_pid3, data = gift_pid3_psm, un = TRUE)
love.plot(m_pid3, data = gift_pid3_psm, threshold = 0.1)

# 7) Distribución de pscore en la muestra emparejada
ggplot(matched_pid3, aes(x = ps_pid3, fill = gift_PRD, group = gift_PRD)) +
  geom_density(alpha = 0.5) +
  labs(fill  = "Regalo de PRD",
       x     = "Puntuación de propensión",
       y     = "Densidad",
       title = "Distribución de puntajes de propensión (PRD) — Muestra emparejada") +
  theme_minimal()

# 8) Efecto ATT
psm_effect_pid3 <- glm(pid_PRD ~ gift_PRD,
                       data = matched_pid3,
                       weights = weights,
                       family = binomial(link = "logit"))
summary(psm_effect_pid3)

#
### Sensibilidad a sesgos no observados (Rosenbaum bounds) ###
# Obtén la match.matrix
mm_pid3 <- m_pid3$match.matrix
# Crea un data.frame de índices de pares
pairs_pid3 <- tibble(
  id_treated = as.integer(rownames(mm_pid3)),
  id_control = as.integer(mm_pid3[,1])
)
# Extrae el outcome binario de cada uno
treated_out_pid3 <- matched_pid3[pairs_pid3$id_treated, "pid_PRD"]
control_out_pid3 <- matched_pid3[pairs_pid3$id_control, "pid_PRD"]

# Cuenta los pares discordantes
n10 <- sum(treated_out_pid3 == 1 & control_out_pid3 == 0, na.rm = T)  # tratados ganan  
n01 <- sum(treated_out_pid3 == 0 & control_out_pid3 == 1, na.rm = T)  # controles ganan

res <- binarysens(x = n10, y = n01, Gamma = 1.2, GammaInc = 0.1)
str(res$bounds)
gammas <- seq(1, 2, by = 0.1)

bounds_df <- map_dfr(gammas, function(g) {
  res <- binarysens(x     = n10,
                    y     = n01,
                    Gamma = g,
                    GammaInc = 0.1)   # ajuste interno en 0.1
  # toma la última fila de res$bounds$"Lower bound"
  pbd <- tail(res$bounds[["Lower bound"]], 1)
  tibble(Gamma = g, p_bound = pbd)
})

bounds_df

ggplot(bounds_df, aes(x = Gamma, y = p_bound)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(x = expression(Gamma),
       y = "P-value bound",
       title = "Rosenbaum bounds: sensibilidad del ATT") +
  theme_minimal()

# Función para datos binarios emparejados
binarysens(x = n10, y = n01,
           Gamma    = 1,     # arranca en sin sesgo
           GammaInc = 0.1)   # incremento en 0.1

### E-value
or_est <- exp(coef(psm_effect_pid3)["gift_PRDRecibe regalo"])
ci     <- exp(confint(psm_effect_pid3)["gift_PRDRecibe regalo", ])
evalues.OR(est = or_est, lo = ci[1], hi = ci[2], true = 1, rare = FALSE)


### MRN_ID ###

# 1) Filtrar casos completos *sin* p_id
vars_pid4 <- c("gift_MRN","margin","edu","age","sex","type","ln(pob)","YEAR","ponderador_norm","eth")
gift_pid4_psm <- gift[complete.cases(gift[, vars_pid4]), ] %>%
  filter(age >= 18)

# 2) Estimar el propensity score para gift_MRN (sin incluir p_id)
ps_pid4 <- glm(
  gift_MRN ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR)+eth,
  data   = gift_pid4_psm,
  family = binomial(link="logit")
)
gift_pid4_psm$ps_pid4 <- ps_pid4$fitted.values

# 3) Preparar variable de tratamiento como factor (opcional para los plots)
gift_pid4_psm <- gift_pid4_psm %>%
  mutate(gift_MRN = factor(gift_MRN, levels = c(0,1),
                           labels = c("No recibe regalo", "Recibe regalo")))

# 4) Diagnóstico de solapamiento
ggplot(gift_pid4_psm, aes(x = ps_pid4, fill = gift_MRN, group = gift_MRN)) +
  geom_density(alpha = 0.5, adjust = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(fill  = "Regalo de MRN",
       x     = "Puntaje de propensión",
       y     = "Densidad",
       title = "Solapamiento de puntuaciones de propensión (MRN)") +
  theme_minimal()

# 5) Matching 1:1 nearest neighbor (sin p_id en la fórmula)
m_pid4 <- matchit(
  gift_MRN ~ margin + edu + age + sex + type + `ln(pob)` + factor(YEAR) +eth,
  data      = gift_pid4_psm,
  method    = "nearest",
  distance  = gift_pid4_psm$ps_pid4,
  s.weights = gift_pid4_psm$ponderador_norm,
  ratio     = 1,
  replace   = TRUE
)

matched_pid4 <- match.data(m_pid4)

# 6) Balance post–matching
bal.tab(m_pid4, data = gift_pid4_psm, un = TRUE)
love.plot(m_pid4, data = gift_pid4_psm, threshold = 0.1)

# 7) Distribución de pscore en la muestra emparejada
ggplot(matched_pid4, aes(x = ps_pid4, fill = gift_MRN, group = gift_MRN)) +
  geom_density(alpha = 0.5) +
  labs(fill  = "Regalo de MRN",
       x     = "Puntuación de propensión",
       y     = "Densidad",
       title = "Distribución de puntajes de propensión (MRN) — Muestra emparejada") +
  theme_minimal()

# 8) Efecto ATT
psm_effect_pid4 <- glm(pid_MRN ~ gift_MRN,
                       data = matched_pid4,
                       weights = weights,
                       family = binomial(link = "logit"))
summary(psm_effect_pid4)

### ATT by year ###

effects_by_year <- matched_pid1 %>%
  group_by(YEAR) %>% 
  do(tidy(
    glm(pid_PAN ~ gift_PAN,
        data    = .,
        weights = weights,
        family  = binomial(link="logit"))
  )) %>%
  filter(term == "gift_PAN") %>%
  select(YEAR, estimate, std.error, p.value) %>%
  mutate(
    OR    = exp(estimate),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )
effects_by_year

# ******************************************************************************
### WLS por partido ###
# ******************************************************************************

pid1_fe <- felm(pid_PAN ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + margin + edu + age + sex + type + `ln(pob)`+ eth
                | YEAR,
                data = gift,
                weights = gift$ponderador_norm)
summary(pid1_fe)

pid2_fe <- felm(pid_PRI ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + margin + edu + age + sex + type + `ln(pob)` + eth 
                | YEAR,
                data = gift,
                weights = gift$ponderador_norm)
summary(pid2_fe)

pid3_fe <- felm(pid_PRD ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + margin + edu + age + sex + type + `ln(pob)` + eth 
                | YEAR,
                data = gift,
                weights = gift$ponderador_norm)
summary(pid3_fe)

pid4_fe <- felm(pid_MRN ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + margin + edu + age + sex + type + `ln(pob)` + eth 
                | YEAR,
                data = gift,
                weights = gift$ponderador_norm)
summary(pid4_fe)

# ******************************************************************************
# ---- XI. Trend by party ----
# ******************************************************************************
wls_trend_PAN <- lm(vote_PAN ~ gift_PAN * factor(YEAR)
                    + margin + edu + age + sex + p_id + type + `ln(pob)`,
                    data    = gift,
                    weights = ponderador_norm)
summary(wls_trend_PAN)

wls_trend_PRI <- lm(vote_PRI ~ gift_PRI * factor(YEAR)
                + margin + edu + age + sex + p_id + type + `ln(pob)`,
                data    = gift,
                weights = ponderador_norm)
summary(wls_trend_PRI)
#Frecuencia por año
table(gift$YEAR, gift$gift_PRI)


# ******************************************************************************
# ---- XII. Exlusion ----
# ******************************************************************************

excluded_fe <- felm(VOTED ~ excluded + margin + edu + age + sex + p_id + type + `ln(pob)`
                  | YEAR,
                  data = gift,
                  weights = gift$ponderador_norm)
summary(excluded_fe)

gift <- gift %>%
  mutate(
    excluded = if_else(
      knowofclientelism == 1 &
        (gift == 0 | is.na(gift)),
      1L,   # sí excluido
      0L    # no excluido
    )
  )

gift$excluded <- as.numeric(gift$excluded)

vars_exclu <- c("excluded", "margin", "edu", "age", "sex", "p_id", "type", "ln(pob)", "YEAR", "ponderador_norm")
exclu_psm <- gift[complete.cases(gift[, vars_exclu]), ]
ps_model_exclu <- glm(excluded ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                      data = exclu_psm, 
                      family = binomial(link = "logit"))
exclu_psm$pscore <- ps_model_exclu$fitted.values

##
ggplot(exclu_psm, aes(x = pscore, fill = factor(excluded))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Excluido", x = "Puntuación de propensión", y = "Densidad",
       title = "Solapamiento de los puntajes de propensión")+
  theme_minimal()

# Match entre excluidos vs. no excluidos
match_exclusion <- matchit(excluded ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR), 
                           data = exclu_psm, 
                           method = "nearest",
                           distance = exclu_psm$pscore,
                           s.weights = exclu_psm$ponderador_norm,
                           ratio = 1,
                           replace = T)

matched_data_exclu <- match.data(match_exclusion)


ggplot(matched_data_exclu, aes(x = pscore, fill = factor(excluded))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Tratamiento", x = "Puntuación de Propensión", y = "Densidad",
       title = "Distribución de los Puntajes de Propensión")+
  theme_minimal()

# Efecto de la exclusión sobre turnout

glm_exclusion <- glm(VOTED ~ excluded, 
                     data = matched_data_exclu, 
                     weights = weights,
                     family = binomial(link = "logit"))
summary(glm_exclusion)

# ATT by year 

effects_by_year <- matched_data_exclu %>%
  group_by(YEAR) %>% 
  do(tidy(
    glm(VOTED ~ excluded,
        data    = .,
        weights = weights,
        family  = binomial(link="logit"))
  )) %>%
  filter(term == "excluded") %>%
  select(YEAR, estimate, std.error, p.value) %>%
  mutate(
    OR    = exp(estimate),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )
effects_by_year


# ******************************************************************************
# ---- XIII. Social program ----
# ******************************************************************************


gift <- gift %>% 
  rename("receive_sp" = `RECEIVE_SOCIAL PROGRAMS`)
gift$receive_sp <- NA
gift$receive_sp[gift$YEAR %in% c(2003, 2015, 2018, 2021, 2024)] <- sp$receive_sp
gift <- gift %>%
  mutate(receive_sp = as.numeric(as.character(receive_sp)))

vars_sp <- c("receive_sp", "margin", "edu", "age", "sex", "p_id", "type", "ln(pob)", "YEAR", "ponderador_norm")
sp_psm <- gift[complete.cases(gift[, vars_sp]), ]
ps_model_sp <- glm(receive_sp ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR),
                      data = sp_psm, 
                      family = binomial(link = "logit"))
sp_psm$pscore <- ps_model_sp$fitted.values

##
ggplot(sp_psm, aes(x = pscore, fill = factor(receive_sp))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Beneficiario", x = "Puntuación de propensión", y = "Densidad",
       title = "Solapamiento de los puntajes de propensión")+
  theme_minimal()

# Match social program
match_sp <- matchit(receive_sp ~ margin + edu + age + sex + p_id + type + `ln(pob)` + factor(YEAR), 
                           data = sp_psm, 
                           method = "nearest",
                           distance = sp_psm$pscore,
                           s.weights = sp_psm$ponderador_norm,
                           ratio = 1,
                           replace = T)

matched_data_sp <- match.data(match_sp)

# Balance post–matching
bal.tab(match_sp, data = sp_psm, un = TRUE)
love.plot(match_sp, data = sp_psm, threshold = 0.1)

ggplot(matched_data_sp, aes(x = pscore, fill = factor(receive_sp))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Tratamiento", x = "Puntuación de Propensión", y = "Densidad",
       title = "Distribución de los Puntajes de Propensión")+
  theme_minimal()

# Efecto

effect_sp <- glm(vote_PRI ~ receive_sp, 
                     data = matched_data_sp, 
                     weights = weights,
                     family = binomial(link = "logit"))
summary(effect_sp)

# ATT by year 

effects_by_year <- matched_data_sp %>%
  group_by(YEAR) %>% 
  do(tidy(
    glm(vote_PRI ~ receive_sp,
        data    = .,
        weights = weights,
        family  = binomial(link="logit"))
  )) %>%
  filter(term == "receive_sp") %>%
  select(YEAR, estimate, std.error, p.value) %>%
  mutate(
    OR    = exp(estimate),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )
effects_by_year

## Rosenbaum
# Obtén la match.matrix
mm_sp <- match_sp$match.matrix

# Crea un data.frame de índices de pares
library(tibble)
pairs_sp <- tibble(
  id_treated = as.integer(rownames(mm_sp)),
  id_control = as.integer(mm_sp[,1])
)

# Extrae el outcome binario de cada uno
treated_out_sp <- matched_data_sp[pairs_sp$id_treated, "VOTED"]
control_out_sp <- matched_data_sp[pairs_sp$id_control, "VOTED"]

# Cuenta los pares discordantes
n10 <- sum(treated_out_sp == 1 & control_out_sp == 0, na.rm = T)  # tratados ganan  
n01 <- sum(treated_out_sp == 0 & control_out_sp == 1, na.rm = T)  # controles ganan


# Función para datos binarios emparejados
binarysens(x = n10, y = n01,
           Gamma    = 1,     # arranca en sin sesgo
           GammaInc = 0.1)   # incremento en 0.1

## E-value
install.packages("EValue")
library(EValue)

# Extrae OR y su IC del modelo effect_sp
or_sp <- exp(coef(effect_sp)["receive_sp"])      
ci_sp <- exp(confint(effect_sp)["receive_sp", ])

#Calculando prevalencia
prop_voted <- mean(matched_data_sp$VOTED == 1, na.rm=T)

# Calcula E-value
evalues.OR(est = or_sp,
           lo  = ci_sp[1],
           hi  = ci_sp[2],
           true= 1,
           rare = FALSE)
