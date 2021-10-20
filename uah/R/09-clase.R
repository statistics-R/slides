# Sesion 8: Regresion Lineal Multiple -------------------------------------
## Codigo de Procesamiento

# 1. Cargar paquetes ------------------------------------------------------
pacman::p_load(tidyverse, sjPlot, sjmisc, survey, srvyr)

# 2. Cargar datos ---------------------------------------------------------
datos <- haven::read_dta("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/stata_esi/2020/esi-2020---personas.dta?sfvrsn=7a4b0e2b_4&amp;download=true")

# 3. Explorar datos -------------------------------------------------------
names(datos)

view_df(datos)

sjmisc::find_var(datos, "conglomerado")

# 4. Procesar variables ------------------------------------------------

datos_proc <- datos %>%
  select(ing_t_t, edad, sexo, est_conyugal, ciuo08 = b1, fact_cal_esi) %>% 
  mutate(est_conyugal = if_else(est_conyugal %in% c(1,2), "Con pareja",
                                if_else(est_conyugal %in% c(3,4,5,6),"Sin pareja", NA_character_)))


# 5. Guardar --------------------------------------------------------------
saveRDS(file = "output/data/datos_proc.rds")


# 6. Modelo ------------------------------------------------------------------

modelo0 <- lm(ing_t_t ~ 1,
              data = datos_proc)
modelo0 <- lm(ing_t_t ~ 1,
              data = datos_proc, weights = fact_cal_esi)
summary(modelo0)


modelo1 <- lm(ing_t_t ~ edad,
              data = datos_proc, weights = fact_cal_esi)

modelo2 <- lm(ing_t_t ~ sexo,
              data = datos_proc, weights = fact_cal_esi)
datos$sexo <-  forcats::as_factor(datos$sexo)
modelo2 <- lm(ing_t_t ~ sexo,
              data = datos_proc, weights = fact_cal_esi)

modelo3 <- lm(ing_t_t ~ ciuo08,
              data = datos_proc, weights = fact_cal_esi)
datos_proc$ciuo08 <-  forcats::as_factor(datos_proc$ciuo08)
modelo3 <- lm(ing_t_t ~ ciuo08,
              data = datos_proc, weights = fact_cal_esi)

modelo4 <- lm(ing_t_t ~ est_conyugal,
              data = datos_proc, weights = fact_cal_esi)

modelo5 <- lm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
              data = datos_proc, weights = fact_cal_esi)

modelo6 <- glm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
               family = gaussian(link = "identity"),
               data = datos_proc, weights = fact_cal_esi)

# Exploratorio
modelo_general <- lm(ing_t_t ~ .,
             data = datos_proc, weights = datos_proc$fact_cal_esi,
             na.action= na.omit)

step(modelo_general, keep = nobs)


#
datos_proc <- fastDummies::dummy_cols(datos_proc, select_columns = "est_conyugal")
modelo6 <- glm(`est_conyugal_Con pareja` ~ edad + sexo + ciuo08 ,
               family = binomial(link = "logit"),
               data = datos_proc, weights = fact_cal_esi)

