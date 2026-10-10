# ANALÍTICA DE LOS NEGOCIOS
# CASO 2: Hollywood Rules (Kellogg, KEL700)
# Integrantes: Valerie Beltrán, Juanita Heredia, Mariana Ávila


install.packages(c("readxl", "dplyr"))
library(readxl)
library(dplyr)


options(scipen = 999)   # evita notación científica


# Carga y limpieza de datos
datos <- read_excel("Hollywood.xls", sheet = "Exhibit 1")


# Para evitar espacios y nombres extraños
names(datos) <- c("movie", "opening_gross", "us_gross", "non_us_gross",
                  "budget", "opening_theatres", "known_story", "sequel",
                  "origin_us", "genre", "summer", "holiday", "christmas",
                  "mpaa", "mpaa_d", "critics", "oscar_nom", "oscar_won")


datos <- datos %>%
  mutate(across(c(movie, genre, mpaa), trimws),
         comedy = ifelse(genre == "Comedy", 1, 0),      # 1 = comedia, 0 = otro género
         roi    = (us_gross - budget) / budget)         # ROI en EE. UU.
 
print(dim(datos))


# PUNTO 1: Panorama inicial de los datos
vars_1 <- c("opening_gross", "us_gross", "non_us_gross", "opening_theatres")


resumen_1 <- data.frame(
  Variable = c("Opening gross", "Total U.S. gross",
               "Total non-U.S. gross", "Opening theatres"),
  Minimo   = sapply(datos[vars_1], min),
  Promedio = sapply(datos[vars_1], mean),
  Maximo   = sapply(datos[vars_1], max),
  row.names = NULL
)
cat("\nTabla 1.1: Mínimo, promedio y máximo de las variables principales\n")
print(resumen_1)
write.csv(resumen_1, "tabla_1_1_descriptivos.csv", row.names = FALSE)


n_comedias <- sum(datos$genre == "Comedy")
n_R        <- sum(datos$mpaa == "R")
cat("\nNúmero de películas de comedia:", n_comedias, "\n")
cat("Número de películas con clasificación R:", n_R, "\n")


# Tabla 1.2
tab_genero <- as.data.frame(table(datos$genre))
names(tab_genero) <- c("Genero", "Frecuencia")
cat("\nTabla 1.2: Número de películas por género\n")
print(tab_genero)
write.csv(tab_genero, "tabla_1_2_peliculas_por_genero.csv", row.names = FALSE)


# Tabla 1.3
tab_mpaa <- as.data.frame(table(datos$mpaa))
names(tab_mpaa) <- c("MPAA", "Frecuencia")
cat("\nTabla 1.3: Número de películas por clasificación MPAA\n")
print(tab_mpaa)
write.csv(tab_mpaa, "tabla_1_3_peliculas_por_mpaa.csv", row.names = FALSE)




# PUNTO 2: ROI en EE. UU. vs. el 12 % que cita Michael London
# 2a. ROI por película
cat("\nTabla 2a.1: ROI de EE. UU. por película (primeras 10)\n")
tabla_2a <- head(datos %>% select(movie, budget, us_gross, roi), 10)
print(tabla_2a)
write.csv(tabla_2a, "tabla_2a_roi_por_pelicula.csv", row.names = FALSE)


cat("\nTabla 2a.2: Resumen estadístico del ROI\n")
resumen_roi <- as.data.frame(t(summary(datos$roi)))
print(summary(datos$roi))
write.csv(resumen_roi, "tabla_2a_2_resumen_roi.csv", row.names = FALSE)


# 2b. IC 95 % para el ROI medio
ic_roi <- t.test(datos$roi, conf.level = 0.95)
cat("\nPunto 2b: Intervalo de confianza del 95 % para el ROI medio\n")
cat("ROI medio:", ic_roi$estimate, "\n")
cat("IC 95 %: [", ic_roi$conf.int[1], ",", ic_roi$conf.int[2], "]\n")


tabla_2b <- data.frame(
  Estadistico = c("ROI medio", "Limite inferior IC 95%", "Limite superior IC 95%",
                  "t", "gl", "p-value"),
  Valor = c(ic_roi$estimate, ic_roi$conf.int[1], ic_roi$conf.int[2],
            ic_roi$statistic, ic_roi$parameter, ic_roi$p.value)
)
write.csv(tabla_2b, "tabla_2b_ic_roi.csv", row.names = FALSE)


# 2c. H0: mu <= 0.12 vs H1: mu > 0.12
prueba_roi <- t.test(datos$roi, mu = 0.12, alternative = "greater")
cat("\nPunto 2c: Prueba t del ROI medio (H0: mu <= 0.12 vs H1: mu > 0.12)\n")
print(prueba_roi)


tabla_2c <- data.frame(
  Estadistico = c("t", "gl", "p-value", "media muestral", "H0 mu"),
  Valor = c(prueba_roi$statistic, prueba_roi$parameter, prueba_roi$p.value,
            prueba_roi$estimate, 0.12)
)
write.csv(tabla_2c, "tabla_2c_prueba_t_roi.csv", row.names = FALSE)




# PUNTO 3: Antes de la producción: ¿importa el género comedia?
# 3a. Total U.S. gross: comedias vs. no comedias
tabla_3a <- datos %>%
  group_by(comedy) %>%
  summarise(n = n(),
            promedio_us_gross = mean(us_gross),
            sd_us_gross = sd(us_gross)) %>%
  as.data.frame()
cat("\nTabla 3a.1: Total U.S. gross por grupo (comedy: 1 = comedia, 0 = otro género)\n")
print(tabla_3a)
write.csv(tabla_3a, "tabla_3a_us_gross_por_grupo.csv", row.names = FALSE)


t_3a <- t.test(us_gross ~ comedy, data = datos)
cat("\nTabla 3a.2: Prueba t del Total U.S. gross, comedias vs. no comedias\n")
print(t_3a)


tabla_3a_2 <- data.frame(
  Estadistico = c("t", "gl", "p-value",
                  "media grupo 0", "media grupo 1",
                  "IC 95% inf", "IC 95% sup"),
  Valor = c(t_3a$statistic, t_3a$parameter, t_3a$p.value,
            t_3a$estimate[1], t_3a$estimate[2],
            t_3a$conf.int[1], t_3a$conf.int[2])
)
write.csv(tabla_3a_2, "tabla_3a_2_prueba_t_us_gross.csv", row.names = FALSE)


# 3b. ROI: comedias vs. no comedias
tabla_3b <- datos %>%
  group_by(comedy) %>%
  summarise(n = n(),
            promedio_roi = mean(roi),
            sd_roi = sd(roi)) %>%
  as.data.frame()
cat("\nTabla 3b.1: ROI por grupo (comedy: 1 = comedia, 0 = otro género)\n")
print(tabla_3b)
write.csv(tabla_3b, "tabla_3b_roi_por_grupo.csv", row.names = FALSE)


t_3b <- t.test(roi ~ comedy, data = datos)
cat("\nTabla 3b.2: Prueba t del ROI, comedias vs. no comedias\n")
print(t_3b)


tabla_3b_2 <- data.frame(
  Estadistico = c("t", "gl", "p-value",
                  "media grupo 0", "media grupo 1",
                  "IC 95% inf", "IC 95% sup"),
  Valor = c(t_3b$statistic, t_3b$parameter, t_3b$p.value,
            t_3b$estimate[1], t_3b$estimate[2],
            t_3b$conf.int[1], t_3b$conf.int[2])
)
write.csv(tabla_3b_2, "tabla_3b_2_prueba_t_roi.csv", row.names = FALSE)


Punto 4a 
# Copiamos los datos de Hollywood que cargaron mis compañeras
datos_b <- as.data.frame(datos[, 1:18])


# Usamos los nombres de la primera parte de su código
names(datos_b) <- c(
  "movie", "opening_gross", "us_gross", "non_us_gross",
  "budget", "opening_theatres", "known_story", "sequel",
  "origin_us", "genre", "summer", "holiday", "christmas",
  "mpaa", "mpaa_d", "critics", "oscar_nom", "oscar_won"
)


# Verificamos que sean las 75 películas de Hollywood
stopifnot(nrow(datos_b) == 75)


# Creamos la variable de comedia
datos_bcomedy<-ifelse(trimws(datosbgenre) == "Comedy", 1, 0)


# Evitamos notación científica
options(scipen = 999)
# Calculamos el promedio de ingresos por clasificación
tabla_4a <- datos_b %>%
  group_by(mpaa_d) %>%
  summarise(
    n = n(),
    promedio_us_gross = mean(us_gross),
    sd_us_gross = sd(us_gross)
  )


# Mostramos la tabla: 0 = otras clasificaciones; 1 = R
print(tabla_4a)


# H0: los ingresos promedio son iguales
# H1: los ingresos promedio son diferentes
t_4a <- t.test(us_gross ~ mpaa_d, data = datos_b)
print(t_4a)


# Guardamos la tabla
write.csv(tabla_4a, "tabla_4a_clasificacion.csv", row.names = FALSE)


# Respuesta: p = 0.3979, mayor que 0.05
# No hay evidencia suficiente de una diferencia de ingresos promedio




# PUNTO 5: Regresión antes de la producción: Total U.S. gross
# Función auxiliar para convertir coefficients de lm a data.frame limpio
coef_df <- function(modelo) {
  m <- as.data.frame(summary(modelo)$coefficients)
  m$Variable <- rownames(m)
  names(m) <- c("Estimate", "Std_Error", "t_value", "p_value", "Variable")
  m <- m[, c("Variable", "Estimate", "Std_Error", "t_value", "p_value")]
  rownames(m) <- NULL
  return(m)
}


# 5a. Modelo completo
modelo_5a <- lm(us_gross ~ budget + comedy + mpaa_d + known_story + sequel,
                data = datos)
cat("\nTabla 5a: Regresión completa del Total U.S. gross\n")
print(summary(modelo_5a))
write.csv(coef_df(modelo_5a), "tabla_5a_regresion_completa.csv", row.names = FALSE)


# 5b. Eliminación hacia atrás al 10 %
# Paso 1: se elimina mpaa_d
modelo_5b_1 <- lm(us_gross ~ budget + comedy + known_story + sequel, data = datos)
cat("\nTabla 5b.1: Regresión sin mpaa_d\n")
print(summary(modelo_5b_1))
write.csv(coef_df(modelo_5b_1), "tabla_5b_1_sin_mpaa_d.csv", row.names = FALSE)


# Paso 2: se elimina known_story -> modelo final
modelo_5b_2 <- lm(us_gross ~ budget + comedy + sequel, data = datos)
cat("\nTabla 5b.2: Regresión sin mpaa_d ni known_story (modelo final)\n")
print(summary(modelo_5b_2))
write.csv(coef_df(modelo_5b_2), "tabla_5b_2_modelo_final.csv", row.names = FALSE)


modelo_final <- modelo_5b_2
cat("\nTabla 5b.3: Intervalos de confianza del 95 % de los coeficientes del modelo final\n")
ic_final <- as.data.frame(confint(modelo_final))
ic_final$Variable <- rownames(ic_final)
names(ic_final) <- c("IC_2.5", "IC_97.5", "Variable")
ic_final <- ic_final[, c("Variable", "IC_2.5", "IC_97.5")]
rownames(ic_final) <- NULL
print(ic_final)
write.csv(ic_final, "tabla_5b_3_ic_modelo_final.csv", row.names = FALSE)


# 5c. Secuela vs. no secuela
cat("\nPunto 5c: Coeficiente de sequel en el modelo final\n")
print(coef(modelo_final)["sequel"])


tabla_5c <- data.frame(
  Variable = "sequel",
  Coeficiente = unname(coef(modelo_final)["sequel"]),
  IC_2.5 = unname(confint(modelo_final)["sequel", 1]),
  IC_97.5 = unname(confint(modelo_final)["sequel", 2])
)
write.csv(tabla_5c, "tabla_5c_coef_sequel.csv", row.names = FALSE)
Punto 6a
# Incluimos factores de preproducción y anteriores al estreno
modelo_6a <- lm(
  opening_gross ~ budget + comedy + mpaa_d + known_story +
    sequel + summer + holiday + christmas + opening_theatres,
  data = datos_b
)


# Mostramos el modelo completo
print(summary(modelo_6a))


# Guardamos los coeficientes con la función de mis compañeras
write.csv(
  coef_df(modelo_6a),
  "tabla_6a_modelo_completo.csv",
  row.names = FALSE
)
Punto 6b
# Eliminamos holiday y revisamos el modelo
modelo_6b_1 <- update(modelo_6a, . ~ . - holiday)
print(summary(modelo_6b_1))


# Eliminamos mpaa_d
modelo_6b_2 <- update(modelo_6b_1, . ~ . - mpaa_d)
print(summary(modelo_6b_2))


# Eliminamos comedy
modelo_6b_3 <- update(modelo_6b_2, . ~ . - comedy)
print(summary(modelo_6b_3))


# Eliminamos christmas
modelo_6b_4 <- update(modelo_6b_3, . ~ . - christmas)
print(summary(modelo_6b_4))


# Eliminamos known_story
modelo_6b_5 <- update(modelo_6b_4, . ~ . - known_story)
print(summary(modelo_6b_5))


# Eliminamos summer y obtenemos el modelo final
modelo_6b <- update(modelo_6b_5, . ~ . - summer)
print(summary(modelo_6b))


# Guardamos los coeficientes finales
write.csv(
  coef_df(modelo_6b),
  "tabla_6b_modelo_final.csv",
  row.names = FALSE
)


# Modelo final:
# Opening gross = -10357244.70 + 0.113807 * budget
#                 + 9095871.81 * sequel
#                 + 7681.01 * opening_theatres


Punto 6c
# Mostramos las pendientes del modelo final
print(coef(modelo_6b))


# Manteniendo los demás factores constantes:
# Budget: USD 1 millón adicional de presupuesto se asocia
# con USD 113806.96 adicionales de ingresos del estreno


# Sequel: una secuela tiene ingresos esperados del estreno
# mayores en USD 9095871.81 frente a una película que no es secuela


# Opening theatres: una sala adicional se asocia
# con USD 7681.01 adicionales de ingresos del estreno


# Son asociaciones estadísticas, no pruebas de causalidad
Punto 6d 
# Calculamos el cambio esperado por 100 salas adicionales
cambio_6d <- 100 * coef(modelo_6b)["opening_theatres"]


# Calculamos el intervalo de confianza del 95 %
ic_6d <- 100 * confint(modelo_6b, "opening_theatres", level = 0.95)


# Organizamos los resultados
tabla_6d <- data.frame(
  Cambio_esperado_USD = unname(cambio_6d),
  IC_95_inferior_USD = ic_6d[1, 1],
  IC_95_superior_USD = ic_6d[1, 2]
)


# Mostramos y guardamos la tabla
print(tabla_6d)
write.csv(tabla_6d, "tabla_6d_100_salas.csv", row.names = FALSE)


# Respuesta: aumento esperado de USD 768101.41
# IC 95 %: USD 476688.21 a USD 1059514.62
# Manteniendo los demás factores constantes


Punto 7a
# Explicamos los ingresos totales a partir de los del estreno
modelo_7a <- lm(us_gross ~ opening_gross, data = datos_b)


# Mostramos y guardamos los resultados
print(summary(modelo_7a))
write.csv(
  coef_df(modelo_7a),
  "tabla_7a_regresion_simple.csv",
  row.names = FALSE
)


# Modelo:
# Total U.S. gross = 5108220.42 + 3.120619 * opening_gross


Punto 7b
# Si opening_gross = 0.25 * us_gross
# Entonces us_gross = 4 * opening_gross
# La pendiente debe ser 4
# La relación exacta también exige intercepto 0


cat("\nLa pendiente teórica es 4 y el intercepto es 0.\n")


Punto 7c 
# H0: pendiente = 4
# H1: pendiente diferente de 4


# Extraemos la pendiente y su error estándar
pendiente_7c <- coef(modelo_7a)["opening_gross"]
error_7c <- summary(modelo_7a)$coefficients["opening_gross", "Std. Error"]


# Calculamos el estadístico t y el p-valor bilateral
t_7c <- (pendiente_7c - 4) / error_7c
p_7c <- 2 * pt(-abs(t_7c), df.residual(modelo_7a))


# Organizamos los resultados
tabla_7c <- data.frame(
  Pendiente_estimada = unname(pendiente_7c),
  Pendiente_teorica = 4,
  t = unname(t_7c),
  p_valor = unname(p_7c)
)


# Mostramos y guardamos la tabla
print(tabla_7c)
write.csv(tabla_7c, "tabla_7c_prueba_pendiente.csv", row.names = FALSE)


# Respuesta: p = 0.000134, menor que 0.05
# La prueba convencional rechaza una pendiente de 4
# Su validez depende de los supuestos que revisamos en 7d


Punto 7d 
# Mostramos los diagnósticos de la regresión simple
par(mfrow = c(2, 2))
plot(modelo_7a, which = c(1, 2, 3, 5))
par(mfrow = c(1, 1))


# Aplicamos la prueba de varianza constante que usan mis compañeras
lmtest::bptest(modelo_7a)


# Crítica:
# Revisamos si la dispersión de los residuos aumenta con los ajustados
# Si la varianza cambia, los errores estándar convencionales
# y el p-valor del punto 7c pueden ser poco confiables


# También revisamos normalidad y observaciones influyentes
# La regla exacta exige pendiente 4 e intercepto 0
# El punto 7c únicamente prueba la pendiente


# La muestra está limitada a 75 películas con presupuestos
# entre USD 20 y 100 millones
# Los puntos 7e y 7f de mis compañeras revisan un modelo alternativo


# =============================================================
# Puntos: 7e, 7g, 8, 9 y 10
# =============================================================
# Archivo de datos: Hollywood.xls (hoja "Exhibit 1", 75 películas)


install.packages(c("readxl", "lmtest"))
library(readxl)
library(lmtest)


datos <- read_excel("Hollywood.xls", sheet = "Exhibit 1")


# Renombramos las 18 columnas (en el orden del Excel) a nombres cortos
names(datos) <- c("movie", "opening", "total", "non_us", "budget", "theatres",
                  "known", "sequel", "origin_us", "genre", "summer", "holiday",
                  "christmas", "mpaa", "r_rated", "critics",
                  "oscar_nom", "oscar_won")


# Dummy de comedia y variables en millones de USD
datos$comedy    <- as.integer(trimws(datos$genre) == "Comedy")
datos$total_m   <- datos$total   / 1e6
datos$opening_m <- datos$opening / 1e6
datos$budget_m  <- datos$budget  / 1e6


# Logaritmos (se usan desde el punto 7e)
datos$ln_total   <- log(datos$total_m)
datos$ln_opening <- log(datos$opening_m)


str(datos)


# Función de eliminación hacia atrás: quita una a una la variable con
# mayor p-valor mientras sea mayor a 0.10
backward_10 <- function(modelo) {
  repeat {
    p <- summary(modelo)$coefficients[-1, 4]
    if (max(p) <= 0.10) break
    quitar <- names(p)[which.max(p)]
    cat("Se elimina:", quitar, "(p =", round(max(p), 4), ")\n")
    modelo <- update(modelo, as.formula(paste(". ~ . -", quitar)))
  }
  modelo
}


# =============================================================
# PUNTO 7e - Modelo sólido: total U.S. gross vs opening gross
# =============================================================
# Regresión simple de 7a (solo para comparar)
m7_simple <- lm(total_m ~ opening_m, data = datos)
summary(m7_simple)


par(mfrow = c(1, 2))
plot(fitted(m7_simple), resid(m7_simple), main = "Simple: residuos",
     xlab = "Ajustados", ylab = "Residuos"); abline(h = 0, col = "red")


# Modelo log-log
m7e <- lm(ln_total ~ ln_opening, data = datos)
summary(m7e)


plot(fitted(m7e), resid(m7e), main = "Log-log: residuos",
     xlab = "Ajustados", ylab = "Residuos"); abline(h = 0, col = "red")
par(mfrow = c(1, 1))


# Prueba de Breusch-Pagan (H0: varianza constante)
bptest(m7_simple)   # p ≈ 0.06 -> evidencia de heterocedasticidad
bptest(m7e)         # p ≈ 0.17 -> no se rechaza varianza constante


# RESPUESTA 7e:
# En la regresión simple los residuos se abren como un embudo: a mayor
# opening gross, mayor dispersión (heterocedasticidad). Además unas pocas
# películas muy taquilleras pesan mucho en el ajuste. Por eso los errores
# estándar y las pruebas t no son confiables.
# Un modelo más sólido es el log-log:
#     ln(Total US gross) = 1.2526 + 0.9766 * ln(Opening gross)
# Los residuos se ven homogéneos y la prueba BP ya no rechaza H0.
# Interpretación: si el opening gross sube 1 %, el total U.S. gross sube
# aproximadamente 0.98 %.


# (Apoyo, punto 7f) Si el opening fuera el 25 % del total:
# total = 4 * opening  ->  ln(total) = ln(4) + 1 * ln(opening)
b  <- coef(m7e)
se <- sqrt(diag(vcov(m7e)))
gl <- df.residual(m7e)
t_pend <- (b["ln_opening"] - 1) / se["ln_opening"]
t_int  <- (b["(Intercept)"] - log(4)) / se["(Intercept)"]
cat("H0: pendiente = 1    -> t =", round(t_pend, 3),
    " p =", round(2 * pt(-abs(t_pend), gl), 4), "\n")
cat("H0: intercepto = ln4 -> t =", round(t_int, 3),
    " p =", round(2 * pt(-abs(t_int), gl), 4), "\n")


# =============================================================
# PUNTO 7g - Proporción de la variación explicada
# =============================================================
r2_7e <- summary(m7e)$r.squared
cat("R2 del modelo log-log:", round(r2_7e, 4), "\n")


# RESPUESTA 7g:
# R2 = 0.7511. Cerca del 75 % de la variación de ln(total U.S. gross) se
# explica por la variación de ln(opening gross). (En el modelo simple sin
# logaritmos el R2 es 0.737, pero ese modelo tiene problemas de varianza.)


# =============================================================
# PUNTO 8 - Total U.S. gross con todo lo que se sabe tras el estreno
# =============================================================
# 8a) Modelo completo: factores de preproducción (budget, comedy, R,
# known story, sequel), de antes del estreno (summer, holiday, christmas,
# theatres) y de después (opening gross y critics' opinion).
m8_full <- lm(ln_total ~ ln_opening + budget_m + comedy + r_rated + known +
                sequel + summer + holiday + christmas + theatres + critics,
              data = datos)
summary(m8_full)


# 8b) Eliminamos lo que no es significativo al 10 %
m8 <- backward_10(m8_full)
summary(m8)
bptest(m8)


# RESPUESTA 8b (modelo final, R2 = 0.845, R2 ajustado = 0.836):
# ln(Total US gross) = 0.7937 + 0.8725 ln(Opening) + 0.00440 Budget(M)
#                      + 0.1599 Comedy + 0.00935 Critics
# Se eliminaron, en este orden: christmas, holiday, theatres, sequel,
# known, r_rated y summer.
# - Opening: +1 % de opening -> +0.87 % de total US gross.
# - Budget: +1 millón de presupuesto -> +0.44 % de total US gross.
# - Comedy: las comedias tienen ~17 % más total US gross
#           (exp(0.1599) - 1 = 0.173), con lo demás constante.
# - Critics: +1 punto de crítica -> +0.94 % de total US gross.


# 8c) Película con las características de Flags of Our Fathers
flags <- datos[datos$movie == "Flags of Our Fathers", ]
flags[, c("movie", "opening_m", "budget_m", "comedy", "critics", "total_m")]


pred_flags <- predict(m8, newdata = flags, interval = "prediction", level = 0.95)
pred_flags_m <- exp(pred_flags)          # devolvemos a millones de USD
round(pred_flags_m, 2)
cat("Valor real (millones USD):", round(flags$total_m, 2), "\n")


# RESPUESTA 8c:
# Estimación puntual: unos USD 52.4 millones.
# Intervalo de predicción al 95 %: entre USD 30.2 y 91.1 millones.
# La película realmente hizo USD 33.6 millones: le fue peor de lo que el
# modelo esperaba, pero está dentro del intervalo.
# Ojo: Flags tuvo crítica de 79 (buena), así que culpar a "los críticos"
# del fracaso no tiene sentido; el modelo dice que la crítica la ayudó.


# 8d) ¿Cuánto vale subir la crítica de 79 a 89 puntos?
flags79 <- flags; flags79$critics <- 79
flags89 <- flags; flags89$critics <- 89
g79 <- exp(predict(m8, newdata = flags79))
g89 <- exp(predict(m8, newdata = flags89))
extra <- g89 - g79
efecto_pct <- exp(10 * coef(m8)["critics"]) - 1
cat("Total US gross esperado con 79 puntos:", round(g79, 2), "M\n")
cat("Total US gross esperado con 89 puntos:", round(g89, 2), "M\n")
cat("Aumento:", round(extra, 2), "M USD (", round(100 * efecto_pct, 2), "% )\n")


# El caso explica que la taquilla se reparte entre exhibidor y
# distribuidor: el distribuidor arranca con 70 % y ese porcentaje baja
# 10 puntos cada dos semanas. Lo que le queda al productor es menos que la
# taquilla adicional.
cat("Lo que recibiría el distribuidor (máx. 70 %):", round(0.7 * extra, 2), "M USD\n")


# RESPUESTA 8d:
# Subir la crítica 10 puntos aumenta el total U.S. gross en ~9.8 %, es
# decir, de USD 52.4 M a USD 57.5 M: unos USD 5.1 millones adicionales.
# Griffith no debería invertir más de esos ~USD 5.1 M; de hecho, como el
# distribuidor recibe como máximo el 70 % de la taquilla, el techo
# razonable es de unos USD 3.6 M (sin contar ingresos fuera de EE. UU.,
# DVD, etc., que podrían justificar algo más).


# =============================================================
# PUNTO 9 - ¿La crítica afecta menos a las comedias?
# =============================================================
# Al modelo del punto 8 le agregamos la interacción critics x comedy
m9 <- lm(ln_total ~ ln_opening + budget_m + comedy + critics + critics:comedy,
         data = datos)
summary(m9)


# Teoría de Griffith: el efecto de la crítica es MENOR en comedias
# H0: b(critics:comedy) >= 0   vs   H1: b(critics:comedy) < 0 (una cola)
ct <- summary(m9)$coefficients
inter <- grep(":", rownames(ct), value = TRUE)   # nombre de la interacción
t9 <- ct[inter, "t value"]
p9 <- pt(t9, df.residual(m9))
cat("Coef. interacción:", round(ct[inter, "Estimate"], 5),
    " t =", round(t9, 3), " p (una cola) =", round(p9, 4), "\n")
cat("Efecto de critics en NO comedias:", round(ct["critics", "Estimate"], 5), "\n")
cat("Efecto de critics en comedias:",
    round(ct["critics", "Estimate"] + ct[inter, "Estimate"], 5), "\n")


# RESPUESTA 9:
# El coeficiente de la interacción es negativo (-0.0033), lo que va en la
# dirección que dice Griffith: cada punto de crítica sube el total U.S.
# gross ~1.0 % en no comedias y ~0.7 % en comedias.
# Pero NO es significativo (t = -0.71, p de una cola ≈ 0.24 > 0.10).
# Conclusión: con estos datos no se puede probar la teoría de Griffith.


# =============================================================
# PUNTO 10 - Star power (pregunta conceptual, no hay datos)
# =============================================================
# RESPUESTA 10:
# Si se agregara "star_power" (número de estrellas A-list) al modelo del
# punto 9, para que Griffith tuviera razón:
#  1) El coeficiente de star_power debería ser POSITIVO y significativo:
#     más estrellas -> más total U.S. gross, con lo demás constante.
#  2) El coeficiente de budget debería BAJAR mucho (acercarse a 0) y dejar
#     de ser significativo. Hoy budget es significativo (0.0043) en parte
#     porque está "absorbiendo" el efecto de las estrellas: sus sueldos
#     (15 M o más) inflan el presupuesto. Es un sesgo por variable omitida:
#     star_power está correlacionada con budget y con la taquilla.
# Si al meter star_power el coeficiente de budget sigue grande y
# significativo, la conclusión de Griffith sería falsa.


# Copiamos los datos de Hollywood que cargaron mis compañeras
datos_b <- as.data.frame(datos[, 1:18])


# Usamos los nombres de la primera parte de su código
names(datos_b) <- c(
  "movie", "opening_gross", "us_gross", "non_us_gross",
  "budget", "opening_theatres", "known_story", "sequel",
  "origin_us", "genre", "summer", "holiday", "christmas",
  "mpaa", "mpaa_d", "critics", "oscar_nom", "oscar_won"
)
