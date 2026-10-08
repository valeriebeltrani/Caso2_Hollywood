# =============================================================
# Caso Hollywood Rules (KEL700) - Analítica de Negocios
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

# Función de eliminación hacia atrás: 
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
