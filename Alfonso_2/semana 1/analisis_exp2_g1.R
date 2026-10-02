# ==============================================================================
# Experimento 2 (Alfonso-2), grupo 1 — análisis básico
# v1 — 2026-10-01
#
# Entrada: exp2_g1.csv (una fila por participante).
# Reconocimiento codificado como <origen>_<respuesta>, origen/respuesta ∈ {l1, l2, n}:
#   l2_l1 = ítem de L2 respondido como L1  → intrusión L2→L1 (la "esperada")
#   l1_l2 = ítem de L1 respondido como L2  → intrusión L1→L2
#   n_l1  = ítem nuevo respondido como L1  (SUPUESTO sin confirmar)
# Cada origen tiene 20 ítems, así que cada terna debería sumar 20.
#
# En RStudio: Session > Set Working Directory > To Source File Location
# Paquetes: dplyr, tidyr, ggplot2, glmmTMB, emmeans, pwr
# ==============================================================================
install.packages(c("Matrix", "TMB"), type = "source")

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2)
  library(glmmTMB); library(emmeans); library(pwr) })

excluir_inconsistentes <- FALSE   # TRUE = sacar filas cuya matriz no suma 20 por origen

# ---- 1. Carga y chequeos ----------------------------------------------------
d <- read.csv("exp2_g1.csv") |>
  filter(!is.na(l1_l1)) |>                                   # agendados sin datos
  mutate(sala = factor(sala, levels = c("ipsii", "profes", "box")),
         sexo = factor(sexo, levels = c("F", "M")),
         tot_l1 = l1_l1 + l1_l2 + l1_n,
         tot_l2 = l2_l2 + l2_l1 + l2_n,
         tot_n  = n_n + n_l1 + n_l2,
         suma_ok = tot_l1 == 20 & tot_l2 == 20 & tot_n == 20)

cat("\n### Filas con sumas incorrectas\n")
print(filter(d, !suma_ok) |> select(codigo, tot_l1, tot_l2, tot_n))
if (excluir_inconsistentes) d <- filter(d, suma_ok)

cat("\n### n por sala y sexo\n");          print(table(d$sala, d$sexo))
cat("\n### n por entrevistador y sexo\n"); print(table(d$entrevistador, d$sexo))

# ---- 2. Aprendizaje: ¿las dos listas se aprendieron igual? -------------------
# Si L1 y L2 difieren en dificultad, las intrusiones en cada dirección no son comparables.
cat("\n### Recuerdo inmediato (de 20): D1 = L1, D2 = L2\n")
print(d |> group_by(sexo) |>
        summarise(n = n(), D1 = mean(rec_d1), DE_D1 = sd(rec_d1),
                  D2 = mean(rec_d2), DE_D2 = sd(rec_d2)) |> mutate(across(where(is.numeric), \(x) round(x, 2))))
tt <- t.test(d$rec_d2, d$rec_d1, paired = TRUE)
cat(sprintf("D2 − D1 = %.2f [%.2f; %.2f], d_z = %.2f, p = %.3f\n",
            tt$estimate, tt$conf.int[1], tt$conf.int[2],
            mean(d$rec_d2 - d$rec_d1) / sd(d$rec_d2 - d$rec_d1), tt$p.value))

# ---- 3. Reconocimiento: aciertos y falsas alarmas ----------------------------
# Chequea techo/piso y si hay sesgo a responder una lista con los ítems nuevos.
cat("\n### Reconocimiento (medias, de 20)\n")
print(d |> group_by(sexo) |>
        summarise(acierto_L1 = mean(l1_l1), acierto_L2 = mean(l2_l2), nuevo_ok = mean(n_n),
                  omision_L1 = mean(l1_n), omision_L2 = mean(l2_n),
                  FA_como_L1 = mean(n_l1), FA_como_L2 = mean(n_l2)) |>
        mutate(across(where(is.numeric), \(x) round(x, 2))))

# ---- 4. Intrusiones: descriptivos --------------------------------------------
largo <- d |>
  transmute(codigo, sala, sexo, entrevistador,
            L1_en_L2 = l1_l2, L2_en_L1 = l2_l1, den_L1 = tot_l1, den_L2 = tot_l2) |>
  pivot_longer(c(L1_en_L2, L2_en_L1), names_to = "tipo", values_to = "k") |>
  mutate(den  = if_else(tipo == "L2_en_L1", den_L2, den_L1),
         tipo = factor(tipo, levels = c("L1_en_L2", "L2_en_L1")))

cat("\n### Intrusiones por sexo (media, DE)\n")
print(largo |> group_by(sexo, tipo) |> summarise(n = n(), media = mean(k), DE = sd(k), .groups = "drop") |>
        mutate(across(where(is.numeric), \(x) round(x, 2))))
cat("Ref. Alfonso (2016) Exp. 3, grupo A: L2→L1 varones 6,25 (3,11), mujeres 2,00 (2,30); L1→L2 ≈ 1,25\n")
cat("\n### Intrusiones por sala y sexo (media)\n")
print(largo |> group_by(sala, sexo, tipo) |> summarise(media = round(mean(k), 2), .groups = "drop") |>
        pivot_wider(names_from = tipo, values_from = media))

# ---- 5. Modelo: GLMM binomial ------------------------------------------------
# k intrusiones de 'den' ítems ~ tipo × sexo + sala, intercepto aleatorio por participante.
m <- glmmTMB(cbind(k, den - k) ~ tipo * sexo + sala + (1 | codigo),
             family = binomial, data = largo)

cat("\n### Tests globales (Wald)\n"); print(joint_tests(m))
cat("\n### Asimetría dentro de cada sexo (OR > 1 = más L2→L1)\n")
print(confint(pairs(emmeans(m, ~ tipo | sexo, type = "response"), reverse = TRUE)))
cat("\n### Diferencia por sexo dentro de cada tipo (OR > 1 = más en varones)\n")
print(confint(pairs(emmeans(m, ~ sexo | tipo, type = "response"), reverse = TRUE)))

# Control: ¿cambia algo si se agrega el entrevistador? (3 personas → efecto fijo)
m_ent <- update(m, . ~ . + entrevistador)
cat(sprintf("\nAIC sin entrevistador = %.1f | con entrevistador = %.1f\n", AIC(m), AIC(m_ent)))

# ---- 6. Tamaños de efecto simples y potencia ---------------------------------
cat("\n### Asimetría pareada (L2→L1 − L1→L2) por sexo\n")
for (s in levels(d$sexo)) {
  x  <- with(filter(d, sexo == s), l2_l1 - l1_l2)
  tt <- t.test(x)
  cat(sprintf("%s (n = %d): dif = %.2f [%.2f; %.2f], d_z = %.2f\n",
              s, length(x), mean(x), tt$conf.int[1], tt$conf.int[2], mean(x) / sd(x)))
}
n_F <- sum(d$sexo == "F"); n_M <- sum(d$sexo == "M")
cat("\n### Sensibilidad (potencia 80 %, α = .05): menor efecto detectable con este n\n")
cat(sprintf("Asimetría dentro de sexo: d_z ≥ %.2f (varones), %.2f (mujeres)\n",
            pwr.t.test(n = n_M, power = .8, type = "one.sample")$d,
            pwr.t.test(n = n_F, power = .8, type = "one.sample")$d))
cat(sprintf("Varones vs mujeres: d ≥ %.2f   (Alfonso Exp. 3, L2→L1: d ≈ 1,55)\n",
            pwr.t2n.test(n1 = n_M, n2 = n_F, power = .8)$d))
cat(sprintf("Entre salas: f ≥ %.2f   (f = .25 es un efecto 'mediano')\n",
            pwr.anova.test(k = 3, n = floor(nrow(d) / 3), power = .8)$f))

# ---- 7. Gráfico --------------------------------------------------------------
g <- ggplot(largo, aes(tipo, k)) +
  geom_line(aes(group = codigo), alpha = .25) +
  geom_point(alpha = .4, position = position_jitter(width = .04, height = .1)) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2, colour = "firebrick") +
  stat_summary(fun.data = mean_se, colour = "firebrick") +
  facet_grid(sexo ~ sala) +
  labs(x = NULL, y = "Intrusiones (de 20)",
       title = "Exp. 2, grupo 1: intrusiones por tipo, sexo y sala",
       subtitle = "Gris: cada participante. Rojo: media ± 1 EE") +
  theme_minimal(base_size = 11)
ggsave("exp2_g1_intrusiones.png", g, width = 8, height = 5, dpi = 150)
