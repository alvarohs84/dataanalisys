# 02_peso_por_parto.R
# Calcula peso médio ao nascer por tipo de parto (SINASC, SP, 2022).
# Antes de reportar a média, valida faixa plausível de peso e checa faltantes,
# conforme protocolo de qualidade de dados do projeto.

library(here)
library(dplyr)
library(arrow)

# --- Preparação de pastas ----------------------------------------------
# write.csv()/sink() não criam diretórios automaticamente.
dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

sinasc <- read_parquet(here("data", "interim", "sinasc_sp_2022.parquet"))

# --- Checagem de variáveis relevantes ------------------------------------
stopifnot("peso" %in% names(sinasc))
stopifnot("parto" %in% names(sinasc))

cat("Categorias de PARTO encontradas:\n")
print(table(sinasc$parto, useNA = "ifany"))

cat("\nResumo bruto de PESO (gramas):\n")
print(summary(sinasc$peso))

# --- Validação de plausibilidade ------------------------------------------
# Faixa fisiologicamente plausível de peso ao nascer: 200g a 8000g.
# Valores fora dessa faixa são prováveis erros de registro, não exclusão
# automática de "outliers" reais — sinalizar antes de decidir remover.
n_total <- nrow(sinasc)
n_peso_na <- sum(is.na(sinasc$peso))
n_fora_faixa <- sum(sinasc$peso < 200 | sinasc$peso > 8000, na.rm = TRUE)

cat("\n--- Qualidade dos dados: PESO ---\n")
cat("Total de registros:", n_total, "\n")
cat("Faltantes em peso:", n_peso_na,
    sprintf("(%.2f%%)\n", 100 * n_peso_na / n_total))
cat("Fora da faixa plausível (200-8000g):", n_fora_faixa,
    sprintf("(%.2f%%)\n", 100 * n_fora_faixa / n_total))

n_parto_na <- sum(is.na(sinasc$parto) | sinasc$parto == "Ignorado")
cat("Faltantes/ignorados em tipo de parto:", n_parto_na,
    sprintf("(%.2f%%)\n", 100 * n_parto_na / n_total))

# --- Peso médio por tipo de parto (caso completo) --------------------------
# Estratégia: análise de caso completo para esta descritiva simples, excluindo
# peso fora da faixa plausível (tratado como erro de registro, não outlier real)
# e categoria "Ignorado" de parto. Decisão registrada em docs/decisoes.md.
tab_peso_parto <- sinasc |>
  filter(!is.na(peso), peso >= 200, peso <= 8000,
         !is.na(parto), parto != "Ignorado") |>
  group_by(parto) |>
  summarise(
    n            = n(),
    peso_medio   = mean(peso, na.rm = TRUE),
    peso_dp      = sd(peso, na.rm = TRUE),
    peso_mediana = median(peso, na.rm = TRUE),
    peso_iqr_25  = quantile(peso, 0.25, na.rm = TRUE),
    peso_iqr_75  = quantile(peso, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Peso ao nascer por tipo de parto (SP, 2022) ---\n")
print(tab_peso_parto)

# Supressão de células pequenas (padrão do projeto: n < 5) — não se aplica aqui
# pois os grupos são grandes, mas o teste é feito por robustez.
if (any(tab_peso_parto$n < 5)) {
  warning("Alguma categoria tem n < 5 — aplicar supressão antes de publicar.")
}

write.csv(tab_peso_parto, here("output", "peso_medio_por_parto_sp_2022.csv"),
          row.names = FALSE)

cat("\nTabela salva em output/peso_medio_por_parto_sp_2022.csv\n")

# Registrar versões dos pacotes usados
sink(here("output", "sessionInfo_02_peso_por_parto.txt"))
print(sessionInfo())
sink()
