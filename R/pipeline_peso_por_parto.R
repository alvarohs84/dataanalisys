# pipeline_peso_por_parto.R
# Script único: baixa o SINASC (SP, 2022), calcula o peso médio ao nascer por
# tipo de parto e exibe uma tabela formatada no painel Viewer do RStudio.
#
# Requer: install.packages(c("here", "microdatasus", "dplyr", "arrow", "janitor", "gt"))
# microdatasus não está no CRAN:
#   remotes::install_github("rfsaldanha/microdatasus")

library(here)
library(microdatasus)
library(dplyr)
library(arrow)
library(janitor)
library(gt)

set.seed(42)

# --- Preparação de pastas ----------------------------------------------------
# As funções de salvar (saveRDS, write_parquet, write.csv, gtsave) não criam
# diretórios automaticamente.
dir.create(here("data", "raw"),     recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "interim"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"),          recursive = TRUE, showWarnings = FALSE)

## 1. Download -----------------------------------------------------------
# fetch_datasus baixa diretamente do FTP público do DATASUS (SINASC/DNRES).
# Nenhum dado do projeto é enviado para fora; é extração de dado público.
sinasc_raw <- fetch_datasus(
  year_start = 2022,
  year_end   = 2022,
  uf         = "SP",
  information_system = "SINASC"
)

saveRDS(sinasc_raw, here("data", "raw", "sinasc_sp_2022_raw.rds"))
cat("Linhas baixadas:", nrow(sinasc_raw), "\n")

## 2. Processamento de rótulos (PARTO etc.) -------------------------------
# process_sinasc() decodifica os campos brutos (códigos numéricos) em fatores
# legíveis, incluindo PARTO (1 Vaginal / 2 Cesário / 9 Ignorado).
sinasc <- process_sinasc(sinasc_raw, municipality_data = FALSE) |>
  janitor::clean_names()

# BUG conhecido: process_sinasc() (microdatasus 3.0.0) zera a coluna `peso`
# (retorna tudo NA) em vez de manter o valor numérico em gramas. Reconstituída
# aqui a partir do campo bruto PESO. A reatribuição é posicional (linha a
# linha), então só é segura se process_sinasc() preservar a ordem/contagem de
# linhas do dado bruto — a checagem abaixo garante isso em vez de casar
# peso/parto errado silenciosamente.
stopifnot(
  "process_sinasc() alterou o número de linhas — reatribuição de peso não é segura!" =
    nrow(sinasc) == nrow(sinasc_raw)
)
sinasc$peso <- as.numeric(sinasc_raw$PESO)

n_fora_faixa_bruto <- sum(sinasc$peso < 200 | sinasc$peso > 8000, na.rm = TRUE)
if (n_fora_faixa_bruto > 0) {
  warning(sprintf(
    "%d registro(s) com peso fora da faixa plausivel (200-8000g) nesta extracao - revisar antes de seguir.",
    n_fora_faixa_bruto
  ))
}

write_parquet(sinasc, here("data", "interim", "sinasc_sp_2022.parquet"))
cat("Base processada salva em data/interim/sinasc_sp_2022.parquet\n")

## 3. Qualidade dos dados ---------------------------------------------------
stopifnot("peso" %in% names(sinasc))
stopifnot("parto" %in% names(sinasc))

n_total <- nrow(sinasc)
n_peso_na <- sum(is.na(sinasc$peso))
n_fora_faixa <- sum(sinasc$peso < 200 | sinasc$peso > 8000, na.rm = TRUE)
n_parto_na <- sum(is.na(sinasc$parto) | sinasc$parto == "Ignorado")

cat("\n--- Qualidade dos dados: PESO ---\n")
cat("Total de registros:", n_total, "\n")
cat("Faltantes em peso:", n_peso_na, sprintf("(%.2f%%)\n", 100 * n_peso_na / n_total))
cat("Fora da faixa plausível (200-8000g):", n_fora_faixa,
    sprintf("(%.2f%%)\n", 100 * n_fora_faixa / n_total))
cat("Faltantes/ignorados em tipo de parto:", n_parto_na,
    sprintf("(%.2f%%)\n", 100 * n_parto_na / n_total))

## 4. Peso médio por tipo de parto (caso completo) --------------------------
# Estratégia: análise de caso completo, excluindo peso fora da faixa plausível
# (tratado como erro de registro, não outlier real) e categoria "Ignorado" de
# parto. Decisão registrada em docs/decisoes.md.
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
  ) |>
  arrange(desc(n))

cat("\n--- Peso ao nascer por tipo de parto (SP, 2022) ---\n")
print(tab_peso_parto)

# Supressão de células pequenas (padrão do projeto: n < 5).
if (any(tab_peso_parto$n < 5)) {
  warning("Alguma categoria tem n < 5 — aplicar supressão antes de publicar.")
}

write.csv(tab_peso_parto, here("output", "peso_medio_por_parto_sp_2022.csv"),
          row.names = FALSE)

## 5. Tabela formatada no Viewer (gt) ----------------------------------------
tabela_gt <- tab_peso_parto |>
  gt() |>
  tab_header(
    title = "Peso ao nascer por tipo de parto",
    subtitle = "SINASC · Nascidos vivos · São Paulo (SP) · 2022"
  ) |>
  cols_label(
    parto        = "Tipo de parto",
    n            = "n",
    peso_medio   = "Média (g)",
    peso_dp      = "DP",
    peso_mediana = "Mediana (g)",
    peso_iqr_25  = "P25",
    peso_iqr_75  = "P75"
  ) |>
  fmt_integer(columns = n, sep_mark = ".") |>
  fmt_number(
    columns  = c(peso_medio, peso_dp, peso_mediana, peso_iqr_25, peso_iqr_75),
    decimals = 0,
    sep_mark = "."
  ) |>
  cols_align(align = "right", columns = -parto) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = parto)
  ) |>
  tab_source_note(
    "Fonte: SINASC/DATASUS (UF SP, 2022), via pacote microdatasus. Exclui peso fora da faixa plausível (200-8000g) e tipo de parto ignorado (análise de caso completo)."
  ) |>
  tab_options(
    table.font.size  = px(14),
    data_row.padding = px(8),
    heading.align    = "left"
  )

print(tabela_gt)  # abre no painel Viewer do RStudio

gtsave(tabela_gt, here("output", "tabela_peso_parto_sp_2022.html"))

# Registrar versões dos pacotes usados
sink(here("output", "sessionInfo_pipeline_peso_por_parto.txt"))
print(sessionInfo())
sink()

cat("\nConcluído. Tabela salva em output/tabela_peso_parto_sp_2022.html\n")
