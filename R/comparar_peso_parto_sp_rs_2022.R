# comparar_peso_parto_sp_rs_2022.R
# Compara o peso médio ao nascer por tipo de parto entre SP e RS, 2022 (SINASC).
# Reaproveita a mesma lógica e salvaguardas de pipeline_peso_por_parto.R,
# generalizada para múltiplas UFs.
#
# Requer: install.packages(c("here", "microdatasus", "dplyr", "tidyr", "arrow", "janitor", "gt"))
# microdatasus não está no CRAN:
#   remotes::install_github("rfsaldanha/microdatasus")

library(here)
library(microdatasus)
library(dplyr)
library(arrow)
library(janitor)
library(gt)

set.seed(42)

UFS <- c("SP", "RS")
ANO <- 2022

# --- Preparação de pastas ----------------------------------------------------
dir.create(here("data", "raw"),     recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "interim"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"),          recursive = TRUE, showWarnings = FALSE)

## 1-2. Download + processamento, UF por UF -----------------------------------
# Uma UF por vez (em vez de vetor único em fetch_datasus) para poder marcar cada
# registro com a UF pedida sem depender de decodificar código de município.
sinasc_por_uf <- list()

for (uf in UFS) {
  cat(sprintf("\n=== Baixando SINASC %s/%d ===\n", uf, ANO))

  raw <- fetch_datasus(
    year_start = ANO,
    year_end   = ANO,
    uf         = uf,
    information_system = "SINASC"
  )
  saveRDS(raw, here("data", "raw", sprintf("sinasc_%s_%d_raw.rds", tolower(uf), ANO)))
  cat("Linhas baixadas:", nrow(raw), "\n")

  proc <- process_sinasc(raw, municipality_data = FALSE) |>
    janitor::clean_names()

  # Mesma checagem de segurança do pipeline original: reatribuição de peso é
  # posicional, então só é segura se a contagem de linhas não mudou.
  stopifnot(
    "process_sinasc() alterou o número de linhas - reatribuição de peso não é segura!" =
      nrow(proc) == nrow(raw)
  )
  proc$peso <- as.numeric(raw$PESO)
  proc$uf   <- uf  # UF pedida no fetch, não depende de decodificar código de município

  sinasc_por_uf[[uf]] <- proc
}

sinasc <- bind_rows(sinasc_por_uf)
write_parquet(sinasc, here("data", "interim", sprintf("sinasc_sp_rs_%d.parquet", ANO)))
cat(sprintf("\nBase combinada (%d linhas) salva em data/interim/sinasc_sp_rs_%d.parquet\n",
            nrow(sinasc), ANO))

## 3. Qualidade dos dados, por UF ---------------------------------------------
stopifnot("peso" %in% names(sinasc))
stopifnot("parto" %in% names(sinasc))

qualidade <- sinasc |>
  group_by(uf) |>
  summarise(
    n_total          = n(),
    n_peso_na        = sum(is.na(peso)),
    n_fora_faixa     = sum(peso < 200 | peso > 8000, na.rm = TRUE),
    n_parto_ignorado = sum(is.na(parto) | parto == "Ignorado"),
    .groups = "drop"
  ) |>
  mutate(
    pct_peso_na    = round(100 * n_peso_na / n_total, 2),
    pct_fora_faixa = round(100 * n_fora_faixa / n_total, 2),
    pct_parto_ign  = round(100 * n_parto_ignorado / n_total, 2)
  )

cat("\n--- Qualidade dos dados por UF ---\n")
print(qualidade)

## 4. Peso médio por UF e tipo de parto (caso completo) -----------------------
tab_peso_parto_uf <- sinasc |>
  filter(!is.na(peso), peso >= 200, peso <= 8000,
         !is.na(parto), parto != "Ignorado") |>
  group_by(uf, parto) |>
  summarise(
    n            = n(),
    peso_medio   = mean(peso, na.rm = TRUE),
    peso_dp      = sd(peso, na.rm = TRUE),
    peso_mediana = median(peso, na.rm = TRUE),
    peso_iqr_25  = quantile(peso, 0.25, na.rm = TRUE),
    peso_iqr_75  = quantile(peso, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(uf, desc(n))

cat("\n--- Peso ao nascer por UF e tipo de parto (2022) ---\n")
print(tab_peso_parto_uf)

if (any(tab_peso_parto_uf$n < 5)) {
  warning("Alguma categoria tem n < 5 - aplicar supressão antes de publicar.")
}

write.csv(tab_peso_parto_uf, here("output", sprintf("peso_medio_por_parto_sp_rs_%d.csv", ANO)),
          row.names = FALSE)

## 5. Diferença SP vs. RS, por tipo de parto -----------------------------------
diferenca_sp_rs <- tab_peso_parto_uf |>
  select(uf, parto, peso_medio) |>
  tidyr::pivot_wider(names_from = uf, values_from = peso_medio) |>
  mutate(diferenca_sp_menos_rs_g = round(SP - RS, 1))

cat("\n--- Diferença de peso médio, SP - RS (g) ---\n")
print(diferenca_sp_rs)

## 6. Tabela formatada no Viewer (gt), agrupada por UF -------------------------
tabela_gt <- tab_peso_parto_uf |>
  gt(groupname_col = "uf", rowname_col = "parto") |>
  tab_header(
    title = "Peso ao nascer por tipo de parto: SP vs. RS",
    subtitle = "SINASC · Nascidos vivos · 2022"
  ) |>
  cols_label(
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
    locations = cells_row_groups()
  ) |>
  tab_source_note(
    "Fonte: SINASC/DATASUS (SP e RS, 2022), via pacote microdatasus. Exclui peso fora da faixa plausível (200-8000g) e tipo de parto ignorado (análise de caso completo)."
  ) |>
  tab_options(
    table.font.size  = px(14),
    data_row.padding = px(8),
    heading.align    = "left"
  )

print(tabela_gt)  # abre no painel Viewer do RStudio

gtsave(tabela_gt, here("output", sprintf("tabela_peso_parto_sp_rs_%d.html", ANO)))

cat(sprintf("\nConcluído. Tabela salva em output/tabela_peso_parto_sp_rs_%d.html\n", ANO))
