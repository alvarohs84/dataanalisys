# 03_tabela_peso_parto.R
# Monta uma tabela formatada (gt) a partir do resultado de 02_peso_por_parto.R
# e exibe no painel Viewer do RStudio. Também salva uma cópia em HTML.
#
# Requer: install.packages("gt")

library(here)
library(dplyr)
library(readr)
library(gt)

# --- Preparação de pastas ---------------------------------------------------
dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

# --- Leitura do resultado já calculado --------------------------------------
tab_peso_parto <- read_csv(
  here("output", "peso_medio_por_parto_sp_2022.csv"),
  show_col_types = FALSE
)

# --- Construção da tabela (gt) ----------------------------------------------
tabela_gt <- tab_peso_parto |>
  arrange(desc(n)) |>
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
    table.font.size   = px(14),
    data_row.padding  = px(8),
    heading.align     = "left"
  )

# Abre no painel Viewer do RStudio (funciona ao rodar interativamente).
print(tabela_gt)

# --- Salvar cópia em HTML ----------------------------------------------------
gtsave(tabela_gt, here("output", "tabela_peso_parto_sp_2022.html"))

cat("\nTabela salva em output/tabela_peso_parto_sp_2022.html\n")

# Para exportar como imagem (PNG), instale webshot2 e rode:
# install.packages("webshot2")
# gtsave(tabela_gt, here("output", "tabela_peso_parto_sp_2022.png"))
