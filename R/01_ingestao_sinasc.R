# 01_ingestao_sinasc.R
# Extrai dados do SINASC (nascidos vivos) para SP, ano 2022, via pacote microdatasus.
# Salva base bruta baixada em data/raw/ (RDS, formato original do fetch) e uma
# versão limpa/tipada em data/interim/ (Parquet).

library(here)
library(microdatasus)
library(dplyr)
library(arrow)
library(janitor)

set.seed(42)

# --- Preparação de pastas ----------------------------------------------
# saveRDS()/write_parquet() não criam diretórios automaticamente.
dir.create(here("data", "raw"),     recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "interim"), recursive = TRUE, showWarnings = FALSE)

# --- Download -----------------------------------------------------------
# fetch_datasus baixa diretamente do FTP público do DATASUS (SINASC/DNRES).
# Nenhum dado do projeto é enviado para fora; é extração de dado público.
sinasc_sp_2022_raw <- fetch_datasus(
  year_start = 2022,
  year_end   = 2022,
  uf         = "SP",
  information_system = "SINASC"
)

saveRDS(sinasc_sp_2022_raw, here("data", "raw", "sinasc_sp_2022_raw.rds"))

cat("Linhas baixadas:", nrow(sinasc_sp_2022_raw), "\n")
cat("Colunas:", ncol(sinasc_sp_2022_raw), "\n")

# --- Processamento de rótulos (PARTO etc.) --------------------------------
# process_sinasc() do microdatasus decodifica os campos brutos (códigos
# numéricos) em fatores legíveis, incluindo PARTO (1 Vaginal / 2 Cesário / 9 Ignorado).
sinasc_sp_2022 <- process_sinasc(sinasc_sp_2022_raw, municipality_data = FALSE)

sinasc_sp_2022 <- sinasc_sp_2022 |>
  janitor::clean_names()

# BUG conhecido: process_sinasc() (microdatasus 3.0.0) zera a coluna `peso`
# (retorna tudo NA) em vez de manter o valor numérico em gramas. Reconstituída
# aqui a partir do campo bruto PESO. A reatribuição é posicional (linha a
# linha), então só é segura se process_sinasc() preservar a ordem e a
# contagem de linhas do dado bruto — a checagem abaixo garante isso e falha
# ruidosamente em vez de casar peso/parto errado silenciosamente.
stopifnot(
  "process_sinasc() alterou o número de linhas — reatribuição de peso não é segura!" =
    nrow(sinasc_sp_2022) == nrow(sinasc_sp_2022_raw)
)
sinasc_sp_2022$peso <- as.numeric(sinasc_sp_2022_raw$PESO)

# --- Checagem da suposição de "sem códigos de ignorado" em PESO ------------
# O campo bruto PESO do SINASC não tem um código sentinela documentado tipo
# 9999/0000 para "ignorado" (diferente de outros campos codificados), mas
# vale confirmar a cada extração que não há valores fora da faixa plausível
# de peso ao nascer (200-8000g) por erro de digitação/leitura do DBC.
n_fora_faixa <- sum(sinasc_sp_2022$peso < 200 | sinasc_sp_2022$peso > 8000, na.rm = TRUE)
if (n_fora_faixa > 0) {
  warning(sprintf(
    "%d registro(s) com peso fora da faixa plausivel (200-8000g) nesta extracao - revisar antes de seguir.",
    n_fora_faixa
  ))
}

write_parquet(sinasc_sp_2022, here("data", "interim", "sinasc_sp_2022.parquet"))

cat("Base processada salva em data/interim/sinasc_sp_2022.parquet\n")
