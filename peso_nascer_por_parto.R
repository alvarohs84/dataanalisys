# Peso médio ao nascer por tipo de parto (dados do SINASC via microdatasus)
#
# Fonte: Sistema de Informações sobre Nascidos Vivos (SINASC), DATASUS.
# Pacote: microdatasus (https://github.com/rfsaldanha/microdatasus)

# install.packages("remotes")
# remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(dplyr)

## 1. Parâmetros de download -------------------------------------------------

uf <- "SP"          # UF de interesse (sigla de 2 letras)
ano_inicio <- 2022
ano_fim <- 2022

## 2. Download dos dados brutos do SINASC ------------------------------------

dados_brutos <- fetch_datasus(
  year_start = ano_inicio,
  year_end   = ano_fim,
  uf         = uf,
  information_system = "SINASC"
)

## 3. Padronização das variáveis ---------------------------------------------
# process_sinasc() converte códigos em rótulos legíveis (ex.: PARTO em
# "Vaginal"/"Cesário") e mantém PESO em gramas.

dados <- process_sinasc(dados_brutos, municipality_data = FALSE)

## 4. Limpeza -----------------------------------------------------------------
# Remove registros sem peso informado ou com peso implausível (ex.: 0 ou
# valores fora da faixa esperada para nascidos vivos, entre 200g e 7000g).

dados_limpos <- dados %>%
  filter(
    !is.na(PESO),
    !is.na(PARTO),
    PESO >= 200,
    PESO <= 7000
  )

## 5. Peso médio ao nascer por tipo de parto ----------------------------------

resumo_peso_parto <- dados_limpos %>%
  group_by(PARTO) %>%
  summarise(
    n_nascidos = n(),
    peso_medio_g = mean(PESO, na.rm = TRUE),
    peso_mediana_g = median(PESO, na.rm = TRUE),
    desvio_padrao_g = sd(PESO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_nascidos))

print(resumo_peso_parto)

## 6. (Opcional) salvar resultado ---------------------------------------------

# write.csv(resumo_peso_parto, "peso_medio_por_tipo_parto.csv", row.names = FALSE)
