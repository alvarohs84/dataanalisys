# saudeemdado_client.R
# Cliente R para a API pública do "Saúde em Dado" (DataSUS + IBGE), a mesma
# API REST que o servidor MCP (github.com/pedropaulofernandes88-stack/saude-publica-br)
# consulta por baixo dos panos.
#
# O MCP em si é feito para assistentes de IA (Claude Desktop/Code) via stdio,
# não para scripts — este arquivo fala diretamente com a API PostgREST/Supabase
# pública que o cliente Python oficial (clients/python/saudeemdado) usa.
# A chave abaixo é a mesma ANON_KEY documentada nesse repositório: pública,
# somente leitura, sem necessidade de cadastro.
#
# Requer: install.packages(c("httr2", "dplyr", "purrr"))

library(httr2)
library(dplyr)
library(purrr)

.SED_BASE_URL <- "https://zekjhmxjamatlxpkykde.supabase.co/rest/v1"
.SED_ANON_KEY <- paste0(
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.",
  "eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inpla2pobXhqYW1hdGx4cGt5a2RlIiwicm9sZSI6ImFub24iLCJpYXQiOjE3ODEwNzY4MzIsImV4cCI6MjA5NjY1MjgzMn0.",
  "px8FcU0QK8w9v95kwGlGzASKpY3drsxAvFe0e6wUoCU"
)
.SED_PAGE <- 1000

# --- Função genérica: GET paginado no PostgREST -----------------------------
sed_get <- function(table, params = list(), max_rows = 200000) {
  rows <- list()
  offset <- 0

  repeat {
    req <- request(paste0(.SED_BASE_URL, "/", table)) |>
      req_headers(
        apikey        = .SED_ANON_KEY,
        Authorization = paste("Bearer", .SED_ANON_KEY),
        `Range-Unit`  = "items",
        Range         = sprintf("%d-%d", offset, offset + .SED_PAGE - 1)
      ) |>
      req_url_query(!!!params) |>
      req_timeout(120)

    resp <- req_perform(req)
    chunk <- resp_body_json(resp, simplifyVector = TRUE)

    if (length(chunk) == 0) break
    rows[[length(rows) + 1]] <- chunk

    n_chunk <- if (is.data.frame(chunk)) nrow(chunk) else length(chunk)
    offset <- offset + .SED_PAGE
    if (n_chunk < .SED_PAGE || offset >= max_rows) break
  }

  if (length(rows) == 0) return(tibble())
  bind_rows(rows)
}

# --- Wrappers equivalentes ao cliente Python ---------------------------------

#' Série mensal de óbitos (2015-2024) por UF (uf = NULL para todas)
sed_serie_mensal <- function(uf = NULL, capitulo = "TOTAL", sexo = "TOTAL",
                              faixa_etaria = "TOTAL") {
  params <- list(
    select       = "uf_sigla,ano,mes,mes_competencia,obitos",
    capitulo_cid = paste0("eq.", capitulo),
    sexo         = paste0("eq.", sexo),
    faixa_etaria = paste0("eq.", faixa_etaria),
    order        = "mes_competencia,uf_sigla"
  )
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get("mart_mortalidade_uf_mes", params)
}

#' Óbitos, taxa bruta (IC95%) e taxa padronizada por município
sed_municipios <- function(uf = NULL, ano = 2023, capitulo = "TOTAL",
                            sexo = "TOTAL", pop_min = 0) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,obitos,",
      "obitos_hospital,obitos_domicilio,populacao,taxa_obitos_100k,",
      "ic95_inf,ic95_sup,taxa_padronizada_100k",
      sep = ""
    ),
    ano          = paste0("eq.", ano),
    capitulo_cid = paste0("eq.", capitulo),
    sexo         = paste0("eq.", sexo),
    order        = "municipio_cod"
  )
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  if (pop_min > 0) params$populacao <- paste0("gte.", pop_min)
  sed_get("mart_mortalidade_municipio", params)
}

#' Óbitos por causa básica (CID-10, 3 caracteres), agregados no servidor
sed_causas <- function(uf = NULL, ano = 2023, top = NULL) {
  params <- list(
    select = "causabas_3,obitos:obitos.sum()",
    ano    = paste0("eq.", ano),
    order  = "causabas_3"
  )
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  df <- sed_get("mart_mortalidade_causa", params) |> arrange(desc(obitos))
  if (!is.null(top)) df <- head(df, top)
  df
}

#' Excesso de mortalidade (2020+): observado x esperado (baseline 2015-2019)
sed_excesso <- function(uf = "BR") {
  params <- list(
    select   = "uf_sigla,ano,mes,mes_competencia,obitos,esperado,excesso,pct_excesso",
    uf_sigla = paste0("eq.", toupper(uf)),
    order    = "mes_competencia"
  )
  sed_get("mart_excesso_uf_mes", params)
}

#' Dengue (SINAN). nivel = "ano" (resumo municipal) ou "uf" (série semanal)
sed_dengue <- function(uf = NULL, ano = 2024, nivel = "ano") {
  if (nivel == "uf") {
    params <- list(
      select  = paste(
        "uf_sigla,ano_epi,semana_epi,casos_provaveis,casos_graves,",
        "obitos,municipios_com_casos",
        sep = ""
      ),
      ano_epi = paste0("eq.", ano),
      order   = "uf_sigla,semana_epi"
    )
    table <- "mart_dengue_uf_semana"
  } else {
    params <- list(
      select = paste(
        "municipio_cod,municipio_nome,uf_sigla,regiao,ano_epi,",
        "casos_provaveis,casos_graves,obitos,populacao,incidencia_100k,letalidade_pct",
        sep = ""
      ),
      ano_epi = paste0("eq.", ano),
      order   = "municipio_cod"
    )
    table <- "mart_dengue_municipio_ano"
  }
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get(table, params)
}

#' Internações SUS (SIH/AIH) por município
sed_internacoes <- function(uf = NULL, ano = 2024, capitulo = "TOTAL") {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,capitulo_cid,",
      "internacoes,obitos,dias_permanencia,valor_total,",
      "aih_continuacao,aih_normal,dias_permanencia_normal,valor_normal,",
      "permanencia_media,mortalidade_pct,custo_medio,internacoes_100k,populacao",
      sep = ""
    ),
    ano          = paste0("eq.", ano),
    capitulo_cid = paste0("eq.", capitulo),
    order        = "municipio_cod"
  )
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get("mart_internacoes_municipio", params)
}

#' Descrições das categorias CID-10 (3 caracteres)
sed_cid10 <- function() {
  sed_get("dim_cid10_categoria", list(select = "causabas_3,descricao", order = "causabas_3"))
}

#' Fontes, métodos, exclusões, licença e versão do dataset (citação/procedência)
sed_metadados <- function() {
  df <- sed_get("meta_dataset", list(select = "chave,valor"))
  setNames(as.list(df$valor), df$chave)
}

# --- Exemplo de uso -----------------------------------------------------------
if (sys.nframe() == 0) {
  mg_2023 <- sed_municipios(uf = "MG", ano = 2023, pop_min = 50000) |>
    arrange(desc(taxa_padronizada_100k))
  print(head(mg_2023, 10))

  meta <- sed_metadados()
  como_citar <- if (is.null(meta$como_citar)) "ver metodologia em saudeemdado.com" else meta$como_citar
  cat("\nComo citar:", como_citar, "\n")
}
