# saudeemdado_client.R
# Cliente R completo para a API pública do "Saúde em Dado" (DataSUS + IBGE) —
# réplica em R de TODAS as ~41 ferramentas do servidor MCP
# (github.com/pedropaulofernandes88-stack/saude-publica-br/tree/main/mcp_server).
#
# O MCP é feito para assistentes de IA (Claude Desktop/Code) via stdio, não
# para scripts — este arquivo fala direto com a mesma API REST PostgREST/
# Supabase pública, somente leitura, que o cliente Python oficial usa por
# baixo. A ANON_KEY abaixo é a mesma documentada no repositório: pública e
# sem cadastro.
#
# Fontes cobertas: SIM (mortalidade), SIH (internações/AIH), SINAN (dengue),
# SINASC (natalidade), PNI/RNDS (vacinação), CNES (rede), SIOPS (gasto),
# ANS (saúde suplementar), e-Gestor AB (cobertura APS), SISAGUA (água),
# IBGE (contexto social) — dados de 2015-2024/2026, Brasil.
#
# Requer: install.packages(c("httr2", "dplyr", "purrr", "jsonlite"))

library(httr2)
library(dplyr)
library(purrr)
library(jsonlite)

.SED_BASE_URL <- "https://zekjhmxjamatlxpkykde.supabase.co/rest/v1"
.SED_ANON_KEY <- paste0(
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.",
  "eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inpla2pobXhqYW1hdGx4cGt5a2RlIiwicm9sZSI6ImFub24iLCJpYXQiOjE3ODEwNzY4MzIsImV4cCI6MjA5NjY1MjgzMn0.",
  "px8FcU0QK8w9v95kwGlGzASKpY3drsxAvFe0e6wUoCU"
)
.SED_PAGE <- 1000
.SED_METODOLOGIA_URL <- paste0(
  "https://raw.githubusercontent.com/pedropaulofernandes88-stack/",
  "saude-publica-br/main/mcp_server/saudeemdado_mcp/metodologia.json"
)

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

# Adiciona eq.<uf>/eq.<municipio_cod> a params, preferindo municipio_cod.
.sed_uf_ou_municipio <- function(params, uf = NULL, municipio_cod = NULL) {
  if (!is.null(municipio_cod) && nzchar(municipio_cod)) {
    params$municipio_cod <- paste0("eq.", municipio_cod)
  } else if (!is.null(uf) && nzchar(uf)) {
    params$uf_sigla <- paste0("eq.", toupper(uf))
  }
  params
}

# ══════════════════════════════════════════════════════════════════════════
# MORTALIDADE (SIM)
# ══════════════════════════════════════════════════════════════════════════

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

#' Descrições oficiais de categorias CID-10 de 3 caracteres (ex.: I21, C34)
sed_cid10 <- function(codigos = NULL) {
  df <- sed_get("dim_cid10_categoria", list(select = "causabas_3,descricao", order = "causabas_3"))
  if (!is.null(codigos)) df <- df |> filter(causabas_3 %in% toupper(codigos))
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

#' Confiabilidade do registro de óbitos: % de causas mal-definidas e classe
#' (Bom <5% | Regular 5-10% | Ruim >10%). Informe uf OU municipio_cod.
sed_qualidade_registro <- function(uf = NULL, municipio_cod = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,obitos_total,",
      "obitos_mal_definidas,pct_mal_definidas,classificacao",
      sep = ""
    ),
    order = "pct_mal_definidas.desc"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_qualidade_registro_municipio", params)
}

#' Taxa de mortalidade infantil por UF e ano (óbitos <1 ano / nascidos vivos x 1000)
sed_mortalidade_infantil <- function(uf = NULL, ano = NULL) {
  params <- list(select = "uf_sigla,ano,nascidos,obitos_menor1,tmi_por_mil",
                  order = "uf_sigla,ano")
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  if (!is.null(ano)) params$ano <- paste0("eq.", ano)
  sed_get("mart_mortalidade_infantil_uf", params)
}

#' Cruzamento leitos x local do óbito, com taxa padronizada. uf OU municipio_cod.
sed_vazio_assistencial <- function(uf = NULL, municipio_cod = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,populacao,",
      "porte_quartil,leitos_total,leitos_sus,leitos_sus_por_mil,sem_leito,",
      "obitos,obitos_hospital,obitos_domicilio,pct_obito_domicilio,",
      "pct_obito_hospital,taxa_obitos_100k,taxa_padronizada_100k,ivs_score",
      sep = ""
    ),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_vazio_assistencial_municipio", params)
}

#' Células município x causa x ano com excesso sobre a história do próprio
#' município (2020-2024). Informe municipio_cod (6 dígitos).
sed_anomalia_causa <- function(municipio_cod, ano = 2024, top = 50) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,ano,causabas_3,obitos,esperado,",
      "esperado_relativo,razao,p_proprio,p_relativo,excesso_proprio,excesso_relativo",
      sep = ""
    ),
    municipio_cod = paste0("eq.", municipio_cod),
    ano           = paste0("eq.", ano),
    order         = "excesso_proprio.desc",
    limit         = as.character(top)
  )
  sed_get("mart_anomalia_causa_municipio", params)
}

#' Perfil de causas de morte do município (componentes pc1-pc6, sem porte/idade/
#' registro/COVID). Informe uf OU municipio_cod.
sed_perfil_causas <- function(uf = NULL, municipio_cod = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,obitos_periodo,grupo,",
      "indice_inespecificidade,pc1,pc2,pc3,pc4,pc5,pc6",
      sep = ""
    ),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_perfil_mortalidade_municipio", params)
}

# ══════════════════════════════════════════════════════════════════════════
# INTERNAÇÕES (SIH/AIH)
# ══════════════════════════════════════════════════════════════════════════

#' Internações SUS por município: volume, permanência, mortalidade e custo
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

#' ICSAP - internações por condições sensíveis à atenção primária, por município (2024)
sed_icsap <- function(uf = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,internacoes_total,",
      "internacoes_icsap,aih_continuacao,aih_continuacao_icsap,",
      "pct_icsap,icsap_100k,populacao",
      sep = ""
    ),
    ano   = "eq.2024",
    order = "municipio_cod"
  )
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get("mart_icsap_municipio", params)
}

#' Internações por agravo traçador (diabetes, avc, iam, icc, asma, dpoc,
#' pneumonia, depressao, esquizofrenia, alcool_drogas, tce), por município (2024)
sed_internacoes_agravo <- function(uf = NULL, agravo = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,agravo,agravo_label,grupo,",
      "internacoes,obitos,aih_continuacao,aih_normal,permanencia_media,",
      "mortalidade_pct,custo_medio,internacoes_100k",
      sep = ""
    ),
    order = "municipio_cod"
  )
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  if (!is.null(agravo)) params$agravo <- paste0("eq.", tolower(agravo))
  sed_get("mart_internacoes_agravo", params)
}

#' Visão por estabelecimento (CNES), 2024: volume, permanência, mortalidade
#' bruta e custo. ordenar_por: internacoes | mortalidade_pct | permanencia_media | custo_medio
sed_hospitais <- function(uf = NULL, ordenar_por = "internacoes", top = 50) {
  params <- list(
    select = paste(
      "cnes,municipio_nome,uf_sigla,capitulo_principal,internacoes,",
      "aih_continuacao,aih_normal,permanencia_media,mortalidade_pct,custo_medio",
      sep = ""
    ),
    ano          = "eq.2024",
    internacoes  = "gte.50",
    order        = paste0(ordenar_por, ".desc"),
    limit        = as.character(top)
  )
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get("mart_internacoes_hospital", params)
}

#' Para onde os moradores de um município viajam para se internar (SIH 2024)
sed_fluxo_pacientes <- function(municipio_res_cod) {
  params <- list(
    select        = "municipio_mov,municipio_mov_nome,uf_mov,internacoes",
    municipio_res = paste0("eq.", municipio_res_cod),
    ano           = "eq.2024",
    order         = "internacoes.desc"
  )
  sed_get("mart_fluxo_intermunicipal", params)
}

#' HSMR - mortalidade hospitalar padronizada por estabelecimento, com IC95%.
#' Informe uf OU cnes.
sed_hsmr <- function(uf = NULL, cnes = NULL, ano = 2024, top = 50) {
  params <- list(
    select = paste(
      "cnes,municipio_cod,municipio_nome,uf_sigla,ano,internacoes,",
      "obitos_observados,obitos_esperados,hsmr,estavel,hsmr_ic95_inf,",
      "hsmr_ic95_sup,significancia,tem_uti,leitos_total,leitos_uti,estrato",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "hsmr.desc",
    limit = as.character(top)
  )
  if (!is.null(cnes)) params$cnes <- paste0("eq.", cnes) else if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get("mart_hsmr_hospital", params)
}

#' Permanência por diagnóstico (CID3) vs. mediana nacional, por estabelecimento
sed_permanencia_diagnostico <- function(uf = NULL, cnes = NULL, cid3 = NULL,
                                         ano = 2024, top = 100) {
  params <- list(
    select = paste(
      "cnes,municipio_cod,municipio_nome,uf_sigla,ano,cid3,capitulo_cid,",
      "internacoes,mediana_hospital_dias,mediana_nacional_dias,desvio_dias",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "internacoes.desc",
    limit = as.character(top)
  )
  if (!is.null(cnes)) params$cnes <- paste0("eq.", cnes) else if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  if (!is.null(cid3)) params$cid3 <- paste0("eq.", toupper(cid3))
  sed_get("mart_los_hospital", params)
}

#' Série mensal de internações por estabelecimento (histórico da projeção)
sed_demanda_hospital <- function(cnes = NULL, uf = NULL, top = 500) {
  params <- list(
    select = "cnes,municipio_cod,municipio_nome,uf_sigla,ano_mes,internacoes,obitos,valor_total",
    order  = "ano_mes",
    limit  = as.character(top)
  )
  if (!is.null(cnes)) params$cnes <- paste0("eq.", cnes) else if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get("mart_demanda_mensal_hospital", params)
}

#' Projeção de internações mensais por hospital (até 3 meses, IC95%).
#' Leia sempre a faixa, nunca o ponto — ver docstring do MCP para as ressalvas.
sed_forecast_demanda <- function(cnes = NULL, uf = NULL, top = 200) {
  params <- list(
    select = paste(
      "cnes,municipio_cod,municipio_nome,uf_sigla,ano_mes_previsto,",
      "internacoes_previstas,ic_inferior,ic_superior,n_meses_historico,",
      "confianca,horizonte_meses,faixa_volume,status_validacao,motivo_status,",
      "smape_backtest_pct,modelo,ultima_competencia,treinado_em,commit_codigo",
      sep = ""
    ),
    order = "ano_mes_previsto",
    limit = as.character(top)
  )
  if (!is.null(cnes)) params$cnes <- paste0("eq.", cnes) else if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  df <- sed_get("mart_forecast_demanda_hospital", params)
  if (nrow(df) > 0) {
    mes_atual <- format(Sys.Date(), "%Y-%m")
    df$previsao_vencida <- !is.na(df$ano_mes_previsto) & df$ano_mes_previsto < mes_atual
  }
  df
}

#' Cruzamento de leitos (CNES) com ICSAP (SIH) por município, 2024
sed_oferta_icsap <- function(uf = NULL, municipio_cod = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,populacao,",
      "leitos_total,leitos_sus,leitos_sus_por_mil,sem_leito,internacoes_total,",
      "internacoes_por_mil,internacoes_icsap,pct_icsap,icsap_100k,ivs_score",
      sep = ""
    ),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_leitos_icsap_municipio", params)
}

#' TRADUÇÃO: pct_icsap do município vs. mediana dos pares, em internações,
#' leitos-ano e R$. Informe municipio_cod OU uf (ranking dos maiores).
sed_icsap_distancia_pares <- function(municipio_cod = NULL, uf = NULL, top = 20) {
  campos <- paste(
    "municipio_cod,municipio_nome,uf_sigla,ano,populacao,internacoes_total,",
    "internacoes_icsap,pct_icsap,arquetipo,criterio_pares,n_pares,",
    "mediana_pares_pct,p25_pares_pct,diferenca_pp,internacoes_acima_pares,",
    "internacoes_acima_p25,custo_associado_reais,leitos_dia_associados,",
    "leitos_equivalentes_ano,custo_medio_icsap_ref,amostra_pequena",
    sep = ""
  )
  if (!is.null(municipio_cod) && nzchar(municipio_cod)) {
    params <- list(select = campos, municipio_cod = paste0("eq.", municipio_cod))
  } else {
    params <- list(select = campos, order = "internacoes_acima_pares.desc", limit = as.character(top))
    if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  }
  sed_get("mart_icsap_pares", params)
}

# ══════════════════════════════════════════════════════════════════════════
# DENGUE (SINAN)
# ══════════════════════════════════════════════════════════════════════════

#' Dengue (SINAN). nivel="ano" (resumo municipal) ou "uf" (série semanal por UF)
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

#' ANÁLISE: canal endêmico de dengue de uma UF (diagrama de controle).
#' Compara casos semanais do ano observado com a banda P25-P75 dos anos
#' anteriores (2015+, excluindo o ano observado).
sed_canal_endemico_dengue <- function(uf, ano = 2024) {
  linhas <- sed_get("mart_dengue_uf_semana", list(
    select   = "ano_epi,semana_epi,casos:casos_provaveis",
    uf_sigla = paste0("eq.", toupper(uf)),
    order    = "ano_epi,semana_epi"
  ))
  if (nrow(linhas) == 0) {
    return(list(erro = sprintf("sem dados de dengue para a UF %s", toupper(uf))))
  }

  quantil_linear <- function(vals, p) {
    if (length(vals) == 0) return(0)
    s <- sort(vals)
    i <- (length(s) - 1) * p
    lo <- floor(i) + 1L; hi <- min(floor(i) + 2L, length(s))
    round(s[lo] + (s[hi] - s[lo]) * (i - floor(i)))
  }

  anos_base <- sort(unique(linhas$ano_epi[linhas$ano_epi != ano]))
  canal <- list(); acima <- 0L

  for (w in 1:52) {
    base <- linhas$casos[linhas$semana_epi == w & linhas$ano_epi != ano]
    obs_vals <- linhas$casos[linhas$semana_epi == w & linhas$ano_epi == ano]
    obs <- if (length(obs_vals) > 0) obs_vals[1] else 0
    p75 <- quantil_linear(base, 0.75)
    if (obs > p75) acima <- acima + 1L
    canal[[w]] <- list(semana = w, p25 = quantil_linear(base, 0.25),
                        mediana = quantil_linear(base, 0.5), p75 = p75, observado = obs)
  }
  status <- if (acima >= 13) "surto prolongado (>=1 trimestre acima da faixa)" else
            if (acima > 0) "acima da faixa em algumas semanas" else "dentro da faixa esperada"

  list(uf = toupper(uf), ano_observado = ano,
       baseline = sprintf("%d-%d (exclui o ano observado)", min(anos_base), max(anos_base)),
       semanas_acima_p75 = acima, status = status,
       canal = bind_rows(canal),
       fonte = "SINAN/DataSUS (casos prováveis por semana de primeiros sintomas)")
}

# ══════════════════════════════════════════════════════════════════════════
# ÁGUA PARA CONSUMO HUMANO (SISAGUA)
# ══════════════════════════════════════════════════════════════════════════

#' Vigilância da água: amostras, E. coli, coliformes totais. Informe uf OU
#' municipio_cod. MEDE VOLUME DE ANÁLISE, NÃO POTABILIDADE.
sed_agua_vigilancia <- function(uf = NULL, municipio_cod = NULL, ano = 2024, parametro = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,parametro,",
      "amostras_analisadas,escherichia_coli,coliformes_totais,",
      "meses_com_analise,formas_de_abastecimento",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "municipio_cod,parametro"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  if (!is.null(parametro)) params$parametro <- paste0("eq.", parametro)
  sed_get("mart_sisagua_municipio", params)
}

#' COBERTURA da coleta do SISAGUA: separa "não analisou" de "não coletamos"
sed_agua_cobertura <- function(uf = NULL, municipio_cod = NULL) {
  params <- list(select = "municipio_cod,uf_sigla,coletado,registros_brutos,linhas_no_mart",
                  order = "municipio_cod")
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_sisagua_cobertura", params)
}

# ══════════════════════════════════════════════════════════════════════════
# VACINAÇÃO (PNI/RNDS)
# ══════════════════════════════════════════════════════════════════════════

#' Doses aplicadas do PNI por competência mensal, UF e imunobiológico.
#' DOSE NÃO É COBERTURA nem PESSOA.
sed_vacinacao_doses <- function(uf = NULL, competencia = NULL, imunobiologico = NULL) {
  params <- list(select = "competencia,uf_sigla,imunobiologico,doses",
                  order = "competencia,uf_sigla,imunobiologico")
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  if (!is.null(competencia)) params$competencia <- paste0("eq.", competencia)
  if (!is.null(imunobiologico)) params$imunobiologico <- paste0("eq.", imunobiologico)
  sed_get("mart_vacinacao_uf_mes", params)
}

#' Cobertura vacinal em menores de 1 ano, só por UF (Penta, Polio, Rotavirus,
#' Pneumo, Meningo - 1a dose). NÃO EXISTE COBERTURA MUNICIPAL nesta fonte.
sed_cobertura_vacinal <- function(uf = NULL, ano = 2024) {
  params <- list(select = "uf_sigla,ano,indicador,doses,nascidos,cobertura_pct",
                  ano = paste0("eq.", ano), order = "uf_sigla,indicador")
  if (!is.null(uf)) params$uf_sigla <- paste0("eq.", toupper(uf))
  sed_get("mart_cobertura_vacinal_uf", params)
}

# ══════════════════════════════════════════════════════════════════════════
# NASCIMENTOS (SINASC)
# ══════════════════════════════════════════════════════════════════════════

#' Nascidos vivos por município e ano: volume, % baixo peso (<2500g),
#' % prematuridade (<37 sem), % 7+ consultas de pré-natal, idade média da mãe.
#' Informe uf OU municipio_cod. Complementa a análise individual do SINASC
#' (peso por tipo de parto) com o agregado oficial por município.
sed_natalidade <- function(uf = NULL, municipio_cod = NULL, ano = 2024) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,nascidos,",
      "pct_baixo_peso,pct_prematuro,pct_prenatal_7mais,idade_media_mae",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_natalidade_municipio", params)
}

# ══════════════════════════════════════════════════════════════════════════
# ATENÇÃO PRIMÁRIA (e-Gestor AB)
# ══════════════════════════════════════════════════════════════════════════

#' Cobertura POTENCIAL da APS por município e mês (equipes, capacidade, %).
#' NÃO é população efetivamente acompanhada — ver docstring do MCP.
sed_cobertura_aps <- function(uf = NULL, municipio_cod = NULL, ano = 2026, mes = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,mes,mes_competencia,",
      "populacao,qt_esf,qt_eap20,qt_eap30,qt_esfr,qt_ecr,qt_eapp20,qt_eapp30,",
      "capacidade_equipe,cobertura_pct",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "municipio_cod,mes"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  if (!is.null(mes)) params$mes <- paste0("eq.", mes)
  sed_get("mart_cobertura_aps_municipio", params)
}

#' Cruzamento cobertura potencial da APS x %ICSAP — já testado, deu NULO
#' (Spearman ~0). Ver sed_metodologia("cobertura_aps").
sed_cobertura_aps_icsap <- function(uf = NULL, municipio_cod = NULL, ano = 2024) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,populacao,",
      "cobertura_pct,cobertura_efetiva,qt_esf,internacoes_total,",
      "internacoes_icsap,pct_icsap,icsap_100k,ivs_score",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_cobertura_icsap_municipio", params)
}

#' Teste de robustez do cruzamento anterior, dentro do quartil de porte —
#' RESULTADO TAMBÉM NULO.
sed_equidade_aps <- function(uf = NULL, municipio_cod = NULL, ano = 2024) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,populacao,",
      "porte_quartil,esf_por_10k,pct_esf_no_porte,pct_icsap,icsap_100k,",
      "pct_icsap_no_porte,ivs_score,ivs_quartil,atencao",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_equidade_aps_municipio", params)
}

# ══════════════════════════════════════════════════════════════════════════
# REDE (CNES), SAÚDE SUPLEMENTAR (ANS) E FINANCIAMENTO (SIOPS)
# ══════════════════════════════════════════════════════════════════════════

#' Vínculos de planos de saúde por município e ano (ANS). VÍNCULO NÃO É PESSOA.
sed_saude_suplementar <- function(uf = NULL, municipio_cod = NULL, ano = 2024) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,populacao,",
      "vinculos_medico_hospitalar,vinculos_plano_por_100_hab,razao_implausivel",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_saude_suplementar_municipio", params)
}

#' Rede de estabelecimentos cadastrados no CNES por município (retrato do
#' cadastro corrente). CADASTRO NÃO É OPERAÇÃO.
sed_rede_cnes <- function(uf = NULL, municipio_cod = NULL) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,estabelecimentos_total,",
      "estabelecimentos_hospitalares,publico,privado_lucrativo,",
      "sem_fins_lucrativos,pessoa_fisica,internacional,populacao,",
      "estab_por_10k,estab_hosp_por_10k,pct_publico,ano_referencia",
      sep = ""
    ),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_cnes_municipio", params)
}

#' Gasto público municipal em saúde (SIOPS): por habitante, transferências
#' SUS, % receita própria, mínimo constitucional (EC 29).
sed_gasto_saude <- function(uf = NULL, municipio_cod = NULL, ano = 2024) {
  params <- list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,ano,populacao_siops,",
      "gasto_proprio_saude_hab,despesa_total_saude,transf_sus_hab,",
      "pct_receita_propria_saude,abaixo_do_minimo_ec29",
      sep = ""
    ),
    ano   = paste0("eq.", ano),
    order = "municipio_cod"
  )
  params <- .sed_uf_ou_municipio(params, uf, municipio_cod)
  sed_get("mart_siops_municipio", params)
}

# ══════════════════════════════════════════════════════════════════════════
# CONTEXTO SOCIAL (IBGE + componentes)
# ══════════════════════════════════════════════════════════════════════════

#' Quatro eixos de contexto social/sistema de saúde (spc1-spc4, 61,5% da
#' variância) mais as 15 variáveis originais.
sed_contexto_social <- function(municipio_cod = NULL, top = 200) {
  params <- list(
    select = paste(
      "municipio_cod,spc1,spc2,spc3,spc4,taxa_analfabetismo,ivs_score,",
      "estab_por_10k,vinculos_plano_por_100_hab,gasto_proprio_saude_hab,",
      "pct_prenatal_7mais,cobertura_pct,leitos_sus_por_mil,hosp_por_10k,log_pop",
      sep = ""
    ),
    order = "municipio_cod",
    limit = as.character(top)
  )
  if (!is.null(municipio_cod)) params$municipio_cod <- paste0("eq.", municipio_cod)
  sed_get("mart_contexto_social_municipio", params)
}

# ══════════════════════════════════════════════════════════════════════════
# ANÁLISES COMPOSTAS (copiloto, comparação com pares)
# ══════════════════════════════════════════════════════════════════════════

#' COPILOTO: resumo priorizado de sinais para um município (6 dígitos) —
#' confiabilidade do registro, ICSAP (com oferta de leitos ao lado) e
#' letalidade de dengue.
sed_detectar_anomalias <- function(municipio_cod) {
  achados <- list()

  q <- sed_get("mart_qualidade_registro_municipio", list(
    select = "municipio_nome,uf_sigla,pct_mal_definidas,classificacao",
    municipio_cod = paste0("eq.", municipio_cod)
  ))
  nome <- if (nrow(q) > 0) q$municipio_nome[1] else municipio_cod
  if (nrow(q) > 0 && q$classificacao[1] == "Ruim") {
    achados[[length(achados) + 1]] <- list(
      sinal = "qualidade_registro", gravidade = "alta",
      detalhe = sprintf("registro RUIM (%s%% mal-definidas) - causas de morte pouco confiáveis",
                         q$pct_mal_definidas[1]),
      fonte = "SIM 2022-2024"
    )
  }

  ic <- sed_get("mart_icsap_municipio", list(
    select = "pct_icsap,internacoes_total,internacoes_icsap",
    municipio_cod = paste0("eq.", municipio_cod), ano = "eq.2024"
  ))
  if (nrow(ic) > 0 && !is.na(ic$pct_icsap[1])) {
    p <- as.numeric(ic$pct_icsap[1])
    if (p > 30 && (if (is.na(ic$internacoes_total[1])) 0 else ic$internacoes_total[1]) >= 200) {
      lt <- sed_get("mart_leitos_icsap_municipio", list(
        select = "leitos_sus,leitos_sus_por_mil",
        municipio_cod = paste0("eq.", municipio_cod), ano = "eq.2024"
      ))
      n_leitos <- if (nrow(lt) > 0 && !is.na(lt$leitos_sus[1])) as.integer(lt$leitos_sus[1]) else NA
      oferta <- if (is.na(n_leitos)) {
        "Oferta local de leitos indisponível para este município - obtenha-a antes de interpretar o número."
      } else if (n_leitos > 0) {
        sprintf(paste0("Este município TEM %d leitos SUS (%s/mil hab), e ter leito local está ",
                        "associado a %%ICSAP mais alto (+51%% a +85%% de internação sensível ",
                        "conforme o porte) - parte deste número pode ser oferta, não APS."),
                n_leitos, lt$leitos_sus_por_mil[1])
      } else {
        paste0("Este município NÃO tem leito SUS local, então a oferta hospitalar local NÃO ",
               "explica o número (municípios sem leito têm %ICSAP mediano MENOR, 17,7%% vs. ",
               "21,4%%). Investigue o fluxo de internação dos residentes antes de qualquer leitura.")
      }
      achados[[length(achados) + 1]] <- list(
        sinal = "icsap", gravidade = if (p < 40) "média" else "alta",
        detalhe = sprintf("%.1f%% de internações evitáveis (média nacional ~21%%). NÃO conclua fragilidade da atenção primária. %s Compare com pares de mesmo porte e oferta antes de interpretar.", p, oferta),
        leitos_sus = n_leitos,
        fonte = "SIH 2024 + CNES 2024 (mart_leitos_icsap_municipio)"
      )
    }
  }

  dg <- sed_get("mart_dengue_municipio_ano", list(
    select = "casos_provaveis,obitos,letalidade_pct,incidencia_100k",
    municipio_cod = paste0("eq.", municipio_cod), ano_epi = "eq.2024"
  ))
  if (nrow(dg) > 0 && !is.na(dg$obitos[1]) && dg$obitos[1] > 0) {
    achados[[length(achados) + 1]] <- list(
      sinal = "dengue", gravidade = "média",
      detalhe = sprintf("%d casos e %d óbitos por dengue (letalidade %s%%)",
                         dg$casos_provaveis[1], dg$obitos[1], dg$letalidade_pct[1]),
      fonte = "SINAN 2024"
    )
  }

  if (length(achados) == 0) {
    achados <- list(list(sinal = "nenhum", detalhe = "sem anomalias nos limiares avaliados"))
  }
  list(municipio = nome, codigo = municipio_cod, n_sinais = length(achados), sinais = achados)
}

#' ANÁLISE: compara um município (6 dígitos) com seus pares (mesmo estrato
#' de mortalidade x vulnerabilidade x internações, 2023). Cobre ~1.700
#' municípios maiores.
sed_comparar_com_pares <- function(municipio_cod) {
  alvo <- sed_get("dim_cluster_municipio", list(
    select = paste(
      "municipio_cod,municipio_nome,uf_sigla,regiao,cluster,estrato_cod,perfil,",
      "taxa_padronizada_100k,ivs_score,internacoes_100k",
      sep = ""
    ),
    municipio_cod = paste0("eq.", municipio_cod)
  ))
  if (nrow(alvo) == 0) {
    return(list(erro = paste(
      "município fora da base de estratos (cobre ~1.700 municípios maiores).",
      "Use sed_municipios() para os indicadores diretos."
    )))
  }
  m <- alvo[1, ]
  pares <- sed_get("dim_cluster_municipio", list(
    select = "municipio_cod,municipio_nome,uf_sigla,taxa_padronizada_100k,ivs_score,internacoes_100k",
    cluster = paste0("eq.", m$cluster)
  ))

  stats_campo <- function(campo) {
    vals <- sort(pares[[campo]][!is.na(pares[[campo]])])
    v <- m[[campo]]
    if (is.na(v) || length(vals) == 0) return(list(valor = v, mediana_pares = NA, percentil = NA))
    mediana <- vals[floor(length(vals) / 2) + 1L]  # replica vals[len(vals)//2] do Python (0-indexado)
    pct <- round(100 * sum(vals <= v) / length(vals))
    list(valor = v, mediana_pares = mediana, percentil = pct)
  }

  outros <- pares |>
    filter(municipio_cod != !!municipio_cod,
           !is.na(taxa_padronizada_100k), !is.na(m$taxa_padronizada_100k)) |>
    mutate(dist = abs(taxa_padronizada_100k - m$taxa_padronizada_100k)) |>
    arrange(dist) |>
    head(5)

  list(
    municipio = m$municipio_nome, uf = m$uf_sigla, codigo = municipio_cod,
    arquetipo = m$perfil, estrato_cod = m$estrato_cod, n_pares = nrow(pares),
    metricas = list(
      taxa_padronizada_100k = stats_campo("taxa_padronizada_100k"),
      ivs_score             = stats_campo("ivs_score"),
      internacoes_100k      = stats_campo("internacoes_100k")
    ),
    pares_mais_proximos = outros |> select(municipio_nome, uf_sigla, taxa_padronizada_100k),
    fonte = paste(
      "SIM/SIH/DataSUS + IBGE Censo 2022; estratos determinísticos por tercis fixos,",
      "2023 (dim_cluster_municipio)."
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════
# BOLETIM SEMANAL (situação atual) E METADADOS
# ══════════════════════════════════════════════════════════════════════════

#' SITUAÇÃO ATUAL: nowcasting InfoDengue (27 capitais), canal endêmico Brasil,
#' excesso de mortalidade e KPIs de internações. edicao = "" pega a mais recente
#' (formato "2026-se30" para uma específica).
sed_boletim_semanal <- function(edicao = "") {
  base <- "https://saudeemdado.com/sdata/boletins"
  idx <- request(paste0(base, "/index.json")) |> req_timeout(30) |> req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  alvo <- if (nzchar(edicao)) edicao else if (length(idx) > 0) idx$edicao[1] else ""
  if (!nzchar(alvo)) return(list(erro = "nenhuma edição publicada ainda"))

  resp <- request(paste0(base, "/", alvo, ".json")) |> req_timeout(30) |>
    req_error(is_error = function(resp) FALSE) |> req_perform()
  if (resp_status(resp) == 404) {
    return(list(erro = sprintf("edição '%s' não encontrada", alvo), disponiveis = idx$edicao))
  }
  b <- resp_body_json(resp, simplifyVector = TRUE)
  b$permalink <- paste0("https://saudeemdado.com/boletim-semanal/?e=", alvo)
  b$edicoes_disponiveis <- idx$edicao
  b
}

#' Fontes, métodos, exclusões, licença, DOI e versão do dataset (citação/procedência)
sed_metadados <- function() {
  df <- sed_get("meta_dataset", list(select = "chave,valor"))
  setNames(as.list(df$valor), df$chave)
}

#' DEFINIÇÃO E LIMITES de um indicador: numerador, denominador, unidade,
#' defasagem, o que NÃO mede e leituras já testadas/refutadas. Aceita o id
#' (ex. "icsap") ou o nome de uma função sed_*. Sem argumento, lista o índice.
#' Baixa metodologia.json direto do repositório GitHub público (cacheado na sessão).
.SED_CATALOGO <- NULL
sed_metodologia <- function(indicador = "") {
  if (is.null(.SED_CATALOGO)) {
    txt <- request(.SED_METODOLOGIA_URL) |> req_timeout(30) |> req_perform() |> resp_body_string()
    assign(".SED_CATALOGO", fromJSON(txt, simplifyVector = FALSE), envir = .GlobalEnv)
  }
  catalogo <- .SED_CATALOGO
  indicadores <- catalogo$indicadores
  chave <- tolower(trimws(indicador))

  if (!nzchar(chave)) {
    return(list(
      versao = catalogo$versao,
      indicadores = purrr::imap(indicadores, ~ list(id = .y, nome = .x$nome, ferramentas = .x$ferramentas)),
      como_usar = "Chame sed_metodologia('<id>') ou sed_metodologia('<nome_da_funcao>')."
    ))
  }
  if (chave %in% names(indicadores)) return(c(id = chave, indicadores[[chave]]))

  for (k in names(indicadores)) {
    ferramentas <- tolower(unlist(indicadores[[k]]$ferramentas))
    if (chave %in% ferramentas) return(c(id = k, indicadores[[k]]))
  }
  list(erro = sprintf("indicador '%s' não está no catálogo", indicador),
       disponiveis = sort(names(indicadores)),
       nota = "Sem entrada aqui, não há limite documentado para citar.")
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
