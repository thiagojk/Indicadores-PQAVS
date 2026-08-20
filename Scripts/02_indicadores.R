# =========================================================
# PROCESSAMENTO DOS INDICADORES
# =========================================================

# ---------------------------------------------------------
# 1. Leitura dos dados brutos
# ---------------------------------------------------------

Indicadores_Brutos <- lapply(
  arquivos,
  ler_indicador
)


# ---------------------------------------------------------
# 2. Exclusão do município definido em 00_config.R
# ---------------------------------------------------------

Indicadores_Brutos <- Map(
  function(dados, nome) {
    remover_municipio(
      dados,
      municipio_excluido,
      nome
    )
  },
  Indicadores_Brutos,
  names(Indicadores_Brutos)
)


# ---------------------------------------------------------
# 3. Cálculo dos indicadores com regra padrão
# ---------------------------------------------------------

Indicadores <- Map(
  function(
      indicador,
      meta,
      multiplicar_res,
      coluna_mun
  ) {

    calcular_indicador(
      Indicadores_Brutos[[indicador]],
      meta = meta,
      multiplicar_res = multiplicar_res,
      coluna_mun = coluna_mun
    )
  },
  especificacoes_padrao$indicador,
  especificacoes_padrao$meta,
  especificacoes_padrao$multiplicar_res,
  especificacoes_padrao$coluna_mun
) |>
  setNames(
    especificacoes_padrao$indicador
  )


# ---------------------------------------------------------
# 4. Indicadores 11 e 12
# ---------------------------------------------------------

Indicadores$IND_11 <- calcular_indicador_11_12(
  Indicadores_Brutos$IND_11,
  coluna_mun = "COD_MUN"
)

Indicadores$IND_12 <- calcular_indicador_11_12(
  Indicadores_Brutos$IND_12,
  coluna_mun = "COD_MUN",
  multiplicar_res = TRUE
)


# ---------------------------------------------------------
# 5. Ajustes específicos
# ---------------------------------------------------------

Indicadores$IND_03 <- Indicadores$IND_03 |>
  dplyr::mutate(
    METAS = tidyr::replace_na(
      METAS,
      "NÃO ALCANÇOU"
    )
  )

Indicadores$IND_14 <- Indicadores$IND_14 |>
  tidyr::drop_na(UF)

Indicadores$IND_12 <- Indicadores$IND_12 |>
  tidyr::drop_na(COD_MUN)


# ---------------------------------------------------------
# 6. Conferência e ordenação dos 14 indicadores
# ---------------------------------------------------------

faltantes <- setdiff(
  esperados,
  names(Indicadores)
)

if (length(faltantes) > 0) {
  stop(
    paste(
      "Indicadores faltantes:",
      paste(
        faltantes,
        collapse = ", "
      )
    )
  )
}

Indicadores <- Indicadores[
  esperados
]
