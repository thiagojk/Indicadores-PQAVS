# =========================================================
# FUNÇÕES AUXILIARES
# =========================================================

ler_indicador <- function(arquivo) {
  readxl::read_excel(arquivo) |>
    dplyr::select(where(~ sum(!is.na(.)) > 1))
}


converter_numero <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x_chr <- stringr::str_trim(as.character(x))

  tem_virgula <- stringr::str_detect(x_chr, ",")
  tem_ponto <- stringr::str_detect(x_chr, "\\.")

  x_limpo <- dplyr::case_when(
    tem_virgula & tem_ponto ~ stringr::str_replace(
      stringr::str_remove_all(x_chr, "\\."),
      ",",
      "."
    ),
    tem_virgula & !tem_ponto ~ stringr::str_replace(x_chr, ",", "."),
    TRUE ~ x_chr
  )

  as.numeric(x_limpo)
}


calcular_indicador <- function(
    indicador,
    meta,
    multiplicar_res = FALSE,
    coluna_mun
) {

  if (!coluna_mun %in% names(indicador)) {
    stop(
      sprintf(
        "A coluna de município '%s' não foi encontrada nos dados.",
        coluna_mun
      )
    )
  }

  nomes <- toupper(names(indicador))

  col_res <- names(indicador)[
    stringr::str_detect(nomes, "^RES($|_|[0-9])")
  ]

  col_num <- names(indicador)[
    stringr::str_detect(nomes, "^NUM($|_|[0-9])")
  ]

  col_den <- names(indicador)[
    stringr::str_detect(nomes, "^DEN($|_|[0-9])")
  ]

  if (length(col_res) >= 1) {

    col_res <- col_res[1]

    indicador <- indicador |>
      dplyr::mutate(
        RES = converter_numero(.data[[col_res]]),
        RES = if (multiplicar_res) RES * 100 else RES,
        RES = round(RES, 1)
      )

  } else if (length(col_num) >= 1 && length(col_den) >= 1) {

    col_num <- col_num[1]
    col_den <- col_den[1]

    indicador <- indicador |>
      dplyr::mutate(
        !!col_num := tidyr::replace_na(
          converter_numero(.data[[col_num]]),
          0
        ),
        !!col_den := tidyr::replace_na(
          converter_numero(.data[[col_den]]),
          0
        ),
        RES = .data[[col_num]] / .data[[col_den]],
        RES = if (multiplicar_res) RES * 100 else RES,
        RES = round(RES, 1)
      )

  } else {

    stop(
      paste(
        "Não foi possível identificar colunas",
        "de NUM/DEN ou RES neste indicador."
      )
    )
  }

  indicador |>
    dplyr::mutate(
      METAS = dplyr::case_when(
        is.nan(RES) ~ "NÃO ALCANÇOU",
        is.na(RES) ~ "NÃO ALCANÇOU",
        RES >= meta ~ "ALCANÇOU",
        TRUE ~ "NÃO ALCANÇOU"
      )
    )
}


calcular_indicador_11_12 <- function(
    indicador,
    coluna_mun,
    multiplicar_res = FALSE
) {

  if (!coluna_mun %in% names(indicador)) {
    stop(
      sprintf(
        "A coluna de município '%s' não foi encontrada nos dados.",
        coluna_mun
      )
    )
  }

  colunas_necessarias <- c("RES_2024", "RES_2025")
  colunas_ausentes <- setdiff(colunas_necessarias, names(indicador))

  if (length(colunas_ausentes) > 0) {
    stop(
      paste(
        "Colunas não encontradas:",
        paste(colunas_ausentes, collapse = ", ")
      )
    )
  }

  indicador |>
    dplyr::mutate(
      RES_2024 = converter_numero(RES_2024),
      RES_2025 = converter_numero(RES_2025),

      RES_2024 = if (multiplicar_res) RES_2024 * 100 else RES_2024,
      RES_2025 = if (multiplicar_res) RES_2025 * 100 else RES_2025,

      RES_BRUTO = RES_2025 - RES_2024,

      RES = dplyr::if_else(
        RES_BRUTO < 0 & RES_BRUTO > -1,
        round(RES_BRUTO, 2),
        round(RES_BRUTO, 1)
      ),

      METAS = dplyr::case_when(
        is.na(RES_2025) |
          is.na(RES_2024) |
          is.na(RES_BRUTO) ~ "NÃO ALCANÇOU",

        RES_2025 == 0 &
          RES_2024 == 0 &
          RES_BRUTO == 0 ~ "ALCANÇOU",

        RES_2025 != 0 &
          RES_2024 != 0 &
          RES_BRUTO == 0 ~ "NÃO ALCANÇOU",

        RES_BRUTO < 0 &
          RES_BRUTO > -1 ~ "NÃO ALCANÇOU",

        RES_BRUTO <= -1 ~ "ALCANÇOU",

        RES_BRUTO > 0 ~ "NÃO ALCANÇOU",

        TRUE ~ "NÃO ALCANÇOU"
      )
    ) |>
    dplyr::select(-RES_BRUTO)
}


calcular_metas <- function(indicador) {

  if (!"METAS" %in% names(indicador)) {
    stop(
      paste(
        "A coluna 'METAS' não foi encontrada.",
        "Execute calcular_indicador() primeiro."
      )
    )
  }

  indicador |>
    dplyr::count(METAS, name = "Quantidade") |>
    dplyr::rename(Resultado_Meta = METAS)
}


descrever_indicador <- function(
    indicador,
    nome_indicador
) {

  total_avaliados <- nrow(indicador)

  n_alcancou <- sum(
    indicador$METAS == "ALCANÇOU",
    na.rm = TRUE
  )

  n_nao_alcancou <- sum(
    indicador$METAS == "NÃO ALCANÇOU",
    na.rm = TRUE
  )

  n_na <- sum(is.na(indicador$METAS))

  res_valido <- suppressWarnings(
    as.numeric(indicador$RES)
  )

  res_valido <- res_valido[
    !is.na(res_valido)
  ]

  tibble::tibble(
    Indicador = nome_indicador,
    Total_Municipios = total_avaliados,
    Qtd_Alcancou = n_alcancou,
    Pct_Alcancou = 100 * n_alcancou / total_avaliados,
    Qtd_Nao_Alcancou = n_nao_alcancou,
    Pct_Nao_Alcancou = 100 * n_nao_alcancou / total_avaliados,
    Qtd_Sem_Dado = n_na,

    RES_Minimo = if (length(res_valido) > 0) {
      min(res_valido)
    } else {
      NA_real_
    },

    RES_Maximo = if (length(res_valido) > 0) {
      max(res_valido)
    } else {
      NA_real_
    },

    RES_Media = if (length(res_valido) > 0) {
      mean(res_valido)
    } else {
      NA_real_
    },

    RES_Mediana = if (length(res_valido) > 0) {
      median(res_valido)
    } else {
      NA_real_
    },

    RES_Desvio_Padrao = if (length(res_valido) > 1) {
      stats::sd(res_valido)
    } else {
      NA_real_
    }
  )
}


remover_municipio <- function(
    dados,
    codigo,
    nome_base = NULL
) {

  col_cod <- names(dados)[
    stringr::str_detect(
      toupper(names(dados)),
      "COD_MUN|CD_MUN|^COD$|^COD_IBGE$|MUNICÍPIO|IBGE"
    )
  ]

  if (length(col_cod) == 0) {

    message(
      sprintf(
        "%s: nenhuma coluna de código encontrada.",
        if (is.null(nome_base)) "Base" else nome_base
      )
    )

    return(dados)
  }

  col_cod <- col_cod[1]

  cod_num <- suppressWarnings(
    as.numeric(dados[[col_cod]])
  )

  dados |>
    dplyr::filter(
      is.na(cod_num) |
        cod_num != codigo
    )
}


padronizar_tipos_mun <- function(dados) {

  dados |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches(
          "COD_MUN|cod_mun|NOME_MUN|Munic[ií]pio",
          ignore.case = TRUE
        ),
        as.character
      )
    )
}


preparar_meta <- function(
    dados,
    nome
) {

  nomes <- names(dados)

  col_uf <- grep(
    "^UF$|SG_UF|SIGLA_UF",
    nomes,
    ignore.case = TRUE,
    value = TRUE
  )[1]

  col_cod <- grep(
    "COD_MUN|CD_MUN|COD_IBGE|IBGE",
    nomes,
    ignore.case = TRUE,
    value = TRUE
  )[1]

  col_nome <- grep(
    "NOME_MUN|NM_MUN",
    nomes,
    ignore.case = TRUE,
    value = TRUE
  )[1]

  if (is.na(col_cod)) {

    col_mun <- grep(
      "^Município$|^Municipio$",
      nomes,
      ignore.case = TRUE,
      value = TRUE
    )[1]

    if (!is.na(col_mun)) {

      x <- as.character(dados[[col_mun]])

      x_valido <- x[
        !is.na(x) &
          x != ""
      ]

      if (
        length(x_valido) > 0 &&
        mean(grepl("^[0-9]+$", x_valido)) > 0.8
      ) {
        col_cod <- col_mun
      } else {
        col_nome <- col_mun
      }
    }
  }

  if (is.na(col_cod)) {
    stop(
      paste(
        "Não encontrei a coluna de código do município em",
        nome
      )
    )
  }

  numero <- as.integer(
    stringr::str_extract(nome, "\\d+")
  )

  resultado <- tibble::tibble(
    UF = if (!is.na(col_uf)) {
      as.character(dados[[col_uf]])
    } else {
      NA_character_
    },

    COD_MUN = as.character(
      dados[[col_cod]]
    ),

    NOME_MUN = if (!is.na(col_nome)) {
      as.character(dados[[col_nome]])
    } else {
      NA_character_
    },

    META = dplyr::case_when(
      dados$METAS == "ALCANÇOU" ~ "SIM",
      dados$METAS == "NÃO ALCANÇOU" ~ "NÃO",
      TRUE ~ NA_character_
    )
  ) |>
    dplyr::filter(
      !is.na(COD_MUN),
      COD_MUN != ""
    ) |>
    dplyr::distinct(
      COD_MUN,
      .keep_all = TRUE
    )

  names(resultado)[4] <- paste0(
    "META_IND_",
    numero
  )

  resultado
}
