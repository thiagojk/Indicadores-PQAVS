# =========================================================
# CONSOLIDAÇÃO DAS BASES
# =========================================================

# ---------------------------------------------------------
# 1. Base completa em formato longo
# ---------------------------------------------------------

Indicadores_Completo <- Indicadores |>
  lapply(padronizar_tipos_mun) |>
  dplyr::bind_rows(
    .id = "Indicador"
  ) |>
  dplyr::relocate(
    Indicador,
    .before = 1
  )


# ---------------------------------------------------------
# 2. Preparar metas dos 14 indicadores
# ---------------------------------------------------------

Metas_Indicadores <- lapply(
  esperados,
  function(x) {
    preparar_meta(
      Indicadores[[x]],
      x
    )
  }
)

names(Metas_Indicadores) <- esperados


# ---------------------------------------------------------
# 3. Cadastro único de municípios
# ---------------------------------------------------------

Municipios <- dplyr::bind_rows(
  Metas_Indicadores
) |>
  dplyr::select(
    UF,
    COD_MUN,
    NOME_MUN
  ) |>
  dplyr::group_by(
    COD_MUN
  ) |>
  dplyr::summarise(
    UF = dplyr::first(
      stats::na.omit(UF)
    ),
    NOME_MUN = dplyr::first(
      stats::na.omit(NOME_MUN)
    ),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 4. Consolidar META_IND_1 até META_IND_14
# ---------------------------------------------------------

Todos_Indicadores <- Reduce(
  function(x, y) {
    dplyr::full_join(
      x,
      y,
      by = "COD_MUN"
    )
  },
  lapply(
    Metas_Indicadores,
    function(x) {
      dplyr::select(
        x,
        COD_MUN,
        dplyr::starts_with("META_IND_")
      )
    }
  )
) |>
  dplyr::left_join(
    Municipios,
    by = "COD_MUN"
  ) |>
  dplyr::select(
    UF,
    COD_MUN,
    NOME_MUN,
    dplyr::all_of(
      paste0(
        "META_IND_",
        1:14
      )
    )
  ) |>
  dplyr::arrange(
    UF,
    NOME_MUN
  )


# ---------------------------------------------------------
# 5. População
# ---------------------------------------------------------

Populacao <- readxl::read_excel(
  arquivo_populacao
) |>
  dplyr::transmute(
    COD_MUN = as.character(
      as.numeric(COD_MUN)
    ),
    POP = as.numeric(POP)
  ) |>
  dplyr::distinct(
    COD_MUN,
    .keep_all = TRUE
  )


# ---------------------------------------------------------
# 6. Base final dos municípios
# ---------------------------------------------------------

Dados_Completos <- Todos_Indicadores |>
  dplyr::mutate(
    COD_MUN = as.character(
      as.numeric(COD_MUN)
    )
  ) |>
  dplyr::left_join(
    Populacao,
    by = "COD_MUN"
  ) |>
  dplyr::relocate(
    POP,
    .after = COD_MUN
  ) |>
  dplyr::mutate(

    # Porte populacional
    PORTE = dplyr::case_when(
      POP <= 10000 ~ 1L,
      POP <= 30000 ~ 2L,
      POP <= 50000 ~ 3L,
      POP <= 100000 ~ 4L,
      POP > 100000 ~ 5L,
      TRUE ~ NA_integer_
    ),

    # Número de metas alcançadas
    METAS_ALCANCADAS = rowSums(
      dplyr::across(
        dplyr::starts_with("META_IND_"),
        ~ .x == "SIM"
      ),
      na.rm = TRUE
    ),

    # Percentual conforme porte
    PERCENTUAL_METAS = dplyr::case_when(

      # Porte 1
      PORTE == 1 & METAS_ALCANCADAS == 0 ~ 0,
      PORTE == 1 & METAS_ALCANCADAS == 1 ~ 10,
      PORTE == 1 & METAS_ALCANCADAS == 2 ~ 30,
      PORTE == 1 & METAS_ALCANCADAS == 3 ~ 50,
      PORTE == 1 & METAS_ALCANCADAS == 4 ~ 70,
      PORTE == 1 & METAS_ALCANCADAS == 5 ~ 90,
      PORTE == 1 & METAS_ALCANCADAS >= 6 ~ 100,

      # Porte 2
      PORTE == 2 & METAS_ALCANCADAS == 0 ~ 0,
      PORTE == 2 & METAS_ALCANCADAS == 1 ~ 10,
      PORTE == 2 & METAS_ALCANCADAS == 2 ~ 25,
      PORTE == 2 & METAS_ALCANCADAS == 3 ~ 40,
      PORTE == 2 & METAS_ALCANCADAS == 4 ~ 55,
      PORTE == 2 & METAS_ALCANCADAS == 5 ~ 75,
      PORTE == 2 & METAS_ALCANCADAS == 6 ~ 90,
      PORTE == 2 & METAS_ALCANCADAS >= 7 ~ 100,

      # Porte 3
      PORTE == 3 & METAS_ALCANCADAS == 0 ~ 0,
      PORTE == 3 & METAS_ALCANCADAS == 1 ~ 10,
      PORTE == 3 & METAS_ALCANCADAS == 2 ~ 25,
      PORTE == 3 & METAS_ALCANCADAS == 3 ~ 40,
      PORTE == 3 & METAS_ALCANCADAS == 4 ~ 50,
      PORTE == 3 & METAS_ALCANCADAS == 5 ~ 65,
      PORTE == 3 & METAS_ALCANCADAS == 6 ~ 80,
      PORTE == 3 & METAS_ALCANCADAS == 7 ~ 90,
      PORTE == 3 & METAS_ALCANCADAS >= 8 ~ 100,

      # Porte 4
      PORTE == 4 & METAS_ALCANCADAS == 0 ~ 0,
      PORTE == 4 & METAS_ALCANCADAS == 1 ~ 10,
      PORTE == 4 & METAS_ALCANCADAS == 2 ~ 20,
      PORTE == 4 & METAS_ALCANCADAS == 3 ~ 30,
      PORTE == 4 & METAS_ALCANCADAS == 4 ~ 40,
      PORTE == 4 & METAS_ALCANCADAS == 5 ~ 50,
      PORTE == 4 & METAS_ALCANCADAS == 6 ~ 60,
      PORTE == 4 & METAS_ALCANCADAS == 7 ~ 70,
      PORTE == 4 & METAS_ALCANCADAS == 8 ~ 90,
      PORTE == 4 & METAS_ALCANCADAS >= 9 ~ 100,

      # Porte 5
      PORTE == 5 & METAS_ALCANCADAS == 0 ~ 0,
      PORTE == 5 & METAS_ALCANCADAS == 1 ~ 10,
      PORTE == 5 & METAS_ALCANCADAS == 2 ~ 20,
      PORTE == 5 & METAS_ALCANCADAS == 3 ~ 30,
      PORTE == 5 & METAS_ALCANCADAS == 4 ~ 40,
      PORTE == 5 & METAS_ALCANCADAS == 5 ~ 50,
      PORTE == 5 & METAS_ALCANCADAS == 6 ~ 60,
      PORTE == 5 & METAS_ALCANCADAS == 7 ~ 70,
      PORTE == 5 & METAS_ALCANCADAS == 8 ~ 80,
      PORTE == 5 & METAS_ALCANCADAS == 9 ~ 90,
      PORTE == 5 & METAS_ALCANCADAS == 10 ~ 95,
      PORTE == 5 & METAS_ALCANCADAS >= 11 ~ 100,

      TRUE ~ NA_real_
    )
  )







# ---------------------------------------------------------
# Adicionar incentivo PQAVS
# ---------------------------------------------------------

Dados_Completos <- Dados_Completos |>
  mutate(
    COD_MUN = as.character(COD_MUN)
  ) |>
  left_join(
    SMS |>
      mutate(
        COD_MUN = as.character(COD_MUN)
      ) |>
      select(
        COD_MUN,
        PQAVS_Incentivo
      ),
    by = "COD_MUN"
  )


  Dados_Completos <- Dados_Completos %>%
  mutate(
    Valor_a_Repassar = PQAVS_Incentivo * (PERCENTUAL_METAS / 100)
  )
  




