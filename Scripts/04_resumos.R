# =========================================================
# RESUMOS E ESTATÍSTICAS
# =========================================================

# ---------------------------------------------------------
# 1. Resumo das metas de cada indicador
# ---------------------------------------------------------

Metas <- lapply(
  Indicadores,
  calcular_metas
)


# ---------------------------------------------------------
# 2. Estatísticas descritivas dos indicadores
# ---------------------------------------------------------

Descritivas <- dplyr::bind_rows(
  lapply(
    names(Indicadores),
    function(nome) {
      descrever_indicador(
        Indicadores[[nome]],
        nome
      )
    }
  )
)


# ---------------------------------------------------------
# 3. Resumo por Estado
# ---------------------------------------------------------

Estados <- Dados_Completos |>
  dplyr::group_by(UF) |>
  dplyr::summarise(

    # Total de municípios aderidos
    `Nº Mun Aderidos` = dplyr::n(),

    # Municípios que alcançaram cada indicador
    dplyr::across(
      dplyr::starts_with("META_IND_"),
      ~ sum(
        .x == "SIM",
        na.rm = TRUE
      )
    ),

    # 90% ou mais
    Mun_90_N = sum(
      PERCENTUAL_METAS >= 90,
      na.rm = TRUE
    ),

    `Mun_90_%` = round(
      Mun_90_N /
        `Nº Mun Aderidos` *
        100,
      2
    ),

    # 70% a 89%
    Mun_70_N = sum(
      PERCENTUAL_METAS >= 70 &
        PERCENTUAL_METAS < 90,
      na.rm = TRUE
    ),

    `Mun_70_%` = round(
      (
        Mun_90_N +
          Mun_70_N
      ) /
        `Nº Mun Aderidos` *
        100,
      2
    ),

    # 50% a 69%
    Mun_50_N = sum(
      PERCENTUAL_METAS >= 50 &
        PERCENTUAL_METAS < 70,
      na.rm = TRUE
    ),

    `Mun_50_%` = round(
      (
        Mun_90_N +
          Mun_70_N +
          Mun_50_N
      ) /
        `Nº Mun Aderidos` *
        100,
      2
    ),

    # 30% a 49%
    Mun_30_N = sum(
      PERCENTUAL_METAS >= 30 &
        PERCENTUAL_METAS < 50,
      na.rm = TRUE
    ),

    `Mun_30_%` = round(
      (
        Mun_90_N +
          Mun_70_N +
          Mun_50_N +
          Mun_30_N
      ) /
        `Nº Mun Aderidos` *
        100,
      2
    ),

    # Menos de 30%
    Mun_menor30_N = sum(
      PERCENTUAL_METAS < 30,
      na.rm = TRUE
    ),

    `Mun_menor30_%` = round(
      Mun_menor30_N /
        `Nº Mun Aderidos` *
        100,
      2
    ),

    .groups = "drop"
  )


print(Descritivas)




# ---------------------------------------------------------
# Adicionar incentivo PQAVS dos Estados
# ---------------------------------------------------------

Estados <- Estados |>
  mutate(
    UF = as.character(UF)
  ) |>
  left_join(
    SES |>
      mutate(
        UF = as.character(UF)
      ) |>
      select(
        UF,
        PQAVS_Incentivo
      ),
    by = "UF"
  )




# Regra PFVS --------------------------------------------------------------


Estados <- Estados %>%
  mutate(
    
    # Arredondamento dos percentuais para aplicação da regra
    PERC_MUN_90 = floor(`Mun_90_%` + 0.5),
    PERC_MUN_70 = floor(`Mun_70_%` + 0.5),
    PERC_MUN_50 = floor(`Mun_50_%` + 0.5),
    PERC_MUN_30 = floor(`Mun_30_%` + 0.5),
    
    # Definição do estrato de repasse
    PERCENTUAL_REPASSE = case_when(
      PERC_MUN_90 >= 80 ~ 100,
      PERC_MUN_70 >= 90 ~ 75,
      PERC_MUN_50 >= 90 ~ 50,
      PERC_MUN_30 >= 90 ~ 25,
      TRUE ~ 0
    ),
    
    # Valor final a ser repassado
    Valor_a_Repassar = PQAVS_Incentivo * (PERCENTUAL_REPASSE / 100)
  )