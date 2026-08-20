# =========================================================
# BASE PFVS
# =========================================================

# ---------------------------------------------------------
# 1. SMS - Municípios
# ---------------------------------------------------------

SMS <- readxl::read_excel(
  "Dados/PFVS/PFVS.xlsx",
  sheet = "SMS"
) |>
  dplyr::select(
    UF,
    COD_MUN,
    NOME_MUN,
    PFVS_ANUAL,
    PFVS_MENSAL
  ) |>
  dplyr::mutate(
    PQAVS_Incentivo = PFVS_ANUAL * 0.20
  )


# ---------------------------------------------------------
# 2. SES - Estados
# ---------------------------------------------------------

SES <- readxl::read_excel(
  "Dados/PFVS/PFVS.xlsx",
  sheet = "SES"
) |>
  dplyr::select(
    UF,
    PFVS_ANUAL,
    PFVS_MENSAL
  ) |>
  dplyr::mutate(
    PQAVS_Incentivo = PFVS_ANUAL * 0.20
  )
