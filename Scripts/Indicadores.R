library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)

# ── 1. Dados brutos (apenas leitura) ─────────────────────────────────────────
Indicadores_Brutos <- list()

Indicadores_Brutos$IND_14 <- read_excel("Dados/IND_14_PQAVS_2025 jan-dez Final Extracao 12-06-2026.xlsx")
Indicadores_Brutos$IND_12 <- read_excel("Dados/IND_12_PQA-VS 2025_Avaliação_Final.xlsx")

# ── 2. Dados transformados ────────────────────────────────────────────────────
Indicadores <- list()

# Indicador 14
Indicadores$IND_14 <- Indicadores_Brutos$IND_14 %>%
  mutate(
    across(c(NUM_2025, DEN_2025, RES_2025), ~replace_na(., 0)),
    RES_2_2025 = ifelse(DEN_2025 == 0, 0, (NUM_2025 / DEN_2025) * 100),
    Resultado = ifelse(round(RES_2025, 1) == round(RES_2_2025, 1), "OK", "DIFERENTE"),
    METAS = case_when(
      Resultado == "OK" & RES_2025 >= 94.47 ~ "ALCANÇOU",
      Resultado == "OK" & RES_2025 < 94.47 ~ "NÃO ALCANÇOU",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(NOME_MUN)


# Indicador 12
Indicadores$IND_12 <- Indicadores_Brutos$IND_12 %>%
  mutate(
    # Trata os NAs, garante tipo numérico e arredonda para 2 casas decimais
    across(
      c(RES_2024, NUM_2025, DEN_2025, RES_2025), 
      ~ round(replace_na(as.numeric(.), 0), 2)
    ),
    
    # Diferença sem arredondamento direto (usará os valores que já foram arredondados acima)
    DIFF = RES_2025 - RES_2024,
    
    METAS = case_when(
      DEN_2025 == 0                               ~ "ALCANÇOU A META",
      DIFF > 0                                    ~ "NÃO ALCANÇOU A META",
      DIFF == 0 & RES_2025 > 0                    ~ "NÃO ALCANÇOU A META",
      TRUE                                        ~ "ALCANÇOU A META"
    )
  ) %>%
  drop_na(NOME_MUN) %>%
  mutate(
    DIVERGENCIA = if_else(
      (METAS == "ALCANÇOU A META" & `ALCANCOU A META?` == "Não") |
        (METAS == "NÃO ALCANÇOU A META" & `ALCANCOU A META?` == "Sim"),
      "Sim",
      "Não"
    )
  )

# Para conferir o resultado das divergências após o arredondamento de 2 casas:
table(Indicadores$IND_12$DIVERGENCIA)

View(Indicadores$IND_12)
divergencias <- Indicadores$IND_12 %>%
  filter(
    (METAS == "ALCANÇOU A META" & `ALCANCOU A META?` == "Não") |
      (METAS == "NÃO ALCANÇOU A META" & `ALCANCOU A META?` == "Sim")
  )

nrow(divergencias)
