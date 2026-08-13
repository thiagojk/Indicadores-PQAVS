library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)




# Funções -----------------------------------------------------------------


comparar_metas <- function(indicador) {
  
  coluna_meta <- grep("meta", names(indicador), value = TRUE, ignore.case = TRUE)[1]
  
  if (is.na(coluna_meta)) {
    stop("Não encontrei nenhuma coluna com 'meta' no nome.")
  }
  
  dados <- indicador %>%
    mutate(
      Meta_Bruta = dplyr::case_when(
        toupper(.data[[coluna_meta]]) == "SIM" ~ "ALCANÇOU",
        toupper(.data[[coluna_meta]]) %in% c("NÃO", "NAO") ~ "NÃO ALCANÇOU",
        TRUE ~ NA_character_
      )
    )
  
  tabela_bruta <- dados %>%
    count(Meta_Bruta, name = "Quantidade")
  
  tabela_calculada <- dados %>%
    count(METAS, name = "Quantidade")
  
  list(
    coluna_usada = coluna_meta,
    tabela_bruta = tabela_bruta,
    tabela_calculada = tabela_calculada
  )
}

# ── 1. Dados brutos (apenas leitura) ─────────────────────────────────────────
Indicadores_Brutos <- list()


Indicadores_Brutos$IND_01 <- read_excel("Dados/IND_01.xlsx")
Indicadores_Brutos$IND_02 <- read_excel("Dados/IND_02.xlsx")
Indicadores_Brutos$IND_03 <- read_excel("Dados/IND03_2025_24062026.xlsx")
Indicadores_Brutos$IND_04 <- read_excel("Dados/IND_04_PQAVS_2025_JAN_DEZ_Final.xlsx")
Indicadores_Brutos$IND_05 <- read_excel("Dados/IND_05_PQA-VS 2025_Avaliação_Final_atualizada.xlsx")
Indicadores_Brutos$IND_06 <- read_excel("Dados/IND_06_PQAVS_Jan-Dez_2025_Indicador 6_Sinan_atualizado.xlsx")

Indicadores_Brutos$IND_14 <- read_excel("Dados/IND_14_PQAVS_2025 jan-dez Final Extracao 12-06-2026.xlsx")
Indicadores_Brutos$IND_12 <- read_excel("Dados/IND_12_PQA-VS 2025_Avaliação_Final.xlsx")


# ── 2. Dados transformados ────────────────────────────────────────────────────
Indicadores <- list()






# Indicador 1 -------------------------------------------------------------

Indicadores$IND_01 <- Indicadores_Brutos$IND_01 %>%
  mutate(
    RES_2025 = round(RES_2025, 2), 
    RES_2_2025 = ifelse(DEN_2025 == 0, 0, round((NUM_2025 / DEN_2025) * 100, 2)),
    Resultado = ifelse(round(RES_2025, 1) == round(RES_2_2025, 1), "OK", "DIFERENTE"),
    METAS = case_when(
      RES_2_2025 >= 89.47 ~ "ALCANÇOU",
      RES_2_2025 < 89.47 ~ "NÃO ALCANÇOU",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(NOME_MUN)




# Indicador 2 -------------------------------------------------------------


Indicadores$IND_02 <- Indicadores_Brutos$IND_02 %>%
  mutate(
    RES_2025 = round(RES_2025, 2), 
    RES_2_2025 = ifelse(DEN_2025 == 0, 0, round((NUM_2025 / DEN_2025) * 100, 2)),
    Resultado = ifelse(round(RES_2025, 1) == round(RES_2_2025, 1), "OK", "DIFERENTE"),
    METAS = case_when(
      RES_2_2025 >= 89.47 ~ "ALCANÇOU",
      RES_2_2025 < 89.47 ~ "NÃO ALCANÇOU",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(NOME_MUN)







# Indicador 03 ------------------------------------------------------------

# ==== Falta Nome do municipio, faz um merge e puxa de outra tabela

Indicadores$IND_03 <- Indicadores_Brutos$IND_03 %>%
  mutate(
    RES_2025 = ifelse(is.na(RES_2025), 0, RES_2025),
    RES_2025 = round(RES_2025 * 100, 2), 
    RES_2_2025 = ifelse(DEN_2025 == 0, 0, round((NUM_2025 / DEN_2025) * 100, 2)),
    Resultado = ifelse(round(RES_2025, 1) == round(RES_2_2025, 1), "OK", "DIFERENTE"),
    METAS = case_when(
      Resultado == "OK" & RES_2_2025 >= 79.47 ~ "ALCANÇOU",
      Resultado == "OK" & RES_2_2025 < 79.47 ~ "NÃO ALCANÇOU",
      TRUE ~ NA_character_
    ),
    `Alcançou a meta?` = ifelse(
      is.na(`Alcançou a meta?`),
      "Não",
      `Alcançou a meta?`
    )
  )



# Indicador 04 ------------------------------------------------------------

Indicadores$IND_04 <- Indicadores_Brutos$IND_04 %>%
  mutate(
    across(c(NUM_2025, DEN_2025, RES_2025), ~replace_na(., 0)),
    RES_2025 = round(RES_2025 * 100, 2), 
    RES_2_2025 = ifelse(DEN_2025 == 0, 0, round((NUM_2025 / DEN_2025) * 100, 2)),
    Resultado = ifelse(round(RES_2025, 1) == round(RES_2_2025, 1), "OK", "DIFERENTE"),
    METAS = case_when(
      Resultado == "OK" & RES_2025 >= 94.47 ~ "ALCANÇOU",
      Resultado == "OK" & RES_2025 < 94.47 ~ "NÃO ALCANÇOU",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(Município)






# Indicador 5 -------------------------------------------------------------


Indicadores$IND_05 <- Indicadores_Brutos$IND_05 %>%
  mutate(
    across(c(NUM_2025, DEN_2025, RES_2025), ~replace_na(., 0)),
    RES_2025 = round(RES_2025 * 100, 2), 
    RES_2_2025 = ifelse(DEN_2025 == 0, 0, round((NUM_2025 / DEN_2025) * 100, 2)),
    Resultado = ifelse(round(RES_2025, 1) == round(RES_2_2025, 1), "OK", "DIFERENTE"),
    METAS = case_when(
      Resultado == "OK" & RES_2025 >= 74.47 ~ "ALCANÇOU",
      Resultado == "OK" & RES_2025 < 74.47 ~ "NÃO ALCANÇOU",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(NOME_MUN)






# Indicador 06 ------------------------------------------------------------

Indicadores$IND_06 <- Indicadores_Brutos$IND_06 %>%
  mutate(
    across(c(NUM_2025, DEN_2025, RES_2025), ~replace_na(., 0)),
    RES_2025 = round(RES_2025, 2), 
    RES_2_2025 = ifelse(DEN_2025 == 0, 0, round((NUM_2025 / DEN_2025) * 100, 2)),
    Resultado = ifelse(round(RES_2025, 1) == round(RES_2_2025, 1), "OK", "DIFERENTE"),
    METAS = case_when(
      Resultado == "OK" & RES_2025 >= 70.47 ~ "ALCANÇOU",
      Resultado == "OK" & RES_2025 < 70.47 ~ "NÃO ALCANÇOU",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(Município)






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

