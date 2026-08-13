library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(magrittr)

# =========================================================
# 1. Funções
# =========================================================

ler_indicador <- function(arquivo) {
  
  read_excel(arquivo) %>%
    select(where(~ sum(!is.na(.)) > 1))
}


# =========================================================
# Cálculo dos indicadores
# Não utiliza METAS das bases
# =========================================================

calcular_indicador <- function(dados, meta, multiplicar_res = FALSE, coluna_mun = NULL) {
  
  dados <- dados %>%
    mutate(
      across(
        any_of(c("NUM_2025", "DEN_2025", "RES_2025")),
        ~ replace_na(as.numeric(.), 0)
      )
    )
  
  
  # Caso exista numerador e denominador
  if(all(c("NUM_2025", "DEN_2025") %in% names(dados))) {
    
    dados <- dados %>%
      mutate(
        RES_CALCULADO = case_when(
          
          DEN_2025 == 0 ~ 0,
          
          TRUE ~ (NUM_2025 / DEN_2025) * 100
        )
      )
    
  } else if("RES_2025" %in% names(dados)) {
    
    dados <- dados %>%
      mutate(
        RES_CALCULADO = RES_2025
      )
    
  } else {
    
    stop("Não foram encontradas colunas suficientes para calcular o indicador.")
  }
  
  
  if(multiplicar_res == TRUE){
    
    dados <- dados %>%
      mutate(
        RES_CALCULADO = RES_CALCULADO * 100
      )
  }
  
  
  dados %>%
    mutate(
      METAS = case_when(
        RES_CALCULADO >= meta ~ "ALCANÇOU",
        TRUE ~ "NÃO ALCANÇOU"
      )
    )
}



# =========================================================
# Resumo das metas calculadas
# =========================================================

calcular_metas <- function(indicador) {
  
  indicador %>%
    count(METAS, name = "Quantidade") %>%
    rename(Resultado_Meta = METAS) %>%
    arrange(desc(Resultado_Meta))
}



# =========================================================
# 2. Arquivos
# =========================================================

arquivos <- list(
  
  IND_01 = "Dados/IND_01.xlsx",
  IND_02 = "Dados/IND_02.xlsx",
  IND_03 = "Dados/IND_03.xlsx",
  IND_04 = "Dados/IND_04_PQAVS_2025_JAN_DEZ_Final.xlsx",
  IND_05 = "Dados/IND_05_PQA-VS 2025_Avaliação_Final_atualizada.xlsx",
  IND_06 = "Dados/IND_06_PQAVS_Jan-Dez_2025_Indicador 6_Sinan_atualizado.xlsx",
  IND_07 = "Dados/IND_07_PQA-VS_2025_Avaliacao_Final_Malaria.xlsx",
  IND_08 = "Dados/IND_08_PQAVS_2025_COMPLETO 08_07_2026.xlsx",
  IND_09 = "Dados/IND_09_PQA-VS 2025_Avaliação_Final_verificado.xlsx",
  IND_10 = "Dados/IND_10_PQA-VS 2025_Avaliação_Final.xlsx",
  IND_11 = "Dados/IND_11_PQA-VS 2025_Avaliação_Final_23.06.2026_Corrigido_26.06.2026.xlsx",
  IND_12 = "Dados/IND_12_PQA-VS 2025_Avaliação_Final_atualizada.xlsx",
  IND_13 = "Dados/IND_13_PQA-VS 2025_Avaliação_Final (corrigido).xlsx",
  IND_14 = "Dados/IND14_PQAVS_2025 jan-dez Final Extracao 12-06-2026.xlsx"
)


Indicadores_Brutos <- lapply(arquivos, ler_indicador)



# =========================================================
# 3. Cálculo dos indicadores
# =========================================================

Indicadores <- list()


Indicadores$IND_01 <- calcular_indicador(
  Indicadores_Brutos$IND_01,
  meta = 89.47
)


Indicadores$IND_02 <- calcular_indicador(
  Indicadores_Brutos$IND_02,
  meta = 89.47
)


Indicadores$IND_03 <- calcular_indicador(
  Indicadores_Brutos$IND_03,
  meta = 79.47,
  multiplicar_res = TRUE
)


Indicadores$IND_04 <- calcular_indicador(
  Indicadores_Brutos$IND_04,
  meta = 94.47,
  multiplicar_res = TRUE
)


Indicadores$IND_05 <- calcular_indicador(
  Indicadores_Brutos$IND_05,
  meta = 74.47,
  multiplicar_res = TRUE
)


Indicadores$IND_06 <- calcular_indicador(
  Indicadores_Brutos$IND_06,
  meta = 79.47
)


Indicadores$IND_07 <- calcular_indicador(
  Indicadores_Brutos$IND_07,
  meta = 69.47
)


Indicadores$IND_08 <- calcular_indicador(
  Indicadores_Brutos$IND_08,
  meta = 74.47
)


Indicadores$IND_09 <- calcular_indicador(
  Indicadores_Brutos$IND_09,
  meta = 81.47
)


Indicadores$IND_10 <- calcular_indicador(
  Indicadores_Brutos$IND_10,
  meta = 69.47
)


Indicadores$IND_11 <- calcular_indicador(
  Indicadores_Brutos$IND_11,
  meta = 89.47
)


Indicadores$IND_13 <- calcular_indicador(
  Indicadores_Brutos$IND_13,
  meta = 89.47
)


Indicadores$IND_14 <- calcular_indicador(
  Indicadores_Brutos$IND_14,
  meta = 94.47
)



# =========================================================
# 4. Indicador 12
# =========================================================

Indicadores$IND_12 <- Indicadores_Brutos$IND_12 %>%
  
  mutate(
    
    across(
      c(RES_2024, NUM_2025, DEN_2025, RES_2025),
      ~ round(replace_na(as.numeric(.),0),2)
    ),
    
    DIFF = RES_2025 - RES_2024,
    
    METAS = case_when(
      
      DEN_2025 == 0 ~ "ALCANÇOU",
      
      DIFF > 0 ~ "NÃO ALCANÇOU",
      
      DIFF == 0 & RES_2025 > 0 ~ "NÃO ALCANÇOU",
      
      TRUE ~ "ALCANÇOU"
    )
  )



# =========================================================
# 5. Resultado final
# =========================================================

calcular_metas(Indicadores$IND_01)

calcular_metas(Indicadores$IND_02)

calcular_metas(Indicadores$IND_03)

calcular_metas(Indicadores$IND_05)

calcular_metas(Indicadores$IND_08)

