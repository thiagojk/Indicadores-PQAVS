library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(magrittr)
library(stringr)


pacman::p_load(sf, leaflet, geobr)
# =========================================================
# 1. Funções
# =========================================================

ler_indicador <- function(arquivo) {
  read_excel(arquivo) %>%
    select(where(~ sum(!is.na(.)) > 1))
}


converter_numero <- function(x) {
  
  if (is.numeric(x)) {
    return(x)
  }
  
  x_chr <- as.character(x) %>%
    str_trim()
  
  tem_virgula <- str_detect(x_chr, ",")
  tem_ponto <- str_detect(x_chr, "\\.")
  
  x_limpo <- case_when(
    tem_virgula & tem_ponto ~
      str_replace(str_remove_all(x_chr, "\\."), ",", "."),
    
    tem_virgula & !tem_ponto ~
      str_replace(x_chr, ",", "."),
    
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
    str_detect(nomes, "^RES($|_|[0-9])")
  ]
  
  col_num <- names(indicador)[
    str_detect(nomes, "^NUM($|_|[0-9])")
  ]
  
  col_den <- names(indicador)[
    str_detect(nomes, "^DEN($|_|[0-9])")
  ]
  
  
  if (length(col_res) >= 1) {
    
    col_res <- col_res[1]
    
    indicador <- indicador %>%
      mutate(
        RES = converter_numero(.data[[col_res]]),
        RES = if (multiplicar_res) RES * 100 else RES,
        RES = round(RES, 5)
      )
    
    
  } else if (length(col_num) >= 1 && length(col_den) >= 1) {
    
    col_num <- col_num[1]
    col_den <- col_den[1]
    
    indicador <- indicador %>%
      mutate(
        !!col_num := replace_na(
          converter_numero(.data[[col_num]]),
          0
        ),
        
        !!col_den := replace_na(
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
        "Não foi possível identificar colunas de NUM/DEN",
        "ou RES neste indicador. Ajuste manualmente."
      )
    )
  }
  
  
  indicador %>%
    mutate(
      METAS = case_when(
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
  
  colunas_necessarias <- c(
    "RES_2024",
    "RES_2025"
  )
  
  colunas_ausentes <- setdiff(
    colunas_necessarias,
    names(indicador)
  )
  
  if (length(colunas_ausentes) > 0) {
    stop(
      paste(
        "Colunas não encontradas:",
        paste(
          colunas_ausentes,
          collapse = ", "
        )
      )
    )
  }
  
  indicador %>%
    mutate(
      
      # Converte para numérico
      RES_2024 = converter_numero(RES_2024),
      RES_2025 = converter_numero(RES_2025),
      
      
      # Multiplica por 100 ANTES da classificação
      RES_2024 = if (
        multiplicar_res
      ) RES_2024 * 100 else RES_2024,
      
      RES_2025 = if (
        multiplicar_res
      ) RES_2025 * 100 else RES_2025,
      
      
      # Diferença entre 2025 e 2024
      RES = RES_2025 - RES_2024,
      
      RES = round(
        RES,
        1
      ),
      
      
      # Classificação
      METAS = case_when(
        
        # Dados ausentes
        is.na(RES_2025) |
          is.na(RES_2024) |
          is.na(RES) ~ "NÃO ALCANÇOU",
        
        
        # Era zero e permaneceu zero
        RES_2025 == 0 &
          RES_2024 == 0 &
          RES == 0 ~ "ALCANÇOU",
        
        
        # Permaneceu igual, mas diferente de zero
        RES_2025 != 0 &
          RES_2024 != 0 &
          RES == 0 ~ "NÃO ALCANÇOU",
        
        
        # Redução menor que 1 ponto percentual
        # Exemplo: -0.5, -0.7, -0.99
        RES < 0 &
          RES > -1 ~ "NÃO ALCANÇOU",
        
        
        # Redução de pelo menos 1 ponto percentual
        RES <= -1 ~ "ALCANÇOU",
        
        
        # Aumento
        RES > 0 ~ "NÃO ALCANÇOU",
        
        
        TRUE ~ "NÃO ALCANÇOU"
      )
    )
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
  
  indicador %>%
    count(METAS, name = "Quantidade") %>%
    rename(Resultado_Meta = METAS) %>%
    arrange(desc(Resultado_Meta))
}


descrever_indicador <- function(
    indicador,
    nome_indicador
) {
  
  if (!"METAS" %in% names(indicador)) {
    stop(
      paste(
        "A coluna 'METAS' não foi encontrada.",
        "Execute calcular_indicador() primeiro."
      )
    )
  }
  
  total_avaliados <- nrow(indicador)
  
  n_alcancou <- sum(
    indicador$METAS == "ALCANÇOU",
    na.rm = TRUE
  )
  
  n_nao_alcancou <- sum(
    indicador$METAS == "NÃO ALCANÇOU",
    na.rm = TRUE
  )
  
  n_na <- sum(
    is.na(indicador$METAS)
  )
  
  res_valido <- suppressWarnings(
    as.numeric(indicador$RES)
  )
  
  res_valido <- res_valido[
    !is.na(res_valido)
  ]
  
  
  tibble(
    Indicador = nome_indicador,
    
    Total_Municipios = total_avaliados,
    
    Qtd_Alcancou = n_alcancou,
    
    Pct_Alcancou =
      100 * n_alcancou / total_avaliados,
    
    Qtd_Nao_Alcancou = n_nao_alcancou,
    
    Pct_Nao_Alcancou =
      100 * n_nao_alcancou / total_avaliados,
    
    Qtd_Sem_Dado = n_na,
    
    RES_Minimo =
      if (length(res_valido) > 0) {
        min(res_valido)
      } else {
        NA
      },
    
    RES_Maximo =
      if (length(res_valido) > 0) {
        max(res_valido)
      } else {
        NA
      },
    
    RES_Media =
      if (length(res_valido) > 0) {
        mean(res_valido)
      } else {
        NA
      },
    
    RES_Mediana =
      if (length(res_valido) > 0) {
        median(res_valido)
      } else {
        NA
      },
    
    RES_Desvio_Padrao =
      if (length(res_valido) > 1) {
        sd(res_valido)
      } else {
        NA
      }
  )
}


# =========================================================
# 2. Leitura dos dados brutos
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

Indicadores_Brutos <- lapply(
  arquivos,
  ler_indicador
)


# =========================================================
# 3. Exclusão de município
# =========================================================

municipio_excluido <- 260545


remover_municipio <- function(
    dados,
    codigo,
    nome_base = NULL
) {
  
  col_cod <- names(dados)[
    str_detect(
      toupper(names(dados)),
      "COD_MUN|CD_MUN|^COD$|^COD_IBGE$|Município|IBGE"
    )
  ]
  
  
  if (length(col_cod) == 0) {
    
    message(
      sprintf(
        "%s: nenhuma coluna de código de município encontrada.",
        if (is.null(nome_base)) "Base" else nome_base
      )
    )
    
    return(dados)
  }
  
  
  col_cod <- col_cod[1]
  
  cod_num <- suppressWarnings(
    as.numeric(dados[[col_cod]])
  )
  
  
  dados %>%
    filter(
      is.na(cod_num) |
        cod_num != codigo
    )
}


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


# =========================================================
# 4. Cálculo dos indicadores
# =========================================================

Indicadores <- list()


Indicadores$IND_01 <- calcular_indicador(
  Indicadores_Brutos$IND_01,
  meta = 89.47555,
  multiplicar_res = FALSE,
  coluna_mun = "NOME_MUN"
)


Indicadores$IND_02 <- calcular_indicador(
  Indicadores_Brutos$IND_02,
  meta = 89.47555,
  multiplicar_res = FALSE,
  coluna_mun = "NOME_MUN"
)


Indicadores$IND_03 <- calcular_indicador(
  Indicadores_Brutos$IND_03,
  meta = 79.47555,
  multiplicar_res = TRUE,
  coluna_mun = "cod_mun"
) %>%
  mutate(
    METAS = replace_na(
      METAS,
      "NÃO ALCANÇOU"
    )
  )


Indicadores$IND_04 <- calcular_indicador(
  Indicadores_Brutos$IND_04,
  meta = 94.47555,
  multiplicar_res = TRUE,
  coluna_mun = "Município"
)


Indicadores$IND_05 <- calcular_indicador(
  Indicadores_Brutos$IND_05,
  meta = 74.47555,
  multiplicar_res = TRUE,
  coluna_mun = "NOME_MUN"
)


Indicadores$IND_06 <- calcular_indicador(
  Indicadores_Brutos$IND_06,
  meta = 79.47555,
  multiplicar_res = FALSE,
  coluna_mun = "Município"
)


Indicadores$IND_07 <- calcular_indicador(
  Indicadores_Brutos$IND_07,
  meta = 69.47555,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)


Indicadores$IND_08 <- calcular_indicador(
  Indicadores_Brutos$IND_08,
  meta = 74.47555,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)


Indicadores$IND_09 <- calcular_indicador(
  Indicadores_Brutos$IND_09,
  meta = 81.47555,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)


Indicadores$IND_10 <- calcular_indicador(
  Indicadores_Brutos$IND_10,
  meta = 69.47555,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)


Indicadores$IND_11 <- calcular_indicador_11_12(
  Indicadores_Brutos$IND_11,
  coluna_mun = "COD_MUN",
  multiplicar_res = FALSE
)


Indicadores$IND_13 <- calcular_indicador(
  Indicadores_Brutos$IND_13,
  meta = 94.47555,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)


Indicadores$IND_14 <- calcular_indicador(
  Indicadores_Brutos$IND_14,
  meta = 94.47555,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
) |> 
  tidyr::drop_na("UF")


# =========================================================
# 5. Indicador 12 - mesma regra do indicador 11
# =========================================================

Indicadores$IND_12 <- calcular_indicador_11_12(
  Indicadores_Brutos$IND_12,
  coluna_mun = "COD_MUN",
  multiplicar_res = TRUE
) |> 
  tidyr::drop_na(COD_MUN)


# =========================================================
# 6. Cálculo das metas
# =========================================================

Metas <- lapply(
  Indicadores,
  calcular_metas
)


# =========================================================
# 7. Estatísticas descritivas
# =========================================================

Descritivas <- bind_rows(
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
print(Descritivas)


# =========================================================
# 8. Exportação para Excel
# =========================================================

wb <- createWorkbook()


adicionar_aba <- function(
    wb,
    nome,
    dados
) {
  
  if (nome %in% names(wb)) {
    removeWorksheet(
      wb,
      nome
    )
  }
  
  addWorksheet(
    wb,
    nome
  )
  
  writeData(
    wb,
    nome,
    dados
  )
}


for (nome in names(Indicadores)) {
  
  adicionar_aba(
    wb,
    nome,
    Indicadores[[nome]]
  )
}


# =========================================================
# 9. Todos os indicadores em uma única aba
# =========================================================

padronizar_tipos_mun <- function(dados) {
  
  dados %>%
    mutate(
      across(
        matches(
          "COD_MUN|cod_mun|NOME_MUN|Munic[ií]pio",
          ignore.case = TRUE
        ),
        as.character
      )
    )
}


Indicadores_Completo <- Indicadores %>%
  lapply(
    padronizar_tipos_mun
  ) %>%
  bind_rows(
    .id = "Indicador"
  ) %>%
  relocate(
    Indicador,
    .before = 1
  )


adicionar_aba(
  wb,
  "Todos_Indicadores",
  Indicadores_Completo
)


saveWorkbook(
  wb,
  "Resultado_Indicadores_Metas.xlsx",
  overwrite = TRUE
)





