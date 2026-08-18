# Pacotes -----------------------------------------------------------------



library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(stringr)
library(tidyverse)
# =========================================================
# 1. Funções auxiliares
# =========================================================

ler_indicador <- function(arquivo) {
  read_excel(arquivo) |>
    select(where(~ sum(!is.na(.)) > 1))
}

converter_numero <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  
  x_chr <- str_trim(as.character(x))
  
  tem_virgula <- str_detect(x_chr, ",")
  tem_ponto <- str_detect(x_chr, "\\.")
  
  x_limpo <- case_when(
    tem_virgula & tem_ponto ~ str_replace(str_remove_all(x_chr, "\\."), ",", "."),
    tem_virgula & !tem_ponto ~ str_replace(x_chr, ",", "."),
    TRUE ~ x_chr
  )
  
  as.numeric(x_limpo)
}

# Regra padrão: RES vem pronto (RES_*) ou é calculado como NUM/DEN,
# depois classificado em METAS conforme a meta do indicador.
calcular_indicador <- function(indicador, meta, multiplicar_res = FALSE, coluna_mun) {
  if (!coluna_mun %in% names(indicador)) {
    stop(sprintf("A coluna de município '%s' não foi encontrada nos dados.", coluna_mun))
  }
  
  nomes <- toupper(names(indicador))
  col_res <- names(indicador)[str_detect(nomes, "^RES($|_|[0-9])")]
  col_num <- names(indicador)[str_detect(nomes, "^NUM($|_|[0-9])")]
  col_den <- names(indicador)[str_detect(nomes, "^DEN($|_|[0-9])")]
  
  if (length(col_res) >= 1) {
    col_res <- col_res[1]
    
    indicador <- indicador |>
      mutate(
        RES = converter_numero(.data[[col_res]]),
        RES = if (multiplicar_res) RES * 100 else RES,
        RES = round(RES, 1)
      )
  } else if (length(col_num) >= 1 && length(col_den) >= 1) {
    col_num <- col_num[1]
    col_den <- col_den[1]
    
    indicador <- indicador |>
      mutate(
        !!col_num := replace_na(converter_numero(.data[[col_num]]), 0),
        !!col_den := replace_na(converter_numero(.data[[col_den]]), 0),
        RES = .data[[col_num]] / .data[[col_den]],
        RES = if (multiplicar_res) RES * 100 else RES,
        RES = round(RES, 1)
      )
  } else {
    stop("Não foi possível identificar colunas de NUM/DEN ou RES neste indicador. Ajuste manualmente.")
  }
  
  indicador |>
    mutate(
      METAS = case_when(
        is.nan(RES) ~ "NÃO ALCANÇOU",
        is.na(RES) ~ "NÃO ALCANÇOU",
        RES >= meta ~ "ALCANÇOU",
        TRUE ~ "NÃO ALCANÇOU"
      )
    )
}

# Regra dos indicadores 11 e 12: RES é a variação (RES_2025 - RES_2024)
# e a meta é alcançada com queda de pelo menos 1 ponto percentual.
calcular_indicador_11_12 <- function(indicador, coluna_mun, multiplicar_res = FALSE) {
  
  if (!coluna_mun %in% names(indicador)) {
    stop(sprintf(
      "A coluna de município '%s' não foi encontrada nos dados.",
      coluna_mun
    ))
  }
  
  colunas_necessarias <- c("RES_2024", "RES_2025")
  colunas_ausentes <- setdiff(colunas_necessarias, names(indicador))
  
  if (length(colunas_ausentes) > 0) {
    stop(paste(
      "Colunas não encontradas:",
      paste(colunas_ausentes, collapse = ", ")
    ))
  }
  
  indicador |>
    mutate(
      RES_2024 = converter_numero(RES_2024),
      RES_2025 = converter_numero(RES_2025),
      
      RES_2024 = if (multiplicar_res) RES_2024 * 100 else RES_2024,
      RES_2025 = if (multiplicar_res) RES_2025 * 100 else RES_2025,
      
      # Diferença sem arredondamento para fazer a classificação correta
      RES_BRUTO = RES_2025 - RES_2024,
      
      # Entre 0 e -1 mantém duas casas decimais.
      # Nos demais casos, uma casa decimal.
      RES = if_else(
        RES_BRUTO < 0 & RES_BRUTO > -1,
        round(RES_BRUTO, 2),
        round(RES_BRUTO, 1)
      ),
      
      METAS = case_when(
        # Dados ausentes
        is.na(RES_2025) | is.na(RES_2024) | is.na(RES_BRUTO) ~ "NÃO ALCANÇOU",
        
        # Era zero e permaneceu zero
        RES_2025 == 0 & RES_2024 == 0 & RES_BRUTO == 0 ~ "ALCANÇOU",
        
        # Permaneceu igual, mas diferente de zero
        RES_2025 != 0 & RES_2024 != 0 & RES_BRUTO == 0 ~ "NÃO ALCANÇOU",
        
        # Redução menor que 1 ponto percentual
        RES_BRUTO < 0 & RES_BRUTO > -1 ~ "NÃO ALCANÇOU",
        
        # Redução de pelo menos 1 ponto percentual
        RES_BRUTO <= -1 ~ "ALCANÇOU",
        
        # Aumento
        RES_BRUTO > 0 ~ "NÃO ALCANÇOU",
        
        TRUE ~ "NÃO ALCANÇOU"
      )
    ) |>
    select(-RES_BRUTO)
}

calcular_metas <- function(indicador) {
  if (!"METAS" %in% names(indicador)) {
    stop("A coluna 'METAS' não foi encontrada. Execute calcular_indicador() primeiro.")
  }
  
  indicador |>
    count(METAS, name = "Quantidade") |>
    rename(Resultado_Meta = METAS) |>
    arrange(desc(Resultado_Meta))
}

descrever_indicador <- function(indicador, nome_indicador) {
  if (!"METAS" %in% names(indicador)) {
    stop("A coluna 'METAS' não foi encontrada. Execute calcular_indicador() primeiro.")
  }
  
  total_avaliados <- nrow(indicador)
  n_alcancou <- sum(indicador$METAS == "ALCANÇOU", na.rm = TRUE)
  n_nao_alcancou <- sum(indicador$METAS == "NÃO ALCANÇOU", na.rm = TRUE)
  n_na <- sum(is.na(indicador$METAS))
  
  res_valido <- suppressWarnings(as.numeric(indicador$RES))
  res_valido <- res_valido[!is.na(res_valido)]
  
  tibble(
    Indicador = nome_indicador,
    Total_Municipios = total_avaliados,
    Qtd_Alcancou = n_alcancou,
    Pct_Alcancou = 100 * n_alcancou / total_avaliados,
    Qtd_Nao_Alcancou = n_nao_alcancou,
    Pct_Nao_Alcancou = 100 * n_nao_alcancou / total_avaliados,
    Qtd_Sem_Dado = n_na,
    RES_Minimo = if (length(res_valido) > 0) min(res_valido) else NA,
    RES_Maximo = if (length(res_valido) > 0) max(res_valido) else NA,
    RES_Media = if (length(res_valido) > 0) mean(res_valido) else NA,
    RES_Mediana = if (length(res_valido) > 0) median(res_valido) else NA,
    RES_Desvio_Padrao = if (length(res_valido) > 1) sd(res_valido) else NA
  )
}

# Remove o código de município excluído de qualquer base, detectando
# automaticamente a coluna de código/nome de município.
remover_municipio <- function(dados, codigo, nome_base = NULL) {
  col_cod <- names(dados)[
    str_detect(toupper(names(dados)), "COD_MUN|CD_MUN|^COD$|^COD_IBGE$|Município|IBGE")
  ]
  
  if (length(col_cod) == 0) {
    message(sprintf(
      "%s: nenhuma coluna de código de município encontrada.",
      if (is.null(nome_base)) "Base" else nome_base
    ))
    return(dados)
  }
  
  col_cod <- col_cod[1]
  cod_num <- suppressWarnings(as.numeric(dados[[col_cod]]))
  
  dados |>
    filter(is.na(cod_num) | cod_num != codigo)
}

adicionar_aba <- function(wb, nome, dados) {
  if (nome %in% names(wb)) {
    removeWorksheet(wb, nome)
  }
  
  addWorksheet(wb, nome)
  writeData(wb, nome, dados)
}

padronizar_tipos_mun <- function(dados) {
  dados |>
    mutate(
      across(
        matches("COD_MUN|cod_mun|NOME_MUN|Munic[ií]pio", ignore.case = TRUE),
        as.character
      )
    )
}

# =========================================================
# 2. Leitura dos dados brutos
# =========================================================

arquivos <- list(
  IND_01 = "Dados/Indicadores Individuais/IND_01.xlsx",
  IND_02 = "Dados/Indicadores Individuais/IND_02.xlsx",
  IND_03 = "Dados/Indicadores Individuais/IND_03.xlsx",
  IND_04 = "Dados/Indicadores Individuais/IND_04_PQAVS_2025_JAN_DEZ_Final.xlsx",
  IND_05 = "Dados/Indicadores Individuais/IND_05_PQA-VS 2025_Avaliação_Final_atualizada.xlsx",
  IND_06 = "Dados/Indicadores Individuais/IND_06_PQAVS_Jan-Dez_2025_Indicador 6_Sinan_atualizado.xlsx",
  IND_07 = "Dados/Indicadores Individuais/IND_07_PQA-VS_2025_Avaliacao_Final_Malaria.xlsx",
  IND_08 = "Dados/Indicadores Individuais/IND_08_PQAVS_2025_COMPLETO 08_07_2026.xlsx",
  IND_09 = "Dados/Indicadores Individuais/IND_09_PQA-VS 2025_Avaliação_Final_verificado.xlsx",
  IND_10 = "Dados/Indicadores Individuais/IND_10_PQA-VS 2025_Avaliação_Final.xlsx",
  IND_11 = "Dados/Indicadores Individuais/IND_11_PQA-VS 2025_Avaliação_Final_23.06.2026_Corrigido_26.06.2026.xlsx",
  IND_12 = "Dados/Indicadores Individuais/IND_12_PQA-VS 2025_Avaliação_Final_atualizada.xlsx",
  IND_13 = "Dados/Indicadores Individuais/IND_13_PQA-VS 2025_Avaliação_Final (corrigido).xlsx",
  IND_14 = "Dados/Indicadores Individuais/IND14_PQAVS_2025 jan-dez Final Extracao 12-06-2026.xlsx"
)



Indicadores_Brutos <- lapply(arquivos, ler_indicador)

# =========================================================
# 3. Exclusão de município
# =========================================================

municipio_excluido <- 260545

Indicadores_Brutos <- Map(
  function(dados, nome) remover_municipio(dados, municipio_excluido, nome),
  Indicadores_Brutos,
  names(Indicadores_Brutos)
)

# =========================================================
# 4. Cálculo dos indicadores (regra padrão RES / NUM-DEN)
# =========================================================

especificacoes_padrao <- tibble::tribble(
  ~indicador, ~meta,    ~multiplicar_res, ~coluna_mun,
  "IND_01",   89.47555, FALSE,            "NOME_MUN",
  "IND_02",   89.47555, FALSE,            "NOME_MUN",
  "IND_03",   79.47555, TRUE,             "cod_mun",
  "IND_04",   94.47555, TRUE,             "Município",
  "IND_05",   74.47555, TRUE,             "NOME_MUN",
  "IND_06",   79.47555, FALSE,            "Município",
  "IND_07",   69.47555, FALSE,            "COD_MUN",
  "IND_08",   74.47555, FALSE,            "COD_MUN",
  "IND_09",   81.47555, FALSE,            "COD_MUN",
  "IND_10",   69.47555, FALSE,            "COD_MUN",
  "IND_13",   94.47555, FALSE,            "COD_MUN",
  "IND_14",   94.47555, FALSE,            "COD_MUN"
)

Indicadores <- Map(
  function(indicador, meta, multiplicar_res, coluna_mun) {
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
  setNames(especificacoes_padrao$indicador)

# =========================================================
# 5. Indicadores 11 e 12 (variação 2024 -> 2025)
# =========================================================

Indicadores$IND_11 <- calcular_indicador_11_12(
  Indicadores_Brutos$IND_11,
  coluna_mun = "COD_MUN",
  multiplicar_res = FALSE
)

Indicadores$IND_12 <- calcular_indicador_11_12(
  Indicadores_Brutos$IND_12,
  coluna_mun = "COD_MUN",
  multiplicar_res = TRUE
)

# Ordem final de exibição/exportação
Indicadores <- Indicadores[c(
  "IND_01", "IND_02", "IND_03", "IND_04", "IND_05",
  "IND_06", "IND_07", "IND_08", "IND_09", "IND_10",
  "IND_11", "IND_13", "IND_14", "IND_12"
)]

# =========================================================
# 6. Ajustes específicos por indicador
# =========================================================

# IND_03: municípios sem RES calculável contam como meta não alcançada
Indicadores$IND_03 <- Indicadores$IND_03 |>
  mutate(METAS = replace_na(METAS, "NÃO ALCANÇOU"))

# IND_14: descarta linhas sem UF (registros inválidos na planilha de origem)
Indicadores$IND_14 <- Indicadores$IND_14 |>
  drop_na(UF)

# IND_12: descarta linhas sem código de município
Indicadores$IND_12 <- Indicadores$IND_12 |>
  drop_na(COD_MUN)

# =========================================================
# 7. Metas e estatísticas descritivas
# =========================================================

Metas <- lapply(Indicadores, calcular_metas)

Descritivas <- bind_rows(
  lapply(names(Indicadores), function(nome) descrever_indicador(Indicadores[[nome]], nome))
)
print(Descritivas)

# =========================================================
# 8. Consolidação em uma única base
# =========================================================

Indicadores_Completo <- Indicadores |>
  lapply(padronizar_tipos_mun) |>
  bind_rows(.id = "Indicador") |>
  relocate(Indicador, .before = 1)











# =========================================================
# 9. Salvamento em um único arquivo Excel
# =========================================================

dir.create("Resultados", showWarnings = FALSE)

# Função para extrair UF, código, município e meta
preparar_meta <- function(dados, nome) {
  
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
  
  # Quando a coluna se chama Município
  if (is.na(col_cod)) {
    
    col_mun <- grep(
      "^Município$|^Municipio$",
      nomes,
      ignore.case = TRUE,
      value = TRUE
    )[1]
    
    if (!is.na(col_mun)) {
      
      x <- as.character(dados[[col_mun]])
      x_valido <- x[!is.na(x) & x != ""]
      
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
  
  # Para se algum indicador não tiver código
  if (is.na(col_cod)) {
    stop(
      paste(
        "Não encontrei a coluna de código do município em",
        nome
      )
    )
  }
  
  numero <- as.integer(
    str_extract(nome, "\\d+")
  )
  
  # Preparar dados
  resultado <- tibble(
    
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
    
    # Recodificação das metas
    META = case_when(
      dados$METAS == "ALCANÇOU" ~ "SIM",
      dados$METAS == "NÃO ALCANÇOU" ~ "NÃO",
      TRUE ~ NA_character_
    )
    
  ) |>
    filter(
      !is.na(COD_MUN),
      COD_MUN != ""
    ) |>
    distinct(
      COD_MUN,
      .keep_all = TRUE
    )
  
  # META -> META_IND_1, META_IND_2 etc.
  names(resultado)[4] <- paste0(
    "META_IND_",
    numero
  )
  
  resultado
}


# ---------------------------------------------------------
# Conferir se existem os 14 indicadores
# ---------------------------------------------------------

esperados <- sprintf(
  "IND_%02d",
  1:14
)

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


# ---------------------------------------------------------
# Preparar as metas dos 14 indicadores
# ---------------------------------------------------------

Metas_Indicadores <- lapply(
  esperados,
  \(x) preparar_meta(
    Indicadores[[x]],
    x
  )
)

names(Metas_Indicadores) <- esperados


# ---------------------------------------------------------
# Criar cadastro único de municípios
# ---------------------------------------------------------

Municipios <- bind_rows(
  Metas_Indicadores
) |>
  select(
    UF,
    COD_MUN,
    NOME_MUN
  ) |>
  group_by(COD_MUN) |>
  summarise(
    UF = first(na.omit(UF)),
    NOME_MUN = first(na.omit(NOME_MUN)),
    .groups = "drop"
  )


# ---------------------------------------------------------
# Juntar META_IND_1 até META_IND_14
# ---------------------------------------------------------

Todos_Indicadores <- Reduce(
  
  \(x, y) full_join(
    x,
    y,
    by = "COD_MUN"
  ),
  
  lapply(
    Metas_Indicadores,
    \(x) select(
      x,
      COD_MUN,
      starts_with("META_IND_")
    )
  )
  
) |>
  left_join(
    Municipios,
    by = "COD_MUN"
  ) |>
  select(
    UF,
    COD_MUN,
    NOME_MUN,
    all_of(
      paste0(
        "META_IND_",
        1:14
      )
    )
  ) |>
  arrange(
    UF,
    NOME_MUN
  )




# =========================================================
# Adicionar apenas a população ao Todos_Indicadores
# =========================================================

PopMun <- read_excel(
  "Dados/Pop/PopMun.xlsx"
) |>
  mutate(
    COD_MUN = as.character(as.numeric(COD_MUN))
  ) |>
  select(
    COD_MUN,
    POP
  )


Todos_Indicadores <- Todos_Indicadores |>
  mutate(
    COD_MUN = as.character(as.numeric(COD_MUN))
  ) |>
  left_join(
    PopMun,
    by = "COD_MUN"
  )

# ---------------------------------------------------------
# Criar um único Excel com todas as abas
# ---------------------------------------------------------

abas <- c(
  Indicadores[esperados],
  list(
    TODOS_INDICADORES = Todos_Indicadores
  )
)


write.xlsx(
  abas,
  file = "Resultados/Indicadores_PQAVS.xlsx",
  overwrite = TRUE
)


# ---------------------------------------------------------
# Conferência final
# ---------------------------------------------------------

message(
  "Arquivo salvo com ",
  length(abas),
  " abas e ",
  nrow(Todos_Indicadores),
  " municípios."
)

