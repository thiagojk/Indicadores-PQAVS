library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(magrittr)
library(stringr)

# =========================================================
# 1. Funções
# =========================================================

ler_indicador <- function(arquivo) {
  read_excel(arquivo) %>%
    select(where(~ sum(!is.na(.)) > 1))
}

# ---------------------------------------------------------
# converter_numero()
# Converte uma coluna para numérico de forma robusta.
# ⚠️ CORREÇÃO: as.numeric() sozinho falha silenciosamente
# (retorna NA) quando o valor está como texto com vírgula
# decimal (ex: "85,47"), formato comum em planilhas brasileiras
# digitadas manualmente. Isso pode fazer o resultado final
# parecer "arredondado" ou até virar 0 depois de replace_na().
# Esta função trata:
#   - números que já são double/numeric (retorna direto)
#   - texto com vírgula decimal ("85,47" -> 85.47)
#   - texto com ponto decimal ("85.47" -> 85.47)
#   - texto com separador de milhar + vírgula ("1.234,56" -> 1234.56)
# ---------------------------------------------------------
converter_numero <- function(x) {
  if (is.numeric(x)) return(x)
  
  x_chr <- as.character(x)
  x_chr <- str_trim(x_chr)
  
  # Se tem vírgula E ponto -> assume ponto = milhar, vírgula = decimal
  tem_virgula <- str_detect(x_chr, ",")
  tem_ponto   <- str_detect(x_chr, "\\.")
  
  x_limpo <- dplyr::case_when(
    tem_virgula & tem_ponto ~ str_remove_all(x_chr, "\\.") %>% str_replace(",", "."),
    tem_virgula & !tem_ponto ~ str_replace(x_chr, ",", "."),
    TRUE ~ x_chr
  )
  
  as.numeric(x_limpo)
}

# ---------------------------------------------------------
calcular_indicador <- function(indicador, meta, multiplicar_res = FALSE, coluna_mun) {
  
  if (!coluna_mun %in% names(indicador)) {
    stop(sprintf("A coluna de município '%s' não foi encontrada nos dados.", coluna_mun))
  }
  
  # ⚠️ CORREÇÃO: regex mais precisa. Antes, "^RES" também podia
  # casar com colunas indesejadas (ex: "RESPONSAVEL", "RESULTADO_TEXTO").
  # Agora exige que depois de RES/NUM/DEN venha "_", fim de string,
  # ou dígito -- reduz falsos positivos na escolha da coluna.
  col_res <- names(indicador)[str_detect(toupper(names(indicador)), "^RES($|_|[0-9])")]
  col_num <- names(indicador)[str_detect(toupper(names(indicador)), "^NUM($|_|[0-9])")]
  col_den <- names(indicador)[str_detect(toupper(names(indicador)), "^DEN($|_|[0-9])")]
  
  if (length(col_res) >= 1) {
    col_res <- col_res[1]
    
    # --- DIAGNÓSTICO: mostra tipo original e amostra de valores ---
    message(sprintf(
      "[DIAG] Coluna RES usada: '%s' | tipo original: %s | amostra: %s",
      col_res, class(indicador[[col_res]])[1],
      paste(head(as.character(indicador[[col_res]]), 5), collapse = ", ")
    ))
    
    indicador <- indicador %>%
      mutate(
        RES = converter_numero(.data[[col_res]]),
        RES = if (multiplicar_res) RES * 100 else RES,
        RES = round(RES, 2)
      )
    
  } else if (length(col_num) >= 1 && length(col_den) >= 1) {
    col_num <- col_num[1]
    col_den <- col_den[1]
    
    # --- DIAGNÓSTICO ---
    message(sprintf(
      "[DIAG] Colunas NUM/DEN usadas: '%s' / '%s' | tipos: %s / %s | amostra NUM: %s | amostra DEN: %s",
      col_num, col_den,
      class(indicador[[col_num]])[1], class(indicador[[col_den]])[1],
      paste(head(as.character(indicador[[col_num]]), 5), collapse = ", "),
      paste(head(as.character(indicador[[col_den]]), 5), collapse = ", ")
    ))
    
    indicador <- indicador %>%
      mutate(
        !!col_num := replace_na(converter_numero(.data[[col_num]]), 0),
        !!col_den := replace_na(converter_numero(.data[[col_den]]), 0),
        RES = .data[[col_num]] / .data[[col_den]],
        RES = if (multiplicar_res) RES * 100 else RES,
        RES = round(RES, 2)
      )
    
  } else {
    stop("Não foi possível identificar colunas de NUM/DEN ou RES neste indicador. Ajuste manualmente.")
  }
  
  indicador %>%
    mutate(
      METAS = case_when(
        is.nan(RES) ~ "NÃO ALCANÇOU",   # NUM/DEN = 0/0
        is.na(RES) ~ "NÃO ALCANÇOU",    # RES ausente
        RES >= meta ~ "ALCANÇOU",
        TRUE ~ "NÃO ALCANÇOU"
      )
    )
}

calcular_metas <- function(indicador) {
  
  if (!"METAS" %in% names(indicador)) {
    stop("A coluna 'METAS' não foi encontrada. Execute calcular_indicador() primeiro.")
  }
  
  indicador %>%
    count(METAS, name = "Quantidade") %>%
    rename(Resultado_Meta = METAS) %>%
    arrange(desc(Resultado_Meta))
}

# ---------------------------------------------------------
# descrever_indicador()
# Estatísticas descritivas de cada indicador: total de
# municípios avaliados, quantos alcançaram/não alcançaram a
# meta (com percentual), e estatísticas do resultado (RES).
# ---------------------------------------------------------
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
    Pct_Alcancou = round(100 * n_alcancou / total_avaliados, 1),
    Qtd_Nao_Alcancou = n_nao_alcancou,
    Pct_Nao_Alcancou = round(100 * n_nao_alcancou / total_avaliados, 1),
    Qtd_Sem_Dado = n_na,
    RES_Minimo = if (length(res_valido) > 0) round(min(res_valido), 2) else NA,
    RES_Maximo = if (length(res_valido) > 0) round(max(res_valido), 2) else NA,
    RES_Media = if (length(res_valido) > 0) round(mean(res_valido), 2) else NA,
    RES_Mediana = if (length(res_valido) > 0) round(median(res_valido), 2) else NA,
    RES_Desvio_Padrao = if (length(res_valido) > 1) round(sd(res_valido), 2) else NA
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

Indicadores_Brutos <- lapply(arquivos, ler_indicador)

# ---------------------------------------------------------
# Exclusão de um município específico de todas as bases
# ⚠️ AJUSTAR: a exclusão é feita pela coluna de código do
# município (procura por colunas como COD_MUN, CD_MUN, cod_mun).
# Em bases que só têm o NOME do município (ex: NOME_MUN,
# Município) e não têm coluna de código, a função não consegue
# filtrar e avisa via mensagem — confira manualmente esses casos.
# ---------------------------------------------------------
municipio_excluido <- 260545

remover_municipio <- function(dados, codigo, nome_base = NULL) {
  col_cod <- names(dados)[str_detect(toupper(names(dados)), "COD_MUN|CD_MUN|^COD$|^COD_IBGE$")]
  
  if (length(col_cod) == 0) {
    message(sprintf(
      "⚠️ %s: nenhuma coluna de código de município encontrada — nenhuma linha removida.",
      if (is.null(nome_base)) "Base" else nome_base
    ))
    return(dados)
  }
  
  col_cod <- col_cod[1]
  cod_num <- suppressWarnings(as.numeric(dados[[col_cod]]))
  
  dados %>%
    filter(is.na(cod_num) | cod_num != codigo)
}

Indicadores_Brutos <- Map(
  function(dados, nome) remover_municipio(dados, municipio_excluido, nome),
  Indicadores_Brutos,
  names(Indicadores_Brutos)
)

# =========================================================
# 3. Cálculo dos indicadores
# =========================================================

Indicadores <- list()

message("===== IND_01 =====")
Indicadores$IND_01 <- calcular_indicador(
  Indicadores_Brutos$IND_01,
  meta = 89.47,
  multiplicar_res = FALSE,
  coluna_mun = "NOME_MUN"
)

message("===== IND_02 =====")
Indicadores$IND_02 <- calcular_indicador(
  Indicadores_Brutos$IND_02,
  meta = 89.47,
  multiplicar_res = FALSE,
  coluna_mun = "NOME_MUN"
)

message("===== IND_03 =====")
Indicadores$IND_03 <- calcular_indicador(
  Indicadores_Brutos$IND_03,
  meta = 79.47,
  multiplicar_res = TRUE,
  coluna_mun = "cod_mun"
) %>%
  mutate(
    METAS = replace_na(METAS, "NÃO ALCANÇOU")
  )

message("===== IND_04 =====")
Indicadores$IND_04 <- calcular_indicador(
  Indicadores_Brutos$IND_04,
  meta = 94.47,
  multiplicar_res = TRUE,
  coluna_mun = "Município"
)

message("===== IND_05 =====")
Indicadores$IND_05 <- calcular_indicador(
  Indicadores_Brutos$IND_05,
  meta = 74.47,
  multiplicar_res = TRUE,
  coluna_mun = "NOME_MUN"
)

message("===== IND_06 =====")
Indicadores$IND_06 <- calcular_indicador(
  Indicadores_Brutos$IND_06,
  meta = 79.47,
  multiplicar_res = FALSE,
  coluna_mun = "Município"
)

message("===== IND_07 =====")
Indicadores$IND_07 <- calcular_indicador(
  Indicadores_Brutos$IND_07,
  meta = 69.47,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)

message("===== IND_08 =====")
Indicadores$IND_08 <- calcular_indicador(
  Indicadores_Brutos$IND_08,
  meta = 74.47,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)

message("===== IND_09 =====")
Indicadores$IND_09 <- calcular_indicador(
  Indicadores_Brutos$IND_09,
  meta = 81.47,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)

message("===== IND_10 =====")
Indicadores$IND_10 <- calcular_indicador(
  Indicadores_Brutos$IND_10,
  meta = 69.47,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)

message("===== IND_11 =====")
# ⚠️ AJUSTAR: IND_11 era lido no script original mas nunca calculado.
# A meta abaixo é um placeholder — substitua pelo valor correto.
Indicadores$IND_11 <- calcular_indicador(
  Indicadores_Brutos$IND_11,
  meta = 79.47,          # ⚠️ AJUSTAR valor da meta real
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"  # ⚠️ AJUSTAR nome da coluna de município, se diferente
)

message("===== IND_13 =====")
Indicadores$IND_13 <- calcular_indicador(
  Indicadores_Brutos$IND_13,
  meta = 94.47,
  multiplicar_res = FALSE,
  coluna_mun = "COD_MUN"
)

message("===== IND_14 =====")
Indicadores$IND_14 <- calcular_indicador(
  Indicadores_Brutos$IND_14,
  meta = 94.47,
  multiplicar_res = FALSE,
  coluna_mun = "NOME_MUN"
)

# =========================================================
# 4. Indicador 12 (regra própria, mantida do script original)
# ⚠️ CORREÇÃO: troquei as.numeric() por converter_numero() aqui
# também, pelo mesmo motivo (vírgula decimal em texto).
# =========================================================

message("===== IND_12 =====")
message(sprintf(
  "[DIAG] RES_2025 | tipo original: %s | amostra: %s",
  class(Indicadores_Brutos$IND_12$RES_2025)[1],
  paste(head(as.character(Indicadores_Brutos$IND_12$RES_2025), 5), collapse = ", ")
))

Indicadores$IND_12 <- Indicadores_Brutos$IND_12 %>%
  mutate(
    RES_2025_Era_NA = is.na(converter_numero(RES_2025)),   # guarda o status original antes de zerar
    across(
      c(RES_2024, NUM_2025, DEN_2025, RES_2025),
      ~ round(replace_na(converter_numero(.), 0), 2)
    ),
    DIFF = RES_2025 - RES_2024,
    METAS = case_when(
      RES_2025_Era_NA ~ "NÃO ALCANÇOU",   # RES_2025 ausente = Não Alcançou (regra própria do IND_12)
      DEN_2025 == 0 ~ "ALCANÇOU",
      DIFF > 0 ~ "NÃO ALCANÇOU",
      DIFF == 0 & RES_2025 > 0 ~ "NÃO ALCANÇOU",
      TRUE ~ "ALCANÇOU"
    ),
    RES = RES_2025
  ) %>%
  select(-RES_2025_Era_NA) %>%
  drop_na(NOME_MUN)

# =========================================================
# 5. Cálculo das metas (contagem) por indicador
# =========================================================

Metas <- lapply(Indicadores, calcular_metas)

# =========================================================
# 6. Estatísticas descritivas por indicador
# =========================================================

Descritivas <- bind_rows(
  lapply(names(Indicadores), function(nome) {
    descrever_indicador(Indicadores[[nome]], nome)
  })
)

print(Descritivas)

# =========================================================
# 7. Exportação para Excel (dados completos dos indicadores)
# =========================================================

wb <- createWorkbook()

# ---------------------------------------------------------
# Função auxiliar: adiciona (ou substitui) uma aba no workbook,
# evitando erro de "aba já existe" ao rodar o script mais de
# uma vez na mesma sessão. Também aplica formatação numérica
# com 2 casas decimais fixas nas colunas numéricas, pois o
# Excel por padrão (formato "Geral") esconde zeros à direita
# e pode exibir menos casas decimais do que o valor real tem.
# ---------------------------------------------------------
adicionar_aba <- function(wb, nome, dados) {
  if (nome %in% names(wb)) {
    removeWorksheet(wb, nome)
  }
  addWorksheet(wb, nome)
  writeData(wb, nome, dados)
  
  col_numericas <- which(sapply(dados, is.numeric))
  
  if (length(col_numericas) > 0) {
    addStyle(
      wb, nome,
      style = createStyle(numFmt = "0.00"),
      rows = 2:(nrow(dados) + 1),
      cols = col_numericas,
      gridExpand = TRUE
    )
  }
}

# Uma aba por indicador, com os dados completos (todas as
# colunas originais + RES calculado)
for (nome in names(Indicadores)) {
  adicionar_aba(wb, nome, Indicadores[[nome]])
}

# ---------------------------------------------------------
# Aba única com todos os indicadores empilhados
# ---------------------------------------------------------
padronizar_tipos_mun <- function(dados) {
  dados %>%
    mutate(across(
      matches("COD_MUN|cod_mun|NOME_MUN|Munic[ií]pio", ignore.case = TRUE),
      as.character
    ))
}

Indicadores_Completo <- Indicadores %>%
  lapply(padronizar_tipos_mun) %>%
  bind_rows(.id = "Indicador") %>%
  relocate(Indicador, .before = 1)

adicionar_aba(wb, "Todos_Indicadores", Indicadores_Completo)

saveWorkbook(wb, "Resultado_Indicadores_Metas.xlsx", overwrite = TRUE)