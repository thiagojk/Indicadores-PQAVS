# =========================================================
# CONFIGURAÇÕES DO PROJETO
# =========================================================

# Município excluído das bases
municipio_excluido <- 260545

# Indicadores esperados
esperados <- sprintf("IND_%02d", 1:14)

# Arquivos dos indicadores
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

# Especificações dos indicadores com regra padrão
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

# Arquivo de população
arquivo_populacao <- "Dados/Pop/Pop_IBGE_2025.xlsx"

# Pastas e arquivos de saída
pasta_resultados <- "Resultados"
pasta_dashboard <- file.path(pasta_resultados, "Dashboard")
arquivo_excel_final <- file.path(pasta_resultados, "Indicadores_PQAVS.xlsx")
