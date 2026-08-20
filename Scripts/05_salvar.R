# =========================================================
# SALVAMENTO DOS RESULTADOS
# =========================================================

dir.create(
  pasta_resultados,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  pasta_dashboard,
  showWarnings = FALSE,
  recursive = TRUE
)


# ---------------------------------------------------------
# 1. Excel completo para conferência
# ---------------------------------------------------------

abas <- c(
  Indicadores,
  list(
    TODOS_INDICADORES = Dados_Completos,
    RESUMO_ESTADOS = Estados,
    DESCRITIVAS = Descritivas
  )
)

openxlsx::write.xlsx(
  abas,
  file = arquivo_excel_final,
  overwrite = TRUE
)


# ---------------------------------------------------------
# 2. Bases prontas para o dashboard
# ---------------------------------------------------------

saveRDS(
  Dados_Completos,
  file.path(
    pasta_dashboard,
    "Dados_Completos.rds"
  )
)

saveRDS(
  Estados,
  file.path(
    pasta_dashboard,
    "Estados.rds"
  )
)

saveRDS(
  Descritivas,
  file.path(
    pasta_dashboard,
    "Descritivas.rds"
  )
)

saveRDS(
  Indicadores_Completo,
  file.path(
    pasta_dashboard,
    "Indicadores_Completo.rds"
  )
)

saveRDS(
  Indicadores,
  file.path(
    pasta_dashboard,
    "Indicadores.rds"
  )
)


# ---------------------------------------------------------
# 3. Conferência final
# ---------------------------------------------------------

message(
  "Processamento concluído com sucesso."
)

message(
  "Excel salvo em: ",
  arquivo_excel_final
)

message(
  "Bases do dashboard salvas em: ",
  pasta_dashboard
)

message(
  "Total de municípios: ",
  nrow(Dados_Completos)
)
