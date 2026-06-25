library(ggplot2)


# Graficos ----------------------------------------------------------------

# ── Prepara os dados para o gráfico ──────────────────────────────────────────

# Conta METAS para cada indicador e empilha em um único data frame
df_plot <- bind_rows(
  Indicadores$IND_12 %>%
    filter(!is.na(METAS)) %>%
    count(METAS) %>%
    mutate(Indicador = "Indicador 12"),
  
  Indicadores$IND_14 %>%
    filter(!is.na(METAS)) %>%
    mutate(
      # Padroniza os rótulos do IND_14 para o mesmo padrão do IND_12
      METAS = case_when(
        METAS == "ALCANÇOU"     ~ "ALCANÇOU A META",
        METAS == "NÃO ALCANÇOU" ~ "NÃO ALCANÇOU A META",
        TRUE ~ METAS
      )
    ) %>%
    count(METAS) %>%
    mutate(Indicador = "Indicador 14")
)

# Paleta de cores
cores <- c("ALCANÇOU A META"     = "#2E8B57",   # verde
           "NÃO ALCANÇOU A META" = "#C0392B")   # vermelho

# ── Gráfico ───────────────────────────────────────────────────────────────────
p <- ggplot(df_plot, aes(x = METAS, y = n, fill = METAS)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = n),
    vjust = -0.5,
    fontface = "bold",
    size = 4.5
  ) +
  facet_wrap(~Indicador, scales = "free_y") +
  scale_fill_manual(values = cores) +
  scale_x_discrete(labels = function(x) gsub(" A META", "", x)) +  # rótulo curto no eixo
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Cumprimento de Metas — Indicadores 12 e 14",
    subtitle = "PQAVS 2025  |  Municípios por situação da meta",
    x        = NULL,
    y        = "Número de Municípios",
    caption  = "Fonte: PQAVS 2025"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle    = element_text(color = "gray45", size = 11, hjust = 0),
    plot.caption     = element_text(color = "gray55", size = 9),
    strip.text       = element_text(face = "bold", size = 12),
    axis.text.x      = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
p

