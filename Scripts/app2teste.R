# =========================================================
# DASHBOARD PQAVS 2025
# Dashboard/app.R
#
# Usa somente as bases .rds geradas pelo pipeline modular.
# Não recalcula os indicadores ao iniciar o dashboard.
#
# REVISÃO: removidas funções/outputs órfãos (não referenciados
# em nenhuma UI), corrigido mismatch plotOutput/renderPlotly,
# adicionado render que faltava para o gráfico de dispersão,
# e consolidado o gráfico de faixas por Estado em uma única
# função ggplot2 com rótulos.
# =========================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(DT)
library(kableExtra)
library(scales)
library(ggplot2)


# =========================================================
# 1. LOCALIZAÇÃO DO PROJETO E LEITURA DOS DADOS
# =========================================================

raiz_projeto <- if (dir.exists("Resultados/Dashboard")) {
  "."
} else if (dir.exists("../Resultados/Dashboard")) {
  ".."
} else {
  stop(
    paste(
      "Não foi possível localizar Resultados/Dashboard.",
      "Execute primeiro Scripts/executar_tudo.R."
    )
  )
}

pasta_dados <- file.path(
  raiz_projeto,
  "Resultados",
  "Dashboard"
)

arquivos_necessarios <- c(
  "Dados_Completos.rds",
  "Estados.rds",
  "Descritivas.rds",
  "Indicadores_Completo.rds",
  "Indicadores.rds"
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(
    file.path(
      pasta_dados,
      arquivos_necessarios
    )
  )
]

if (length(arquivos_ausentes) > 0) {
  stop(
    paste(
      "Arquivos ausentes:",
      paste(arquivos_ausentes, collapse = ", "),
      "\nExecute Scripts/executar_tudo.R."
    )
  )
}

Dados_Completos <- readRDS(
  file.path(pasta_dados, "Dados_Completos.rds")
)

Estados <- readRDS(
  file.path(pasta_dados, "Estados.rds")
)

Descritivas <- readRDS(
  file.path(pasta_dados, "Descritivas.rds")
)

Indicadores_Completo <- readRDS(
  file.path(pasta_dados, "Indicadores_Completo.rds")
)

Indicadores <- readRDS(
  file.path(pasta_dados, "Indicadores.rds")
)


# =========================================================
# 2. PREPARAÇÃO DOS DADOS
# =========================================================

Resumo_Indicadores <- Descritivas |>
  transmute(
    Indicador,
    Total_Municipios,
    Alcancou = Qtd_Alcancou,
    Nao_Alcancou = Qtd_Nao_Alcancou,
    Percentual_Alcancou = round(Pct_Alcancou, 2)
  ) |>
  arrange(
    as.integer(
      str_extract(Indicador, "\\d+")
    )
  )

Total_Indicadores <- nrow(Resumo_Indicadores)

Total_Avaliacoes <- sum(
  Resumo_Indicadores$Total_Municipios,
  na.rm = TRUE
)

Total_Alcancou <- sum(
  Resumo_Indicadores$Alcancou,
  na.rm = TRUE
)

Total_Nao_Alcancou <- sum(
  Resumo_Indicadores$Nao_Alcancou,
  na.rm = TRUE
)

Total_Municipios <- nrow(Dados_Completos)

Total_UFs <- n_distinct(
  Dados_Completos$UF,
  na.rm = TRUE
)

Total_Incentivo_Municipios <- if (
  "PQAVS_Incentivo" %in% names(Dados_Completos)
) {
  sum(
    Dados_Completos$PQAVS_Incentivo,
    na.rm = TRUE
  )
} else {
  NA_real_
}

Total_Incentivo_Estados <- if (
  "PQAVS_Incentivo" %in% names(Estados)
) {
  sum(
    Estados$PQAVS_Incentivo,
    na.rm = TRUE
  )
} else {
  NA_real_
}


Total_Valor_Repassar_Municipios <- if (
  "Valor_a_Repassar" %in% names(Dados_Completos)
) {
  sum(
    Dados_Completos$Valor_a_Repassar,
    na.rm = TRUE
  )
} else {
  NA_real_
}

Total_Valor_Repassar_Estados <- if (
  "Valor_a_Repassar" %in% names(Estados)
) {
  sum(
    Estados$Valor_a_Repassar,
    na.rm = TRUE
  )
} else {
  NA_real_
}


# =========================================================
# 3. FUNÇÕES DE FORMATAÇÃO
# =========================================================

fmt_num <- function(x) {
  scales::label_number(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = 1
  )(x)
}

fmt_pct <- function(x, casas = 1) {
  paste0(
    formatC(
      x,
      format = "f",
      digits = casas,
      decimal.mark = ","
    ),
    "%"
  )
}

fmt_moeda <- function(x) {
  scales::label_currency(
    prefix = "R$ ",
    big.mark = ".",
    decimal.mark = ",",
    accuracy = 0.01
  )(x)
}

kable_pqavs <- function(
    dados,
    altura = "600px",
    font_size = 13
) {
  
  dados |>
    kbl(
      format = "html",
      escape = FALSE,
      align = "c",
      table.attr = 'class="table table-hover pqavs-kable"'
    ) |>
    kable_styling(
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed",
        "responsive"
      ),
      full_width = FALSE,
      position = "center",
      font_size = font_size
    ) |>
    row_spec(
      0,
      bold = TRUE,
      color = "white",
      background = "#176B57"
    ) |>
    scroll_box(
      width = "100%",
      height = altura
    )
}


# =========================================================
# 4. TABELAS PEQUENAS
# =========================================================

Tabela_Resumo_Indicadores <- Resumo_Indicadores |>
  transmute(
    Indicador,
    `Total de municípios` = Total_Municipios,
    `Alcançou` = Alcancou,
    `Não alcançou` = Nao_Alcancou,
    `% alcançou` = fmt_pct(
      Percentual_Alcancou,
      2
    )
  )

Tabela_Descritivas <- Descritivas |>
  mutate(
    across(
      starts_with("Pct_"),
      ~ fmt_pct(.x, 2)
    ),
    across(
      starts_with("RES_"),
      ~ round(.x, 2)
    )
  )


# =========================================================
# 5. TEMA E CSS
# =========================================================

tema_pqavs <- bs_theme(
  version = 5,
  bg = "#F4F7F9",
  fg = "#17222E",
  primary = "#176B57",
  secondary = "#356B8C",
  success = "#1B7F5A",
  danger = "#B4543A",
  base_font = font_collection(
    "Segoe UI",
    "Arial",
    "sans-serif"
  )
)

css_pqavs <- "
:root {
  --verde: #176B57;
  --verde-claro: #E7F5EE;
  --verde-escuro: #0F4F40;
  --azul: #356B8C;
  --vermelho: #B4543A;
  --vermelho-claro: #FCEBE7;
  --texto: #17222E;
  --texto-secundario: #64748B;
  --borda: #E2E8EE;
  --fundo: #F4F7F9;
}

body {
  background: var(--fundo);
}

.navbar {
  box-shadow: 0 2px 14px rgba(20, 38, 55, 0.08);
}

.navbar-brand {
  font-weight: 750;
  letter-spacing: -0.02em;
}

.dashboard-container {
  max-width: 1650px;
  margin: 0 auto;
  padding: 24px 18px 42px 18px;
}

.hero {
  color: white;
  background:
    radial-gradient(
      circle at top right,
      rgba(255,255,255,0.16),
      transparent 32%
    ),
    linear-gradient(
      120deg,
      #0F4F40 0%,
      #176B57 55%,
      #356B8C 100%
    );
  border-radius: 20px;
  padding: 28px 32px;
  margin-bottom: 22px;
  box-shadow: 0 16px 34px rgba(15,79,64,0.15);
}

.hero h2 {
  font-size: 2rem;
  font-weight: 760;
  margin: 0 0 7px 0;
  letter-spacing: -0.03em;
}

.hero p {
  margin: 0;
  opacity: 0.89;
  font-size: 1rem;
}

.section-heading {
  margin: 28px 0 16px 0;
}

.section-heading h3 {
  margin: 0 0 4px 0;
  font-size: 1.23rem;
  font-weight: 760;
}

.section-heading p {
  margin: 0;
  color: var(--texto-secundario);
  font-size: 0.92rem;
}

.pqavs-card {
  background: white;
  border: 1px solid var(--borda);
  border-radius: 17px;
  box-shadow: 0 7px 24px rgba(27,44,63,0.055);
}

.pqavs-card .card-header {
  background: white;
  border-bottom: 1px solid var(--borda);
  padding: 16px 18px;
  font-weight: 720;
}

.bslib-value-box {
  border: 0 !important;
  border-radius: 17px !important;
  box-shadow: 0 7px 24px rgba(27,44,63,0.07);
  overflow: hidden;
}

.bslib-value-box .value-box-value {
  font-weight: 780;
  letter-spacing: -0.025em;
}

.filtros-card {
  background: white;
  border: 1px solid var(--borda);
  border-radius: 16px;
  padding: 16px 18px 4px 18px;
  margin-bottom: 18px;
  box-shadow: 0 7px 24px rgba(27,44,63,0.05);
}

.pqavs-table-wrap {
  background: white;
  border: 1px solid var(--borda);
  border-radius: 16px;
  padding: 12px;
  box-shadow: 0 7px 24px rgba(27,44,63,0.055);
  overflow: hidden;
}

.pqavs-kable {
  margin-bottom: 0 !important;
  white-space: nowrap;
}

.pqavs-kable thead th {
  background: #176B57 !important;
  color: white !important;
  text-align: center !important;
}

.pqavs-kable tbody tr:hover td {
  background: #EAF5F1 !important;
}

.dataTables_wrapper {
  font-size: 0.90rem;
}

table.dataTable thead th {
  background: #176B57 !important;
  color: white !important;
  font-weight: 700;
  vertical-align: middle;
}

table.dataTable tbody tr:hover {
  background-color: #EAF5F1 !important;
}

.dataTables_filter input,
.dataTables_length select,
.form-select,
.form-control {
  border-radius: 10px !important;
  border: 1px solid var(--borda) !important;
}

.form-select:focus,
.form-control:focus,
.dataTables_filter input:focus {
  border-color: rgba(23,107,87,0.55) !important;
  box-shadow: 0 0 0 3px rgba(23,107,87,0.10) !important;
}


.indicadores-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 18px;
  align-items: stretch;
}

.indicador-card {
  background: #FFFFFF;
  border: 1px solid var(--borda);
  border-radius: 18px;
  box-shadow: 0 8px 25px rgba(27,44,63,0.065);
  overflow: hidden;
  min-width: 0;
  transition:
    transform 0.16s ease,
    box-shadow 0.16s ease;
}

.indicador-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 13px 30px rgba(27,44,63,0.10);
}

.indicador-card-header {
  padding: 17px 19px 12px 19px;
  border-bottom: 1px solid #EDF1F4;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
}

.indicador-titulo {
  font-size: 1.08rem;
  font-weight: 780;
  color: var(--texto);
  letter-spacing: -0.015em;
}

.indicador-total {
  color: var(--texto-secundario);
  background: #F3F6F8;
  border: 1px solid #E5EBEF;
  border-radius: 999px;
  padding: 5px 9px;
  font-size: 0.78rem;
  font-weight: 650;
  white-space: nowrap;
}

.indicador-grafico {
  padding: 4px 9px 2px 5px;
}

.indicador-footer {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 9px;
  padding: 0 17px 16px 17px;
}

.resultado-badge {
  border-radius: 12px;
  padding: 9px 11px;
  line-height: 1.25;
}

.resultado-badge small {
  display: block;
  font-size: 0.71rem;
  font-weight: 750;
  letter-spacing: 0.02em;
  margin-bottom: 2px;
}

.resultado-badge strong {
  display: block;
  font-size: 1.18rem;
  font-weight: 800;
}

.resultado-badge.alcancou {
  color: #125B40;
  background: var(--verde-claro);
}

.resultado-badge.nao-alcancou {
  color: #913C2D;
  background: var(--vermelho-claro);
}

@media (max-width: 1100px) {
  .indicadores-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

@media (max-width: 720px) {
  .indicadores-grid {
    grid-template-columns: 1fr;
  }
}


.grafico-nota {
  margin-top: 8px;
  padding: 10px 14px;
  background: #F8FAFC;
  border-left: 4px solid #176B57;
  border-radius: 8px;
  color: #64748B;
  font-size: 0.84rem;
  line-height: 1.45;
}

.grafico-card {
  background: white;
  border: 1px solid var(--borda);
  border-radius: 17px;
  padding: 12px;
  box-shadow: 0 7px 24px rgba(27,44,63,0.055);
}

.info-mini {
  color: var(--texto-secundario);
  font-size: 0.84rem;
  margin-bottom: 10px;
}

@media (max-width: 720px) {
  .dashboard-container {
    padding: 14px 10px 28px 10px;
  }

  .hero {
    border-radius: 15px;
    padding: 22px 19px;
  }

  .hero h2 {
    font-size: 1.55rem;
  }
}
"


# =========================================================
# 6. FUNÇÕES DOS GRÁFICOS
#
# Observação: foram removidas as seguintes funções, que
# existiam no script original mas não eram referenciadas
# por nenhum output usado na UI (código morto):
#   - grafico_indicadores()          (barras agrupadas por indicador)
#   - grafico_faixas_municipios()    (pizza de faixas por município)
#   - grafico_financeiro_uf()        (barras de incentivo por UF)
#   - grafico_estados_90()           (e o alias grafico_estados_desempenho)
#   - grafico_estados_repassar()     (duplicata de grafico_repassar_estados)
# Se algum desses gráficos for necessário em outra aba, é só
# reaproveitar a lógica — os dados (Dados_Completos/Estados) já
# estão carregados e prontos.
# =========================================================

# ---------------------------------------------------------
# Estados > Ranking de desempenho (% municípios 90%+)
# ---------------------------------------------------------

grafico_ranking_estados <- function() {
  
  if (!"Mun_90_%" %in% names(Estados)) {
    return(plot_ly())
  }
  
  dados <- Estados |>
    arrange(`Mun_90_%`)
  
  media_nacional <- mean(
    dados$`Mun_90_%`,
    na.rm = TRUE
  )
  
  plot_ly(
    dados,
    x = ~`Mun_90_%`,
    y = ~reorder(UF, `Mun_90_%`),
    type = "bar",
    orientation = "h",
    marker = list(
      color = "#176B57"
    ),
    text = ~paste0(
      formatC(
        `Mun_90_%`,
        digits = 1,
        format = "f",
        decimal.mark = ","
      ),
      "%"
    ),
    textposition = "outside",
    cliponaxis = FALSE,
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "% municípios 90%+: %{x:.1f}%",
      "<extra></extra>"
    )
  ) |>
    layout(
      shapes = list(
        list(
          type = "line",
          x0 = media_nacional,
          x1 = media_nacional,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(
            dash = "dash",
            color = "#356B8C"
          )
        )
      ),
      margin = list(
        l = 55,
        r = 55,
        t = 20,
        b = 45
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(
        title = "% de municípios com 90% ou mais",
        gridcolor = "#EDF1F4"
      ),
      yaxis = list(
        title = ""
      )
    ) |>
    config(
      displayModeBar = FALSE,
      responsive = TRUE
    )
}


# ---------------------------------------------------------
# Estados > Distribuição das faixas de desempenho
# (barras empilhadas, plotly — interativo, com hover)
# ---------------------------------------------------------

grafico_faixas_estados_plotly <- function() {
  
  colunas <- intersect(
    c(
      "Mun_90_%",
      "Mun_70_%",
      "Mun_50_%",
      "Mun_30_%"
    ),
    names(Estados)
  )
  
  if (length(colunas) == 0) {
    return(
      plot_ly() |>
        layout(
          annotations = list(list(
            text = "Colunas de faixas não disponíveis em Estados",
            showarrow = FALSE
          ))
        )
    )
  }
  
  rotulos_faixa <- c(
    "Mun_90_%" = "90% ou mais",
    "Mun_70_%" = "70% a 89%",
    "Mun_50_%" = "50% a 69%",
    "Mun_30_%" = "Abaixo de 50%"
  )
  
  niveis_faixa <- c(
    "90% ou mais",
    "70% a 89%",
    "50% a 69%",
    "Abaixo de 50%"
  )
  
  cores_faixa <- c(
    "90% ou mais"   = "#176B57",
    "70% a 89%"     = "#1B7F5A",
    "50% a 69%"     = "#356B8C",
    "Abaixo de 50%" = "#B4543A"
  )
  
  dados <- Estados |>
    select(
      UF,
      all_of(colunas)
    ) |>
    pivot_longer(
      cols = -UF,
      names_to = "Faixa",
      values_to = "Percentual"
    ) |>
    mutate(
      Percentual = as.numeric(
        gsub(
          ",",
          ".",
          as.character(Percentual)
        )
      ),
      Faixa = recode(Faixa, !!!rotulos_faixa)
    ) |>
    filter(!is.na(Percentual))
  
  if (nrow(dados) == 0) {
    return(
      plot_ly() |>
        layout(
          annotations = list(list(
            text = "Sem dados disponíveis",
            showarrow = FALSE
          ))
        )
    )
  }
  
  # Ordena os Estados pelo percentual na faixa "90% ou mais"
  ordem_uf <- dados |>
    filter(Faixa == "90% ou mais") |>
    arrange(desc(Percentual)) |>
    pull(UF)
  
  dados <- dados |>
    mutate(
      UF = factor(UF, levels = ordem_uf),
      # rev(): "90% ou mais" empilhado no topo da barra
      Faixa = factor(Faixa, levels = rev(niveis_faixa)),
      Rotulo = ifelse(
        Percentual >= 4,
        paste0(
          formatC(
            Percentual,
            digits = 1,
            format = "f",
            decimal.mark = ","
          ),
          "%"
        ),
        ""
      )
    )
  
  plot_ly(
    dados,
    x = ~UF,
    y = ~Percentual,
    color = ~Faixa,
    colors = cores_faixa[levels(dados$Faixa)],
    type = "bar",
    text = ~Rotulo,
    textposition = "inside",
    textfont = list(
      color = "white",
      size = 11
    ),
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "Faixa: %{fullData.name}<br>",
      "Municípios: %{y:.1f}%",
      "<extra></extra>"
    )
  ) |>
    layout(
      barmode = "stack",
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.28,
        font = list(size = 13)
      ),
      margin = list(
        l = 60,
        r = 20,
        t = 20,
        b = 95
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(
        size = 14,
        family = "Segoe UI, Arial, sans-serif"
      ),
      xaxis = list(
        title = "",
        tickangle = -45,
        tickfont = list(size = 13)
      ),
      yaxis = list(
        title = list(
          text = "Percentual de municípios",
          font = list(size = 14)
        ),
        ticksuffix = "%",
        gridcolor = "#EDF1F4",
        tickfont = list(size = 12)
      )
    ) |>
    config(
      displayModeBar = FALSE,
      responsive = TRUE
    )
}


# ---------------------------------------------------------
# Estados > Valor a repassar por Estado
# ---------------------------------------------------------

grafico_repassar_estados <- function() {
  
  if (!"Valor_a_Repassar" %in% names(Estados)) {
    return(
      plot_ly() |>
        layout(
          annotations = list(
            list(
              text = "Valor_a_Repassar não disponível",
              showarrow = FALSE
            )
          )
        )
    )
  }
  
  dados <- Estados |>
    arrange(Valor_a_Repassar)
  
  plot_ly(
    dados,
    x = ~Valor_a_Repassar,
    y = ~reorder(UF, Valor_a_Repassar),
    type = "bar",
    orientation = "h",
    marker = list(
      color = "#356B8C"
    ),
    text = ~fmt_moeda(Valor_a_Repassar),
    textposition = "outside",
    cliponaxis = FALSE,
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "Valor a repassar: R$ %{x:,.2f}",
      "<extra></extra>"
    )
  ) |>
    layout(
      margin = list(
        l = 55,
        r = 120,
        t = 20,
        b = 45
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(
        title = "Valor a repassar",
        gridcolor = "#EDF1F4"
      ),
      yaxis = list(
        title = ""
      )
    ) |>
    config(
      displayModeBar = FALSE,
      responsive = TRUE
    )
}


# ---------------------------------------------------------
# Estados > Desempenho x Valor a repassar (dispersão)
# ---------------------------------------------------------

grafico_dispersao_estados <- function() {
  
  if (
    !all(
      c(
        "Mun_90_%",
        "Valor_a_Repassar"
      ) %in% names(Estados)
    )
  ) {
    return(
      ggplot() +
        theme_void() +
        annotate(
          "text",
          x = 1,
          y = 1,
          label = "Colunas necessárias não disponíveis"
        )
    )
  }
  
  ggplot(
    Estados,
    aes(
      x = `Mun_90_%`,
      y = Valor_a_Repassar,
      label = UF
    )
  ) +
    geom_point(
      size = 4,
      color = "#176B57"
    ) +
    geom_text(
      vjust = -0.8,
      size = 3
    ) +
    scale_y_continuous(
      labels = scales::label_currency(
        prefix = "R$ ",
        big.mark = ".",
        decimal.mark = ","
      )
    ) +
    labs(
      x = "% municípios com 90%+",
      y = "Valor a repassar"
    ) +
    theme_minimal(base_size = 12)
}


# ---------------------------------------------------------
# Cards individuais dos indicadores
# ---------------------------------------------------------

card_indicador <- function(linha) {
  
  id_grafico <- paste0(
    "grafico_",
    tolower(linha$Indicador)
  )
  
  div(
    class = "indicador-card",
    
    div(
      class = "indicador-card-header",
      
      span(
        class = "indicador-titulo",
        linha$Indicador
      ),
      
      span(
        class = "indicador-total",
        paste0(
          "Total: ",
          linha$Total_Municipios
        )
      )
    ),
    
    div(
      class = "indicador-grafico",
      plotlyOutput(
        id_grafico,
        height = "215px"
      )
    ),
    
    div(
      class = "indicador-footer",
      
      div(
        class = "resultado-badge alcancou",
        tags$small("ALCANÇOU"),
        tags$strong(
          linha$Alcancou
        )
      ),
      
      div(
        class = "resultado-badge nao-alcancou",
        tags$small("NÃO ALCANÇOU"),
        tags$strong(
          linha$Nao_Alcancou
        )
      )
    )
  )
}


grafico_barra_indicador <- function(linha) {
  
  dados_grafico <- data.frame(
    Situacao = c(
      "NÃO ALCANÇOU",
      "ALCANÇOU"
    ),
    Quantidade = c(
      linha$Nao_Alcancou,
      linha$Alcancou
    ),
    stringsAsFactors = FALSE
  )
  
  dados_grafico$Rotulo <- paste0(
    dados_grafico$Quantidade,
    ifelse(
      dados_grafico$Quantidade == 1,
      " município",
      " municípios"
    )
  )
  
  limite_x <- max(
    c(
      linha$Total_Municipios,
      dados_grafico$Quantidade
    ),
    na.rm = TRUE
  )
  
  if (!is.finite(limite_x) || limite_x <= 0) {
    limite_x <- 1
  }
  
  limite_x <- limite_x * 1.22
  
  plot_ly(
    data = dados_grafico,
    x = ~Quantidade,
    y = ~Situacao,
    type = "bar",
    orientation = "h",
    text = ~Rotulo,
    textposition = "outside",
    cliponaxis = FALSE,
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "%{x} municípios",
      "<extra></extra>"
    ),
    marker = list(
      color = c(
        "#B4543A",
        "#1B7F5A"
      ),
      line = list(
        color = c(
          "#9D4633",
          "#156C4D"
        ),
        width = 1
      )
    )
  ) |>
    layout(
      showlegend = FALSE,
      bargap = 0.46,
      margin = list(
        l = 112,
        r = 54,
        t = 14,
        b = 35
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(
        title = "",
        range = c(
          0,
          limite_x
        ),
        zeroline = FALSE,
        showline = FALSE,
        showgrid = TRUE,
        gridcolor = "#EDF1F4",
        tickfont = list(
          color = "#64748B",
          size = 10
        ),
        rangemode = "tozero"
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = c(
          "NÃO ALCANÇOU",
          "ALCANÇOU"
        ),
        tickfont = list(
          color = "#334155",
          size = 11
        ),
        fixedrange = TRUE
      ),
      font = list(
        family = "Segoe UI, Arial, sans-serif",
        color = "#334155"
      )
    ) |>
    config(
      displayModeBar = FALSE,
      responsive = TRUE
    )
}


cards_indicadores <- lapply(
  seq_len(
    nrow(
      Resumo_Indicadores
    )
  ),
  function(i) {
    card_indicador(
      Resumo_Indicadores[i, ]
    )
  }
)


# =========================================================
# 7. INTERFACE
# =========================================================

ui <- page_navbar(
  title = "PQAVS 2025",
  theme = tema_pqavs,
  fillable = FALSE,
  
  header = tags$head(
    tags$style(
      HTML(
        css_pqavs
      )
    )
  ),
  
  
  # -------------------------------------------------------
  # RESULTADOS
  # -------------------------------------------------------
  
  nav_panel(
    "Resultados",
    
    div(
      class = "dashboard-container",
      
      div(
        class = "hero",
        h2(
          "Resultados dos Indicadores — PQAVS 2025"
        ),
        p(
          paste(
            "Resultado individual de cada indicador,",
            "com a quantidade de municípios que alcançaram",
            "e não alcançaram a meta."
          )
        )
      ),
      
      div(
        class = "section-heading",
        h3(
          "Resultado por indicador"
        ),
        p(
          paste(
            "Cada gráfico apresenta separadamente",
            "o número de municípios em cada situação",
            "de cumprimento da meta."
          )
        )
      ),
      
      div(
        class = "indicadores-grid",
        cards_indicadores
      ),
      
      div(
        class = "section-heading",
        h3(
          "Estatísticas descritivas dos indicadores"
        )
      ),
      
      div(
        class = "pqavs-table-wrap",
        uiOutput(
          "tabela_resultados"
        )
      )
    )
  ),
  
  # -------------------------------------------------------
  # MUNICÍPIOS
  # -------------------------------------------------------
  
  nav_panel(
    "Municípios",
    
    div(
      class = "dashboard-container",
      
      div(
        class = "hero",
        h2(
          "Consulta municipal"
        ),
        p(
          paste(
            "A tabela inicia com uma amostra reduzida.",
            "Use os filtros para localizar um município específico."
          )
        )
      ),
      
      layout_columns(
        col_widths = c(
          3,
          3,
          3,
          3
        ),
        
        value_box(
          title = "Municípios",
          value = fmt_num(
            Total_Municipios
          ),
          theme = "primary"
        ),
        
        value_box(
          title = "Unidades Federativas",
          value = fmt_num(
            Total_UFs
          ),
          theme = "secondary"
        ),
        
        value_box(
          title = "Incentivo municipal",
          value = if (
            is.na(
              Total_Incentivo_Municipios
            )
          ) {
            "Não disponível"
          } else {
            fmt_moeda(
              Total_Incentivo_Municipios
            )
          },
          theme = "success"
        ),
        
        
        value_box(
          title = "Valor a repassar",
          value = if (
            is.na(
              Total_Valor_Repassar_Municipios
            )
          ) {
            "Não disponível"
          } else {
            fmt_moeda(
              Total_Valor_Repassar_Municipios
            )
          },
          theme = "secondary"
        )
      ),
      
      div(
        class = "filtros-card",
        
        layout_columns(
          col_widths = c(
            3,
            3,
            4,
            2
          ),
          
          selectInput(
            "filtro_uf_mun",
            "UF",
            choices = c(
              "Todas",
              sort(
                unique(
                  Dados_Completos$UF
                )
              )
            ),
            selected = "Todas"
          ),
          
          selectInput(
            "filtro_porte_mun",
            "Porte",
            choices = c(
              "Todos",
              sort(
                unique(
                  na.omit(
                    Dados_Completos$PORTE
                  )
                )
              )
            ),
            selected = "Todos"
          ),
          
          textInput(
            "filtro_nome_mun",
            "Município ou código",
            placeholder = "Ex.: Recife ou 2611606"
          ),
          
          actionButton(
            "limpar_mun",
            "Limpar filtros",
            class = "btn btn-outline-secondary",
            style = "margin-top: 31px;"
          )
        )
      ),
      
      div(
        class = "info-mini",
        "Sem filtros, são exibidos apenas os primeiros 30 municípios."
      ),
      
      card(
        class = "pqavs-card",
        full_screen = TRUE,
        card_header(
          "Municípios"
        ),
        DTOutput(
          "tabela_municipios"
        )
      )
    )
  ),
  
  
  # -------------------------------------------------------
  # ESTADOS
  # -------------------------------------------------------
  
  nav_panel(
    "Estados",
    
    div(
      class = "dashboard-container",
      
      div(
        class = "hero",
        h2(
          "Painel Estadual PQAVS 2025"
        ),
        p(
          paste(
            "Visão executiva de desempenho,",
            "distribuição das metas e recursos por Estado."
          )
        )
      ),
      
      layout_columns(
        col_widths = c(
          6,
          6
        ),
        
        value_box(
          title = "Quantidade de UFs",
          value = fmt_num(
            nrow(Estados)
          ),
          theme = "primary"
        ),
        
        value_box(
          title = "Valor total a repassar",
          value = if (
            is.na(Total_Valor_Repassar_Estados)
          ) {
            "Não disponível"
          } else {
            fmt_moeda(
              Total_Valor_Repassar_Estados
            )
          },
          theme = "success"
        )
      ),
      
      div(
        class = "section-heading",
        h3(
          "Ranking de desempenho estadual"
        )
      ),
      
      div(
        class = "grafico-card",
        plotlyOutput(
          "grafico_ranking_estados",
          height = "450px"
        ),
        div(
          class = "grafico-nota",
          "Este gráfico apresenta o ranking dos Estados conforme o percentual de municípios que alcançaram 90% ou mais das metas previstas. Os Estados são ordenados do menor para o maior desempenho e a linha tracejada representa a média nacional."
        )
      ),
      
      div(
        class = "section-heading",
        h3(
          "Distribuição das faixas de desempenho"
        )
      ),
      
      div(
        class = "grafico-card",
        plotlyOutput(
          "grafico_faixas_estados",
          height = "450px"
        ),
        div(
          class = "grafico-nota",
          "Este gráfico mostra como os municípios de cada Estado estão distribuídos nas faixas de desempenho. A barra é composta pelas proporções de municípios com 90% ou mais, 70% a 89%, 50% a 69% e abaixo de 50% das metas alcançadas. Passe o mouse sobre as barras para ver os detalhes."
        )
      ),
      
      div(
        class = "section-heading",
        h3(
          "Valor a repassar por Estado"
        )
      ),
      
      div(
        class = "grafico-card",
        plotlyOutput(
          "grafico_repassar_estados",
          height = "450px"
        ),
        div(
          class = "grafico-nota",
          "Este gráfico apresenta o ranking financeiro dos Estados considerando o Valor a Repassar. Ele permite comparar a distribuição dos recursos previstos entre as Unidades Federativas."
        )
      ),
      
      div(
        class = "section-heading",
        h3(
          "Desempenho x Valor a repassar"
        )
      ),
      
      div(
        class = "grafico-card",
        plotOutput(
          "grafico_dispersao_estados",
          height = "450px"
        ),
        div(
          class = "grafico-nota",
          "Este gráfico analisa a relação entre desempenho e recurso financeiro. Cada ponto representa um Estado: quanto mais à direita, maior o percentual de municípios com alto desempenho; quanto mais acima, maior o valor a repassar."
        )
      ),
      
      div(
        class = "section-heading",
        h3(
          "Tabela estadual"
        )
      ),
      
      div(
        class = "pqavs-table-wrap",
        uiOutput(
          "tabela_estados"
        )
      )
    )
  )
  
)


# =========================================================
# 8. SERVIDOR
# =========================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # -------------------------------------------------------
  # GRÁFICOS INDIVIDUAIS DOS INDICADORES
  # -------------------------------------------------------
  
  lapply(
    seq_len(
      nrow(
        Resumo_Indicadores
      )
    ),
    function(i) {
      
      local({
        
        linha <- Resumo_Indicadores[
          i,
        ]
        
        id_grafico <- paste0(
          "grafico_",
          tolower(
            linha$Indicador
          )
        )
        
        output[[id_grafico]] <- renderPlotly({
          
          grafico_barra_indicador(
            linha
          )
        })
      })
    }
  )
  
  
  # -------------------------------------------------------
  # GRÁFICOS — ABA ESTADOS
  # -------------------------------------------------------
  
  output$grafico_ranking_estados <- renderPlotly({
    grafico_ranking_estados()
  })
  
  output$grafico_faixas_estados <- renderPlotly({
    grafico_faixas_estados_plotly()
  })
  
  output$grafico_repassar_estados <- renderPlotly({
    grafico_repassar_estados()
  })
  
  # Faltava esse render — o plotOutput já existia na UI,
  # mas o gráfico nunca era gerado.
  output$grafico_dispersao_estados <- renderPlot({
    grafico_dispersao_estados()
  })
  
  
  # -------------------------------------------------------
  # TABELA RESUMO
  # -------------------------------------------------------
  
  output$tabela_resultados <- renderUI({
    
    HTML(
      as.character(
        kable_pqavs(
          Tabela_Descritivas,
          altura = "620px",
          font_size = 12
        )
      )
    )
  })
  
  
  # -------------------------------------------------------
  # MUNICÍPIOS
  # -------------------------------------------------------
  
  observeEvent(
    input$limpar_mun,
    {
      updateSelectInput(
        session,
        "filtro_uf_mun",
        selected = "Todas"
      )
      
      updateSelectInput(
        session,
        "filtro_porte_mun",
        selected = "Todos"
      )
      
      updateTextInput(
        session,
        "filtro_nome_mun",
        value = ""
      )
    }
  )
  
  dados_municipios_filtrados <- reactive({
    
    dados <- Dados_Completos
    
    tem_filtro <- FALSE
    
    if (
      !is.null(
        input$filtro_uf_mun
      ) &&
      input$filtro_uf_mun != "Todas"
    ) {
      dados <- dados |>
        filter(
          UF == input$filtro_uf_mun
        )
      
      tem_filtro <- TRUE
    }
    
    if (
      !is.null(
        input$filtro_porte_mun
      ) &&
      input$filtro_porte_mun != "Todos"
    ) {
      dados <- dados |>
        filter(
          as.character(
            PORTE
          ) == input$filtro_porte_mun
        )
      
      tem_filtro <- TRUE
    }
    
    termo <- trimws(
      input$filtro_nome_mun %||% ""
    )
    
    if (
      nzchar(
        termo
      )
    ) {
      
      termo_min <- str_to_lower(
        termo
      )
      
      dados <- dados |>
        filter(
          str_detect(
            str_to_lower(
              coalesce(
                NOME_MUN,
                ""
              )
            ),
            fixed(
              termo_min
            )
          ) |
            str_detect(
              coalesce(
                as.character(
                  COD_MUN
                ),
                ""
              ),
              fixed(
                termo
              )
            )
        )
      
      tem_filtro <- TRUE
    }
    
    dados <- dados |>
      select(
        any_of(
          c(
            "UF",
            "COD_MUN",
            "NOME_MUN",
            "POP",
            "PORTE",
            "METAS_ALCANCADAS",
            "PERCENTUAL_METAS",
            "Valor_a_Repassar",
            "PQAVS_Incentivo"
          )
        )
      ) |>
      arrange(
        UF,
        NOME_MUN
      )
    
    if (
      !tem_filtro
    ) {
      dados <- dados |>
        slice_head(
          n = 30
        )
    }
    
    dados
  })
  
  
  
  output$tabela_municipios <- renderDT({
    
    tabela <- dados_municipios_filtrados()
    
    if ("POP" %in% names(tabela)) {
      tabela <- tabela |>
        mutate(
          POP = fmt_num(POP)
        )
    }
    
    if ("PERCENTUAL_METAS" %in% names(tabela)) {
      tabela <- tabela |>
        mutate(
          PERCENTUAL_METAS = fmt_pct(
            PERCENTUAL_METAS,
            0
          )
        )
    }
    
    if ("Valor_a_Repassar" %in% names(tabela)) {
      tabela <- tabela |>
        mutate(
          Valor_a_Repassar = fmt_moeda(
            Valor_a_Repassar
          )
        )
    }
    
    if ("PQAVS_Incentivo" %in% names(tabela)) {
      tabela <- tabela |>
        mutate(
          PQAVS_Incentivo = fmt_moeda(
            PQAVS_Incentivo
          )
        )
    }
    
    datatable(
      tabela,
      rownames = FALSE,
      filter = "none",
      options = list(
        pageLength = 15,
        lengthMenu = c(
          15,
          30,
          50
        ),
        scrollX = TRUE,
        autoWidth = TRUE,
        deferRender = TRUE,
        searchDelay = 500,
        language = list(
          search = "Buscar na tabela:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_",
          zeroRecords = "Nenhum município encontrado",
          paginate = list(
            previous = "Anterior",
            `next` = "Próximo"
          )
        )
      )
    )
    
  }, server = TRUE)
  
  
  
  # -------------------------------------------------------
  # ESTADOS — TABELA
  # -------------------------------------------------------
  
  output$tabela_estados <- renderUI({
    
    tabela <- Estados |>
      arrange(
        UF
      )
    
    colunas_fin <- intersect(
      c("PQAVS_Incentivo", "Valor_a_Repassar"),
      names(tabela)
    )
    
    if(length(colunas_fin) > 0){
      tabela <- tabela |>
        mutate(
          across(
            all_of(colunas_fin),
            fmt_moeda
          )
        )
    }
    
    colunas_pct <- names(
      tabela
    )[
      str_detect(
        names(
          tabela
        ),
        "_%$"
      )
    ]
    
    if (
      length(
        colunas_pct
      ) > 0
    ) {
      tabela <- tabela |>
        mutate(
          across(
            all_of(
              colunas_pct
            ),
            ~ fmt_pct(
              .x,
              2
            )
          )
        )
    }
    
    HTML(
      as.character(
        kable_pqavs(
          tabela,
          altura = "620px",
          font_size = 12
        )
      )
    )
  })
  
}


# =========================================================
# 9. APLICAÇÃO
# =========================================================

shinyApp(
  ui = ui,
  server = server
)