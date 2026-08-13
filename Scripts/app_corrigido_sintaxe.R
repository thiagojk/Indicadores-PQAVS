# =========================================================
# Dashboard PQAVS
# app.R
#
# Este arquivo NÃO altera o código de cálculo.
# Ele apenas executa source() no script final e usa os objetos
# Indicadores, Metas, Descritivas e Indicadores_Completo.
# =========================================================

pacotes <- c(
  "shiny",
  "bslib",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "plotly",
  "DT",
  "openxlsx"
)

faltantes <- pacotes[
  !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "Instale os pacotes ausentes antes de abrir o dashboard: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(openxlsx)


# =========================================================
# 1. Source do código final
# =========================================================

pasta_scripts <- getwd()
setwd("..")
source(file.path("Scripts", "calculo_indicadores.R"), encoding = "UTF-8")
setwd(pasta_scripts)


# =========================================================
# 2. Funções auxiliares apenas para apresentação
# =========================================================

detectar_coluna_municipio <- function(dados) {

  candidatos <- c(
    "NOME_MUN",
    "Município",
    "Municipio",
    "COD_MUN",
    "cod_mun",
    "CD_MUN",
    "COD_IBGE"
  )

  encontrada <- candidatos[
    candidatos %in% names(dados)
  ]

  if (length(encontrada) > 0) {
    return(encontrada[1])
  }


  encontrada_regex <- names(dados)[
    str_detect(
      toupper(names(dados)),
      "NOME_MUN|MUNIC[IÍ]PIO|COD_MUN|CD_MUN|COD_IBGE"
    )
  ]

  if (length(encontrada_regex) > 0) {
    return(encontrada_regex[1])
  }

  NULL
}


formatar_numero <- function(x) {

  if (
    length(x) == 0 ||
    is.na(x) ||
    is.nan(x) ||
    is.infinite(x)
  ) {
    return("—")
  }

  format(
    x,
    digits = 15,
    scientific = FALSE,
    trim = TRUE
  )
}


formatar_inteiro <- function(x) {

  if (
    length(x) == 0 ||
    is.na(x)
  ) {
    return("—")
  }

  format(
    x,
    scientific = FALSE,
    trim = TRUE
  )
}


criar_base_dashboard <- function(lista_indicadores) {

  bind_rows(
    lapply(
      names(lista_indicadores),
      function(nome) {

        dados <- lista_indicadores[[nome]]
        coluna_municipio <- detectar_coluna_municipio(dados)

        municipio <- if (is.null(coluna_municipio)) {
          as.character(seq_len(nrow(dados)))
        } else {
          as.character(dados[[coluna_municipio]])
        }

        tibble(
          Indicador = nome,
          Municipio = municipio,
          RES = suppressWarnings(as.numeric(dados$RES)),
          METAS = as.character(dados$METAS)
        )
      }
    )
  )
}


Base_Dashboard <- criar_base_dashboard(
  Indicadores
)


resumo_global <- Base_Dashboard %>%
  summarise(
    Total_Avaliacoes = n(),
    Alcancou = sum(METAS == "ALCANÇOU", na.rm = TRUE),
    Nao_Alcancou = sum(METAS == "NÃO ALCANÇOU", na.rm = TRUE)
  ) %>%
  mutate(
    Pct_Alcancou = if_else(
      Total_Avaliacoes > 0,
      100 * Alcancou / Total_Avaliacoes,
      NA_real_
    )
  )


# =========================================================
# 3. Tema
# =========================================================

tema_pqavs <- bs_theme(
  version = 5,
  bg = "#F4F7F9",
  fg = "#17222E",
  primary = "#176B57",
  secondary = "#2E6F95",
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
  --pqavs-green: #176B57;
  --pqavs-green-dark: #0F4F40;
  --pqavs-blue: #2E6F95;
  --pqavs-orange: #C56A3A;
  --pqavs-red: #B4543A;
  --pqavs-ink: #17222E;
  --pqavs-muted: #64748B;
  --pqavs-border: #E3E9EE;
  --pqavs-bg: #F4F7F9;
}

body {
  background: var(--pqavs-bg);
}

.navbar {
  box-shadow: 0 2px 16px rgba(24, 39, 56, 0.08);
}

.navbar-brand {
  font-weight: 750;
  letter-spacing: -0.02em;
}

.pqavs-page {
  max-width: 1600px;
  margin: 0 auto;
  padding: 26px 18px 42px 18px;
}

.pqavs-hero {
  background:
    radial-gradient(circle at top right, rgba(255,255,255,0.18), transparent 32%),
    linear-gradient(120deg, #0F4F40 0%, #176B57 52%, #2E6F95 100%);
  color: #FFFFFF;
  border-radius: 22px;
  padding: 30px 34px;
  margin-bottom: 22px;
  box-shadow: 0 18px 40px rgba(15, 79, 64, 0.16);
}

.pqavs-hero h2 {
  font-size: clamp(1.7rem, 2.6vw, 2.45rem);
  font-weight: 760;
  margin: 0 0 7px 0;
  letter-spacing: -0.035em;
}

.pqavs-hero p {
  margin: 0;
  opacity: 0.88;
  font-size: 1rem;
}

.pqavs-card {
  border: 1px solid var(--pqavs-border);
  border-radius: 18px;
  box-shadow: 0 8px 28px rgba(30, 51, 73, 0.06);
  background: #FFFFFF;
}

.pqavs-card .card-header {
  background: transparent;
  border-bottom: 1px solid var(--pqavs-border);
  font-weight: 700;
  padding: 16px 18px;
}

.bslib-value-box {
  border: 0 !important;
  border-radius: 18px !important;
  box-shadow: 0 8px 28px rgba(30, 51, 73, 0.07);
  overflow: hidden;
}

.bslib-value-box .value-box-value {
  font-weight: 780;
  letter-spacing: -0.03em;
}

.pqavs-filter {
  background: #FFFFFF;
  border: 1px solid var(--pqavs-border);
  border-radius: 18px;
  padding: 18px;
  box-shadow: 0 8px 28px rgba(30, 51, 73, 0.05);
}

.pqavs-section-title {
  font-size: 1.08rem;
  font-weight: 730;
  margin-bottom: 13px;
  color: var(--pqavs-ink);
}

.pqavs-note {
  color: var(--pqavs-muted);
  font-size: 0.88rem;
}

.form-control,
.form-select,
.selectize-input {
  border-radius: 11px !important;
  border-color: #D7E0E7 !important;
}

.btn-primary {
  border-radius: 11px;
  font-weight: 650;
}

.dataTables_wrapper {
  font-size: 0.9rem;
}

table.dataTable thead th {
  background: #F6F8FA;
  color: #334155;
}

@media (max-width: 768px) {
  .pqavs-page {
    padding: 14px 10px 28px 10px;
  }

  .pqavs-hero {
    border-radius: 16px;
    padding: 23px 20px;
  }
}
"


# =========================================================
# 4. Interface
# =========================================================

ui <- page_navbar(

  title = "PQAVS 2025",
  theme = tema_pqavs,
  fillable = FALSE,

  header = tags$head(
    tags$style(
      HTML(css_pqavs)
    )
  ),


  nav_panel(
    "Visão geral",

    div(
      class = "pqavs-page",

      div(
        class = "pqavs-hero",
        h2("Painel de Resultados — PQAVS 2025"),
        p(
          paste(
            "Visão consolidada do desempenho dos indicadores,",
            "com foco no alcance das metas e na distribuição dos resultados."
          )
        )
      ),

      layout_columns(
        col_widths = c(3, 3, 3, 3),

        value_box(
          title = "Indicadores",
          value = textOutput("kpi_indicadores"),
          theme = "primary"
        ),

        value_box(
          title = "Avaliações",
          value = textOutput("kpi_avaliacoes"),
          theme = "secondary"
        ),

        value_box(
          title = "Metas alcançadas",
          value = textOutput("kpi_alcancou"),
          theme = "success"
        ),

        value_box(
          title = "Taxa global de alcance",
          value = textOutput("kpi_pct"),
          theme = "primary"
        )
      ),

      br(),

      layout_columns(
        col_widths = c(8, 4),

        card(
          class = "pqavs-card",
          full_screen = TRUE,
          card_header("Percentual de alcance por indicador"),
          plotlyOutput(
            "grafico_pct_indicador",
            height = "430px"
          )
        ),

        card(
          class = "pqavs-card",
          full_screen = TRUE,
          card_header("Resultado global"),
          plotlyOutput(
            "grafico_global",
            height = "430px"
          )
        )
      ),

      br(),

      card(
        class = "pqavs-card",
        full_screen = TRUE,
        card_header("Resumo estatístico dos indicadores"),
        DTOutput("tabela_descritivas")
      )
    )
  ),


  nav_panel(
    "Explorar indicador",

    div(
      class = "pqavs-page",

      div(
        class = "pqavs-hero",
        h2("Exploração por indicador"),
        p(
          paste(
            "Selecione um indicador e filtre municípios e situação da meta",
            "para investigar os resultados em detalhe."
          )
        )
      ),

      layout_columns(
        col_widths = c(3, 9),

        div(
          class = "pqavs-filter",

          div(
            class = "pqavs-section-title",
            "Filtros"
          ),

          selectInput(
            "indicador_explorar",
            "Indicador",
            choices = names(Indicadores),
            selected = names(Indicadores)[1]
          ),

          selectInput(
            "situacao_explorar",
            "Situação da meta",
            choices = c(
              "Todos" = "",
              "ALCANÇOU" = "ALCANÇOU",
              "NÃO ALCANÇOU" = "NÃO ALCANÇOU"
            ),
            selected = ""
          ),

          selectizeInput(
            "municipio_explorar",
            "Município",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Todos os municípios"
            )
          ),

          div(
            class = "pqavs-note",
            paste(
              "Os filtros afetam os gráficos e a tabela desta aba.",
              "Os cálculos originais permanecem inalterados."
            )
          )
        ),

        div(

          layout_columns(
            col_widths = c(3, 3, 3, 3),

            value_box(
              title = "Registros",
              value = textOutput("kpi_exp_total"),
              theme = "secondary"
            ),

            value_box(
              title = "Alcançou",
              value = textOutput("kpi_exp_alcancou"),
              theme = "success"
            ),

            value_box(
              title = "Não alcançou",
              value = textOutput("kpi_exp_nao"),
              theme = "danger"
            ),

            value_box(
              title = "Média de RES",
              value = textOutput("kpi_exp_media"),
              theme = "primary"
            )
          ),

          br(),

          layout_columns(
            col_widths = c(7, 5),

            card(
              class = "pqavs-card",
              full_screen = TRUE,
              card_header("Resultados por município"),
              plotlyOutput(
                "grafico_ranking",
                height = "510px"
              )
            ),

            card(
              class = "pqavs-card",
              full_screen = TRUE,
              card_header("Distribuição de RES"),
              plotlyOutput(
                "grafico_distribuicao",
                height = "510px"
              )
            )
          )
        )
      ),

      br(),

      card(
        class = "pqavs-card",
        full_screen = TRUE,
        card_header("Registros filtrados"),
        DTOutput("tabela_explorar")
      )
    )
  ),


  nav_panel(
    "Comparativo",

    div(
      class = "pqavs-page",

      div(
        class = "pqavs-hero",
        h2("Comparação entre indicadores"),
        p(
          paste(
            "Compare a taxa de alcance das metas, o resultado médio",
            "e a distribuição de RES entre os indicadores."
          )
        )
      ),

      layout_columns(
        col_widths = c(7, 5),

        card(
          class = "pqavs-card",
          full_screen = TRUE,
          card_header("Taxa de alcance"),
          plotlyOutput(
            "grafico_comparativo_pct",
            height = "480px"
          )
        ),

        card(
          class = "pqavs-card",
          full_screen = TRUE,
          card_header("Percentual de alcance × resultado médio"),
          plotlyOutput(
            "grafico_media_pct",
            height = "480px"
          )
        )
      ),

      br(),

      card(
        class = "pqavs-card",
        full_screen = TRUE,
        card_header("Distribuição de RES por indicador"),
        plotlyOutput(
          "grafico_boxplot",
          height = "520px"
        )
      ),

      br(),

      card(
        class = "pqavs-card",
        full_screen = TRUE,
        card_header("Estatísticas comparativas"),
        DTOutput("tabela_comparativo")
      )
    )
  ),


  nav_panel(
    "Dados",

    div(
      class = "pqavs-page",

      div(
        class = "pqavs-hero",
        h2("Consulta e exportação"),
        p(
          paste(
            "Acesse os registros completos de cada indicador",
            "diretamente a partir dos objetos gerados pelo código final."
          )
        )
      ),

      layout_columns(
        col_widths = c(3, 9),

        div(
          class = "pqavs-filter",

          div(
            class = "pqavs-section-title",
            "Consulta"
          ),

          selectInput(
            "indicador_dados",
            "Indicador",
            choices = names(Indicadores),
            selected = names(Indicadores)[1]
          ),

          selectInput(
            "situacao_dados",
            "Situação da meta",
            choices = c(
              "Todos" = "",
              "ALCANÇOU" = "ALCANÇOU",
              "NÃO ALCANÇOU" = "NÃO ALCANÇOU"
            ),
            selected = ""
          ),

          downloadButton(
            "baixar_dados",
            "Baixar seleção em Excel",
            class = "btn-primary w-100"
          )
        ),

        card(
          class = "pqavs-card",
          full_screen = TRUE,
          card_header(
            textOutput("titulo_tabela_dados")
          ),
          DTOutput("tabela_dados")
        )
      )
    )
  ),


  nav_spacer(),

  nav_item(
    span(
      style = "opacity:.72;font-size:.86rem;",
      "Resultados PQAVS"
    )
  )
)


# =========================================================
# 5. Servidor
# =========================================================

server <- function(input, output, session) {


  # -------------------------------------------------------
  # KPIs gerais
  # -------------------------------------------------------

  output$kpi_indicadores <- renderText({
    formatar_inteiro(
      length(Indicadores)
    )
  })


  output$kpi_avaliacoes <- renderText({
    formatar_inteiro(
      resumo_global$Total_Avaliacoes
    )
  })


  output$kpi_alcancou <- renderText({
    formatar_inteiro(
      resumo_global$Alcancou
    )
  })


  output$kpi_pct <- renderText({
    paste0(
      formatar_numero(
        resumo_global$Pct_Alcancou
      ),
      "%"
    )
  })


  # -------------------------------------------------------
  # Visão geral
  # -------------------------------------------------------

  output$grafico_pct_indicador <- renderPlotly({

    dados <- Descritivas %>%
      arrange(Pct_Alcancou) %>%
      mutate(
        texto = paste0(
          "<b>", Indicador, "</b><br>",
          "Alcançou: ",
          formatar_inteiro(Qtd_Alcancou),
          "<br>Não alcançou: ",
          formatar_inteiro(Qtd_Nao_Alcancou),
          "<br>Percentual: ",
          vapply(Pct_Alcancou, formatar_numero, character(1)),
          "%"
        )
      )


    grafico <- ggplot(
      dados,
      aes(
        x = reorder(Indicador, Pct_Alcancou),
        y = Pct_Alcancou,
        text = texto
      )
    ) +
      geom_col(
        fill = "#176B57",
        width = 0.68
      ) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Percentual de alcance (%)"
      ) +
      theme_minimal(
        base_size = 12
      ) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(
          face = "bold",
          color = "#334155"
        ),
        axis.title.x = element_text(
          color = "#64748B"
        ),
        plot.margin = margin(
          12, 18, 12, 8
        )
      )


    ggplotly(
      grafico,
      tooltip = "text"
    ) %>%
      config(
        displayModeBar = FALSE
      )
  })


  output$grafico_global <- renderPlotly({

    dados <- tibble(
      Situacao = c(
        "ALCANÇOU",
        "NÃO ALCANÇOU"
      ),
      Quantidade = c(
        resumo_global$Alcancou,
        resumo_global$Nao_Alcancou
      )
    )


    plot_ly(
      dados,
      labels = ~Situacao,
      values = ~Quantidade,
      type = "pie",
      hole = 0.67,
      textinfo = "label+value",
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "%{value} avaliações",
        "<extra></extra>"
      ),
      marker = list(
        colors = c(
          "#1B7F5A",
          "#B4543A"
        ),
        line = list(
          color = "#FFFFFF",
          width = 3
        )
      )
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(
          l = 8,
          r = 8,
          b = 8,
          t = 8
        )
      ) %>%
      config(
        displayModeBar = FALSE
      )
  })


  output$tabela_descritivas <- renderDT({

    datatable(
      Descritivas,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 14,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c(
          "copy",
          "csv",
          "excel"
        ),
        language = list(
          search = "Buscar:",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "Nenhum registro",
          zeroRecords = "Nenhum resultado encontrado",
          paginate = list(
            first = "Primeiro",
            last = "Último",
            `next` = "Próximo",
            previous = "Anterior"
          )
        )
      )
    )
  })


  # -------------------------------------------------------
  # Exploração por indicador
  # -------------------------------------------------------

  observeEvent(
    input$indicador_explorar,
    {

      dados <- Base_Dashboard %>%
        filter(
          Indicador == input$indicador_explorar
        )

      municipios <- sort(
        unique(
          dados$Municipio[
            !is.na(dados$Municipio)
          ]
        )
      )

      updateSelectizeInput(
        session,
        "municipio_explorar",
        choices = municipios,
        selected = character(0),
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )


  dados_explorar <- reactive({

    req(
      input$indicador_explorar
    )

    dados <- Base_Dashboard %>%
      filter(
        Indicador == input$indicador_explorar
      )


    if (
      !is.null(input$situacao_explorar) &&
      nzchar(input$situacao_explorar)
    ) {

      dados <- dados %>%
        filter(
          METAS == input$situacao_explorar
        )
    }


    if (
      !is.null(input$municipio_explorar) &&
      length(input$municipio_explorar) > 0
    ) {

      dados <- dados %>%
        filter(
          Municipio %in% input$municipio_explorar
        )
    }


    dados
  })


  output$kpi_exp_total <- renderText({
    formatar_inteiro(
      nrow(dados_explorar())
    )
  })


  output$kpi_exp_alcancou <- renderText({

    dados <- dados_explorar()

    formatar_inteiro(
      sum(
        dados$METAS == "ALCANÇOU",
        na.rm = TRUE
      )
    )
  })


  output$kpi_exp_nao <- renderText({

    dados <- dados_explorar()

    formatar_inteiro(
      sum(
        dados$METAS == "NÃO ALCANÇOU",
        na.rm = TRUE
      )
    )
  })


  output$kpi_exp_media <- renderText({

    dados <- dados_explorar()

    valor <- mean(
      dados$RES,
      na.rm = TRUE
    )

    formatar_numero(
      valor
    )
  })


  output$grafico_ranking <- renderPlotly({

    dados <- dados_explorar() %>%
      filter(
        !is.na(RES)
      ) %>%
      arrange(
        desc(RES)
      ) %>%
      slice_head(
        n = 30
      ) %>%
      mutate(
        Municipio = factor(
          Municipio,
          levels = rev(Municipio)
        ),
        texto = paste0(
          "<b>", Municipio, "</b><br>",
          "RES: ",
          vapply(RES, formatar_numero, character(1)),
          "<br>Situação: ",
          METAS
        )
      )


    validate(
      need(
        nrow(dados) > 0,
        "Não há valores de RES para os filtros selecionados."
      )
    )


    grafico <- ggplot(
      dados,
      aes(
        x = Municipio,
        y = RES,
        fill = METAS,
        text = texto
      )
    ) +
      geom_col(
        width = 0.72
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "ALCANÇOU" = "#1B7F5A",
          "NÃO ALCANÇOU" = "#B4543A"
        ),
        drop = FALSE
      ) +
      labs(
        x = NULL,
        y = "RES",
        fill = NULL
      ) +
      theme_minimal(
        base_size = 11
      ) +
      theme(
        legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(
          size = 9
        )
      )


    ggplotly(
      grafico,
      tooltip = "text"
    ) %>%
      config(
        displayModeBar = FALSE
      )
  })


  output$grafico_distribuicao <- renderPlotly({

    dados <- dados_explorar() %>%
      filter(
        !is.na(RES)
      )


    validate(
      need(
        nrow(dados) > 0,
        "Não há valores de RES para os filtros selecionados."
      )
    )


    grafico <- ggplot(
      dados,
      aes(
        x = RES,
        fill = METAS,
        text = paste0(
          "RES: ",
          vapply(RES, formatar_numero, character(1)),
          "<br>Situação: ",
          METAS
        )
      )
    ) +
      geom_histogram(
        bins = 24,
        alpha = 0.86,
        position = "identity"
      ) +
      scale_fill_manual(
        values = c(
          "ALCANÇOU" = "#1B7F5A",
          "NÃO ALCANÇOU" = "#B4543A"
        ),
        drop = FALSE
      ) +
      labs(
        x = "RES",
        y = "Quantidade",
        fill = NULL
      ) +
      theme_minimal(
        base_size = 11
      ) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )


    ggplotly(
      grafico,
      tooltip = "text"
    ) %>%
      config(
        displayModeBar = FALSE
      )
  })


  output$tabela_explorar <- renderDT({

    dados <- dados_explorar()

    datatable(
      dados,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          zeroRecords = "Nenhum resultado encontrado"
        )
      )
    ) %>%
      formatStyle(
        "METAS",
        target = "cell",
        backgroundColor = styleEqual(
          c(
            "ALCANÇOU",
            "NÃO ALCANÇOU"
          ),
          c(
            "#E7F5EE",
            "#FCEBE7"
          )
        ),
        color = styleEqual(
          c(
            "ALCANÇOU",
            "NÃO ALCANÇOU"
          ),
          c(
            "#125B40",
            "#913C2D"
          )
        ),
        fontWeight = "600"
      )
  })


  # -------------------------------------------------------
  # Comparativo
  # -------------------------------------------------------

  output$grafico_comparativo_pct <- renderPlotly({

    dados <- Descritivas %>%
      arrange(Pct_Alcancou) %>%
      mutate(
        texto = paste0(
          "<b>", Indicador, "</b><br>",
          "Percentual alcançou: ",
          vapply(Pct_Alcancou, formatar_numero, character(1)),
          "%<br>Total municípios: ",
          formatar_inteiro(Total_Municipios)
        )
      )


    grafico <- ggplot(
      dados,
      aes(
        x = reorder(Indicador, Pct_Alcancou),
        y = Pct_Alcancou,
        text = texto
      )
    ) +
      geom_col(
        fill = "#2E6F95",
        width = 0.68
      ) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Percentual de alcance (%)"
      ) +
      theme_minimal(
        base_size = 11
      ) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(
          face = "bold"
        )
      )


    ggplotly(
      grafico,
      tooltip = "text"
    ) %>%
      config(
        displayModeBar = FALSE
      )
  })


  output$grafico_media_pct <- renderPlotly({

    dados <- Descritivas %>%
      mutate(
        texto = paste0(
          "<b>", Indicador, "</b><br>",
          "Média RES: ",
          vapply(RES_Media, formatar_numero, character(1)),
          "<br>Percentual alcançou: ",
          vapply(Pct_Alcancou, formatar_numero, character(1)),
          "%"
        )
      )


    grafico <- ggplot(
      dados,
      aes(
        x = RES_Media,
        y = Pct_Alcancou,
        text = texto
      )
    ) +
      geom_point(
        size = 4,
        color = "#176B57",
        alpha = 0.88
      ) +
      geom_text(
        aes(
          label = Indicador
        ),
        nudge_y = 2,
        size = 3,
        color = "#475569",
        check_overlap = TRUE
      ) +
      labs(
        x = "Média de RES",
        y = "Percentual de alcance (%)"
      ) +
      theme_minimal(
        base_size = 11
      ) +
      theme(
        panel.grid.minor = element_blank()
      )


    ggplotly(
      grafico,
      tooltip = "text"
    ) %>%
      config(
        displayModeBar = FALSE
      )
  })


  output$grafico_boxplot <- renderPlotly({

    dados <- Base_Dashboard %>%
      filter(
        !is.na(RES)
      )


    grafico <- ggplot(
      dados,
      aes(
        x = Indicador,
        y = RES,
        fill = Indicador,
        text = paste0(
          "<b>", Indicador, "</b><br>",
          "Município: ",
          Municipio,
          "<br>RES: ",
          vapply(RES, formatar_numero, character(1)),
          "<br>Situação: ",
          METAS
        )
      )
    ) +
      geom_boxplot(
        alpha = 0.78,
        outlier.alpha = 0.42
      ) +
      labs(
        x = NULL,
        y = "RES"
      ) +
      guides(
        fill = "none"
      ) +
      theme_minimal(
        base_size = 11
      ) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )


    ggplotly(
      grafico,
      tooltip = "text"
    ) %>%
      config(
        displayModeBar = FALSE
      )
  })


  output$tabela_comparativo <- renderDT({

    datatable(
      Descritivas,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 14,
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          zeroRecords = "Nenhum resultado encontrado"
        )
      )
    )
  })


  # -------------------------------------------------------
  # Dados completos
  # -------------------------------------------------------

  dados_completos_filtrados <- reactive({

    req(
      input$indicador_dados
    )

    dados <- Indicadores[[input$indicador_dados]]


    if (
      !is.null(input$situacao_dados) &&
      nzchar(input$situacao_dados) &&
      "METAS" %in% names(dados)
    ) {

      dados <- dados %>%
        filter(
          METAS == input$situacao_dados
        )
    }


    dados
  })


  output$titulo_tabela_dados <- renderText({
    paste(
      "Dados completos —",
      input$indicador_dados
    )
  })


  output$tabela_dados <- renderDT({

    dados <- dados_completos_filtrados()

    tabela <- datatable(
      dados,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 18,
        scrollX = TRUE,
        scrollY = "590px",
        language = list(
          search = "Buscar:",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "Nenhum registro",
          zeroRecords = "Nenhum resultado encontrado"
        )
      )
    )


    if ("METAS" %in% names(dados)) {

      tabela <- tabela %>%
        formatStyle(
          "METAS",
          target = "cell",
          backgroundColor = styleEqual(
            c(
              "ALCANÇOU",
              "NÃO ALCANÇOU"
            ),
            c(
              "#E7F5EE",
              "#FCEBE7"
            )
          ),
          color = styleEqual(
            c(
              "ALCANÇOU",
              "NÃO ALCANÇOU"
            ),
            c(
              "#125B40",
              "#913C2D"
            )
          ),
          fontWeight = "600"
        )
    }


    tabela
  })


  output$baixar_dados <- downloadHandler(

    filename = function() {
      paste0(
        input$indicador_dados,
        "_dashboard.xlsx"
      )
    },

    content = function(file) {

      dados <- dados_completos_filtrados()

      wb_download <- createWorkbook()

      addWorksheet(
        wb_download,
        input$indicador_dados
      )

      writeData(
        wb_download,
        input$indicador_dados,
        dados
      )

      saveWorkbook(
        wb_download,
        file,
        overwrite = TRUE
      )
    }
  )
}


# =========================================================
# 6. Aplicação
# =========================================================

shinyApp(
  ui = ui,
  server = server
)
