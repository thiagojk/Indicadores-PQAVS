# =========================================================
# DASHBOARD PQAVS 2025
# app.R
#
# Usa o código de cálculo (Scripts/calculo_indicadores.R)
# sem modificá-lo.
# =========================================================

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)

# =========================================================
# 1. SOURCE DO CÓDIGO DE CÁLCULO
# =========================================================

# O app e o calculo_indicadores.R estão na pasta Scripts.
# O código de cálculo usa caminhos "Dados/...", por isso o
# diretório de trabalho é alterado para a raiz do projeto.
# ATENÇÃO: setwd() muda o diretório de trabalho da sessão inteira,
# não só deste script — mantenha o app.R sempre em Scripts/.
setwd("..")

source(file.path("Scripts", "calculo_indicadores.R"), encoding = "UTF-8")

# =========================================================
# 2. DADOS AGREGADOS PARA O DASHBOARD
# =========================================================

Resumo_Indicadores <- Descritivas |>
  transmute(
    Indicador,
    Total_Municipios,
    Alcancou = Qtd_Alcancou,
    Nao_Alcancou = Qtd_Nao_Alcancou
  )

# Ordena os indicadores numericamente: IND_01, IND_02, ..., IND_14
Resumo_Indicadores <- Resumo_Indicadores[
  order(as.integer(gsub("\\D", "", Resumo_Indicadores$Indicador))),
]
row.names(Resumo_Indicadores) <- NULL

Total_Indicadores <- nrow(Resumo_Indicadores)
Total_Avaliacoes <- sum(Resumo_Indicadores$Total_Municipios, na.rm = TRUE)
Total_Alcancou <- sum(Resumo_Indicadores$Alcancou, na.rm = TRUE)
Total_Nao_Alcancou <- sum(Resumo_Indicadores$Nao_Alcancou, na.rm = TRUE)

# =========================================================
# 3. TEMA, CSS E JAVASCRIPT
# =========================================================

tema_pqavs <- bs_theme(
  version = 5,
  bg = "#F4F7F9",
  fg = "#17222E",
  primary = "#176B57",
  secondary = "#356B8C",
  success = "#1B7F5A",
  danger = "#B4543A",
  base_font = font_collection("Segoe UI", "Arial", "sans-serif")
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
  max-width: 1550px;
  margin: 0 auto;
  padding: 24px 18px 42px 18px;
}

.hero {
  color: white;
  background:
    radial-gradient(
      circle at top right,
      rgba(255, 255, 255, 0.16),
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
  box-shadow: 0 16px 34px rgba(15, 79, 64, 0.15);
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
  color: var(--texto);
}

.section-heading p {
  margin: 0;
  color: var(--texto-secundario);
  font-size: 0.92rem;
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
  box-shadow: 0 8px 25px rgba(27, 44, 63, 0.065);
  overflow: hidden;
  min-width: 0;
  transition:
    transform 0.16s ease,
    box-shadow 0.16s ease;
}

.indicador-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 13px 30px rgba(27, 44, 63, 0.10);
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

.pqavs-card {
  background: white;
  border: 1px solid var(--borda);
  border-radius: 17px;
  box-shadow: 0 7px 24px rgba(27, 44, 63, 0.055);
}

.pqavs-card .card-header {
  background: white;
  border-bottom: 1px solid var(--borda);
  padding: 16px 18px;
  font-weight: 720;
  color: var(--texto);
}

.bslib-value-box {
  border: 0 !important;
  border-radius: 17px !important;
  box-shadow: 0 7px 24px rgba(27, 44, 63, 0.07);
  overflow: hidden;
}

.bslib-value-box .value-box-value {
  font-weight: 780;
  letter-spacing: -0.025em;
}

.dataTables_wrapper {
  font-size: 0.91rem;
}

table.dataTable thead th {
  background: #F6F8FA;
  color: #334155;
  font-weight: 700;
}

@media (max-width: 1100px) {
  .indicadores-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
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

  .indicadores-grid {
    grid-template-columns: 1fr;
  }

  .indicador-card-header {
    padding-left: 15px;
    padding-right: 15px;
  }
}

/* ========================================================
   ACABAMENTO VISUAL CONTROLADO PELO JAVASCRIPT
   ======================================================== */

html {
  scroll-behavior: smooth;
}

.navbar {
  transition:
    box-shadow 0.25s ease,
    background-color 0.25s ease;
}

.navbar.navbar-scrolled {
  box-shadow: 0 8px 24px rgba(20, 38, 55, 0.12);
}

.navbar .nav-link {
  position: relative;
  transition:
    color 0.2s ease,
    opacity 0.2s ease;
}

.navbar .nav-link::after {
  content: '';
  position: absolute;
  left: 50%;
  bottom: 2px;
  width: 0;
  height: 2px;
  border-radius: 999px;
  background: currentColor;
  opacity: 0.78;
  transform: translateX(-50%);
  transition: width 0.25s ease;
}

.navbar .nav-link.active::after {
  width: 55%;
}

.hero {
  --hero-x: 88%;
  --hero-y: 8%;
  background:
    radial-gradient(
      circle at var(--hero-x) var(--hero-y),
      rgba(255, 255, 255, 0.20),
      transparent 34%
    ),
    linear-gradient(
      120deg,
      #0F4F40 0%,
      #176B57 55%,
      #356B8C 100%
    );
  transition: box-shadow 0.25s ease;
}

.hero:hover {
  box-shadow: 0 20px 42px rgba(15, 79, 64, 0.19);
}

.indicador-card {
  position: relative;
  --mouse-x: 50%;
  --mouse-y: 50%;
}

.indicador-card::after {
  content: '';
  position: absolute;
  inset: 0;
  border-radius: inherit;
  pointer-events: none;
  background:
    radial-gradient(
      260px circle at var(--mouse-x) var(--mouse-y),
      rgba(23, 107, 87, 0.085),
      transparent 66%
    );
  opacity: 0;
  transition: opacity 0.22s ease;
}

.indicador-card.js-hover::after {
  opacity: 1;
}

.indicador-card.reveal-visible:hover {
  transform: translateY(-4px);
  box-shadow: 0 16px 34px rgba(27, 44, 63, 0.12);
}

.resultado-badge {
  transition:
    transform 0.2s ease,
    box-shadow 0.2s ease;
}

.resultado-badge:hover {
  transform: translateY(-1px);
}

.resultado-badge.alcancou:hover {
  box-shadow: 0 7px 18px rgba(27, 127, 90, 0.12);
}

.resultado-badge.nao-alcancou:hover {
  box-shadow: 0 7px 18px rgba(180, 84, 58, 0.12);
}

/* Elementos recebem essas classes via IntersectionObserver */
.reveal-ready {
  opacity: 0;
  transform: translateY(16px) scale(0.995);
  transition:
    opacity 0.55s cubic-bezier(0.22, 1, 0.36, 1),
    transform 0.55s cubic-bezier(0.22, 1, 0.36, 1),
    box-shadow 0.2s ease;
  transition-delay: var(--reveal-delay, 0ms);
}

.reveal-ready.reveal-visible {
  opacity: 1;
  transform: translateY(0) scale(1);
}

.tab-pane.tab-enter {
  animation: tabEnter 0.34s cubic-bezier(0.22, 1, 0.36, 1);
}

@keyframes tabEnter {
  from {
    opacity: 0;
    transform: translateY(7px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.pqavs-scroll-top {
  position: fixed;
  right: 22px;
  bottom: 22px;
  z-index: 1050;
  width: 44px;
  height: 44px;
  border: 0;
  border-radius: 14px;
  background: rgba(15, 79, 64, 0.94);
  color: #FFFFFF;
  font-size: 21px;
  line-height: 1;
  box-shadow: 0 10px 24px rgba(15, 79, 64, 0.24);
  cursor: pointer;
  opacity: 0;
  visibility: hidden;
  transform: translateY(10px);
  transition:
    opacity 0.2s ease,
    transform 0.2s ease,
    visibility 0.2s ease,
    background-color 0.2s ease;
}

.pqavs-scroll-top:hover {
  background: #0F4F40;
  transform: translateY(-2px);
}

.pqavs-scroll-top.is-visible {
  opacity: 1;
  visibility: visible;
  transform: translateY(0);
}

.dataTables_filter input {
  border-radius: 10px !important;
  border: 1px solid var(--borda) !important;
  padding: 7px 10px !important;
  outline: none !important;
  transition:
    border-color 0.2s ease,
    box-shadow 0.2s ease;
}

.dataTables_filter input:focus {
  border-color: rgba(23, 107, 87, 0.55) !important;
  box-shadow: 0 0 0 3px rgba(23, 107, 87, 0.10) !important;
}

@media (prefers-reduced-motion: reduce) {
  html {
    scroll-behavior: auto;
  }

  .reveal-ready,
  .reveal-ready.reveal-visible,
  .tab-pane.tab-enter,
  .indicador-card,
  .resultado-badge,
  .hero,
  .pqavs-scroll-top {
    animation: none !important;
    transition: none !important;
    transform: none !important;
  }
}

"

js_pqavs <- "
(function () {
  'use strict';

  const reducedMotion = window.matchMedia(
    '(prefers-reduced-motion: reduce)'
  ).matches;

  const numberFormatter = new Intl.NumberFormat('pt-BR');

  function animateNumber(element) {
    if (!element || element.dataset.pqavsAnimated === '1') return;

    const originalText = element.textContent.trim();
    const numericText = originalText.replace(/[^0-9-]/g, '');
    const target = Number(numericText);

    if (!Number.isFinite(target)) return;

    element.dataset.pqavsAnimated = '1';

    if (reducedMotion) {
      element.textContent = numberFormatter.format(target);
      return;
    }

    const duration = 850;
    const startTime = performance.now();

    function update(now) {
      const progress = Math.min((now - startTime) / duration, 1);
      const eased = 1 - Math.pow(1 - progress, 3);
      const current = Math.round(target * eased);

      element.textContent = numberFormatter.format(current);

      if (progress < 1) {
        requestAnimationFrame(update);
      } else {
        element.textContent = numberFormatter.format(target);
      }
    }

    requestAnimationFrame(update);
  }

  function animateNumbersInside(element) {
    if (!element) return;

    if (
      element.matches &&
      element.matches('.bslib-value-box .value-box-value, .resultado-badge strong')
    ) {
      animateNumber(element);
    }

    element
      .querySelectorAll(
        '.bslib-value-box .value-box-value, .resultado-badge strong'
      )
      .forEach(animateNumber);
  }

  let revealObserver = null;

  if (!reducedMotion && 'IntersectionObserver' in window) {
    revealObserver = new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (!entry.isIntersecting) return;

          entry.target.classList.add('reveal-visible');
          animateNumbersInside(entry.target);
          revealObserver.unobserve(entry.target);
        });
      },
      {
        threshold: 0.10,
        rootMargin: '0px 0px -30px 0px'
      }
    );
  }

  function prepareReveal(root) {
    const base = root || document;
    const selector = [
      '.hero',
      '.bslib-value-box',
      '.section-heading',
      '.indicador-card',
      '.pqavs-card'
    ].join(',');

    base.querySelectorAll(selector).forEach(function (element, index) {
      if (element.dataset.pqavsReveal === '1') return;

      element.dataset.pqavsReveal = '1';
      element.classList.add('reveal-ready');

      if (element.classList.contains('indicador-card')) {
        const delay = (index % 3) * 70;
        element.style.setProperty('--reveal-delay', delay + 'ms');
      }

      if (reducedMotion || !revealObserver) {
        element.classList.add('reveal-visible');
        animateNumbersInside(element);
      } else {
        revealObserver.observe(element);
      }
    });
  }

  function setupCardGlow() {
    if (reducedMotion || !window.matchMedia('(pointer: fine)').matches) return;

    document.addEventListener('pointermove', function (event) {
      const card = event.target.closest('.indicador-card');
      if (!card) return;

      const rect = card.getBoundingClientRect();
      const x = ((event.clientX - rect.left) / rect.width) * 100;
      const y = ((event.clientY - rect.top) / rect.height) * 100;

      card.style.setProperty('--mouse-x', x + '%');
      card.style.setProperty('--mouse-y', y + '%');
      card.classList.add('js-hover');
    });

    document.addEventListener('pointerout', function (event) {
      const card = event.target.closest('.indicador-card');
      if (!card) return;

      if (!card.contains(event.relatedTarget)) {
        card.classList.remove('js-hover');
      }
    });
  }

  function setupHeroLight() {
    if (reducedMotion || !window.matchMedia('(pointer: fine)').matches) return;

    document.addEventListener('pointermove', function (event) {
      const hero = event.target.closest('.hero');
      if (!hero) return;

      const rect = hero.getBoundingClientRect();
      const x = ((event.clientX - rect.left) / rect.width) * 100;
      const y = ((event.clientY - rect.top) / rect.height) * 100;

      hero.style.setProperty('--hero-x', x + '%');
      hero.style.setProperty('--hero-y', y + '%');
    });

    document.addEventListener('pointerout', function (event) {
      const hero = event.target.closest('.hero');
      if (!hero) return;

      if (!hero.contains(event.relatedTarget)) {
        hero.style.setProperty('--hero-x', '88%');
        hero.style.setProperty('--hero-y', '8%');
      }
    });
  }

  function resizePlotlyCharts() {
    if (!window.Plotly) return;

    document.querySelectorAll('.js-plotly-plot').forEach(function (plot) {
      try {
        window.Plotly.Plots.resize(plot);
      } catch (error) {
        // Não interrompe o dashboard caso um gráfico ainda esteja renderizando.
      }
    });
  }

  function setupTabTransitions() {
    document.addEventListener('shown.bs.tab', function () {
      const activePane = document.querySelector('.tab-pane.active');

      if (activePane) {
        activePane.classList.remove('tab-enter');
        void activePane.offsetWidth;
        activePane.classList.add('tab-enter');
      }

      prepareReveal(document);

      window.setTimeout(resizePlotlyCharts, 80);
    });
  }

  function setupScrollEffects() {
    const navbar = document.querySelector('.navbar');

    const button = document.createElement('button');
    button.className = 'pqavs-scroll-top';
    button.type = 'button';
    button.setAttribute('aria-label', 'Voltar ao topo');
    button.setAttribute('title', 'Voltar ao topo');
    button.textContent = '↑';
    document.body.appendChild(button);

    function updateScrollState() {
      const y = window.scrollY || document.documentElement.scrollTop;

      if (navbar) {
        navbar.classList.toggle('navbar-scrolled', y > 12);
      }

      button.classList.toggle('is-visible', y > 450);
    }

    button.addEventListener('click', function () {
      window.scrollTo({
        top: 0,
        behavior: reducedMotion ? 'auto' : 'smooth'
      });
    });

    window.addEventListener('scroll', updateScrollState, { passive: true });
    updateScrollState();
  }

  function initPQAVS() {
    prepareReveal(document);
    setupCardGlow();
    setupHeroLight();
    setupTabTransitions();
    setupScrollEffects();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initPQAVS);
  } else {
    initPQAVS();
  }
})();
"

# =========================================================
# 4. FUNÇÕES AUXILIARES DE UI
# =========================================================

# Um card por indicador (linha de Resumo_Indicadores), com o gráfico
# plugado depois em renderPlotly()
card_indicador <- function(linha) {
  id_grafico <- paste0("grafico_", tolower(linha$Indicador))

  div(
    class = "indicador-card",

    div(
      class = "indicador-card-header",
      span(class = "indicador-titulo", linha$Indicador),
      span(class = "indicador-total", paste0("Total: ", linha$Total_Municipios))
    ),

    div(
      class = "indicador-grafico",
      plotlyOutput(id_grafico, height = "215px")
    ),

    div(
      class = "indicador-footer",
      div(
        class = "resultado-badge alcancou",
        tags$small("ALCANÇOU"),
        tags$strong(linha$Alcancou)
      ),
      div(
        class = "resultado-badge nao-alcancou",
        tags$small("NÃO ALCANÇOU"),
        tags$strong(linha$Nao_Alcancou)
      )
    )
  )
}

# Gráfico de barras horizontal (Alcançou x Não alcançou) de uma linha
# de Resumo_Indicadores
grafico_barra_indicador <- function(linha) {
  dados_grafico <- data.frame(
    Situacao = c("NÃO ALCANÇOU", "ALCANÇOU"),
    Quantidade = c(linha$Nao_Alcancou, linha$Alcancou),
    stringsAsFactors = FALSE
  )

  dados_grafico$Rotulo <- paste0(
    dados_grafico$Quantidade,
    ifelse(dados_grafico$Quantidade == 1, " município", " municípios")
  )

  limite_x <- max(c(linha$Total_Municipios, dados_grafico$Quantidade), na.rm = TRUE)
  if (!is.finite(limite_x) || limite_x <= 0) limite_x <- 1
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
    hovertemplate = "<b>%{y}</b><br>%{x} municípios<extra></extra>",
    marker = list(
      color = c("#B4543A", "#1B7F5A"),
      line = list(color = c("#9D4633", "#156C4D"), width = 1)
    )
  ) |>
    layout(
      showlegend = FALSE,
      bargap = 0.46,
      margin = list(l = 112, r = 54, t = 14, b = 35),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(
        title = "",
        range = c(0, limite_x),
        zeroline = FALSE,
        showline = FALSE,
        showgrid = TRUE,
        gridcolor = "#EDF1F4",
        tickfont = list(color = "#64748B", size = 10),
        rangemode = "tozero"
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = c("NÃO ALCANÇOU", "ALCANÇOU"),
        tickfont = list(color = "#334155", size = 11),
        fixedrange = TRUE
      ),
      font = list(family = "Segoe UI, Arial, sans-serif", color = "#334155")
    ) |>
    config(displayModeBar = FALSE, responsive = TRUE)
}

cards_indicadores <- lapply(
  seq_len(nrow(Resumo_Indicadores)),
  function(i) card_indicador(Resumo_Indicadores[i, ])
)

# =========================================================
# 5. INTERFACE
# =========================================================

ui <- page_navbar(
  title = "PQAVS 2025",
  theme = tema_pqavs,
  fillable = FALSE,

  header = tags$head(
    tags$style(HTML(css_pqavs)),
    tags$script(HTML(js_pqavs))
  ),

  # -------------------------------------------------------
  # ABA RESULTADOS
  # -------------------------------------------------------
  nav_panel(
    "Resultados",

    div(
      class = "dashboard-container",

      div(
        class = "hero",
        h2("Resultados dos Indicadores — PQAVS 2025"),
        p(paste(
          "Resultado individual de cada indicador, com a quantidade",
          "de municípios que alcançaram e não alcançaram a meta."
        ))
      ),

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Indicadores avaliados", value = Total_Indicadores, theme = "primary"),
        value_box(title = "Avaliações municipais", value = Total_Avaliacoes, theme = "secondary"),
        value_box(title = "Alcançaram a meta", value = Total_Alcancou, theme = "success"),
        value_box(title = "Não alcançaram a meta", value = Total_Nao_Alcancou, theme = "danger")
      ),

      div(
        class = "section-heading",
        h3("Resultado por indicador"),
        p(paste(
          "Cada gráfico apresenta separadamente o número de municípios",
          "em cada situação de cumprimento da meta."
        ))
      ),

      div(class = "indicadores-grid", cards_indicadores),

      br(),

      card(
        class = "pqavs-card",
        full_screen = TRUE,
        card_header("Resumo dos resultados por indicador"),
        DTOutput("tabela_resultados")
      )
    )
  ),

  # -------------------------------------------------------
  # ABA DADOS
  # -------------------------------------------------------
  nav_panel(
    "Dados",

    div(
      class = "dashboard-container",

      div(
        class = "hero",
        h2("Dados detalhados"),
        p(paste(
          "Tabela consolidada para conferência dos resultados",
          "de todos os indicadores."
        ))
      ),

      card(
        class = "pqavs-card",
        full_screen = TRUE,
        card_header("Todos os indicadores"),
        DTOutput("tabela_dados")
      )
    )
  )
)

# =========================================================
# 6. SERVIDOR
# =========================================================

server <- function(input, output, session) {

  # -------------------------------------------------------
  # GRÁFICOS INDIVIDUAIS DOS INDICADORES
  # -------------------------------------------------------

  lapply(seq_len(nrow(Resumo_Indicadores)), function(i) {
    linha <- Resumo_Indicadores[i, ]
    id_grafico <- paste0("grafico_", tolower(linha$Indicador))

    output[[id_grafico]] <- renderPlotly(grafico_barra_indicador(linha))
  })

  # -------------------------------------------------------
  # TABELA-RESUMO
  # -------------------------------------------------------

  output$tabela_resultados <- renderDT({
    tabela <- Resumo_Indicadores |>
      rename(
        `Total de municípios` = Total_Municipios,
        `Alcançou` = Alcancou,
        `Não alcançou` = Nao_Alcancou
      )

    datatable(
      tabela,
      rownames = FALSE,
      options = list(
        pageLength = 14,
        ordering = FALSE,
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE,
        scrollX = TRUE,
        language = list(emptyTable = "Nenhum resultado disponível")
      )
    )
  })

  # -------------------------------------------------------
  # TABELA DE DADOS DETALHADOS
  # -------------------------------------------------------

  output$tabela_dados <- renderDT({
    datatable(
      Indicadores_Completo,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "650px",
        autoWidth = TRUE,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "Nenhum registro disponível",
          zeroRecords = "Nenhum registro encontrado",
          paginate = list(
            first = "Primeiro",
            last = "Último",
            previous = "Anterior",
            `next` = "Próximo"
          )
        )
      )
    )
  })
}

# =========================================================
# 7. APLICAÇÃO
# =========================================================

shinyApp(ui = ui, server = server)