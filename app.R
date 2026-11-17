# =============================================================
# app.R - aplicativo Shiny do grupo (CE302, trabalho final)
#
# O app fica na RAIZ do projeto de proposito: o shinyapps.io publica a
# pasta do app, e e assim que dados/tratados/companies.rds sobe junto.
#
# COMO ADICIONAR SUA ABA
#   1. crie R/abaNN_seu_assunto.R com duas funcoes: abaNNUI(id, dados) e
#      abaNNServer(id, dados) - copie R/aba02_distribuicao.R como molde;
#   2. acrescente UMA linha na lista ABAS, abaixo.
# Cada aba em seu arquivo e o que permite cinco pessoas trabalharem sem
# conflito de merge: dois PRs que mexem no mesmo app.R brigam por cada
# linha, dois PRs em arquivos diferentes nao se encostam.
# =============================================================

library(shiny)
library(ggplot2)

# Carrega os ajudantes e as abas. Todos esses arquivos SO definem funcoes.
for (arq in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(arq)

dados <- carrega_dados()

# Uma linha por aba: numero, funcao de UI, funcao de servidor.
# As abas 3 a 10 entram aqui conforme forem ficando prontas.
ABAS <- list(
  list(ui = aba01UI, server = aba01Server, id = "aba01"),
  list(ui = aba02UI, server = aba02Server, id = "aba02"),
  list(ui = aba03UI, server = aba03Server, id = "aba03"),
  list(ui = aba04UI, server = aba04Server, id = "aba04"),
  list(ui = aba05UI, server = aba05Server, id = "aba05"),
  list(ui = aba06UI, server = aba06Server, id = "aba06"),
  list(ui = aba07UI, server = aba07Server, id = "aba07")
)

rodape <- tags$div(
  style = "margin: 24px 12px; color: #6B7280; font-size: 12px;",
  "Dados: TidyTuesday 2026-01-27 (Brazilian Companies), a partir do",
  "Cadastro Nacional da Pessoa Jurídica — Receita Federal.",
  tags$br(),
  "141.332 empresas com capital social declarado acima de R$ 150 mil."
)

# do.call, e nao navbarPage(...) direto: o numero de abas vem da lista ABAS,
# e o navbarPage recebe as abas como argumentos soltos no "...".
ui <- do.call(navbarPage, c(
  list(
    title  = "Empresas brasileiras · capital social",
    id     = "navbar",
    header = tags$head(tags$style(HTML("
      .well { background: #F7F9FB; border: 1px solid #E3EAF0; }
      .navbar-brand { font-weight: 700; }
    ")))
  ),
  lapply(ABAS, function(a) a$ui(a$id, dados)),
  list(footer = rodape)
))

server <- function(input, output, session) {
  for (a in ABAS) a$server(a$id, dados)
}

shinyApp(ui, server)
