# =============================================================
# ABA 9 - Qualidade do dado
#
# Aspecto desta aba: O DADO COMO OBJETO. As outras nove tratam o capital
# social como medida; esta trata como DECLARACAO - alguem digitou aquele
# numero num formulario, e isso deixa marcas.
#
# Nao e aba de rodape. Os 262 valores coringa que as outras abas filtram
# sao decididos aqui, e um trabalho que apresentasse a media de R$ 353
# milhoes sem olhar para eles estaria errado por 97%.
#
# Tres verificacoes, cada uma com seu grafico:
#   valores repetidos  - capital nao e continuo, e um punhado de ancoras
#   numeros redondos   - a assinatura de quem arredonda ao declarar
#   nomes repetidos    - homonimos, que NAO sao duplicatas
# =============================================================

aba09UI <- function(id, dados) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "9. Qualidade do dado",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::radioButtons(
          ns("exame"), "O que examinar",
          choices = c("Valores mais repetidos" = "repetidos",
                      "Arredondamento" = "redondos",
                      "Valores implausíveis" = "coringas",
                      "Nomes repetidos" = "nomes"),
          selected = "repetidos"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'repetidos' || input['%s'] == 'coringas' || input['%s'] == 'nomes'",
                              ns("exame"), ns("exame"), ns("exame")),
          shiny::sliderInput(ns("quantos"), "Quantos mostrar",
                             min = 5, max = 30, value = 15, step = 5)
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'coringas'", ns("exame")),
          shiny::sliderInput(ns("limiar"), "Limiar de implausível (bilhões de R$)",
                             min = 1, max = 100, value = 10, step = 1),
          shiny::helpText(
            "O projeto adota R$ 10 bi. Vale mexer: o salto está entre uma",
            "empresa real de R$ 9,9 bi e as 184 com exatamente R$ 100 bi."
          )
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("O capital social como declaração"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(4, shiny::wellPanel(shiny::strong("Valores distintos"),  shiny::br(), shiny::textOutput(ns("kpi_distintos")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Múltiplos de R$ 10 mil"), shiny::br(), shiny::textOutput(ns("kpi_redondos")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Implausíveis"),       shiny::br(), shiny::textOutput(ns("kpi_coringas"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "420px"),
        shiny::hr(),
        shiny::h4(shiny::textOutput(ns("titulo_tabela"))),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba09Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    # Esta aba olha a base COMPLETA, sempre: filtrar os implausiveis aqui
    # seria esconder justamente o objeto de estudo.
    todas <- dados

    repetidos <- shiny::reactive({
      t <- sort(table(todas$capital_stock), decreasing = TRUE)
      data.frame(
        valor = as.numeric(names(t)),
        n = as.integer(t),
        row.names = NULL
      )
    })

    redondeza <- shiny::reactive({
      x <- todas$capital_stock
      faixas <- c("Centavos (não inteiro)" = sum(x %% 1 != 0),
                  "Inteiro, não múltiplo de mil" = sum(x %% 1 == 0 & x %% 1e3 != 0),
                  "Múltiplo de R$ 1 mil" = sum(x %% 1e3 == 0 & x %% 1e4 != 0),
                  "Múltiplo de R$ 10 mil" = sum(x %% 1e4 == 0 & x %% 1e5 != 0),
                  "Múltiplo de R$ 100 mil" = sum(x %% 1e5 == 0 & x %% 1e6 != 0),
                  "Múltiplo de R$ 1 milhão" = sum(x %% 1e6 == 0))
      data.frame(faixa = factor(names(faixas), levels = rev(names(faixas))),
                 n = as.integer(faixas), row.names = NULL)
    })

    coringas <- shiny::reactive({
      lim <- input$limiar * 1e9
      todas[todas$capital_stock >= lim, ]
    })

    nomes <- shiny::reactive({
      t <- sort(table(todas$company_name), decreasing = TRUE)
      t <- t[t > 1]
      data.frame(nome = names(t), n = as.integer(t), row.names = NULL)
    })

    output$kpi_distintos <- shiny::renderText({
      paste0(numero_pt(length(unique(todas$capital_stock))), " em ",
             numero_pt(nrow(todas)), " empresas")
    })
    output$kpi_redondos <- shiny::renderText({
      x <- todas$capital_stock
      paste0(numero_pt(100 * mean(x %% 1e4 == 0), 1), "% das empresas")
    })
    output$kpi_coringas <- shiny::renderText({
      paste(numero_pt(sum(todas$capital_suspeito)), "acima de R$ 10 bi")
    })

    output$leitura <- shiny::renderText({
      x <- todas$capital_stock
      switch(input$exame,
        repetidos = {
          r <- repetidos()
          sprintf(
            paste("São %s valores distintos para %s empresas, e o mais repetido,",
                  "%s, aparece %s vezes. Os dez valores mais comuns cobrem %s%% da",
                  "base: capital declarado não é uma variável contínua, é um punhado",
                  "de âncoras com cauda."),
            numero_pt(nrow(r)), numero_pt(nrow(todas)), reais(r$valor[1]),
            numero_pt(r$n[1]), numero_pt(100 * sum(utils::head(r$n, 10)) / nrow(todas), 1))
        },
        redondos = sprintf(
          paste("%s%% das empresas declaram múltiplo de R$ 10 mil e %s%% múltiplo de",
                "R$ 1 milhão. Apenas %s empresas declaram centavos. Isso importa para",
                "a leitura: os picos do histograma da aba 2 são arredondamento humano,",
                "não concentração econômica."),
          numero_pt(100 * mean(x %% 1e4 == 0), 1),
          numero_pt(100 * mean(x %% 1e6 == 0), 1),
          numero_pt(sum(x %% 1 != 0))),
        coringas = {
          cg <- coringas()
          sprintf(
            paste("Acima de R$ %s bi há %s empresas, que somam %s — %s%% de todo o",
                  "capital da base, declarado por %s%% das empresas. Repare nos valores:",
                  "não é uma distribuição, são repetições de números redondos enormes."),
            numero_pt(input$limiar), numero_pt(nrow(cg)), reais_curto(sum(cg$capital_stock)),
            numero_pt(100 * sum(cg$capital_stock) / sum(x), 1),
            numero_pt(100 * nrow(cg) / nrow(todas), 3))
        },
        nomes = {
          nm <- nomes()
          sprintf(
            paste("Há %s razões sociais que aparecem mais de uma vez, somando %s",
                  "cadastros. NÃO são duplicatas: o company_id é único nas %s linhas,",
                  "e nomes como esses se repetem legitimamente em cidades diferentes.",
                  "Apagá-las seria perder empresas reais."),
            numero_pt(nrow(nm)), numero_pt(sum(nm$n)), numero_pt(nrow(todas)))
        }
      )
    })

    output$grafico <- shiny::renderPlot({
      if (identical(input$exame, "redondos")) {
        r <- redondeza()
        r$p <- r$n / sum(r$n)
        return(
          ggplot2::ggplot(r, ggplot2::aes(x = n, y = faixa)) +
            ggplot2::geom_col(fill = CORES$principal, width = 0.72) +
            ggplot2::geom_text(ggplot2::aes(label = paste0(numero_pt(n), "  (",
                                                           numero_pt(100 * p, 1), "%)")),
                               hjust = -0.05, size = 3.6, colour = CORES$neutro) +
            ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.3)),
                                        labels = function(v) numero_pt(v)) +
            ggplot2::labs(title = "Quão redondo é o capital declarado",
                          subtitle = "Cada empresa entra em uma faixa só, a mais específica que couber",
                          x = NULL, y = NULL) +
            tema_app() +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
        )
      }

      if (identical(input$exame, "nomes")) {
        nm <- utils::head(nomes(), input$quantos)
        nm$nome <- factor(encurtar(nm$nome, 40), levels = rev(encurtar(nm$nome, 40)))
        return(
          ggplot2::ggplot(nm, ggplot2::aes(x = n, y = nome)) +
            ggplot2::geom_col(fill = CORES$principal, width = 0.7) +
            ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.3, size = 3.6,
                               colour = CORES$neutro) +
            ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)),
                                        breaks = function(l) unique(round(pretty(l)))) +
            ggplot2::labs(title = "Razões sociais que aparecem mais de uma vez",
                          subtitle = "Homônimos, não duplicatas: cada linha tem company_id próprio",
                          x = "Cadastros", y = NULL) +
            tema_app() +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
        )
      }

      dados_graf <- if (identical(input$exame, "coringas")) {
        cg <- coringas()
        t <- sort(table(cg$capital_stock), decreasing = TRUE)
        data.frame(valor = as.numeric(names(t)), n = as.integer(t), row.names = NULL)
      } else {
        repetidos()
      }
      dados_graf <- utils::head(dados_graf, input$quantos)
      shiny::validate(shiny::need(nrow(dados_graf) > 0, "Nenhum valor no filtro atual."))

      dados_graf$rotulo <- reais(dados_graf$valor)
      dados_graf$rotulo <- factor(dados_graf$rotulo, levels = rev(dados_graf$rotulo))

      ggplot2::ggplot(dados_graf, ggplot2::aes(x = n, y = rotulo)) +
        ggplot2::geom_col(fill = CORES$principal, width = 0.72) +
        ggplot2::geom_text(ggplot2::aes(label = numero_pt(n)), hjust = -0.15,
                           size = 3.6, colour = CORES$neutro) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18)),
                                    labels = function(v) numero_pt(v)) +
        ggplot2::labs(
          title = if (identical(input$exame, "coringas"))
            "Valores implausíveis, por quantas vezes se repetem"
          else "Os valores de capital mais declarados",
          subtitle = "Empresas que declaram exatamente o mesmo valor",
          x = "Empresas", y = NULL
        ) +
        tema_app() +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    })

    output$titulo_tabela <- shiny::renderText({
      switch(input$exame,
             repetidos = "Valores mais repetidos",
             redondos = "Arredondamento, faixa a faixa",
             coringas = "As empresas acima do limiar",
             nomes = "Razões sociais repetidas")
    })

    output$tabela <- DT::renderDataTable({
      saida <- switch(input$exame,
        repetidos = {
          r <- utils::head(repetidos(), max(input$quantos, 10))
          data.frame(`Capital declarado` = reais(r$valor), Empresas = numero_pt(r$n),
                     `% da base` = paste0(numero_pt(100 * r$n / nrow(todas), 2), "%"),
                     check.names = FALSE)
        },
        redondos = {
          r <- redondeza()
          data.frame(Faixa = as.character(r$faixa), Empresas = numero_pt(r$n),
                     `% da base` = paste0(numero_pt(100 * r$n / sum(r$n), 1), "%"),
                     check.names = FALSE)
        },
        coringas = {
          cg <- coringas()
          cg <- cg[order(-cg$capital_stock), ]
          data.frame(Empresa = encurtar(cg$company_name, 45),
                     `Capital declarado` = reais(cg$capital_stock),
                     Porte = as.character(porte_pt(cg$company_size)),
                     `Natureza jurídica` = encurtar(cg$legal_nature, 35),
                     check.names = FALSE)
        },
        nomes = {
          nm <- utils::head(nomes(), max(input$quantos, 10))
          data.frame(`Razão social` = nm$nome, Cadastros = numero_pt(nm$n),
                     check.names = FALSE)
        }
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "tp", pageLength = 10, ordering = FALSE, scrollX = TRUE),
        caption = "Esta aba olha a base completa: filtrar os implausíveis aqui seria esconder o objeto de estudo."
      )
    })
  })
}
