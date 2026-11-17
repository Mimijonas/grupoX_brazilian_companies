# =============================================================
# ABA 2 - Distribuicao do capital social
#
# Aspecto desta aba: a distribuicao da UNICA variavel quantitativa da base,
# vista sozinha. Comparacao entre portes e entre naturezas juridicas e
# assunto das abas 3 e 4; aqui o porte entra apenas como filtro.
#
# Modulo Shiny: todo input e output vive dentro do namespace ns(), entao
# nada daqui colide com as outras nove abas, mesmo que outra aba tambem
# tenha um input chamado "escala".
# =============================================================

aba02UI <- function(id, dados) {
  ns <- shiny::NS(id)

  # Os limites do slider saem dos dados, nao de numero digitado: se a base
  # for republicada com outro corte, o controle acompanha.
  lim <- range(dados$log10_capital, na.rm = TRUE)
  portes <- levels(porte_pt(dados$company_size))

  shiny::tabPanel(
    title = "2. Distribuição",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::radioButtons(
          ns("tipo"), "Como ver a distribuição",
          choices = c("Histograma", "Densidade", "Acumulada (ECDF)"),
          selected = "Histograma"
        ),
        shiny::radioButtons(
          ns("escala"), "Escala do capital",
          choices = c("log10 (recomendado)" = "log", "Reais" = "linear"),
          selected = "log"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Histograma'", ns("tipo")),
          shiny::sliderInput(ns("classes"), "Número de classes",
                             min = 10, max = 120, value = 50, step = 5)
        ),
        shiny::sliderInput(
          ns("faixa"), "Faixa de capital (expoente de 10)",
          min = floor(lim[1] * 10) / 10, max = ceiling(lim[2] * 10) / 10,
          value = c(floor(lim[1] * 10) / 10, ceiling(lim[2] * 10) / 10),
          step = 0.1
        ),
        shiny::helpText(shiny::textOutput(ns("faixa_legivel"), inline = TRUE)),
        shiny::checkboxGroupInput(
          ns("portes"), "Porte", choices = portes, selected = portes
        ),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        ),
        shiny::helpText(
          "262 empresas declaram capital de R$ 10 bi a R$ 1 tri, sendo 184",
          "com exatamente R$ 100 bi. São valores coringa de cadastro, e por",
          "isso ficam de fora por padrão."
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("Como o capital social se distribui"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(3, shiny::wellPanel(shiny::strong("Empresas"),   shiny::br(), shiny::textOutput(ns("kpi_n")))),
          shiny::column(3, shiny::wellPanel(shiny::strong("Mediana"),    shiny::br(), shiny::textOutput(ns("kpi_mediana")))),
          shiny::column(3, shiny::wellPanel(shiny::strong("Média"),      shiny::br(), shiny::textOutput(ns("kpi_media")))),
          shiny::column(3, shiny::wellPanel(shiny::strong("Assimetria"), shiny::br(), shiny::textOutput(ns("kpi_assimetria"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "420px"),
        shiny::hr(),
        shiny::h4("Medidas e quantis"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba02Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Dado filtrado, uma vez por mudanca de controle --------
    base <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d$porte <- porte_pt(d$company_size)
      d <- d[as.character(d$porte) %in% input$portes, ]
      d[d$log10_capital >= input$faixa[1] & d$log10_capital <= input$faixa[2], ]
    })

    output$faixa_legivel <- shiny::renderText({
      paste("de", reais_curto(10^input$faixa[1]), "a", reais_curto(10^input$faixa[2]))
    })

    # ---- Caixas de resumo --------------------------------------
    output$kpi_n <- shiny::renderText({
      paste(numero_pt(nrow(base())), "de", numero_pt(nrow(dados)))
    })
    output$kpi_mediana <- shiny::renderText({
      shiny::req(nrow(base()) > 0)
      reais(median(base()$capital_stock))
    })
    output$kpi_media <- shiny::renderText({
      shiny::req(nrow(base()) > 0)
      reais(mean(base()$capital_stock))
    })
    output$kpi_assimetria <- shiny::renderText({
      shiny::req(nrow(base()) > 2)
      numero_pt(assimetria(base()$capital_stock), 1)
    })

    # ---- A frase que explica o grafico -------------------------
    # Fica ACIMA do desenho e muda com os filtros: um grafico de
    # distribuicao sem a leitura em palavras obriga o espectador a
    # descobrir sozinho que a media nao descreve nada aqui.
    output$leitura <- shiny::renderText({
      d <- base()
      if (nrow(d) == 0) return("Nenhuma empresa no filtro atual.")
      med <- median(d$capital_stock)
      mea <- mean(d$capital_stock)
      razao <- mea / med
      sprintf(
        paste("Metade das %s empresas tem capital de até %s, mas a média é %s —",
              "%s vez%s a mediana. É uma distribuição fortemente assimétrica à",
              "direita: poucas empresas de capital muito alto puxam a média, e é",
              "por isso que a mediana é a medida honesta aqui."),
        numero_pt(nrow(d)), reais(med), reais(mea),
        numero_pt(razao, 1), if (razao >= 2) "es" else ""
      )
    })

    # ---- Grafico -----------------------------------------------
    output$grafico <- shiny::renderPlot({
      d <- base()
      shiny::validate(shiny::need(nrow(d) > 0, "Nenhuma empresa no filtro atual."))

      log_escala <- identical(input$escala, "log")
      x <- if (log_escala) d$log10_capital else d$capital_stock
      med <- median(x)
      mea <- mean(x)

      rotulo_x <- if (log_escala) "Capital social (escala log10)" else "Capital social (R$)"
      sub <- if (log_escala) {
        "Cada passo no eixo é dez vezes mais capital"
      } else {
        "Em reais, a cauda longa esmaga o gráfico — é exatamente o que a escala log resolve"
      }

      p <- ggplot2::ggplot(d, ggplot2::aes(x = x))

      if (identical(input$tipo, "Histograma")) {
        p <- p + ggplot2::geom_histogram(bins = input$classes,
                                         fill = CORES$principal, colour = "white",
                                         linewidth = 0.2)
        rotulo_y <- "Empresas"
      } else if (identical(input$tipo, "Densidade")) {
        p <- p + ggplot2::geom_density(fill = CORES$claro, colour = CORES$principal,
                                       linewidth = 0.8)
        rotulo_y <- "Densidade"
      } else {
        p <- p + ggplot2::stat_ecdf(colour = CORES$principal, linewidth = 0.9)
        rotulo_y <- "Proporção acumulada"
      }

      # Mediana e media no mesmo desenho: a distancia entre as duas linhas
      # E o resultado desta aba.
      p <- p +
        ggplot2::geom_vline(xintercept = med, colour = CORES$destaque, linewidth = 0.8) +
        ggplot2::geom_vline(xintercept = mea, colour = CORES$neutro,
                            linetype = "dashed", linewidth = 0.8) +
        ggplot2::annotate("label", x = med, y = Inf, vjust = 1.2,
                          label = paste("mediana", reais_curto(if (log_escala) 10^med else med)),
                          colour = CORES$destaque, fill = "white", size = 3.6,
                          label.size = 0) +
        ggplot2::annotate("label", x = mea, y = Inf, vjust = 2.8,
                          label = paste("média", reais_curto(if (log_escala) 10^mea else mea)),
                          colour = CORES$neutro, fill = "white", size = 3.6,
                          label.size = 0) +
        ggplot2::labs(title = "Distribuição do capital social declarado",
                      subtitle = sub, x = rotulo_x, y = rotulo_y) +
        tema_app()

      if (log_escala) {
        p <- p + ggplot2::scale_x_continuous(labels = rotulo_log10)
      } else {
        p <- p + ggplot2::scale_x_continuous(labels = reais_curto)
      }
      p + ggplot2::scale_y_continuous(labels = function(v) numero_pt(v))
    })

    # ---- Tabela ------------------------------------------------
    output$tabela <- DT::renderDataTable({
      d <- base()
      shiny::validate(shiny::need(nrow(d) > 0, "Nenhuma empresa no filtro atual."))
      x <- d$capital_stock
      q <- quantile(x, c(.01, .10, .25, .50, .75, .90, .99))

      tab <- data.frame(
        Medida = c("Empresas", "Mínimo", "1% menores (p1)", "p10", "1º quartil (p25)",
                   "Mediana (p50)", "3º quartil (p75)", "p90", "p99", "Máximo",
                   "Média", "Desvio padrão", "Média / mediana", "Assimetria"),
        Valor = c(
          numero_pt(length(x)), reais(min(x)), reais(q[1]), reais(q[2]), reais(q[3]),
          reais(q[4]), reais(q[5]), reais(q[6]), reais(q[7]), reais(max(x)),
          reais(mean(x)), reais(sd(x)),
          numero_pt(mean(x) / median(x), 1), numero_pt(assimetria(x), 2)
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        tab, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE),
        caption = "Medidas calculadas sobre as empresas que passam pelos filtros ao lado."
      )
    })
  })
}
