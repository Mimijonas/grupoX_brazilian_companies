# =============================================================
# ABA 5 - Qualificacao do responsavel
#
# Aspecto desta aba: QUEM ASSINA PELA EMPRESA. O registro guarda a
# qualificacao de quem responde pela empresa - socio-administrador,
# diretor, presidente, administrador judicial, inventariante.
#
# A pergunta e se o TIPO de responsavel acompanha o tamanho do capital.
# A resposta esperada e que sim, mas nao por causalidade: "Presidente" e
# "Diretor" existem em sociedade anonima, que e a forma juridica de
# empresa grande. Ou seja, a qualificacao e em boa medida um reflexo da
# natureza juridica, e a aba 6 e quem cruza as duas.
#
# Por isso aqui se mostra a DISTRIBUICAO (e nao so a mediana, como na
# aba 4): o que interessa e ver se as caixas se sobrepoem ou nao.
# =============================================================

aba05UI <- function(id, dados) {
  ns <- shiny::NS(id)
  portes <- levels(porte_pt(dados$company_size))

  shiny::tabPanel(
    title = "5. Responsável",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::sliderInput(ns("minimo"), "Mínimo de empresas na categoria",
                           min = 1, max = 1000, value = 100, step = 1),
        shiny::helpText(
          "Das 13 qualificações, várias têm menos de 50 empresas — uma caixa",
          "desenhada sobre 13 empresas não é uma distribuição."
        ),
        shiny::radioButtons(
          ns("ordem"), "Ordenar as categorias por",
          choices = c("Capital mediano" = "mediana", "Número de empresas" = "n"),
          selected = "mediana"
        ),
        shiny::checkboxGroupInput(ns("portes"), "Porte", choices = portes, selected = portes),
        shiny::checkboxInput(ns("rotulos"), "Mostrar a mediana sobre cada caixa", value = TRUE),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("Quem responde pela empresa"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(4, shiny::wellPanel(shiny::strong("Qualificações"),        shiny::br(), shiny::textOutput(ns("kpi_n")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Concentração na 1ª"),   shiny::br(), shiny::textOutput(ns("kpi_top")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Maior capital mediano"), shiny::br(), shiny::textOutput(ns("kpi_maior"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "460px"),
        shiny::hr(),
        shiny::h4("Qualificação em números"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba05Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    base <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d$porte <- porte_pt(d$company_size)
      d[as.character(d$porte) %in% input$portes, ]
    })

    resumo <- shiny::reactive({
      d <- base()
      shiny::req(nrow(d) > 0)
      g <- split(d$capital_stock, d$owner_qualification)
      tab <- data.frame(
        qualificacao = names(g),
        n       = vapply(g, length, integer(1)),
        p25     = vapply(g, quantile, numeric(1), probs = .25),
        mediana = vapply(g, median, numeric(1)),
        p75     = vapply(g, quantile, numeric(1), probs = .75),
        media   = vapply(g, mean, numeric(1)),
        total   = vapply(g, sum, numeric(1)),
        row.names = NULL, stringsAsFactors = FALSE
      )
      tab$p_empresas <- tab$n / sum(tab$n)
      tab[tab$n >= input$minimo, ]
    })

    output$kpi_n <- shiny::renderText({
      d <- base()
      paste(length(unique(d$owner_qualification)), "—", nrow(resumo()), "acima do mínimo")
    })
    output$kpi_top <- shiny::renderText({
      d <- base()
      t <- sort(table(d$owner_qualification), decreasing = TRUE)
      paste0(numero_pt(100 * t[[1]] / sum(t), 1), "% das empresas")
    })
    output$kpi_maior <- shiny::renderText({
      tab <- resumo()
      shiny::req(nrow(tab) > 0)
      l <- tab[which.max(tab$mediana), ]
      paste0(reais(l$mediana), " — ", l$qualificacao)
    })

    output$leitura <- shiny::renderText({
      tab <- resumo()
      if (nrow(tab) < 2) return("Poucas qualificações no filtro atual para comparar.")
      maior <- tab[which.max(tab$mediana), ]
      menor <- tab[which.min(tab$mediana), ]
      comum <- tab[which.max(tab$n), ]
      sprintf(
        paste("A qualificação mais comum é \"%s\", em %s%% das empresas, com capital",
              "mediano de %s. No outro extremo, \"%s\" tem mediana de %s — %s vezes",
              "mais. A leitura honesta não é que o cargo cause capital alto: títulos",
              "como diretor e presidente existem em sociedade anônima, que é a forma",
              "jurídica de empresa grande. Quem separa os dois efeitos é a aba 6."),
        comum$qualificacao, numero_pt(100 * comum$p_empresas, 1), reais(comum$mediana),
        maior$qualificacao, reais(maior$mediana),
        numero_pt(maior$mediana / menor$mediana, 1)
      )
    })

    output$grafico <- shiny::renderPlot({
      d <- base()
      tab <- resumo()
      shiny::validate(shiny::need(nrow(tab) > 0, "Nenhuma qualificação atinge o mínimo de empresas."))

      d <- d[d$owner_qualification %in% tab$qualificacao, ]
      ordem <- tab$qualificacao[order(tab[[input$ordem]])]
      d$qualificacao <- factor(d$owner_qualification, levels = ordem)
      tab$qualificacao <- factor(tab$qualificacao, levels = ordem)

      g <- ggplot2::ggplot(d, ggplot2::aes(x = log10_capital, y = qualificacao)) +
        ggplot2::geom_boxplot(outlier.shape = NA, width = 0.6,
                              fill = CORES$claro, colour = CORES$principal) +
        ggplot2::coord_cartesian(
          xlim = as.numeric(stats::quantile(d$log10_capital, c(0.002, 0.995)))
        ) +
        ggplot2::scale_x_continuous(labels = rotulo_log10) +
        ggplot2::labs(
          title = "Capital social por qualificação do responsável",
          subtitle = "Caixa = metade central das empresas; pontos extremos omitidos",
          x = "Capital social (escala log10)", y = NULL
        ) +
        tema_app()

      if (isTRUE(input$rotulos)) {
        g <- g + ggplot2::geom_text(
          data = tab,
          ggplot2::aes(x = log10(mediana), y = qualificacao, label = reais_curto(mediana)),
          vjust = -1.1, size = 3.3, colour = CORES$destaque
        )
      }
      g
    })

    output$tabela <- DT::renderDataTable({
      tab <- resumo()
      shiny::validate(shiny::need(nrow(tab) > 0, "Nenhuma qualificação atinge o mínimo de empresas."))
      tab <- tab[order(-tab[[input$ordem]]), ]
      saida <- data.frame(
        Qualificação = tab$qualificacao,
        Empresas = numero_pt(tab$n),
        `% das empresas` = paste0(numero_pt(100 * tab$p_empresas, 2), "%"),
        `1º quartil` = reais(tab$p25),
        Mediana = reais(tab$mediana),
        `3º quartil` = reais(tab$p75),
        Média = reais(tab$media),
        Total = reais_curto(tab$total),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "tp", pageLength = 12, ordering = FALSE, scrollX = TRUE),
        caption = "Qualificações com pelo menos o mínimo de empresas escolhido ao lado."
      )
    })
  })
}
