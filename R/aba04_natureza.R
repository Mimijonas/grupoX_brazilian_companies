# =============================================================
# ABA 4 - Natureza juridica
#
# Aspecto desta aba: QUEM PESA MAIS DO QUE O PROPRIO TAMANHO. A aba 1 ja
# mostrou que 84% das empresas sao LLC; repetir isso aqui nao acrescenta
# nada. A pergunta aqui e outra: entre as naturezas juridicas, quais
# concentram capital ALEM da sua participacao em numero de empresas?
#
# O indice e a razao entre a fatia do capital e a fatia das empresas.
# Acima de 1 a natureza concentra mais capital do que o seu tamanho
# sugeriria; abaixo de 1, menos. E uma medida relativa, e e ela que faz
# aparecer o que a contagem esconde: as 51 "Publicly Traded Corporation"
# sao 0,04% das empresas e respondem por 3,2% do capital.
#
# O filtro de minimo de empresas nao e enfeite: sem ele o ranking vira
# uma lista de categorias com tres empresas e uma delas gigante.
# =============================================================

aba04UI <- function(id, dados) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "4. Natureza jurídica",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::radioButtons(
          ns("medida"), "Ordenar e desenhar por",
          choices = c("Peso relativo (capital ÷ empresas)" = "peso",
                      "Capital mediano" = "mediana",
                      "Capital total" = "total"),
          selected = "peso"
        ),
        shiny::sliderInput(ns("top"), "Naturezas no gráfico",
                           min = 3, max = 20, value = 10, step = 1),
        shiny::sliderInput(ns("minimo"), "Mínimo de empresas na categoria",
                           min = 1, max = 500, value = 20, step = 1),
        shiny::helpText(
          "Sem um mínimo, o ranking vira uma lista de categorias com três",
          "empresas, uma delas gigante."
        ),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("Quais naturezas jurídicas concentram capital"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(4, shiny::wellPanel(shiny::strong("Naturezas na base"),   shiny::br(), shiny::textOutput(ns("kpi_total")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Acima do peso"),       shiny::br(), shiny::textOutput(ns("kpi_acima")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Maior peso relativo"), shiny::br(), shiny::textOutput(ns("kpi_campea"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "440px"),
        shiny::hr(),
        shiny::h4("Natureza jurídica em números"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba04Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    base <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d
    })

    # As fatias sao calculadas sobre a base INTEIRA, antes de qualquer
    # corte de minimo ou de top-N. Recalcular depois do corte faria o
    # "peso relativo" mudar de valor conforme o seletor, e ele deixaria
    # de significar o que o nome diz.
    agregado <- shiny::reactive({
      d <- base()
      shiny::req(nrow(d) > 0)
      g <- split(d$capital_stock, d$legal_nature)
      tab <- data.frame(
        natureza = names(g),
        n        = vapply(g, length, integer(1)),
        total    = vapply(g, sum, numeric(1)),
        mediana  = vapply(g, median, numeric(1)),
        media    = vapply(g, mean, numeric(1)),
        row.names = NULL, stringsAsFactors = FALSE
      )
      tab$p_empresas <- tab$n / sum(tab$n)
      tab$p_capital  <- tab$total / sum(tab$total)
      tab$peso <- tab$p_capital / tab$p_empresas
      tab
    })

    elegiveis <- shiny::reactive({
      tab <- agregado()
      tab[tab$n >= input$minimo, ]
    })

    output$kpi_total <- shiny::renderText({
      paste(nrow(agregado()), "—", nrow(elegiveis()), "com o mínimo de empresas")
    })
    output$kpi_acima <- shiny::renderText({
      tab <- elegiveis()
      paste(sum(tab$peso > 1), "de", nrow(tab), "naturezas")
    })
    output$kpi_campea <- shiny::renderText({
      tab <- elegiveis()
      shiny::req(nrow(tab) > 0)
      l <- tab[which.max(tab$peso), ]
      paste0(numero_pt(l$peso, 1), "× — ", l$natureza)
    })

    output$leitura <- shiny::renderText({
      tab <- elegiveis()
      if (nrow(tab) == 0) return("Nenhuma natureza jurídica atinge o mínimo de empresas.")
      l <- tab[which.max(tab$peso), ]
      sprintf(
        paste("\"%s\" responde por %s%% do capital com apenas %s%% das empresas:",
              "concentra %s vezes mais capital do que o seu tamanho sugeriria.",
              "Das %s naturezas com pelo menos %s empresas, %s estão acima do",
              "próprio peso e %s abaixo — e essa conta não muda quando você mexe",
              "no top-N, porque as fatias são sobre a base inteira."),
        l$natureza, numero_pt(100 * l$p_capital, 1), numero_pt(100 * l$p_empresas, 2),
        numero_pt(l$peso, 1), nrow(tab), numero_pt(input$minimo),
        sum(tab$peso > 1), sum(tab$peso <= 1)
      )
    })

    para_desenhar <- shiny::reactive({
      tab <- elegiveis()
      shiny::req(nrow(tab) > 0)
      tab$valor <- tab[[input$medida]]
      tab <- tab[order(-tab$valor), ]
      utils::head(tab, input$top)
    })

    output$grafico <- shiny::renderPlot({
      tab <- para_desenhar()
      shiny::validate(shiny::need(nrow(tab) > 0, "Nenhuma natureza jurídica no filtro atual."))

      formata <- switch(input$medida,
        peso    = function(v) paste0(numero_pt(v, 1), "×"),
        mediana = reais_curto,
        total   = reais_curto
      )
      titulo <- switch(input$medida,
        peso    = "Peso relativo: fatia do capital dividida pela fatia das empresas",
        mediana = "Capital social mediano da natureza jurídica",
        total   = "Capital social somado da natureza jurídica"
      )
      sub <- switch(input$medida,
        peso    = "Acima de 1× concentra mais capital do que o seu tamanho sugeriria",
        mediana = "A mediana não depende do tamanho da categoria",
        total   = "Soma, então categorias grandes aparecem grandes"
      )

      tab$rotulo <- paste0(formata(tab$valor), "   (", numero_pt(tab$n), " empresas)")
      tab$natureza <- factor(tab$natureza, levels = rev(tab$natureza))

      # A linha de referencia em 1x so faz sentido na medida de peso: e ela
      # que separa quem esta acima de quem esta abaixo do proprio tamanho.
      g <- ggplot2::ggplot(tab, ggplot2::aes(x = valor, y = natureza)) +
        ggplot2::geom_col(fill = CORES$principal, width = 0.75) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo), hjust = -0.05,
                           size = 3.6, colour = CORES$neutro) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.32)),
                                    labels = formata) +
        ggplot2::labs(title = titulo, subtitle = sub, x = NULL, y = NULL) +
        tema_app() +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(colour = "#EEF2F6"))

      if (identical(input$medida, "peso")) {
        g <- g + ggplot2::geom_vline(xintercept = 1, colour = CORES$destaque,
                                     linetype = "dashed", linewidth = 0.7)
      }
      g
    })

    output$tabela <- DT::renderDataTable({
      tab <- elegiveis()
      shiny::validate(shiny::need(nrow(tab) > 0, "Nenhuma natureza jurídica no filtro atual."))
      tab <- tab[order(-tab[[input$medida]]), ]
      saida <- data.frame(
        `Natureza jurídica` = tab$natureza,
        Empresas = numero_pt(tab$n),
        `% das empresas` = paste0(numero_pt(100 * tab$p_empresas, 2), "%"),
        `% do capital` = paste0(numero_pt(100 * tab$p_capital, 2), "%"),
        `Peso relativo` = paste0(numero_pt(tab$peso, 2), "×"),
        Mediana = reais(tab$mediana),
        Média = reais(tab$media),
        Total = reais_curto(tab$total),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "tp", pageLength = 12, ordering = FALSE, scrollX = TRUE),
        caption = "Todas as naturezas que atingem o mínimo de empresas, não só as do gráfico."
      )
    })
  })
}
