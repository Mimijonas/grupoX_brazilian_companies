# =============================================================
# ABA 8 - Concentracao
#
# Aspecto desta aba: DESIGUALDADE, medida. A aba 7 mostra quem esta no
# topo; aqui a pergunta e o quanto o capital esta concentrado, com um
# numero so, comparavel entre grupos.
#
# Curva de Lorenz e indice de Gini, as duas ferramentas padrao. A curva
# responde "quanto do capital esta nas X% menores"; o Gini resume a
# distancia entre essa curva e a igualdade perfeita, de 0 (todas as
# empresas com o mesmo capital) a 1 (uma empresa com tudo).
#
# O Gini aqui mede capital DECLARADO entre empresas, e nao renda entre
# pessoas. Nao se compara com o Gini de um pais - o subtitulo diz isso
# na tela, porque e o erro de leitura mais provavel.
# =============================================================

# Gini pela formula da soma ponderada pela posicao, sobre o vetor
# ordenado. Equivale a area entre a curva de Lorenz e a diagonal, e
# evita depender do pacote ineq so por uma linha de conta.
gini <- function(x) {
  x <- sort(x[!is.na(x) & x > 0])
  n <- length(x)
  if (n < 2) return(NA_real_)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n
}

aba08UI <- function(id, dados) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "8. Concentração",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::selectInput(
          ns("grupo"), "Comparar grupos de",
          choices = c("Não separar (base inteira)" = "__nenhum__",
                      "Porte" = "company_size",
                      "Natureza jurídica" = "legal_nature",
                      "Qualificação do responsável" = "owner_qualification"),
          selected = "company_size"
        ),
        shiny::sliderInput(ns("top"), "Grupos no gráfico",
                           min = 2, max = 8, value = 4, step = 1),
        shiny::sliderInput(ns("minimo"), "Mínimo de empresas no grupo",
                           min = 30, max = 2000, value = 200, step = 10),
        shiny::helpText("Gini sobre poucas empresas oscila muito; o mínimo evita comparar ruído."),
        shiny::checkboxInput(ns("diagonal"), "Mostrar a linha de igualdade", value = TRUE),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("O quanto o capital está concentrado"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(4, shiny::wellPanel(shiny::strong("Gini da base"),   shiny::br(), shiny::textOutput(ns("kpi_gini")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Top 1% detém"),   shiny::br(), shiny::textOutput(ns("kpi_top1")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Metade inferior"), shiny::br(), shiny::textOutput(ns("kpi_metade"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "440px"),
        shiny::hr(),
        shiny::h4("Concentração por grupo"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba08Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    base <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d
    })

    grupo_de <- function(d) {
      if (identical(input$grupo, "__nenhum__")) rep("Base inteira", nrow(d))
      else if (identical(input$grupo, "company_size")) as.character(porte_pt(d$company_size))
      else as.character(d[[input$grupo]])
    }

    grupos <- shiny::reactive({
      d <- base()
      d$.g <- grupo_de(d)
      tamanhos <- sort(table(d$.g), decreasing = TRUE)
      tamanhos <- tamanhos[tamanhos >= input$minimo]
      shiny::validate(shiny::need(
        length(tamanhos) > 0,
        "Nenhum grupo atinge o mínimo de empresas."
      ))
      nomes <- utils::head(names(tamanhos), input$top)
      d[d$.g %in% nomes, ]
    })

    # Uma curva de Lorenz tem uma coordenada por empresa; com 141 mil
    # empresas o desenho fica pesado sem ganhar nenhum detalhe visivel.
    # Entao amostram-se 400 pontos ao longo da curva ja calculada - a
    # curva e monotona, nao ha pico entre dois pontos vizinhos para
    # perder.
    curva <- function(x, n_pontos = 400) {
      x <- sort(x[x > 0])
      n <- length(x)
      if (n < 2) return(NULL)
      p <- seq_len(n) / n
      acumulado <- cumsum(x) / sum(x)
      idx <- unique(round(seq(1, n, length.out = min(n, n_pontos))))
      data.frame(p = c(0, p[idx]), acumulado = c(0, acumulado[idx]))
    }

    resumo <- shiny::reactive({
      d <- grupos()
      partes <- split(d$capital_stock, d$.g)
      data.frame(
        grupo = names(partes),
        n = vapply(partes, length, integer(1)),
        gini = vapply(partes, gini, numeric(1)),
        top1 = vapply(partes, function(x) {
          corte <- stats::quantile(x, .99); sum(x[x >= corte]) / sum(x)
        }, numeric(1)),
        top10 = vapply(partes, function(x) {
          corte <- stats::quantile(x, .90); sum(x[x >= corte]) / sum(x)
        }, numeric(1)),
        metade = vapply(partes, function(x) {
          corte <- stats::median(x); sum(x[x <= corte]) / sum(x)
        }, numeric(1)),
        row.names = NULL, stringsAsFactors = FALSE
      )
    })

    output$kpi_gini <- shiny::renderText(numero_pt(gini(base()$capital_stock), 3))
    output$kpi_top1 <- shiny::renderText({
      x <- base()$capital_stock
      corte <- stats::quantile(x, .99)
      paste0(numero_pt(100 * sum(x[x >= corte]) / sum(x), 1), "% do capital")
    })
    output$kpi_metade <- shiny::renderText({
      x <- base()$capital_stock
      corte <- stats::median(x)
      paste0(numero_pt(100 * sum(x[x <= corte]) / sum(x), 1), "% do capital")
    })

    output$leitura <- shiny::renderText({
      x <- base()$capital_stock
      corte <- stats::median(x)
      r <- resumo()
      extra <- ""
      if (nrow(r) > 1) {
        alto <- r[which.max(r$gini), ]; baixo <- r[which.min(r$gini), ]
        extra <- sprintf(
          " Entre os grupos comparados, \"%s\" é o mais concentrado (Gini %s) e \"%s\" o menos (Gini %s).",
          alto$grupo, numero_pt(alto$gini, 3), baixo$grupo, numero_pt(baixo$gini, 3))
      }
      paste0(sprintf(
        paste("O Gini do capital declarado é %s. A metade das empresas com menos",
              "capital detém %s%% do total, enquanto o 1%% do topo detém %s%%."),
        numero_pt(gini(x), 3),
        numero_pt(100 * sum(x[x <= corte]) / sum(x), 1),
        numero_pt(100 * sum(x[x >= stats::quantile(x, .99)]) / sum(x), 1)), extra)
    })

    output$grafico <- shiny::renderPlot({
      d <- grupos()
      partes <- split(d$capital_stock, d$.g)
      linhas <- do.call(rbind, lapply(names(partes), function(g) {
        cv <- curva(partes[[g]])
        if (is.null(cv)) return(NULL)
        cv$grupo <- g
        cv
      }))
      shiny::validate(shiny::need(!is.null(linhas), "Dados insuficientes para a curva."))

      r <- resumo()
      linhas$grupo <- factor(linhas$grupo,
                             levels = r$grupo[order(-r$gini)])

      g <- ggplot2::ggplot(linhas, ggplot2::aes(x = p, y = acumulado, colour = grupo))
      if (isTRUE(input$diagonal)) {
        g <- g + ggplot2::geom_abline(slope = 1, intercept = 0,
                                      colour = CORES$neutro, linetype = "dashed")
      }
      g +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_x_continuous(labels = function(v) paste0(numero_pt(100 * v), "%")) +
        ggplot2::scale_y_continuous(labels = function(v) paste0(numero_pt(100 * v), "%")) +
        ggplot2::scale_colour_discrete(labels = function(v) encurtar(v, 26), name = NULL) +
        ggplot2::labs(
          title = "Curva de Lorenz do capital social",
          subtitle = "Quanto mais a curva afunda, mais concentrado. Mede capital entre empresas — não se compara com o Gini de renda de um país",
          x = "Empresas, da menor para a maior", y = "Capital acumulado"
        ) +
        tema_app()
    })

    output$tabela <- DT::renderDataTable({
      r <- resumo()
      shiny::validate(shiny::need(nrow(r) > 0, "Nenhum grupo no filtro atual."))
      r <- r[order(-r$gini), ]
      saida <- data.frame(
        Grupo = r$grupo,
        Empresas = numero_pt(r$n),
        Gini = numero_pt(r$gini, 3),
        `Top 1% detém` = paste0(numero_pt(100 * r$top1, 1), "%"),
        `Top 10% detém` = paste0(numero_pt(100 * r$top10, 1), "%"),
        `Metade inferior detém` = paste0(numero_pt(100 * r$metade, 1), "%"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE),
        caption = "Gini de 0 (todas iguais) a 1 (uma empresa com tudo)."
      )
    })
  })
}
