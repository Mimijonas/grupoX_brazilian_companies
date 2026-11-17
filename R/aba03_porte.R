# =============================================================
# ABA 3 - Porte da empresa
#
# Aspecto desta aba: COMPARACAO entre os tres portes. A aba 1 diz quanto
# cada porte pesa na base; a aba 2 mostra a distribuicao do capital como
# um todo; aqui as tres distribuicoes aparecem LADO A LADO, que e o que
# responde "empresa maior tem mais capital declarado?".
#
# O achado que a aba precisa deixar visivel: micro e pequena empresa
# quase nao se separam pelo capital (p25 identico, medianas de R$ 300 mil
# e R$ 350 mil). Nao e defeito do dado - o porte no registro da Receita
# vem da RECEITA BRUTA, nao do capital social, entao nao ha razao para as
# duas coisas andarem juntas. Dizer isso e metade do trabalho.
# =============================================================

aba03UI <- function(id, dados) {
  ns <- shiny::NS(id)

  # As naturezas juridicas entram em ordem de tamanho, nao alfabetica: no
  # balcao de um seletor, quem procura quer achar a maior primeiro.
  naturezas <- names(sort(table(dados$legal_nature), decreasing = TRUE))

  shiny::tabPanel(
    title = "3. Porte",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::radioButtons(
          ns("tipo"), "Como comparar",
          choices = c("Boxplot", "Densidade por porte", "Faixas (mediana e quartis)"),
          selected = "Boxplot"
        ),
        shiny::radioButtons(
          ns("escala"), "Escala do capital",
          choices = c("log10 (recomendado)" = "log", "Reais" = "linear"),
          selected = "log"
        ),
        shiny::selectInput(
          ns("natureza"), "Restringir a uma natureza jurídica",
          choices = c("Todas" = "__todas__", stats::setNames(naturezas, naturezas)),
          selected = "__todas__"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Boxplot'", ns("tipo")),
          shiny::checkboxInput(ns("outliers"), "Desenhar os pontos extremos", value = FALSE),
          shiny::helpText(
            "Desligado por padrão: são dezenas de milhares de pontos, e o",
            "desenho demora sem acrescentar nada que a caixa já não diga."
          )
        ),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("Porte declarado e capital social"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(4, shiny::wellPanel(shiny::strong("Microempresa"),    shiny::br(), shiny::textOutput(ns("kpi_micro")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Pequena empresa"), shiny::br(), shiny::textOutput(ns("kpi_pequena")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Demais empresas"), shiny::br(), shiny::textOutput(ns("kpi_demais"))))
        ),
        shiny::helpText("Capital social mediano de cada porte, no filtro atual."),
        shiny::plotOutput(ns("grafico"), height = "420px"),
        shiny::hr(),
        shiny::h4("Capital social por porte"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba03Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    base <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      if (!identical(input$natureza, "__todas__")) {
        d <- d[d$legal_nature == input$natureza, ]
      }
      d$porte <- porte_pt(d$company_size)
      d
    })

    # Um resumo so, usado pelas caixas, pelo texto, pelo grafico de faixas
    # e pela tabela: se cada um calculasse o seu, bastaria uma diferenca de
    # filtro para a tela se contradizer.
    resumo <- shiny::reactive({
      d <- base()
      shiny::req(nrow(d) > 0)
      partes <- split(d$capital_stock, d$porte)
      partes <- partes[vapply(partes, length, integer(1)) > 0]
      data.frame(
        porte    = factor(names(partes), levels = levels(d$porte)),
        n        = vapply(partes, length, integer(1)),
        p25      = vapply(partes, quantile, numeric(1), probs = .25),
        mediana  = vapply(partes, median, numeric(1)),
        p75      = vapply(partes, quantile, numeric(1), probs = .75),
        p90      = vapply(partes, quantile, numeric(1), probs = .90),
        media    = vapply(partes, mean, numeric(1)),
        maximo   = vapply(partes, max, numeric(1)),
        row.names = NULL, stringsAsFactors = FALSE
      )
    })

    mediana_de <- function(nome) {
      r <- resumo()
      linha <- r[as.character(r$porte) == nome, ]
      if (nrow(linha) == 0) return("—")
      paste0(reais(linha$mediana), "  (", numero_pt(linha$n), " empresas)")
    }
    output$kpi_micro   <- shiny::renderText(mediana_de("Microempresa"))
    output$kpi_pequena <- shiny::renderText(mediana_de("Pequena empresa"))
    output$kpi_demais  <- shiny::renderText(mediana_de("Demais empresas"))

    output$leitura <- shiny::renderText({
      r <- resumo()
      if (nrow(r) < 2) return("Poucos portes no filtro atual para comparar.")
      maior <- r[which.max(r$mediana), ]
      menor <- r[which.min(r$mediana), ]
      razao <- maior$mediana / menor$mediana

      micro <- r[as.character(r$porte) == "Microempresa", ]
      pequena <- r[as.character(r$porte) == "Pequena empresa", ]
      extra <- ""
      if (nrow(micro) == 1 && nrow(pequena) == 1) {
        r_mp <- pequena$mediana / micro$mediana
        if (r_mp < 1.5) {
          extra <- sprintf(
            paste(" Repare que microempresa e pequena empresa quase não se separam:",
                  "as medianas são %s e %s, uma razão de apenas %s. O porte no",
                  "registro da Receita vem da receita bruta, e não do capital social —",
                  "não há motivo para as duas medidas andarem juntas."),
            reais(micro$mediana), reais(pequena$mediana), numero_pt(r_mp, 2))
        }
      }

      paste0(sprintf(
        "No filtro atual, \"%s\" tem o maior capital mediano (%s) e \"%s\" o menor (%s) — %s vezes de diferença.",
        maior$porte, reais(maior$mediana), menor$porte, reais(menor$mediana),
        numero_pt(razao, 1)), extra)
    })

    output$grafico <- shiny::renderPlot({
      d <- base()
      shiny::validate(shiny::need(nrow(d) > 0, "Nenhuma empresa no filtro atual."))
      log_escala <- identical(input$escala, "log")

      eixo <- function(p, qual = "x") {
        f <- if (log_escala) rotulo_log10 else reais_curto
        if (qual == "x") p + ggplot2::scale_x_continuous(labels = f)
        else p + ggplot2::scale_y_continuous(labels = f)
      }
      rotulo_cap <- if (log_escala) "Capital social (escala log10)" else "Capital social (R$)"
      d$valor <- if (log_escala) d$log10_capital else d$capital_stock

      p <- if (identical(input$tipo, "Boxplot")) {
        g <- ggplot2::ggplot(d, ggplot2::aes(x = valor, y = porte, fill = porte)) +
          ggplot2::geom_boxplot(
            outlier.shape = if (isTRUE(input$outliers)) 16 else NA,
            outlier.alpha = 0.15, outlier.size = 0.6,
            width = 0.55, colour = "#3C4858"
          ) +
          ggplot2::labs(title = "Capital social por porte", subtitle = "Caixa = metade central das empresas; linha = mediana",
                        x = rotulo_cap, y = NULL)
        # Sem os pontos extremos o eixo tem de ser recortado, senao ele
        # continua esticado ate R$ 1 tri por causa de caixas invisiveis.
        if (!isTRUE(input$outliers)) {
          lim <- stats::quantile(d$valor, c(0.001, 0.995))
          g <- g + ggplot2::coord_cartesian(xlim = as.numeric(lim))
        }
        eixo(g, "x")

      } else if (identical(input$tipo, "Densidade por porte")) {
        g <- ggplot2::ggplot(d, ggplot2::aes(x = valor, fill = porte, colour = porte)) +
          ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
          ggplot2::labs(title = "Distribuição do capital em cada porte",
                        subtitle = "Curvas sobrepostas: onde elas se encostam, os portes não se distinguem",
                        x = rotulo_cap, y = "Densidade") +
          ggplot2::scale_colour_manual(values = CORES$porte, name = NULL)
        eixo(g, "x")

      } else {
        r <- resumo()
        conv <- function(v) if (log_escala) log10(v) else v
        g <- ggplot2::ggplot(r, ggplot2::aes(y = porte)) +
          ggplot2::geom_segment(ggplot2::aes(x = conv(p25), xend = conv(p90), yend = porte),
                                colour = CORES$claro, linewidth = 7, lineend = "round") +
          ggplot2::geom_segment(ggplot2::aes(x = conv(p25), xend = conv(p75), yend = porte,
                                             colour = porte), linewidth = 7, lineend = "round") +
          ggplot2::geom_point(ggplot2::aes(x = conv(mediana)), size = 3.4, colour = "white") +
          ggplot2::geom_point(ggplot2::aes(x = conv(mediana)), size = 2.2, colour = "#3C4858") +
          ggplot2::geom_text(ggplot2::aes(x = conv(mediana), label = reais_curto(mediana)),
                             vjust = -1.6, size = 3.6, colour = CORES$neutro) +
          ggplot2::labs(title = "Mediana, metade central e 90% das empresas",
                        subtitle = "Barra forte = do 1º ao 3º quartil; barra clara = até o p90; ponto = mediana",
                        x = rotulo_cap, y = NULL) +
          ggplot2::scale_colour_manual(values = CORES$porte, guide = "none")
        eixo(g, "x")
      }

      p + ggplot2::scale_fill_manual(values = CORES$porte, name = NULL) + tema_app()
    })

    output$tabela <- DT::renderDataTable({
      r <- resumo()
      shiny::validate(shiny::need(nrow(r) > 0, "Nenhuma empresa no filtro atual."))
      saida <- data.frame(
        Porte = as.character(r$porte),
        Empresas = numero_pt(r$n),
        `% das empresas` = paste0(numero_pt(100 * r$n / sum(r$n), 1), "%"),
        `1º quartil` = reais(r$p25),
        Mediana = reais(r$mediana),
        `3º quartil` = reais(r$p75),
        p90 = reais(r$p90),
        Média = reais(r$media),
        Máximo = reais_curto(r$maximo),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE),
        caption = "A média aparece ao lado da mediana de propósito: a distância entre as duas mede a assimetria dentro de cada porte."
      )
    })
  })
}
