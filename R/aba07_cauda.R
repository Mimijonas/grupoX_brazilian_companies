# =============================================================
# ABA 7 - A cauda superior
#
# Aspecto desta aba: QUEM ESTA NO TOPO, e o quanto o topo difere do
# resto. A aba 2 mostrou que a distribuicao tem cauda longa; esta aba
# entra na cauda e pergunta quem mora la.
#
# A comparacao e sempre CONTRA A BASE: a fatia de cada categoria dentro
# da cauda nao diz nada sozinha. Se 84% das empresas da base sao LLC,
# 84% de LLC na cauda significa que a natureza juridica nao tem nada a
# ver com estar no topo. O que informa e a DIFERENCA entre as duas
# fatias, e e ela que o grafico desenha.
# =============================================================

aba07UI <- function(id, dados) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "7. Cauda superior",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::radioButtons(
          ns("corte"), "Tamanho da cauda",
          choices = c("Top 0,1%" = "0.001", "Top 1%" = "0.01",
                      "Top 5%" = "0.05", "Top 10%" = "0.1"),
          selected = "0.01"
        ),
        shiny::selectInput(
          ns("dimensao"), "Comparar a composição por",
          choices = c("Natureza jurídica" = "legal_nature",
                      "Porte" = "company_size",
                      "Qualificação do responsável" = "owner_qualification"),
          selected = "legal_nature"
        ),
        shiny::sliderInput(ns("top"), "Categorias no gráfico",
                           min = 3, max = 12, value = 6, step = 1),
        shiny::sliderInput(ns("listar"), "Empresas na lista",
                           min = 5, max = 50, value = 15, step = 5),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        ),
        shiny::helpText(
          "Com os coringas ligados, a lista das maiores vira uma lista de",
          "erros de cadastro — é o melhor jeito de ver por que eles ficam fora."
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("Quem está no topo"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(4, shiny::wellPanel(shiny::strong("Corte da cauda"),    shiny::br(), shiny::textOutput(ns("kpi_corte")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Empresas na cauda"), shiny::br(), shiny::textOutput(ns("kpi_n")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Capital na cauda"),  shiny::br(), shiny::textOutput(ns("kpi_share"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "400px"),
        shiny::hr(),
        shiny::h4("As maiores do filtro atual"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba07Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    base <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d
    })

    limiar <- shiny::reactive({
      p <- as.numeric(input$corte)
      stats::quantile(base()$capital_stock, 1 - p)
    })

    cauda <- shiny::reactive({
      d <- base()
      d[d$capital_stock >= limiar(), ]
    })

    coluna <- function(d) {
      if (identical(input$dimensao, "company_size")) as.character(porte_pt(d$company_size))
      else as.character(d[[input$dimensao]])
    }

    output$kpi_corte <- shiny::renderText(paste("a partir de", reais(limiar())))
    output$kpi_n <- shiny::renderText({
      paste(numero_pt(nrow(cauda())), "de", numero_pt(nrow(base())))
    })
    output$kpi_share <- shiny::renderText({
      paste0(numero_pt(100 * sum(cauda()$capital_stock) / sum(base()$capital_stock), 1), "% do total")
    })

    comparacao <- shiny::reactive({
      d <- base(); ca <- cauda()
      shiny::req(nrow(ca) > 0)
      p_base <- prop.table(table(coluna(d)))
      p_cauda <- prop.table(table(coluna(ca)))
      cats <- names(sort(p_cauda, decreasing = TRUE))
      cats <- utils::head(cats, input$top)
      data.frame(
        categoria = cats,
        na_base = as.numeric(p_base[cats]),
        na_cauda = as.numeric(p_cauda[cats]),
        n_cauda = as.numeric(table(coluna(ca))[cats]),
        stringsAsFactors = FALSE
      )
    })

    output$leitura <- shiny::renderText({
      ca <- cauda(); d <- base()
      cmp <- comparacao()
      cmp$dif <- cmp$na_cauda - cmp$na_base
      sobe <- cmp[which.max(cmp$dif), ]
      sprintf(
        paste("As %s empresas acima de %s detêm %s%% de todo o capital declarado.",
              "Na composição, quem mais ganha espaço no topo é \"%s\": %s%% da base",
              "contra %s%% da cauda. A fatia sozinha enganaria — o que informa é a",
              "diferença entre as duas."),
        numero_pt(nrow(ca)), reais(limiar()),
        numero_pt(100 * sum(ca$capital_stock) / sum(d$capital_stock), 1),
        sobe$categoria, numero_pt(100 * sobe$na_base, 1), numero_pt(100 * sobe$na_cauda, 1)
      )
    })

    output$grafico <- shiny::renderPlot({
      cmp <- comparacao()
      shiny::validate(shiny::need(nrow(cmp) > 0, "Nenhuma empresa na cauda."))

      longo <- rbind(
        data.frame(categoria = cmp$categoria, onde = "Na base inteira", p = cmp$na_base),
        data.frame(categoria = cmp$categoria, onde = "Na cauda", p = cmp$na_cauda)
      )
      ordem <- cmp$categoria[order(cmp$na_cauda)]
      longo$categoria <- factor(longo$categoria, levels = ordem)
      longo$onde <- factor(longo$onde, levels = c("Na base inteira", "Na cauda"))

      ggplot2::ggplot(longo, ggplot2::aes(x = p, y = categoria, fill = onde)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.68) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(numero_pt(100 * p, 1), "%")),
                           position = ggplot2::position_dodge(width = 0.72),
                           hjust = -0.12, size = 3.3, colour = CORES$neutro) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.22)),
                                    labels = function(v) paste0(numero_pt(100 * v), "%")) +
        ggplot2::scale_y_discrete(labels = function(v) encurtar(v, 30)) +
        ggplot2::scale_fill_manual(values = c("Na base inteira" = CORES$claro,
                                              "Na cauda" = CORES$principal), name = NULL) +
        ggplot2::labs(
          title = "Composição da cauda comparada com a da base",
          subtitle = "Barra escura maior que a clara: a categoria está super-representada no topo",
          x = NULL, y = NULL
        ) +
        tema_app() +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(colour = "#EEF2F6"))
    })

    output$tabela <- DT::renderDataTable({
      ca <- cauda()
      shiny::validate(shiny::need(nrow(ca) > 0, "Nenhuma empresa na cauda."))
      ca <- ca[order(-ca$capital_stock), ]
      ca <- utils::head(ca, input$listar)
      saida <- data.frame(
        Empresa = ca$company_name,
        `Capital social` = reais(ca$capital_stock),
        Porte = as.character(porte_pt(ca$company_size)),
        `Natureza jurídica` = encurtar(ca$legal_nature, 40),
        Responsável = encurtar(ca$owner_qualification, 40),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "tp", pageLength = 10, ordering = FALSE, scrollX = TRUE),
        caption = "Nomes como vêm do registro público da Receita Federal."
      )
    })
  })
}
