# =============================================================
# ABA 10 - Busca de empresas
#
# Aspecto desta aba: A EMPRESA INDIVIDUAL. As nove anteriores agregam;
# esta deixa procurar uma empresa pelo nome e ver ONDE ela cai na
# distribuicao - em que percentil, acima de quantas outras.
#
# E o que transforma o app em ferramenta: o visitante procura a empresa
# em que trabalha. Sem isso, dez abas de agregado nao dao a ninguem um
# motivo para mexer.
#
# O grafico mostra a distribuicao da base inteira com as empresas
# encontradas marcadas em cima. Marcar so faz sentido ate umas poucas
# dezenas de linhas; acima disso as marcas viram uma mancha, e a aba
# avisa em vez de desenhar.
# =============================================================

MAXIMO_MARCAS <- 40

aba10UI <- function(id, dados) {
  ns <- shiny::NS(id)
  naturezas <- names(sort(table(dados$legal_nature), decreasing = TRUE))
  portes <- levels(porte_pt(dados$company_size))

  shiny::tabPanel(
    title = "10. Busca",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Procurar"),
        shiny::textInput(ns("termo"), "Nome da empresa", placeholder = "ex.: RENNER"),
        shiny::helpText("Sem acentos e em maiúsculas no registro — a busca ignora caixa."),
        shiny::checkboxGroupInput(ns("portes"), "Porte", choices = portes, selected = portes),
        shiny::selectInput(ns("natureza"), "Natureza jurídica",
                           choices = c("Todas" = "__todas__",
                                       stats::setNames(naturezas, encurtar(naturezas, 40))),
                           selected = "__todas__"),
        shiny::sliderInput(ns("faixa"), "Faixa de capital (expoente de 10)",
                           min = 5, max = 12, value = c(5, 12), step = 0.1),
        shiny::helpText(shiny::textOutput(ns("faixa_legivel"), inline = TRUE)),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("Procurar uma empresa"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(4, shiny::wellPanel(shiny::strong("Encontradas"),     shiny::br(), shiny::textOutput(ns("kpi_n")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Capital mediano"), shiny::br(), shiny::textOutput(ns("kpi_mediana")))),
          shiny::column(4, shiny::wellPanel(shiny::strong("Maior delas"),     shiny::br(), shiny::textOutput(ns("kpi_maior"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "380px"),
        shiny::hr(),
        shiny::h4("Resultado da busca"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba10Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    universo <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d
    })

    achadas <- shiny::reactive({
      d <- universo()
      d$porte <- porte_pt(d$company_size)
      d <- d[as.character(d$porte) %in% input$portes, ]
      if (!identical(input$natureza, "__todas__")) d <- d[d$legal_nature == input$natureza, ]
      d <- d[d$log10_capital >= input$faixa[1] & d$log10_capital <= input$faixa[2], ]
      termo <- trimws(input$termo)
      if (nzchar(termo)) {
        d <- d[grepl(termo, d$company_name, ignore.case = TRUE, fixed = FALSE), ]
      }
      d[order(-d$capital_stock), ]
    })

    # Percentil da empresa dentro do universo, e nao dentro do resultado:
    # "acima de 98% das empresas" so quer dizer algo contra a base.
    # O arredondamento e para BAIXO, com duas casas: com 141 mil empresas,
    # a segunda maior fica em 99,9993%, e arredondar daria "acima de 100%
    # das empresas", que nao existe. Com o piso, so a maior de todas chega
    # a 100%.
    percentil <- function(valor) {
      x <- universo()$capital_stock
      floor(100 * mean(x <= valor) * 100) / 100
    }

    output$faixa_legivel <- shiny::renderText({
      paste("de", reais_curto(10^input$faixa[1]), "a", reais_curto(10^input$faixa[2]))
    })
    output$kpi_n <- shiny::renderText({
      paste(numero_pt(nrow(achadas())), "de", numero_pt(nrow(universo())))
    })
    output$kpi_mediana <- shiny::renderText({
      d <- achadas()
      if (nrow(d) == 0) "—" else reais(median(d$capital_stock))
    })
    output$kpi_maior <- shiny::renderText({
      d <- achadas()
      if (nrow(d) == 0) "—" else reais(max(d$capital_stock))
    })

    output$leitura <- shiny::renderText({
      d <- achadas()
      termo <- trimws(input$termo)
      if (nrow(d) == 0) {
        return(if (nzchar(termo))
          sprintf("Nenhuma empresa com \"%s\" no nome passa pelos filtros atuais.", termo)
        else "Nenhuma empresa passa pelos filtros atuais.")
      }
      maior <- d[1, ]
      sprintf(
        paste("%s empresa%s no resultado. A maior é %s, com %s — acima de %s%% das",
              "empresas da base. %s"),
        numero_pt(nrow(d)), if (nrow(d) == 1) "" else "s",
        maior$company_name, reais(maior$capital_stock),
        numero_pt(percentil(maior$capital_stock), 2),
        if (nrow(d) > MAXIMO_MARCAS)
          sprintf("O gráfico mostra a distribuição do resultado; acima de %s empresas as marcas individuais viram mancha.", MAXIMO_MARCAS)
        else "Cada empresa aparece marcada no gráfico, sobre a distribuição da base."
      )
    })

    output$grafico <- shiny::renderPlot({
      d <- achadas()
      universo_d <- universo()
      shiny::validate(shiny::need(nrow(d) > 0, "Nenhuma empresa no filtro atual."))

      g <- ggplot2::ggplot(universo_d, ggplot2::aes(x = log10_capital)) +
        ggplot2::geom_histogram(bins = 60, fill = CORES$claro, colour = "white",
                                linewidth = 0.2)

      if (nrow(d) <= MAXIMO_MARCAS) {
        g <- g +
          ggplot2::geom_vline(data = d, ggplot2::aes(xintercept = log10_capital),
                              colour = CORES$destaque, linewidth = 0.6, alpha = 0.85)
        if (nrow(d) <= 6) {
          g <- g + ggplot2::geom_text(
            data = d, ggplot2::aes(x = log10_capital, y = Inf,
                                   label = encurtar(company_name, 22)),
            angle = 90, hjust = 1.1, vjust = -0.4, size = 3.2, colour = CORES$destaque)
        }
        sub <- "Barras: a base inteira. Linhas: as empresas encontradas"
      } else {
        g <- g + ggplot2::geom_histogram(data = d, bins = 60,
                                         fill = CORES$destaque, alpha = 0.75,
                                         colour = "white", linewidth = 0.2)
        sub <- "Barras claras: a base inteira. Barras escuras: o resultado da busca"
      }

      g +
        ggplot2::scale_x_continuous(labels = rotulo_log10) +
        ggplot2::scale_y_continuous(labels = function(v) numero_pt(v)) +
        ggplot2::labs(title = "Onde o resultado cai na distribuição",
                      subtitle = sub,
                      x = "Capital social (escala log10)", y = "Empresas") +
        tema_app()
    })

    output$tabela <- DT::renderDataTable({
      d <- achadas()
      shiny::validate(shiny::need(nrow(d) > 0, "Nenhuma empresa no filtro atual."))
      d <- utils::head(d, 200)
      saida <- data.frame(
        Empresa = d$company_name,
        `Capital social` = reais(d$capital_stock),
        `Percentil` = paste0(numero_pt(vapply(d$capital_stock, percentil, numeric(1)), 2), "%"),
        Porte = as.character(porte_pt(d$company_size)),
        `Natureza jurídica` = encurtar(d$legal_nature, 38),
        Responsável = encurtar(d$owner_qualification, 38),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "tp", pageLength = 10, ordering = FALSE, scrollX = TRUE),
        caption = "No máximo 200 linhas. O percentil é a posição contra a base inteira, não contra o resultado."
      )
    })
  })
}
