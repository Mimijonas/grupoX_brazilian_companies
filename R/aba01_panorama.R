# =============================================================
# ABA 1 - Panorama geral
#
# Aspecto desta aba: COMPOSICAO. Quantas empresas e quanto capital cada
# categoria responde, nas tres variaveis qualitativas da base.
#
# A fronteira com as outras abas: aqui so se pergunta "quanto cada grupo
# pesa". Como o capital se ESPALHA dentro de um grupo e assunto da aba 2
# (a distribuicao) e das abas 3 a 5 (cada categoria em profundidade);
# cruzar duas categorias e assunto da aba 6.
#
# O cuidado que esta aba exige: 84% das empresas sao LLC e 76% tem o mesmo
# tipo de responsavel. Um grafico com as 22 naturezas juridicas seria uma
# barra e 21 fios de cabelo, entao o que nao entra no top-N vira "Outros",
# que E a informacao: a base e extremamente concentrada.
# =============================================================

aba01UI <- function(id, dados) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "1. Panorama",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::radioButtons(
          ns("dimensao"), "Olhar a base por",
          choices = c("Porte" = "company_size",
                      "Natureza jurídica" = "legal_nature",
                      "Qualificação do responsável" = "owner_qualification"),
          selected = "company_size"
        ),
        shiny::radioButtons(
          ns("medida"), "Medir por",
          choices = c("Número de empresas" = "n", "Capital social total" = "capital"),
          selected = "n"
        ),
        shiny::sliderInput(ns("top"), "Categorias no gráfico",
                           min = 3, max = 15, value = 8, step = 1),
        shiny::helpText("O que ficar de fora entra como \"Outros\"."),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        ),
        shiny::helpText(
          "262 empresas declaram de R$ 10 bi a R$ 1 tri de capital. Elas quase",
          "não mudam a contagem, mas mexem muito no capital total — vale ligar",
          "e desligar com a medida em \"Capital social total\"."
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("O que há na base"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::fluidRow(
          shiny::column(3, shiny::wellPanel(shiny::strong("Empresas"),        shiny::br(), shiny::textOutput(ns("kpi_n")))),
          shiny::column(3, shiny::wellPanel(shiny::strong("Capital total"),   shiny::br(), shiny::textOutput(ns("kpi_total")))),
          shiny::column(3, shiny::wellPanel(shiny::strong("Capital mediano"), shiny::br(), shiny::textOutput(ns("kpi_mediana")))),
          shiny::column(3, shiny::wellPanel(shiny::strong("Categorias"),      shiny::br(), shiny::textOutput(ns("kpi_categorias"))))
        ),
        shiny::plotOutput(ns("grafico"), height = "420px"),
        shiny::hr(),
        shiny::h4("Participação de cada categoria"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba01Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    ROTULO_DIM <- c(company_size = "porte",
                    legal_nature = "natureza jurídica",
                    owner_qualification = "qualificação do responsável")

    base <- shiny::reactive({
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d
    })

    # ---- Agregado por categoria, antes de cortar o top-N --------
    # Separado do corte de proposito: as porcentagens tem de ser sobre a
    # base inteira, e nao sobre o que sobrou no grafico - senao "Outros"
    # com 2% viraria 100% de si mesmo.
    agregado <- shiny::reactive({
      d <- base()
      cat_col <- as.character(d[[input$dimensao]])
      if (identical(input$dimensao, "company_size")) cat_col <- as.character(porte_pt(d$company_size))

      tab <- data.frame(
        categoria = names(tapply(d$capital_stock, cat_col, length)),
        empresas  = as.vector(tapply(d$capital_stock, cat_col, length)),
        capital   = as.vector(tapply(d$capital_stock, cat_col, sum)),
        mediana   = as.vector(tapply(d$capital_stock, cat_col, median)),
        stringsAsFactors = FALSE
      )
      tab$p_empresas <- tab$empresas / sum(tab$empresas)
      tab$p_capital  <- tab$capital / sum(tab$capital)
      tab[order(-tab[[if (identical(input$medida, "n")) "empresas" else "capital"]]), ]
    })

    # ---- Top-N, com o resto somado em "Outros" ------------------
    com_outros <- shiny::reactive({
      tab <- agregado()
      n_top <- min(input$top, nrow(tab))
      if (nrow(tab) <= n_top) return(tab)

      topo <- tab[seq_len(n_top), ]
      resto <- tab[-seq_len(n_top), ]
      outros <- data.frame(
        categoria  = paste0("Outros (", nrow(resto), " categorias)"),
        empresas   = sum(resto$empresas),
        capital    = sum(resto$capital),
        # Mediana de grupos somados nao existe: nao da para somar medianas.
        # NA aqui e honesto; inventar a media das medianas nao seria.
        mediana    = NA_real_,
        p_empresas = sum(resto$p_empresas),
        p_capital  = sum(resto$p_capital),
        stringsAsFactors = FALSE
      )
      rbind(topo, outros)
    })

    output$kpi_n <- shiny::renderText(numero_pt(nrow(base())))
    output$kpi_total <- shiny::renderText(reais_curto(sum(base()$capital_stock)))
    output$kpi_mediana <- shiny::renderText(reais(median(base()$capital_stock)))
    output$kpi_categorias <- shiny::renderText({
      paste(nrow(agregado()), "em", ROTULO_DIM[[input$dimensao]])
    })

    output$leitura <- shiny::renderText({
      tab <- agregado()
      maior <- tab[1, ]
      campo <- if (identical(input$medida, "n")) "p_empresas" else "p_capital"
      sprintf(
        paste("Das %s empresas, a maior categoria de %s é \"%s\", com %s%% %s.",
              "São %s categorias ao todo, mas a base é muito concentrada:",
              "as três maiores somam %s%%."),
        numero_pt(nrow(base())), ROTULO_DIM[[input$dimensao]], maior$categoria,
        numero_pt(100 * maior[[campo]], 1),
        if (identical(input$medida, "n")) "das empresas" else "do capital total",
        nrow(tab),
        numero_pt(100 * sum(head(tab[[campo]], 3)), 1)
      )
    })

    output$grafico <- shiny::renderPlot({
      tab <- com_outros()
      shiny::validate(shiny::need(nrow(tab) > 0, "Nenhuma empresa no filtro atual."))

      usa_n <- identical(input$medida, "n")
      tab$valor <- if (usa_n) tab$empresas else tab$capital
      tab$proporcao <- if (usa_n) tab$p_empresas else tab$p_capital
      tab$rotulo <- paste0(
        if (usa_n) numero_pt(tab$valor) else reais_curto(tab$valor),
        "  (", numero_pt(100 * tab$proporcao, 1), "%)"
      )

      # Uma cor so, e nao um degrade: o comprimento da barra ja diz quem e
      # maior. "Outros" sai em cinza porque nao e uma categoria, e um resto.
      tab$cor <- ifelse(grepl("^Outros", tab$categoria), CORES$neutro, CORES$principal)
      tab$categoria <- factor(tab$categoria, levels = rev(tab$categoria))

      ggplot2::ggplot(tab, ggplot2::aes(x = valor, y = categoria)) +
        ggplot2::geom_col(fill = tab$cor[order(tab$categoria, decreasing = TRUE)],
                          width = 0.75) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo), hjust = -0.05,
                           size = 3.8, colour = CORES$neutro) +
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.28)),
          labels = if (usa_n) function(v) numero_pt(v) else reais_curto
        ) +
        ggplot2::labs(
          title = paste("Composição da base por", ROTULO_DIM[[input$dimensao]]),
          subtitle = if (usa_n) "Número de empresas" else "Soma do capital social declarado",
          x = NULL, y = NULL
        ) +
        tema_app() +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(colour = "#EEF2F6"))
    })

    output$tabela <- DT::renderDataTable({
      tab <- com_outros()
      shiny::validate(shiny::need(nrow(tab) > 0, "Nenhuma empresa no filtro atual."))

      saida <- data.frame(
        Categoria = tab$categoria,
        Empresas = numero_pt(tab$empresas),
        `% das empresas` = paste0(numero_pt(100 * tab$p_empresas, 1), "%"),
        `Capital total` = reais_curto(tab$capital),
        `% do capital` = paste0(numero_pt(100 * tab$p_capital, 1), "%"),
        `Capital mediano` = ifelse(is.na(tab$mediana), "—", reais(tab$mediana)),
        check.names = FALSE, stringsAsFactors = FALSE
      )

      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE),
        caption = paste("Participação por", ROTULO_DIM[[input$dimensao]],
                        "- as porcentagens são sobre a base inteira.")
      )
    })
  })
}
