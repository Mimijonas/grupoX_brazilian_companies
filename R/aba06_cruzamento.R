# =============================================================
# ABA 6 - Cruzamento de duas categorias
#
# Aspecto desta aba: ASSOCIACAO. As abas 3, 4 e 5 olham uma categoria de
# cada vez, e e exatamente por isso que elas nao conseguem responder a
# pergunta que deixam no ar: quando "Director / Officer" aparece com
# capital alto, e o cargo ou e a forma juridica por tras dele?
#
# Aqui as duas entram ao mesmo tempo. Fixando a natureza juridica e
# comparando as qualificacoes DENTRO dela, o efeito da forma juridica
# sai da conta - e o que sobra e o efeito do cargo, se houver.
#
# A celula pode mostrar contagem, perfil de linha, perfil de coluna ou
# capital mediano. Sao perguntas diferentes: "quantas sao", "como se
# dividem" e "quanto declaram" nao se respondem com o mesmo numero.
# =============================================================

ABA06_DIMENSOES <- c("Porte" = "company_size",
                     "Natureza jurídica" = "legal_nature",
                     "Qualificação do responsável" = "owner_qualification")

aba06UI <- function(id, dados) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "6. Cruzamento",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Controles"),
        shiny::selectInput(ns("linha"), "Nas linhas",
                           choices = ABA06_DIMENSOES, selected = "legal_nature"),
        shiny::selectInput(ns("coluna"), "Nas colunas",
                           choices = ABA06_DIMENSOES, selected = "company_size"),
        shiny::radioButtons(
          ns("celula"), "O que cada célula mostra",
          choices = c("Número de empresas" = "n",
                      "% dentro da linha" = "linha",
                      "% dentro da coluna" = "coluna",
                      "Capital mediano" = "mediana"),
          selected = "linha"
        ),
        shiny::sliderInput(ns("top_linha"), "Categorias nas linhas",
                           min = 3, max = 15, value = 8, step = 1),
        shiny::sliderInput(ns("min_celula"), "Mínimo de empresas na célula",
                           min = 0, max = 200, value = 10, step = 5),
        shiny::helpText(
          "Célula abaixo do mínimo sai em branco: uma mediana calculada",
          "sobre duas empresas não é uma mediana."
        ),
        shiny::checkboxInput(
          ns("coringas"),
          "Incluir valores implausíveis (≥ R$ 10 bi)",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h3("Duas categorias ao mesmo tempo"),
        shiny::p(shiny::textOutput(ns("leitura"))),
        shiny::plotOutput(ns("grafico"), height = "470px"),
        shiny::hr(),
        shiny::h4("Tabela de contingência"),
        DT::dataTableOutput(ns("tabela"))
      )
    )
  )
}

aba06Server <- function(id, dados) {
  shiny::moduleServer(id, function(input, output, session) {

    valores <- function(d, col) {
      if (identical(col, "company_size")) as.character(porte_pt(d$company_size))
      else as.character(d[[col]])
    }

    base <- shiny::reactive({
      shiny::validate(shiny::need(
        !identical(input$linha, input$coluna),
        "Escolha duas categorias diferentes para linha e coluna."
      ))
      d <- dados
      if (!isTRUE(input$coringas)) d <- d[!d$capital_suspeito, ]
      d$.l <- valores(d, input$linha)
      d$.c <- valores(d, input$coluna)
      d
    })

    # As linhas mostradas sao as maiores em numero de empresas - e o
    # criterio que mantem o quadro legivel sem esconder nada relevante.
    grade <- shiny::reactive({
      d <- base()
      linhas <- names(sort(table(d$.l), decreasing = TRUE))
      linhas <- utils::head(linhas, input$top_linha)
      colunas <- names(sort(table(d$.c), decreasing = TRUE))
      d <- d[d$.l %in% linhas, ]

      comb <- expand.grid(l = linhas, c = colunas, stringsAsFactors = FALSE)
      comb$n <- mapply(function(a, b) sum(d$.l == a & d$.c == b), comb$l, comb$c)
      comb$mediana <- mapply(function(a, b) {
        x <- d$capital_stock[d$.l == a & d$.c == b]
        if (length(x) == 0) NA_real_ else median(x)
      }, comb$l, comb$c)

      total_linha <- tapply(comb$n, comb$l, sum)
      total_coluna <- tapply(comb$n, comb$c, sum)
      comb$p_linha <- comb$n / as.numeric(total_linha[comb$l])
      comb$p_coluna <- comb$n / as.numeric(total_coluna[comb$c])

      comb$l <- factor(comb$l, levels = rev(linhas))
      comb$c <- factor(comb$c, levels = colunas)
      comb
    })

    output$leitura <- shiny::renderText({
      g <- grade()
      shiny::req(nrow(g) > 0)
      nomes <- names(ABA06_DIMENSOES)
      nome_l <- nomes[match(input$linha, ABA06_DIMENSOES)]
      nome_c <- nomes[match(input$coluna, ABA06_DIMENSOES)]

      validas <- g[!is.na(g$mediana) & g$n >= max(input$min_celula, 1), ]
      if (nrow(validas) == 0) return("Nenhuma célula atinge o mínimo de empresas.")
      alta <- validas[which.max(validas$mediana), ]
      baixa <- validas[which.min(validas$mediana), ]
      sprintf(
        paste("Cruzando %s (linhas) com %s (colunas): a célula de maior capital",
              "mediano é \"%s\" × \"%s\", com %s em %s empresas; a menor é",
              "\"%s\" × \"%s\", com %s. Comparar células da MESMA linha é o que",
              "isola o efeito da coluna — é essa comparação, e não o ranking",
              "geral, que responde se a diferença vem de uma categoria ou da outra."),
        tolower(nome_l), tolower(nome_c),
        alta$l, alta$c, reais(alta$mediana), numero_pt(alta$n),
        baixa$l, baixa$c, reais(baixa$mediana)
      )
    })

    output$grafico <- shiny::renderPlot({
      g <- grade()
      shiny::validate(shiny::need(nrow(g) > 0, "Nenhum dado no filtro atual."))

      g$valor <- switch(input$celula,
                        n = g$n, linha = g$p_linha, coluna = g$p_coluna, mediana = g$mediana)
      # Celula abaixo do minimo nao e desenhada: um quadrado colorido
      # sobre duas empresas convida a uma leitura que o dado nao sustenta.
      g$valor[g$n < input$min_celula] <- NA
      g$rotulo <- switch(input$celula,
        n       = ifelse(is.na(g$valor), "", numero_pt(g$valor)),
        linha   = ifelse(is.na(g$valor), "", paste0(numero_pt(100 * g$valor, 1), "%")),
        coluna  = ifelse(is.na(g$valor), "", paste0(numero_pt(100 * g$valor, 1), "%")),
        mediana = ifelse(is.na(g$valor), "", reais_curto(g$valor))
      )
      # A escala de cor do capital e logaritmica pela mesma razao de
      # sempre: em reais, uma celula de R$ 150 mi apagaria todas as outras.
      g$cor <- if (identical(input$celula, "mediana")) log10(g$valor) else g$valor

      titulo <- switch(input$celula,
        n       = "Quantas empresas em cada combinação",
        linha   = "Como cada linha se divide entre as colunas",
        coluna  = "Como cada coluna se divide entre as linhas",
        mediana = "Capital social mediano de cada combinação"
      )

      ggplot2::ggplot(g, ggplot2::aes(x = c, y = l, fill = cor)) +
        ggplot2::geom_tile(colour = "white", linewidth = 1.1) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo), size = 3.5, colour = "#1F2937") +
        ggplot2::scale_fill_gradient(low = "#EDF3F8", high = "#7FB3D5",
                                     na.value = "#F6F7F9", guide = "none") +
        ggplot2::scale_y_discrete(labels = function(v) encurtar(v, 30)) +
        ggplot2::scale_x_discrete(labels = function(v) encurtar(v, 18), position = "top") +
        ggplot2::labs(title = titulo,
                      subtitle = "Célula em branco: abaixo do mínimo de empresas",
                      x = NULL, y = NULL) +
        tema_app() +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))
    })

    output$tabela <- DT::renderDataTable({
      g <- grade()
      shiny::validate(shiny::need(nrow(g) > 0, "Nenhum dado no filtro atual."))
      g <- g[order(g$l, g$c), ]
      saida <- data.frame(
        Linha = as.character(g$l),
        Coluna = as.character(g$c),
        Empresas = numero_pt(g$n),
        `% da linha` = paste0(numero_pt(100 * g$p_linha, 1), "%"),
        `% da coluna` = paste0(numero_pt(100 * g$p_coluna, 1), "%"),
        `Capital mediano` = ifelse(is.na(g$mediana) | g$n < input$min_celula,
                                   "—", reais(g$mediana)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        saida, rownames = FALSE,
        options = list(dom = "tp", pageLength = 12, ordering = FALSE, scrollX = TRUE),
        caption = "Uma linha por combinação. O traço marca as células abaixo do mínimo."
      )
    })
  })
}
