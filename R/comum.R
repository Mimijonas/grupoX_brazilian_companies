# =============================================================
# comum.R - o que as 10 abas compartilham
#
# Tudo aqui so DEFINE coisa (funcao e constante). Nada roda, nada le
# arquivo grande, nada baixa nada: este arquivo e carregado na partida do
# app, e qualquer efeito colateral aqui atrasaria o app inteiro.
# =============================================================

# ---- Dados --------------------------------------------------
# O .rds tratado tem 3,3 MB e esta versionado de proposito: o shinyapps.io
# sobe apenas a pasta do app, entao os dados precisam estar aqui dentro.
# Os CSV brutos (20 MB) ficam de fora - scripts/00_baixar.R os traz de volta.
carrega_dados <- function(caminho = "dados/tratados/companies.rds") {
  if (!file.exists(caminho)) {
    stop("Nao achei ", caminho,
         ". Rode scripts/00_baixar.R e scripts/01_importar.R a partir da raiz do projeto.")
  }
  readRDS(caminho)
}

# ---- Rotulos em portugues -----------------------------------
# A base vem em ingles. Os rotulos de porte aparecem em quase toda aba,
# entao a traducao mora num lugar so - caso contrario cada aba escreve a
# sua e o app fala duas linguas.
PORTE_PT <- c(
  "micro-enterprise" = "Microempresa",
  "small-enterprise" = "Pequena empresa",
  "other"            = "Demais empresas"
)

porte_pt <- function(x) {
  rotulos <- PORTE_PT[as.character(x)]
  rotulos[is.na(rotulos)] <- as.character(x)[is.na(rotulos)]
  factor(rotulos, levels = unname(PORTE_PT[levels(droplevels(as.factor(x)))]))
}

# ---- Dinheiro -----------------------------------------------
# Padrao brasileiro: ponto no milhar, virgula no decimal. O format() do R
# usa ponto para os dois se nao receber decimal.mark, e sai "150.000.1".
reais <- function(x, digitos = 0) {
  paste0("R$ ", formatC(x, format = "f", digits = digitos,
                        big.mark = ".", decimal.mark = ","))
}

# Valores grandes por extenso curto, para eixo e caixa de resumo:
# 1.2e9 -> "R$ 1,2 bi". Eixo com "1200000000" nao se le.
reais_curto <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    abs_v <- abs(v)
    if (abs_v >= 1e12)     paste0("R$ ", formatC(v / 1e12, format = "f", digits = 1, decimal.mark = ","), " tri")
    else if (abs_v >= 1e9) paste0("R$ ", formatC(v / 1e9, format = "f", digits = 1, decimal.mark = ","), " bi")
    else if (abs_v >= 1e6) paste0("R$ ", formatC(v / 1e6, format = "f", digits = 1, decimal.mark = ","), " mi")
    else if (abs_v >= 1e3) paste0("R$ ", formatC(v / 1e3, format = "f", digits = 0, decimal.mark = ","), " mil")
    else                   reais(v)
  }, USE.NAMES = FALSE)
}

# Eixo em log10: a quebra 5 vira "R$ 100 mil". O leitor nao le "5".
rotulo_log10 <- function(x) reais_curto(10^x)

numero_pt <- function(x, digitos = 0) {
  formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
}

# ---- Rotulos longos -----------------------------------------
# "Individual Limited Liability Company (Business)" tem 47 caracteres e
# nao cabe no eixo de um heatmap. Cortar no meio de uma palavra e feio,
# entao corta-se na ultima palavra que couber.
encurtar <- function(x, limite = 28) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || nchar(s) <= limite) return(s)
    palavras <- strsplit(s, " ", fixed = TRUE)[[1]]
    acc <- character(0)
    for (p in palavras) {
      se_juntar <- paste(c(acc, p), collapse = " ")
      if (nchar(se_juntar) > limite - 1) break
      acc <- c(acc, p)
    }
    if (length(acc) == 0) return(paste0(substr(s, 1, limite - 1), "…"))
    paste0(paste(acc, collapse = " "), "…")
  }, character(1), USE.NAMES = FALSE)
}

# ---- Cores --------------------------------------------------
# Uma paleta para o app todo, para as 10 abas nao sairem cada uma de uma cor.
CORES <- list(
  principal = "#2C5F8A",
  destaque  = "#C25E00",
  neutro    = "#6B7280",
  claro     = "#DCE7F0",
  porte     = c("Microempresa"    = "#9ECAE1",
                "Pequena empresa" = "#4A90B8",
                "Demais empresas" = "#1F4E6B")
)

# ---- Assimetria ---------------------------------------------
# Terceiro momento padronizado, na mao: evita depender do e1071 so por isto.
assimetria <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sqrt(sum((x - m)^2) / n)
  (sum((x - m)^3) / n) / s^3
}

# ---- Tema dos graficos --------------------------------------
tema_app <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle   = ggplot2::element_text(colour = CORES$neutro),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
