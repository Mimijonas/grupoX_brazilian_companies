# Renderiza o relatorio e, em seguida, imprime o HTML em PDF A4 usando o
# Chrome em modo headless. O caminho por LaTeX exigiria instalar uma
# distribuicao TeX inteira; o Chrome ja esta na maquina e respeita a folha
# de estilo impressao.css, onde as margens e o corpo de 12pt estao
# definidos. O numero de paginas do PDF e, portanto, o numero de paginas
# que a professora ve.
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

rmarkdown::render("relatorio.Rmd", quiet = FALSE)

chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if (file.exists(chrome)) {
  html <- normalizePath("relatorio.html")
  system2(chrome,
          c("--headless", "--disable-gpu", "--no-pdf-header-footer",
            "--virtual-time-budget=20000",
            paste0("--print-to-pdf=", file.path(getwd(), "relatorio.pdf")),
            paste0("file://", html)),
          stdout = NULL, stderr = NULL)
  message("PDF gerado: relatorio.pdf")
}
