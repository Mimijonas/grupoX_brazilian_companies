# =============================================================
# 00_baixar.R - traz os quatro arquivos do TidyTuesday para dados/brutos/
#
# Roda uma vez. Se o arquivo ja existe, nao baixa de novo: o trabalho
# precisa ser reproduzivel, e um arquivo que muda no meio da analise
# faria os numeros do relatorio nao fecharem com os graficos.
# Rode a partir da raiz do projeto.
# =============================================================

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/"

arquivos <- c("companies.csv", "legal_nature.csv", "qualifications.csv",
              "size.csv", "readme.md")

dir.create("dados/brutos", recursive = TRUE, showWarnings = FALSE)

for (arq in arquivos) {
  destino <- file.path("dados/brutos", arq)
  if (file.exists(destino)) {
    cat("ja existe, pulando:", arq, "\n")
    next
  }
  cat("baixando:", arq, "... ")
  download.file(paste0(url, arq), destino, quiet = TRUE)
  cat(round(file.size(destino) / 1024), "KB\n")
}

cat("\nconteudo de dados/brutos:\n")
print(data.frame(arquivo = arquivos,
                 kb = round(file.size(file.path("dados/brutos", arquivos)) / 1024)))
