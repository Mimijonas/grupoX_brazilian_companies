# =============================================================
# 02_perfil.R - diagnostico da base, gravado em saidas/tabelas/perfil.txt
#
# O relatorio cita numeros ("84% sao LLC", "262 empresas com capital
# implausivel"). Eles saem daqui, e nao de conta feita a mao no console:
# se a base mudar, o perfil muda junto e a divergencia aparece.
# Rode a partir da raiz do projeto, depois do 01.
# =============================================================

companies <- readRDS("dados/tratados/companies.rds")
apoio <- readRDS("dados/tratados/apoio.rds")

dir.create("saidas/tabelas", recursive = TRUE, showWarnings = FALSE)
saida <- file("saidas/tabelas/perfil.txt", open = "wt", encoding = "UTF-8")
sink(saida); sink(saida, type = "message")

cat("PERFIL DA BASE - Brazilian Companies (TidyTuesday 2026-01-27)\n")
cat("gerado em", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n")
cat(strrep("=", 62), "\n\n")

cat("## 1. Tamanho e tipos\n")
cat("linhas:", nrow(companies), " colunas:", ncol(companies), "\n\n")
print(sapply(companies, class))

cat("\n## 2. Completude\n")
cat("NA por coluna (todas devem ser 0):\n"); print(colSums(is.na(companies)))
cat("texto vazio em company_name:", sum(trimws(companies$company_name) == ""), "\n")
cat("company_id duplicado:", sum(duplicated(companies$company_id)), "\n")
cat("linha inteira duplicada:", sum(duplicated(companies)), "\n")
cat("nome de empresa repetido:", sum(duplicated(companies$company_name)),
    "(homonimos com id diferente - nao e erro)\n")

cat("\n## 3. Capital social (R$)\n")
print(summary(companies$capital_stock))
cat("\nquantis:\n")
print(quantile(companies$capital_stock, c(0, .01, .25, .5, .75, .9, .99, .999, 1)))
cat("\nmedia / mediana =",
    round(mean(companies$capital_stock) / median(companies$capital_stock), 1),
    "-> a distribuicao e muito assimetrica; mediana e a medida honesta\n")
# Formato brasileiro: ponto separa milhar, virgula separa decimal. Sem o
# decimal.mark, o format() usa ponto para os dois e sai "150.000.1".
cat("minimo:", format(min(companies$capital_stock), big.mark = ".",
                      decimal.mark = ",", nsmall = 2),
    "- a base ja vem filtrada em R$ 150.000 (ver readme do TidyTuesday)\n")
cat("\nvalores implausiveis (>= R$ 10 bi):", sum(companies$capital_suspeito), "\n")
print(head(sort(table(companies$capital_stock[companies$capital_suspeito]),
                decreasing = TRUE), 8))

cat("\n## 4. Categorias\n")
cat("\ncompany_size (", nlevels(companies$company_size), "niveis usados de",
    nrow(apoio$size), "no registro ):\n")
print(table(companies$company_size))
cat("\nlegal_nature:", length(unique(companies$legal_nature)), "niveis usados de",
    nrow(apoio$legal_nature), "no registro\n")
print(head(sort(table(companies$legal_nature), decreasing = TRUE), 10))
cat("\nowner_qualification:", length(unique(companies$owner_qualification)),
    "niveis usados de", nrow(apoio$qualifications), "no registro\n")
print(head(sort(table(companies$owner_qualification), decreasing = TRUE), 10))

cat("\nconcentracao: a maior categoria de legal_nature responde por",
    round(100 * max(table(companies$legal_nature)) / nrow(companies), 1), "% das empresas\n")

cat("\n## 5. Capital por porte\n")
tab <- data.frame(
  n        = as.vector(table(companies$company_size)),
  mediana  = round(tapply(companies$capital_stock, companies$company_size, median)),
  media    = round(tapply(companies$capital_stock, companies$company_size, mean)),
  p90      = round(tapply(companies$capital_stock, companies$company_size,
                          quantile, .9)),
  maximo   = round(tapply(companies$capital_stock, companies$company_size, max))
)
print(tab)
cat("\nmediana e media andam em direcoes diferentes entre os portes:",
    "e o efeito dos valores coringa, concentrados em poucas linhas.\n")

cat("\n## 6. O mesmo, so com as empresas plausiveis\n")
ok <- companies[!companies$capital_suspeito, ]
print(data.frame(
  n       = as.vector(table(ok$company_size)),
  mediana = round(tapply(ok$capital_stock, ok$company_size, median)),
  media   = round(tapply(ok$capital_stock, ok$company_size, mean))
))

sink(type = "message"); sink(); close(saida)
cat("gravado: saidas/tabelas/perfil.txt\n")
cat(readLines("saidas/tabelas/perfil.txt", warn = FALSE), sep = "\n")
