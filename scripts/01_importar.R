# =============================================================
# 01_importar.R - le os brutos, confere o que foi prometido e grava
#                 dados/tratados/companies.rds
#
# So R base, de proposito: a importacao e a parte que precisa rodar
# igual em qualquer maquina, sem depender de pacote instalado.
# Rode a partir da raiz do projeto.
# =============================================================

bruto <- "dados/brutos"

# -------------------------------------------------------------
# 1. Importacao
#
# O arquivo publicado e separado por VIRGULA (confira o cabecalho:
# "company_id","company_name",...), entao read.csv() e nao read.csv2().
# O script de limpeza no readme do TidyTuesday escreve com ponto e virgula,
# mas NAO e esse o arquivo que esta no repositorio - vale o cabecalho.
# -------------------------------------------------------------
companies <- read.csv(file.path(bruto, "companies.csv"), stringsAsFactors = FALSE)
legal_nature <- read.csv(file.path(bruto, "legal_nature.csv"), stringsAsFactors = FALSE)
qualifications <- read.csv(file.path(bruto, "qualifications.csv"), stringsAsFactors = FALSE)

# size.csv nao termina com quebra de linha, e o read.csv avisa
# ("linha final incompleta"). O dado vem completo - as 3 linhas estao la -,
# entao o aviso e sobre o arquivo, nao sobre o conteudo.
size <- suppressWarnings(
  read.csv(file.path(bruto, "size.csv"), stringsAsFactors = FALSE)
)

# -------------------------------------------------------------
# 2. Contrato dos dados
#
# Isto nao e decoracao: se o TidyTuesday republicar o arquivo com outro
# tamanho ou com NA, o script PARA aqui, em vez de deixar o relatorio
# inteiro mudar de numero sem ninguem perceber.
# -------------------------------------------------------------
stopifnot(
  ncol(companies) == 6,
  nrow(companies) == 141332,
  !anyNA(companies),
  !any(duplicated(companies$company_id)),
  is.numeric(companies$capital_stock)
)
cat("contrato ok:", nrow(companies), "empresas,", ncol(companies), "colunas, nenhum NA\n")

# -------------------------------------------------------------
# 3. Porte como fator ORDENADO
#
# A ordem vem do codigo do registro (size.csv): 1 = micro, 3 = small,
# 5 = other. Sem ordenar, qualquer grafico sai em ordem alfabetica
# ("micro, other, small"), que mistura a maior categoria no meio.
# -------------------------------------------------------------
ordem_porte <- size$company_size[order(size$id)]
ordem_porte <- ordem_porte[ordem_porte %in% companies$company_size]
companies$company_size <- factor(companies$company_size,
                                 levels = ordem_porte, ordered = TRUE)

# -------------------------------------------------------------
# 4. Codigos das tabelas de apoio
#
# ATENCAO: companies.csv guarda os ROTULOS, nao os codigos - as tabelas
# de apoio ja foram aplicadas na limpeza do TidyTuesday. Ou seja, nao ha
# join de id para fazer; o que da para fazer e o caminho inverso, trazer
# o codigo de volta pelo nome, que e util para ordenar e para conferir.
# -------------------------------------------------------------
companies$legal_nature_id <- legal_nature$id[match(companies$legal_nature,
                                                   legal_nature$legal_nature)]
companies$owner_qualification_id <- qualifications$id[match(companies$owner_qualification,
                                                            qualifications$owner_qualification)]
companies$company_size_id <- size$id[match(as.character(companies$company_size),
                                           size$company_size)]

# Todo rotulo tem de ter achado codigo. Se sobrar NA aqui, apareceu
# uma categoria que as tabelas de apoio nao conhecem.
stopifnot(!anyNA(companies$legal_nature_id),
          !anyNA(companies$owner_qualification_id),
          !anyNA(companies$company_size_id))

# -------------------------------------------------------------
# 5. Capital social implausivel - MARCADO, nao apagado
#
# O corte de R$ 10 bilhoes nao e arbitrario: acima dele o que aparece
# nao e empresa grande, e valor "coringa" de cadastro - 184 empresas com
# exatamente R$ 100.000.000.000,00 e 23 com R$ 999.999.999.999,00.
# Jogar fora silenciosamente mudaria a media do trabalho inteiro, entao
# fica uma coluna de marca e cada analise decide se usa ou nao.
# -------------------------------------------------------------
LIMIAR_SUSPEITO <- 1e10
companies$capital_suspeito <- companies$capital_stock >= LIMIAR_SUSPEITO
cat("capital implausivel (>= R$ 10 bi):", sum(companies$capital_suspeito),
    "empresas (", round(100 * mean(companies$capital_suspeito), 3), "%)\n")

# Capital em milhoes de reais, para os eixos nao sairem em notacao
# cientifica, e o log10, que e a escala em que essa variavel se le.
companies$capital_mi <- companies$capital_stock / 1e6
companies$log10_capital <- log10(companies$capital_stock)

# -------------------------------------------------------------
# 6. Gravar
# -------------------------------------------------------------
dir.create("dados/tratados", recursive = TRUE, showWarnings = FALSE)
saveRDS(companies, "dados/tratados/companies.rds")
saveRDS(list(legal_nature = legal_nature,
             qualifications = qualifications,
             size = size), "dados/tratados/apoio.rds")

cat("gravado: dados/tratados/companies.rds (",
    round(file.size("dados/tratados/companies.rds") / 1024), "KB )\n")
str(companies)
