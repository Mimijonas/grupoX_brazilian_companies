# grupoX_brazilian_companies

Trabalho final de **CE302 — Elementos de Programação para Estatística** (UFPR,
2026.2). Análise da base **Brazilian Companies** do TidyTuesday (`2026-01-27`):
141.332 empresas brasileiras com capital social declarado acima de R$ 150 mil,
a partir do Cadastro Nacional da Pessoa Jurídica (Receita Federal).

## Integrantes

- Ana Sofia
- Breno Dias
- Camila Ogawa
- Henrique Kotelok
- Maria Oliveira

## Entregas

| Item | Link |
|---|---|
| Aplicativo Shiny | _a publicar (shinyapps.io)_ |
| Vídeo de apresentação | _a gravar (YouTube)_ |
| Relatório | [`relatorio/relatorio.Rmd`](relatorio/relatorio.Rmd) — **pronto**: 30 páginas A4 em [`relatorio.pdf`](relatorio/relatorio.pdf) |

## Como rodar

Precisa de R com `shiny`, `ggplot2` e `DT`:

```r
install.packages(c("shiny", "ggplot2", "DT"))
```

Os dados tratados (`dados/tratados/companies.rds`) já estão no repositório, então
o app roda direto:

```r
shiny::runApp()
```

Para refazer o tratamento a partir dos CSV originais (eles **não** estão
versionados — são 20 MB que o script baixa de novo em segundos):

```r
source("scripts/00_baixar.R")    # baixa os 4 CSV para dados/brutos/
source("scripts/01_importar.R")  # confere, trata e grava dados/tratados/
source("scripts/02_perfil.R")    # diagnóstico em saidas/tabelas/perfil.txt
```

## Organização

```
app.R                      junta as 10 abas e sobe o app
R/comum.R                  o que todas as abas compartilham (cores, formatação)
R/abaNN_assunto.R          uma aba por arquivo
scripts/                   baixar, tratar e perfilar os dados
dados/tratados/            companies.rds (versionado: o app precisa dele)
dados/brutos/              CSV originais (fora do repositório)
relatorio/relatorio.Rmd    o relatório
relatorio/impressao.css    folha de estilo A4 (corpo 12pt, entrelinha 1,5)
relatorio/render.R         renderiza o .Rmd e imprime o PDF
```

## Relatório

```bash
cd relatorio && Rscript render.R
```

O `render.R` compila o `.Rmd` e, em seguida, imprime o HTML em PDF A4 usando o
Chrome em modo headless. O caminho usual seria `bookdown::pdf_document2`, que
exige uma distribuição LaTeX instalada; o Chrome já está na máquina e respeita
a folha de estilo `impressao.css`, onde ficam as margens, o corpo de 12pt e a
entrelinha de 1,5. O número de páginas do PDF é, portanto, o número de páginas
que se vê no papel: **30**.

Nenhum valor numérico do texto foi digitado. Todos saem de código embutido
(`` `r ...` ``) sobre `dados/tratados/companies.rds`, de modo que mudar o
pré-processamento muda o texto na próxima renderização.

**Cada aba vive em seu próprio arquivo.** É o que permite cinco pessoas
trabalharem ao mesmo tempo: dois PRs que mexem no mesmo `app.R` brigam linha a
linha, dois PRs em arquivos diferentes não se encostam. Para somar uma aba:

1. crie `R/abaNN_seu_assunto.R` com `abaNNUI(id, dados)` e `abaNNServer(id, dados)`
   — use `R/aba02_distribuicao.R` como molde;
2. acrescente **uma linha** na lista `ABAS`, em `app.R`.

## Abas

| # | Aba | Responsável | Estado |
|---|---|---|---|
| 1 | Panorama geral | Breno | **pronta** |
| 2 | Distribuição do capital social | Breno | **pronta** |
| 3 | Porte da empresa | Breno | **pronta** |
| 4 | Natureza jurídica | Breno | **pronta** |
| 5 | Qualificação do responsável | Breno | **pronta** |
| 6 | Cruzamento de duas categorias | Breno | **pronta** |
| 7 | Cauda superior (top 1%) | Breno | **pronta** |
| 8 | Concentração (Lorenz / Gini) | Breno | **pronta** |
| 9 | Qualidade do dado | Breno | **pronta** |
| 10 | Busca de empresas | Breno | **pronta** |

A divisão acima é uma **proposta** — vale o que o grupo combinar. O que ela leva
em conta é o que a base sustenta: são 6 colunas, com **uma única variável
quantitativa** (`capital_stock`) e três categóricas. Não há data, UF nem CNAE,
então aba de "tendência no tempo", mapa ou corte por setor **não é possível**
com estes dados.

## Declaração de uso de IA

Conforme exigido no enunciado: o uso de inteligência artificial deve ser
declarado em todas as partes do projeto. Cada integrante registra aqui o que
usou e em quê.

- **Breno** — Claude (Anthropic) no apoio à estruturação do código de
  importação e tratamento (`scripts/`), na construção dos módulos do
  aplicativo (`R/aba*.R`) e na revisão da redação do relatório. As decisões
  analíticas, a escolha das técnicas estatísticas e a interpretação dos
  resultados são de responsabilidade dos autores. Todos os valores numéricos
  foram conferidos contra `saidas/tabelas/perfil.txt`, gerado pelos próprios
  scripts, de forma independente do texto.
