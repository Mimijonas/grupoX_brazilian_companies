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
| Relatório | `relatorio/` — _a fazer_ |

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
relatorio/                 o .Rmd do relatório
```

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
| 3 | Porte da empresa | | |
| 4 | Natureza jurídica | | |
| 5 | Qualificação do responsável | | |
| 6 | Natureza jurídica × porte | | |
| 7 | Cauda superior (top 1%) | | |
| 8 | Concentração (Lorenz / Gini) | | |
| 9 | Qualidade do dado | | |
| 10 | Busca de empresas | | |

A divisão acima é uma **proposta** — vale o que o grupo combinar. O que ela leva
em conta é o que a base sustenta: são 6 colunas, com **uma única variável
quantitativa** (`capital_stock`) e três categóricas. Não há data, UF nem CNAE,
então aba de "tendência no tempo", mapa ou corte por setor **não é possível**
com estes dados.

## Declaração de uso de IA

Conforme exigido no enunciado: o uso de inteligência artificial deve ser
declarado em todas as partes do projeto. Cada integrante registra aqui o que
usou e em quê.

- **Breno** — Claude (Anthropic) no preparo dos dados (`scripts/`) e na aba 2:
  estruturação do código, geração dos gráficos e revisão. Os achados
  estatísticos foram verificados contra `saidas/tabelas/perfil.txt`, gerado
  pelos próprios scripts.
