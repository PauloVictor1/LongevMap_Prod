# LongevMap

Aplicação **Shiny** para análise territorial da longevidade nos municípios brasileiros. Integra bases demográficas, socioeconômicas e de saúde; disponibiliza mapas interativos, exploração tabular, inspeção de resíduos de modelos (OLS / SAR / GWR / MGWR) e resumos automáticos de documentos dos Conselhos Municipais do Idoso (CMIs) via API da OpenAI.

> **Repositório de produção:** [https://github.com/PauloVictor1/LongevMap\_Prod](https://github.com/PauloVictor1/LongevMap_Prod)

---

## Índice

1. [Funcionalidades](#funcionalidades)
2. [Estrutura de pastas](#estrutura-de-pastas)
3. [Instalação rápida](#instalação-rápida)
4. [Variáveis de ambiente](#variáveis-de-ambiente)
5. [Dados & modelos](#dados--modelos)
6. [Uso básico](#uso-básico)
7. [Contribuição](#contribuição)
8. [Licença](#licença)

---

## Funcionalidades

| Painel                             | Finalidade                                                                     |
| ---------------------------------- | ------------------------------------------------------------------------------ |
| **Mapa Interativo do Brasil**      | Mapas Coropléticos de variáveis brutas ou per‑capitas com filtro por UF.       |
| **Tabela de Dados**                | Filtros dinâmicos, DataTable exportável e scatterplots Plotly entre variáveis. |
| **Raio‑X Municipal**               | Gráfico radar normalizado (0‑1) comparando municípios.                         |
| **Resíduos**                       | Mapa dos resíduos dos modelos (OLS, SAR, GWR, MGWR) + sumário estatístico.     |
| **Modelos Locais**                 | Mapeamento de coeficientes locais, significância (\|t\| > 1,96) e bandas.      |
| **Análise de Documentos dos CMIs** | Upload de PDF → resumo em tópicos, nuvem de palavras e gráfico de frequência.  |
| **Municípios Aptos**               | Consulta a municípios ou UFs aptos a receber doações para o Fundo do Idoso.    |

---

## Estrutura de pastas

```text
.
├── app.R                  # Código principal da aplicação
├── data/                  # Objetos espaciais e tabelas (.rds) – **fora do Git**
├── models/                # Modelos estatísticos pré‑treinados (.rds) – **fora do Git**
├── Dados_CSV/             # Versões CSV (bruta & per‑capita) sem geometria – para usuários sem R
└── README.md              # Este documento
```

> As pastas **data/** e **models/** não são versionadas para evitar repositório pesado e questões de licenciamento. A pasta **Dados\_CSV/** *é* versionada: disponibiliza dados tabulares (sem coordenadas) para quem quiser usar Python, Excel etc.

---

## Instalação rápida

### 1 . Pré‑requisitos

* R ≥ 4.2
* Pacotes listados no topo de `app.R`
* [Pandoc](https://pandoc.org) ≥ 2.14 (exigido por **pdftools** em alguns sistemas)
* Chave da API da OpenAI (caso deseje usar o painel de PDFs)

### 2 . Clonar o repositório

```bash
git clone https://github.com/PauloVictor1/LongevMap_Prod.git
cd LongevMap_Prod
```

### 3 . (Opcional) ambiente reprodutível com **renv**

```bash
Rscript -e "install.packages('renv'); renv::restore()"
```

*Instala o pacote **renv** (se necessário) e restaura as versões de pacotes registradas em `renv.lock`. Se preferir instalar pacotes manualmente, pule esta etapa.*

### 4 . Adicionar dados e modelos

Coloque arquivos `.rds` apropriados nas pastas **data/** e **models/**. Caso não deseje usar funcionalidades espaciais ou de modelagem, é possível rodar apenas os painéis **Mapa Interativo** e **Tabela de Dados** com os CSVs da pasta **Dados\_CSV/** — basta ajustar o carregamento em `app.R`.

### 5 . Executar

```r
shiny::runApp('app.R', launch.browser = TRUE)
```

A aplicação abrirá em `http://localhost:<porta>`.

---

## Variáveis de ambiente

| Nome             | Propósito                                             | Definição                                                                     |
| ---------------- | ----------------------------------------------------- | ----------------------------------------------------------------------------- |
| `OPENAI_API_KEY` | Token usado pelo pacote **openai** para resumir PDFs. | Defina em `~/.Renviron` ou antes de iniciar R: `export OPENAI_API_KEY="sk-…"` |

**Nunca** versione sua chave. Garanta que arquivos `.Renviron` estejam no `.gitignore`.

*Se a variável estiver ausente, o painel de PDFs exibirá erro de autenticação; os demais painéis permanecem operacionais.*

---

## Dados & modelos

### Dados espaciais (`data/*.rds`)

* Classe `sf` com polígonos municipais.
* Colunas mínimas: `Código Município`, `Nome Município`, `Sigla_UF` + indicadores numéricos.

### Dados tabulares (`Dados_CSV/*.csv`)

* Arquivos *raw* e *per‑capita* sem geometria — úteis para Python, Excel, Power BI etc.

### Modelos (`models/*.rds`)

* Objetos de **spatialreg** ou **GWmodel** (OLS, SAR, GWR, MGWR) compatíveis com `summary()`.
* As colunas de resíduos devem existir no objeto espacial referenciado por `sf_objeto_GWR` com os nomes esperados (ex.: `model_SAR_Res_TIpc`).

---

## Uso básico

1. **Escolha a base** (bruta ou per‑capita).
2. **Navegue** pelos painéis.
3. **Filtre** por UF ou município quando disponível.
4. **Explore modelos** alternando entre OLS, SAR, GWR Básico e MGWR.
5. **(Opcional)** Faça upload de PDFs dos CMIs.
6. **Exporte** tabelas ou gráficos.

---

## Contribuição

1. Crie um branch: `git checkout -b feature/minha-feature`.
2. Siga o guia de estilo *tidyverse*.
3. Explique *o que* e *por que* no PR.
4. Não suba dados sensíveis nem credenciais.

Para mudanças significativas, abra uma *issue* antes.

---

## Licença

Distribuído sob **MIT License**. Veja [`LICENSE`](LICENSE).

> © 2025 Colaboradores do LongevMap
