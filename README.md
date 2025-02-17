# Dashboard de Emendas Parlamentares

Dashboard interativo para análise das emendas parlamentares individuais de 2024, desenvolvido como projeto prático da disciplina GES109.

## Sobre o Projeto

O dashboard apresenta visualizações sobre:
- Distribuição dos parlamentares por partido e espectro ideológico
- Valores empenhados por partido
- Distribuição dos recursos por área e espectro ideológico

## Como Reproduzir

1. Os dados brutos estão disponíveis em \`data/raw/\`

2. Execute o script de preparação dos dados:
   ```r
   Rscript load_and_clean.R
   ```

3. As análises individuais estão em \`analysis/scripts/\`. Todos os scripts importam as configurações comuns de \`analysis/config/common.R\` que carrega as bibliotecas e dados necessários.

4. Para gerar os relatórios:
   - \`relatorio.qmd\`: Relatório HTML inicial
   - \`dashboard.qmd\`: Dashboard interativo

## Autor

Pedro Mambelli Fernandes

Projeto desenvolvido como trabalho prático da disciplina GES109.
