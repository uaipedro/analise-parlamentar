source("analysis/config/common.R")

fmt_reais_auto <- function(x) {
  ifelse(abs(as.numeric(x)) >= 1e9,
         sprintf("R$ %.2f bi", x/1e9),
         ifelse(abs(x) >= 1e6,
                sprintf("R$ %.2f mi", x/1e6),
                sprintf("R$ %.2f", x)))
}

ammendments_stats <- amendments |>
  summarise(
    Média = mean(valor_empenhado, na.rm = TRUE),
    Mediana = median(valor_empenhado, na.rm = TRUE),
    Mínimo = min(valor_empenhado, na.rm = TRUE),
    Máximo = max(valor_empenhado, na.rm = TRUE),
    Total = sum(valor_empenhado, na.rm = TRUE),
    `CV (%)` = (sd(valor_empenhado, na.rm = TRUE) / mean(valor_empenhado, na.rm = TRUE)) * 100
  )

ammendments_stats_table <- ammendments_stats |>
  gt() |>
  tab_header(
    title = "Estatísticas Descritivas das Emendas Parlamentares",
    subtitle = "Valores empenhados em 2024"
  ) |>
  fmt(
    columns = c(Média, Mediana, Mínimo, Máximo, Total),
    fns = fmt_reais_auto
  ) |>
  fmt_number(
    columns = `CV (%)`,
    decimals = 1,
    locale = "pt-BR"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#E5E5E5"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )

gtsave(ammendments_stats_table, "plots/ammendments_stats.png") 