source("analysis/config/common.R")

# Create ideology distribution table
ideology_table <- members |>
  mutate(
    ideology = if_else(is.na(espectro), "não classificado", espectro),
    ideology = factor(ideology, levels = ideology_order)
  ) |>
  count(ideology, name = "count") |>
  mutate(
    percentage = round(count / sum(count) * 100, 1),
    cumulative_pct = round(cumsum(percentage), 1)
  )

# Create ideology distribution plot
ideology_plot <- ggplot(ideology_table, aes(x = ideology, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  labs(title = "Distribuição dos Parlamentares por Espectro Ideológico",
       x = "Espectro Ideológico",
       y = "Número de Parlamentares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/ideological_spectrum.png", ideology_plot, width = 10, height = 6)

# Create formatted table
ideology_gt_table <- ideology_table |>
  gt() |>
  cols_label(
    ideology = "Espectro Ideológico",
    count = "Quantidade",
    percentage = "% do Total",
    cumulative_pct = "% Acumulada"
  ) |>
  tab_header(
    title = "Tabela de Frequência de Parlamentares por Espectro Ideológico",
    subtitle = "Mais de 25% dos deputados pertencem a partidos sem posicionamento ideológico declarado"
  ) |>
  fmt_number(
    columns = c(percentage, cumulative_pct),
    decimals = 1,
    suffix = "%"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#E5E5E5"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_footnote(
    footnote = md("**Fonte:** Análise baseada na autodeclaração ideológica dos partidos políticos segundo reportagem 'De direita ou esquerda? Veja como os partidos políticos se definem no Brasil' (*Valor Econômico*, 4 de outubro de 2024)"),
    locations = cells_title()
  )

gtsave(ideology_gt_table, "plots/ideology_table.png") 