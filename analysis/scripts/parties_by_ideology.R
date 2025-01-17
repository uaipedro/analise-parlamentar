source("analysis/config/common.R")

# Create parties by ideology table
parties_by_ideology <- members |>
  mutate(
    ideology = if_else(is.na(espectro), "não classificado", espectro),
    ideology = factor(ideology, levels = ideology_order)
  ) |>
  group_by(ideology) |>
  summarise(
    parties = paste(sort(unique(partido)), collapse = ", ")
  ) |>
  pivot_wider(
    names_from = ideology,
    values_from = parties
  ) |>
  gt() |>
  tab_header(
    title = "Classificação dos partidos por espectro ideológico",
    subtitle = "Autodeclaração dos partidos políticos"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#E5E5E5"),
      cell_text(weight = "bold"),
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body()
  ) |>
  cols_width(
    everything() ~ px(150)
  ) |>
  opt_row_striping() |>
  text_transform(
    locations = cells_body(),
    fn = function(x) str_wrap(x, width = 40)
  ) |>
  cols_label(
    esquerda = "Esquerda",
    `centro-esquerda` = "Centro-Esquerda",
    centro = "Centro",
    `centro-direita` = "Centro-Direita",
    direita = "Direita",
    `não classificado` = "Não Classificado"
  ) |>
  tab_footnote(
    footnote = md("**Fonte:** 'De direita ou esquerda? Veja como os partidos políticos se definem no Brasil' (*Valor Econômico*, 4 de outubro de 2024)"),
    locations = cells_title()
  )

gtsave(parties_by_ideology, "plots/parties_by_ideology_table.png") 