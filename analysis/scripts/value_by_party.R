source("analysis/config/common.R")
library(ggiraph)

party_totals <- amendments |>
  group_by(partido) |>
  summarise(
    valor_total = sum(valor_empenhado, na.rm = TRUE),
    num_emendas = n()
  )

grouped_parties <- party_totals |>
  mutate(
    proporcao = valor_total / sum(valor_total),
    partido = partido
  )

consolidated_totals <- grouped_parties |>
  group_by(partido) |>
  summarise(
    valor_total = sum(valor_total),
    num_emendas = sum(num_emendas)
  ) |>
  arrange(desc(valor_total))

parlamentares <- read_csv2("data/processed/parlamentares.csv")



parlamentares_por_partido <- parlamentares |>
  group_by(partido) |>
  summarise(num_parlamentares = n())

proporcoes_por_funcao <- amendments |>
  group_by(partido, funcao) |>
  summarise(
    valor_funcao = sum(valor_empenhado, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  group_by(partido) |>
  mutate(
    proporcao_funcao = valor_funcao / sum(valor_funcao)
  ) |>
  arrange(partido, desc(proporcao_funcao)) |>
  group_by(partido) |>
  slice_head(n = 5)

tooltip_text <- proporcoes_por_funcao |>
  group_by(partido) |>
  summarise(
    texto_funcoes = paste(
      funcao,
      scales::percent(proporcao_funcao, accuracy = 0.1),
      collapse = "<br>"
    ),
    .groups = 'drop'
  )

amendment_data <- consolidated_totals |>
  left_join(parlamentares_por_partido, by = "partido") |>
  mutate(
    proporcao = valor_total / sum(valor_total),
    label_texto = sprintf("R$ %.1f bi\n(%d parlamentares)", 
                         valor_total/1e9, 
                         num_parlamentares)
  )

amendment_data <- amendment_data |>
  mutate(
    valor_por_parlamentar = valor_total / num_parlamentares,
    label_texto = sprintf("R$ %.1f bi\n%d parlamentares\nR$ %.1f mi/parlamentar", 
                         valor_total/1e9, 
                         num_parlamentares,
                         valor_por_parlamentar/1e6)
  )

amendment_data <- amendment_data |>
  mutate(
    label_texto = sprintf("%.1f mi", valor_total/1e6)
  ) |>
  left_join(tooltip_text, by = "partido") |>
  mutate(partido = fct_reorder(partido, valor_total, .desc = TRUE))

amendment_data <- amendment_data |>
  mutate(
    tooltip_text = sprintf(
      "Valor total: R$ %.1f mi<br>Parlamentares: %d<br>Média por parlamentar: R$ %.1f mi<br><br>Principais áreas:<br>%s",
      valor_total/1e6,
      num_parlamentares,
      valor_por_parlamentar/1e6,
      texto_funcoes
    )
  )

# Criar versão estática com ggplot2
value_by_party <- ggplot(amendment_data, aes(x = partido, y = valor_total/1e6)) +
  geom_col(aes(fill = valor_por_parlamentar)) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","),
                    expand = expansion(mult = c(0, 0.05))) +
  scale_fill_gradientn(
    colors = colorRampPalette(c("#C6DBEF", "#2171B5"))(nrow(amendment_data)),
    labels = scales::number_format(
      big.mark = ".", 
      decimal.mark = ",",
      scale = 1e-6,
      suffix = " mi"
    )
  ) +
  labs(
    title = "Valor total empenhado em emendas parlamentares por partido",
    subtitle = "PT e PL lideram destinação de emendas parlamentares em 2024",
    x = NULL,
    y = "Milhões de reais (R$)",
    fill = "Média por\nparlamentar",
    caption = "Dados: Portal da Transparência"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Criar versão interativa com ggiraph
value_by_party_interactive <- ggplot(amendment_data, aes(x = partido, y = valor_total/1e6)) +
  geom_col_interactive(
    aes(
      fill = valor_por_parlamentar,
      tooltip = tooltip_text,
      data_id = partido
    )
  ) +
  scale_y_continuous(
    labels = scales::number_format(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_gradientn(
    colors = colorRampPalette(c("#C6DBEF", "#2171B5"))(nrow(amendment_data)),
    labels = scales::number_format(
      big.mark = ".", 
      decimal.mark = ",",
      scale = 1e-6,
      suffix = " mi"
    )
  ) +
  labs(
    title = "Valor total empenhado em emendas parlamentares por partido",
    subtitle = "PT e PL lideram destinação de emendas parlamentares em 2024",
    x = NULL,
    y = "Milhões de reais (R$)",
    fill = "Média por\nparlamentar",
    caption = "Dados: Portal da Transparência"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Gerar o widget interativo
interactive_plot <- girafe(
  ggobj = value_by_party_interactive,
  width_svg = 10,
  height_svg = 6,
  options = list(
    opts_hover(css = "fill-opacity:0.8;"),
    opts_tooltip(
      css = "background-color:white;color:black;padding:10px;border-radius:5px;font-family:Arial;font-size:12px;",
      opacity = 0.95
    )
  )
)

# Salvar versões
htmlwidgets::saveWidget(
  interactive_plot, 
  "plots/value_by_party.html",
  selfcontained = TRUE
)

# Salvar versão estática
ggsave("plots/value_by_party.png", value_by_party, width = 10, height = 6)
