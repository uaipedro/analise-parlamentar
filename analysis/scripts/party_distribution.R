source("analysis/config/common.R")

members |>
  group_by(partido, tipo) |>
  summarise(count = n(), .groups = "drop") |>
  ggplot(aes(x = reorder(partido, count), y = count, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(
    title = "Distribuição dos Parlamentares em atividade em 2024 por Partido",
    subtitle = "Quantidade de deputados federais e senadores em cada legenda",
    x = "Partido",
    y = "Quantidade de Parlamentares",
    fill = "Cargo"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = "white")
  ) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() # Para melhor visualização dos nomes dos partidos

ggsave("plots/party_distribution.png", width = 10, height = 6) 