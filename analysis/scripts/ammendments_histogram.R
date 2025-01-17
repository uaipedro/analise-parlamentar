source("analysis/config/common.R")

# Criar histograma
histogram_plot <- ggplot(amendments, aes(x = valor_empenhado)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(
    title = "Distribuição dos Valores Empenhados em Emendas Parlamentares Individuais",
    subtitle = "Ano: 2024",
    x = "Valor Empenhado (R$)",
    y = "Frequência"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.background = element_rect(fill = "white")
  )

# Salvar o gráfico
ggsave("plots/ammendments_histogram.png", histogram_plot, width = 8, height = 4) 