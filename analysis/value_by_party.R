library(tidyverse)
library(ggthemes)
library(treemapify)


amendments <- read_csv2("data/processed/emendas_2023_2024_clean.csv")

glimpse(amendments)



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

# Carregar dados dos parlamentares e criar tabela de frequência
parlamentares <- read_csv2("data/processed/parlamentares.csv")

glimpse(parlamentares)
# Tabela de frequência de parlamentares por partido
parlamentares |>
  filter(tipo == "senador") |>
  count(partido, name = "frequencia") |>
  arrange(desc(frequencia)) |>
  print(n = Inf)

View(parlamentares) 

# Contar parlamentares por partido
parlamentares_por_partido <- parlamentares |>
  group_by(partido) |>
  summarise(num_parlamentares = n())

amendment_data <- consolidated_totals |>
  left_join(parlamentares_por_partido, by = "partido") |>
  mutate(
    proporcao = valor_total / sum(valor_total),
    label_texto = sprintf("R$ %.1f bi\n(%d parlamentares)", 
                         valor_total/1e9, 
                         num_parlamentares),
    partido = fct_reorder(partido, -proporcao)
  )

# Calcular valor médio por parlamentar
amendment_data <- amendment_data |>
  mutate(
    valor_por_parlamentar = valor_total / num_parlamentares,
    label_texto = sprintf("R$ %.1f bi\n%d parlamentares\nR$ %.1f mi/parlamentar", 
                         valor_total/1e9, 
                         num_parlamentares,
                         valor_por_parlamentar/1e6),
    partido = fct_reorder(partido, valor_por_parlamentar)  # Reordenar por valor per capita
  )

# Simplificar as labels e oferecer duas opções de ordenação
amendment_data <- amendment_data |>
  mutate(
    # Simplificando o texto da label para mostrar só o valor médio por parlamentar
    label_texto = sprintf("R$ %.1f mi", valor_por_parlamentar/1e6),
    
    # Criar duas versões do fator partido (por valor total e por valor médio)
    partido_by_total = fct_reorder(partido, -valor_total),
    partido_by_media = fct_reorder(partido, -valor_por_parlamentar)
  )

# Versão 1: Ordenado por valor total
p1 <- ggplot(amendment_data, aes(x = partido_by_total, y = valor_total/1e9)) +
  geom_col(aes(fill = valor_por_parlamentar)) +
  geom_text(aes(label = label_texto), 
            vjust = -0.5,
            size = 3)

# Versão 2: Ordenado por valor médio por parlamentar
p2 <- ggplot(amendment_data, aes(x = partido_by_media, y = valor_total/1e9)) +
  geom_col(aes(fill = valor_por_parlamentar)) +
  geom_text(aes(label = label_texto), 
            vjust = -0.5,
            size = 3)

# Configurações comuns para ambos os gráficos
configurar_grafico <- function(p) {
  p +
    scale_y_continuous(labels = scales::number_format(prefix = "R$ ", suffix = " bi"),
                      expand = expansion(mult = c(0, 0.15))) +
    scale_fill_gradientn(
      colors = colorRampPalette(c("#E6F3FF", "#1F77B4"))(nrow(amendment_data)),
      labels = scales::number_format(prefix = "R$ ", suffix = " mi")
    ) +
    labs(
      title = "Emendas Parlamentares por Partido (2024)",
      subtitle = "Altura das barras: valor total | Cor: valor médio por parlamentar",
      x = "Partido",
      y = "Valor Total (R$ bi)",
      fill = "Média por\nParlamentar",
      caption = "Fonte: Portal da Transparência"
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
}

# Aplicar configurações e salvar ambas as versões
p1_final <- configurar_grafico(p1)
p2_final <- configurar_grafico(p2)

# Salvar ambas as versões
ggsave("plots/value_by_party_total.png", p1_final, width = 14, height = 8, dpi = 300)
ggsave("plots/value_by_party_media.png", p2_final, width = 14, height = 8, dpi = 300)
