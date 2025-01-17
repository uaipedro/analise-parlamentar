source("analysis/config/common.R")

amendments_by_ideology <- amendments |>
  mutate(
    espectro = case_when(
      espectro %in% c("esquerda", "centro-esquerda") ~ "Esquerda",
      espectro == "centro" ~ "Centro",
      espectro %in% c("direita", "centro-direita") ~ "Direita",
      TRUE ~ "Não classificado"
    ),
    funcao = case_when(
      funcao == "Segurança pública" ~ "Segurança",
      TRUE ~ funcao
    )
  )

treemap_data <- amendments_by_ideology |>
  filter(
    espectro %in% c("Esquerda", "Centro", "Direita"),
    !funcao %in% c("Saúde", "Encargos especiais")
  ) |>
  group_by(espectro, funcao) |>
  summarise(
    valor_total = sum(valor_empenhado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(espectro) |>
  arrange(espectro, desc(valor_total)) |>
  mutate(
    valor_acumulado = cumsum(valor_total),
    total_espectro = sum(valor_total),
    proporcao_acumulada = valor_acumulado / total_espectro,
    funcao = if_else(proporcao_acumulada <= 0.90, funcao, "Outros")
  ) |>
  group_by(espectro, funcao) |>
  summarise(
    valor_total = sum(valor_total),
    .groups = "drop"
  ) |>
  group_by(espectro) |>
  mutate(
    proporcao = valor_total / sum(valor_total),
    valor_total_mi = valor_total / 1e6,
    label_texto = sprintf("%s\nR$ %d mi\n(%.1f%%)", 
                         funcao,
                         round(valor_total_mi),
                         proporcao * 100)
  ) |>
  mutate(espectro = factor(espectro, levels = c("Esquerda", "Centro", "Direita")))

ggplot(treemap_data, 
       aes(area = valor_total,
           fill = espectro,
           subgroup = espectro,
           subgroup2 = funcao,
           label = label_texto)) +
  geom_treemap(layout = "squarified") +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_subgroup2_border(colour = "white") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE
  ) +
  scale_fill_manual(values = c(
    "Esquerda" = "#DC143C",
    "Centro" = "#FFB000",
    "Direita" = "#4169E1"
  )) +
  labs(
    title = "Esquerda prioriza Educação, enquanto Centro e Direita focam em Assistência Social",
    subtitle = "Distribuição das emendas parlamentares individuais (2024) mostra diferentes prioridades por ideologia",
    caption = "Fonte: Portal da Transparência | Valores em milhões de reais\nNota: Excluídos partidos sem autodeclaração de espectro político",
    fill = "Espectro Ideológico"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray30"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

ggsave("plots/treemap_functions_by_ideology.png", 
       width = 10, 
       height = 8, 
       dpi = 300,
       bg = "white") 