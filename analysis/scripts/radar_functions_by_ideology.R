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

radar_data <- amendments_by_ideology |>
  filter(espectro %in% c("Esquerda", "Centro", "Direita")) |>
  group_by(espectro) |>
  mutate(total_valor = sum(valor_empenhado, na.rm = TRUE)) |>
  group_by(espectro, funcao) |>
  summarise(
    proporcao = sum(valor_empenhado, na.rm = TRUE) / first(total_valor),
    .groups = "drop"
  ) |>
  filter(!funcao %in% c("Saúde", "Encargos especiais")) |>
  group_by(funcao) |>
  mutate(total_prop = sum(proporcao)) |>
  ungroup() |>
  filter(funcao %in% head(
    arrange(data.frame(funcao = unique(funcao), 
                      total_prop = unique(total_prop)), 
           desc(total_prop))$funcao, 
    5))

ggplot(radar_data, aes(x = funcao, y = proporcao, color = espectro, group = espectro)) +
  geom_hline(yintercept = seq(0, max(radar_data$proporcao), by = 0.05), 
             color = "gray90", linewidth = 0.3) +
  geom_polygon(aes(fill = espectro), alpha = 0.15) +
  geom_line(linewidth = 1.2) +
  coord_radar() +
  scale_y_continuous(
    labels = NULL,
    breaks = seq(0, max(radar_data$proporcao), by = 0.05),
    limits = c(0, max(radar_data$proporcao) * 1.2),
    expand = expansion(mult = c(0, 0.2))
  ) +
  geom_text(data = data.frame(
    funcao = "Urbanismo",
    proporcao = seq(0, max(radar_data$proporcao), by = 0.05),
    espectro = "Centro"
  ),
  aes(label = scales::percent(proporcao, accuracy = 1)),
  color = "gray30", size = 3, hjust = 1.1) +
  scale_color_manual(values = c("Centro" = "#FFB000",
                               "Direita" = "#4169E1",
                               "Esquerda" = "#DC143C")) +
  scale_fill_manual(values = c("Centro" = "#FFB000",
                              "Direita" = "#4169E1",
                              "Esquerda" = "#DC143C")) +
  labs(
    title = "Prioridades em Emendas Parlamentares por Espectro Ideológico",
    subtitle = "Distribuição percentual das emendas individuais por área e ideologia do partido\nExcluindo Saúde* e Encargos Especiais",
    x = NULL,
    y = NULL,
    color = "Espectro Ideológico", 
    fill = "Espectro Ideológico",
    caption = "*Nota: Saúde foi excluída por ser de aplicação obrigatória (mínimo de 50% das emendas) e\nEncargos especiais por ser uma função de movimentações financeiras que ultrapassam o ano orçamentário"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.box.margin = margin(t = 10, b = 10),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray30", hjust = 0),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 40, r = 40, b = 40, l = 40)
  )

ggsave("plots/radar_functions_by_ideology.png", 
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white",
       device = "png") 