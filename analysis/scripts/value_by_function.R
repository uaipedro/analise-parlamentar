source("analysis/config/common.R")

glimpse(amendments)



function_totals <- amendments |>
  group_by(funcao) |>
  summarise(
    valor_total = sum(valor_empenhado, na.rm = TRUE),
    num_emendas = n()
  )

amendment_data <- function_totals |>
  mutate(
    proporcao = valor_total / sum(valor_total),
    funcao = if_else(proporcao <= 0.03, "Outros", funcao)
  ) |>
  group_by(funcao) |>
  summarise(
    valor_total = sum(valor_total),
    num_emendas = sum(num_emendas)
  ) |>
  mutate(
    proporcao = valor_total / sum(valor_total),
    label_texto = sprintf("R$ %.0f mi\n(%.1f%%)", 
                        valor_total / 1e6,
                        proporcao * 100),
    funcao = fct_reorder(funcao, -proporcao)
  )

ggplot(amendment_data, aes(
    area = proporcao,
    fill = funcao,
    label = label_texto
  )) +
  geom_treemap() +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 12,
    grow = FALSE
  ) +
  labs(
    title = "Destinação das Emendas Parlamentares Individuais em 2024 pelas áreas de aplicação",
    subtitle = "Com mais de R$ 3 bilhões empenhados, área da saúde recebe maior fatia do orçamento",
    caption = "Fonte: Portal da Transparência"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_fill_brewer(
    palette = "Set3",
    name = "Área de Aplicação"
  )

ggsave("plots/emendas_2024_treemap.png", width = 10, height = 6, dpi = 300)
