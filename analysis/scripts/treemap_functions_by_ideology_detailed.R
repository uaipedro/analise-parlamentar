source("analysis/config/common.R")

amendments_by_ideology <- amendments |>
  mutate(
    espectro = case_when(
      espectro == "esquerda" ~ "Esquerda",
      espectro == "centro-esquerda" ~ "Centro-esquerda",
      espectro == "centro" ~ "Centro",
      espectro == "centro-direita" ~ "Centro-direita",
      espectro == "direita" ~ "Direita",
      TRUE ~ "Não classificado"
    ),
    funcao = case_when(
      funcao == "Segurança pública" ~ "Segurança",
      TRUE ~ funcao
    )
  )

treemap_data <- amendments_by_ideology |>
  filter(
    espectro %in% c("Esquerda", "Centro-esquerda", "Centro", "Centro-direita", "Direita"),
    !funcao %in% c("Saúde", "Encargos especiais")
  ) |>
  group_by(espectro, funcao) |>
  summarise(
    valor_total = sum(valor_empenhado, na.rm = TRUE),
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
  mutate(espectro = factor(espectro, 
                          levels = c("Esquerda", "Centro-esquerda", 
                                   "Centro", "Centro-direita", "Direita")))

cores_espectro <- c(
  "Esquerda" = "#FF9999",
  "Centro-esquerda" = "#FFB6B6",
  "Centro" = "#FFE5B4",
  "Centro-direita" = "#B6D0FF",
  "Direita" = "#99B3FF"
)

criar_paleta <- function(cor_base, n) {
  colorRampPalette(c(cor_base, "white"))(n + 2)[1:n]
}

criar_treemap <- function(dados, espectro_alvo) {
  dados_filtrados <- dados |> filter(espectro == espectro_alvo)
  n_funcoes <- nrow(dados_filtrados)
  
  paleta_cores <- criar_paleta(cores_espectro[espectro_alvo], n_funcoes)
  
  p <- ggplot(dados_filtrados, 
         aes(area = valor_total,
             fill = reorder(funcao, -valor_total),
             label = label_texto)) +
    geom_treemap(layout = "squarified") +
    geom_treemap_text(
      colour = "black",
      place = "centre",
      size = 11,
      grow = FALSE,
      alpha = 0.9,
      fontface = "bold",
      padding.x = grid::unit(1, "mm"),
      padding.y = grid::unit(1, "mm")
    ) +
    scale_fill_manual(values = paleta_cores) +
    labs(
      title = sprintf("Distribuição detalhada das emendas - %s", espectro_alvo),
      subtitle = "Emendas parlamentares individuais por função (2024)",
      caption = "Fonte: Portal da Transparência | Valores em milhões de reais\nNota: Excluídos valores destinados à Saúde e Encargos especiais",
      fill = "Função"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                              color = cores_espectro[espectro_alvo]),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 8, color = "gray30"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
  
  ggsave(
    sprintf("plots/treemap_detailed_%s.png", 
            tolower(gsub("-", "_", espectro_alvo))),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300,
    bg = "white"
  )
}

espectros <- c("Esquerda", "Centro-esquerda", "Centro", "Centro-direita", "Direita")
walk(espectros, ~criar_treemap(treemap_data, .x)) 