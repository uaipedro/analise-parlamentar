source("utils.R")
library(tidyverse)
library(janitor)
library(stringdist)

source("utils.R")

deputies <- read_csv2("data/raw/deputados2.csv", locale = locale(encoding = "UTF-8")) |>
  clean_names() |>
  mutate(
    nome_sanitized = case_when(
      is.na(nome_parlamentar) ~ NA_character_,
      nome_parlamentar == "" ~ NA_character_,
      TRUE ~ nome_parlamentar |> sanitize_name() |> remove_nome_stopwords()
    ),
    nome_civil_sanitized = nome_civil |>
      sanitize_name() |>
      remove_nome_stopwords()
  )

glimpse(deputies)

senators <- read_csv2("data/raw/senadores.csv", locale = locale(encoding = "UTF-8")) |>
  clean_names() |>
  mutate(
    nome_sanitized = case_when(
      is.na(nome_parlamentar) ~ NA_character_,
      nome_parlamentar == "" ~ NA_character_,
      TRUE ~ nome_parlamentar |> sanitize_name() |> remove_nome_stopwords()
    )
  )

glimpse(senators)


amendments <- read_csv2("data/raw/emendas_2023_2024.csv", locale = locale(encoding = "UTF-8")) |>
  clean_names() |>
  mutate(
    across(starts_with("valor"), as.numeric),
    autor_sanitized = autor_da_emenda |>
      sanitize_name() |>
      remove_nome_stopwords()
  ) |>
  filter(ano == "2024")

glimpse(amendments)


amendments <- amendments |>
  filter(str_starts(tipo_de_emenda, "Emenda Individual"))

print("Tipos de emenda:")
print(count(amendments, tipo_de_emenda))




nomes_deputados <- deputies$nome_sanitized |>
  unique() |>
  na.omit()
nomes_senadores <- senators$nome_sanitized |>
  unique() |>
  na.omit()

nomes_parlamentares <- c(nomes_deputados, nomes_senadores)
nomes_emendas <- amendments$autor_sanitized |>
  unique() |>
  na.omit()


nomes_match <- intersect(nomes_parlamentares, nomes_emendas)
nomes_apenas_parlamentares <- setdiff(nomes_parlamentares, nomes_emendas)
nomes_apenas_emendas <- setdiff(nomes_emendas, nomes_parlamentares)


cat("Nomes que correspondem:", length(nomes_match), "\n")
cat("Nomes apenas em parlamentares:", length(nomes_apenas_parlamentares), "\n")
cat("Nomes apenas em emendas:", length(nomes_apenas_emendas), "\n")


amendments |>
  filter(autor_sanitized %in% nomes_apenas_emendas) |>
  group_by(autor_sanitized) |>
  summarise(
    qtd_emendas = n(),
    valor_total = sum(valor_empenhado, na.rm = TRUE),
    valor_medio = mean(valor_empenhado, na.rm = TRUE)
  ) |>
  arrange(desc(qtd_emendas)) |>
  print(n = Inf)



print("Nomes que aparecem apenas no DataFrame de parlamentares:")
print(nomes_apenas_parlamentares)

print("Nomes que aparecem apenas no DataFrame de emendas:")
print(nomes_apenas_emendas)


encontrar_melhor_match_completo <- function(nome, parlamentares_df) {
  matches_parlamentar <- stringdist(nome, parlamentares_df$nome_sanitized, method = "jw")
  matches_civil <- stringdist(nome, parlamentares_df$nome_civil_sanitized, method = "jw")

  melhor_idx_parlamentar <- which.min(matches_parlamentar)
  melhor_idx_civil <- which.min(matches_civil)

  return(list(
    match_parlamentar = parlamentares_df$nome_sanitized[melhor_idx_parlamentar],
    dist_parlamentar = matches_parlamentar[melhor_idx_parlamentar],
    match_civil = parlamentares_df$nome_civil_sanitized[melhor_idx_civil],
    dist_civil = matches_civil[melhor_idx_civil]
  ))
}


parlamentares_combined <- bind_rows(
  deputies |>
    mutate(tipo = "deputado"),
  senators |>
    mutate(
      nome_civil_sanitized = nome_sanitized,
      tipo = "senador"
    )
)


matches_df <- data.frame(
  nome_emenda = nomes_emendas
) |>
  mutate(
    resultados = lapply(nome_emenda, \(x) encontrar_melhor_match_completo(x, parlamentares_combined)),
    match_parlamentar = sapply(resultados, \(x) x$match_parlamentar),
    dist_parlamentar = sapply(resultados, \(x) x$dist_parlamentar),
    match_civil = sapply(resultados, \(x) x$match_civil),
    dist_civil = sapply(resultados, \(x) x$dist_civil)
  ) |>
  select(-resultados)


matches_df <- matches_df |>
  left_join(
    parlamentares_combined |>
      select(nome_sanitized, tipo) |>
      distinct(),
    by = c("match_parlamentar" = "nome_sanitized")
  )


View(matches_df)


parlamentares_info <- parlamentares_combined |>
  select(
    nome_sanitized,
    nome_civil,
    partido,
    tipo
  ) |>
  distinct()

amendments <- amendments |>
  left_join(
    matches_df |> 
    select(nome_emenda, match_parlamentar),
    by = c("autor_sanitized" = "nome_emenda")
  ) |>
  left_join(
    parlamentares_info,
    by = c("match_parlamentar" = "nome_sanitized")
  )

# Verificar o resultado
glimpse(amendments)
View(amendments)

if (!dir.exists("data/processed")) {
  dir.create("data/processed")
}
write.csv2(amendments, "data/processed/emendas_2023_2024_clean.csv", row.names = FALSE)

# Criar DataFrame combinado de todos os parlamentares
todos_parlamentares <- bind_rows(
  deputies |>
    select(
      nome = nome_parlamentar,
      nome_sanitizado = nome_sanitized,
      partido,
      uf
    ) |>
    mutate(tipo = "deputado"),
  
  senators |>
    select(
      nome = nome_parlamentar,
      nome_sanitizado = nome_sanitized,
      partido,
      uf
    ) |>
    mutate(tipo = "senador")
) |>
  mutate(
    nome = str_to_title(nome)
  ) |>
  distinct() # Remove possíveis duplicatas

# Salvar o DataFrame combinado
write.csv2(todos_parlamentares, "data/processed/parlamentares.csv", row.names = FALSE)
