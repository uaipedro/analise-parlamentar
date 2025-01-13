sanitize_name <- function(name) {
  name |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_trim() |>
    str_remove_all("\\.")
}

remove_nome_stopwords <- function(name) {
  # Stopwords comuns em nomes de parlamentares
  nome_stopwords <- c(
    "DEPUTADO", "DEPUTADA", "SENADOR", "SENADORA",
    "VEREADOR", "VEREADORA", "DR", "DRA", "DOUTOR",
    "DOUTORA", "PROFESSOR", "PROFESSORA", "PROF",
    "CORONEL", "CAPITAO", "TENENTE", "MAJOR", "SARGENTO",
    "JR", "JUNIOR", "JR.", "DELEGADO", "DELEGADA",
    "PASTOR", "POLICIAL", "ASTRONAUTA", "SUBTENENTE",
    "GENERAL", "PR.", "DEP.", "COM.", "DA", "DE", "DO",
    "DOS", "DAS"
  )

  name |>
    str_replace_all(paste0("\\b(", paste(nome_stopwords, collapse = "|"), ")\\b"), "") |>
    str_squish()
}
