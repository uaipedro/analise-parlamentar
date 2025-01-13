library(rvest)

url <- "https://www.camara.leg.br/internet/agencia/infograficos-html5/tabelasEleicoes/deputados-eleitos-estado/"

page <- tryCatch(
    {
        read_html(url)
    },
    error = function(e) {
        message("Erro ao ler a página: ", e)
        return(NULL)
    }
)

if (!is.null(page)) {
    table <- page %>%
        html_node("table") %>%
        html_table()

    table <- table[-1, ]

    dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
    write.table(table,
                "data/raw/deputados.csv",
                row.names = FALSE,
                fileEncoding = "UTF-8",
                sep = ";",
                dec = ",")

    print(table)
} else {
    message("Não foi possível acessar a página.")
}
