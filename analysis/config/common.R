library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gt)
library(ggthemes)
library(treemapify)
library(plotly)
library(ggiraph)
library(ggiraphExtra)




ideology_order <- c("esquerda", "centro-esquerda", "centro", "centro-direita", "direita", "não classificado")

# Load data
members <- read_csv2("data/processed/parlamentares.csv") 
amendments <- read_csv2("data/processed/emendas_2023_2024_clean.csv")