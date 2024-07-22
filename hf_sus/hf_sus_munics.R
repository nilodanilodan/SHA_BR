#1. Limpando o ambiente -----------------------------
gc()
rm(list=ls())

#2. Carregar pacotes ---------------
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(reshape2)

#3. Importando a base --------------------------
# Diretório onde estão os arquivos xlsx
diretorio <- getwd()
dir_dados <- file.path(diretorio, "dados_estados_munics")

# Listar os arquivos no diretório de dados
arquivos <- list.files(path = dir_dados, pattern = "munic", full.names = TRUE)
arquivo_munic <- arquivos[1]

# Importar as abas específicas como data frames separados
df_2021_munic <- read_excel(arquivo_munic, sheet = "2021")
df_2022_munic <- read_excel(arquivo_munic, sheet = "2022")

#4. Fazendo as modificações necessárias nos DFs--------
# Definir uma função para renomear as colunas
modificar_dataframe_munic <- function(df, ano) {
  # Renomear as primeiras três colunas
  names(df)[1] <- 'cod_munic'
  names(df)[2] <- 'Munic'
  names(df)[3] <- 'UF'
  
  # Obter os nomes das colunas a partir da quarta coluna
  vecnames <- names(df)[-(1:3)]
  
  # Substituir nomes que contêm números de 1 a 9 por NA
  vecnames[grepl('[1-9]', vecnames)] <- NA
  
  # Preencher valores NA com o último valor não-NA à esquerda
  vecnames <- na.locf(vecnames)
  
  # Obter os nomes originais das colunas para criar vecnames3
  vecnames2 <- df[1, -(1:3)]
  vecnames3 <- paste0(vecnames, '_', vecnames2)
  
  # Atualizar os nomes das colunas a partir da quarta coluna
  names(df)[-(1:3)] <- vecnames3
  
  # Remover a primeira linha (que foi usada para obter vecnames2)
  df <- df[-1, ]
  
  # Transformar o data frame de formato largo para formato longo
  melt.data <- melt(df, id.vars = c('cod_munic', 'Munic', 'UF'))
  
  # Converter a coluna 'variable' para caractere
  melt.data$variable <- as.character(melt.data$variable)
  
  # Separar a coluna 'variable' em 'Fase_Despesa' e 'Natureza'
  melt.data <- melt.data %>%
    separate(variable, c('Fase_Despesa', 'Natureza'), '_')
  
  # Criar a coluna 'ano'
  melt.data$ano <- ano
  
  # Converter a coluna 'value' para numérico
  melt.data$value <- as.numeric(melt.data$value)
  
  # Remover acentos da coluna 'nome_UF'
  melt.data$Munic <- iconv(melt.data$Munic, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  return(melt.data)
}
# Lista de data frames
dfs_munic <- list(df_2021_munic, df_2022_munic)
anos <- c("2021", "2022")

# Aplicar a função de renomeação a cada data frame na lista
dfs_mounics <- mapply(modificar_dataframe_munic, dfs_munic, anos, SIMPLIFY = F)

# Separar os data frames modificados de volta para as variáveis individuais
df_2021_munic <- dfs_mounics[[1]]
df_2022_munic <- dfs_mounics[[2]]

df_2021_2022_munic <- bind_rows(df_2021_munic, df_2022_munic)

#5. Calculando a Despesa total corrente em ASPS dos Estados---------
resultado_municipal <- df_2021_2022_munic %>% 
  filter(Natureza == "Despesas correntes em ASPS") %>% 
  group_by(ano) %>% 
  summarise(total_munics = round(sum(value)/10^9,2))
