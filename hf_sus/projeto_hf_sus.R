#1. Limpando o ambiente -----------------------------
gc()
rm(list=ls())

#2. Carregar pacotes ---------------
library(readxl)
library(dplyr)
library(tidyr)

#3. Importando a base --------------------------
# Diretório onde estão os arquivos xlsx
diretorio <- getwd()

# Lista para armazenar os data frames de cada arquivo
lista_dfs <- list()

# Iterar sobre os arquivos na pasta
arquivos <- list.files(diretorio, pattern = "\\.xlsx$", full.names = TRUE)
for (arquivo in arquivos) {
  # Ler o arquivo xlsx
  df <- read_excel(arquivo)
  
  # Determinar a origem do orçamento
  if (grepl("EDUCACAO", basename(arquivo), ignore.case = TRUE)) {
    origem <- "EDUCACAO"
  } else if (grepl("SAUDE", basename(arquivo), ignore.case = TRUE)) {
    origem <- "SAUDE"
  } else {
    origem <- "OUTRO"
  }
  
  # Adicionar a coluna ORIGEM_ORÇAMENTO
  df <- mutate(df, ORIGEM_ORÇAMENTO = origem)
  
  # Adicionar o data frame modificado à lista
  lista_dfs[[length(lista_dfs) + 1]] <- df
}

# Concatenar todos os data frames em um único data frame
df_final <- bind_rows(lista_dfs)

rm(df, lista_dfs)

#4.Testes da base -----------
teste <- df_final %>% 
  group_by(`Ano Lançamento`, ORIGEM_ORÇAMENTO) %>% 
  summarise(sum(`DESPESAS PAGAS`, na.rm = T))

rm(teste)

gc()

#5. Calculando o gasto MEC com saúde------------
#5.1 Criando filtro para as HUs--------
HUs_ <- df_final %>%
  filter(UO_Orgao_Maximo == "MINISTERIO DA EDUCACAO") %>% 
  select("UG_Executora_COD", "UG_Executora") %>%
  filter(grepl("HOSP|COMPLEXO HOSP|EBSERH|MATERNIDADE|DOENCAS DO TORAX DA UFRJ|GINECOLOGIA DA UFRJ|DEOLINDO COUTO DA UFRJ|PSIQUIATRIA DA UFRJ|PUERIC. PED MAT. GESTEIRA DA UFRJ|HUSM", UG_Executora) & !grepl("EXERCITO|MILITAR", UG_Executora)) %>%
  unique()

#5.2 Utilizando ações e UGs---------
SHA_Uniao_MEC_acoes <- df_final %>%
  filter(UO_Orgao_Maximo == "MINISTERIO DA EDUCACAO") %>% 
  select(`Ano Lançamento`,UG_Executora_COD, UG_Executora, Subfuncao_COD, Acao, Acao_Cod, Categoria_Economica_Despesa_COD, `DESPESAS PAGAS`, `RESTOS A PAGAR PROCESSADOS PAGOS`, `RESTOS A PAGAR NAO PROCESSADOS PAGOS`) %>% 
  filter(Categoria_Economica_Despesa_COD %in% "3") %>%
  filter(UG_Executora_COD %in% HUs_$UG_Executora_COD | Acao_Cod %in% c("20RX", "4086")) %>% 
  mutate_at(c("DESPESAS PAGAS", "RESTOS A PAGAR PROCESSADOS PAGOS", "RESTOS A PAGAR NAO PROCESSADOS PAGOS"), ~replace_na(.,0)) %>%
  mutate(resultado = `DESPESAS PAGAS` + `RESTOS A PAGAR PROCESSADOS PAGOS` + `RESTOS A PAGAR NAO PROCESSADOS PAGOS`) %>%
  select(`Ano Lançamento`, resultado) %>%
  group_by(`Ano Lançamento`) %>%
  summarise(resultado_final_mec = round(sum(resultado),2))

#6. Calculando o gasto Federal com saúde-----------
SHA_Uniao <- df_final %>% 
  filter(UO_Orgao_Maximo == "MINISTERIO DA SAUDE") %>% 
  select(`Ano Lançamento`, Fonte_Recursos_Detalhada_COD, Iduso_COD, Unidade_Orcamentaria_COD, Unidade_Orcamentaria, UG_Executora_COD, UG_Executora, Subfuncao_COD, Acao_Cod, Categoria_Economica_Despesa_COD, Modalidade_Aplicacao_COD ,`DESPESAS PAGAS`, `RESTOS A PAGAR PROCESSADOS PAGOS`, `RESTOS A PAGAR NAO PROCESSADOS PAGOS`) %>% 
  filter(Categoria_Economica_Despesa_COD %in% "3") %>% 
  filter(Iduso_COD == 6 | (Acao_Cod == "2004" & Unidade_Orcamentaria_COD %in% c("36901", "36211", "36213", "36212")) | Acao_Cod %in% c("20G8", "20YS", "217U", "20YL") | Modalidade_Aplicacao_COD == "70" | Fonte_Recursos_Detalhada_COD %in% c("0142000000", "6142000000", "6342000000", "0142369010", "0342369010", "6142369010") | (Unidade_Orcamentaria_COD %in% c("36212", "36213") & Acao_Cod != "2004")) %>% 
  mutate_at(c("DESPESAS PAGAS", "RESTOS A PAGAR PROCESSADOS PAGOS", "RESTOS A PAGAR NAO PROCESSADOS PAGOS"), ~replace_na(.,0)) %>%
  mutate(resultado = `DESPESAS PAGAS` + `RESTOS A PAGAR PROCESSADOS PAGOS` + `RESTOS A PAGAR NAO PROCESSADOS PAGOS`) %>%
  select(`Ano Lançamento`, resultado) %>%
  group_by(`Ano Lançamento`) %>%
  summarise(resultado_final_uniao_sus = round(sum(resultado),2))

#7. Juntando resultados---------
resultado <- left_join(SHA_Uniao, SHA_Uniao_MEC_acoes, by = "Ano Lançamento")
resultado$total <- resultado$resultado_final_uniao_sus + resultado$resultado_final_mec
resultado <- resultado %>% 
  mutate(across(2:4, ~./10^9))
