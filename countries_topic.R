library(dplyr)
library(tidyr)

base_pais <- readxl::read_xlsx("base_combined_ml_prisma.xlsx")

base_pais$pais <- gsub("c\\(|\\)", "", base_pais$pais) # Remove "c(" e ")"
base_pais$pais <- strsplit(base_pais$pais, ",\\s*")    # Separa os países

# Expandir listas
base_expandido <- base_pais %>% 
  unnest(pais)

# Remover aspas extras dos nomes dos países
base_expandido$pais <- gsub("\"", "", base_expandido$pais)

# Contar frequência por tópico
contagem_por_topico <- base_expandido %>% 
  group_by(topic_max, pais) %>% 
  summarise(contagem = n())

writexl::write_xlsx(contagem_por_topico, "paises_por_topico.xlsx")
