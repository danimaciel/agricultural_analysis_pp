library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

base <- read_excel("base_combined_ml_prisma.xlsx")


base_filtrada <- base %>%
  filter(!is.na(metodologia_ml) & metodologia_ml != "") 

# Contar o número de metodologias após filtrar
base_filtrada <- base_filtrada %>%
  mutate(count_method = sapply(strsplit(as.character(metodologia_ml), ",\\s*"), length))


writexl::write_xlsx(base, "base_metodos_prisma_contagem_total.xlsx")


# Cálculo da mediana do número de métodos por década
mediana_por_decada <- base_filtrada %>%
  group_by(Decada) %>%
  summarise(mediana_metodos = median(count_method))


# normalizando quantidade de publicações por década e métdo
publicacoes_por_decada <- base_filtrada %>%
  group_by(Decada) %>%
  summarise(numero_publicacoes = n())

# Calcular a média de métodos por década
media_metodos_por_decada <- base_filtrada %>%
  group_by(Decada) %>%
  summarise(media_metodos = mean(count_method))

# Combinar os dois dataframes
analise_decada <- merge(publicacoes_por_decada, media_metodos_por_decada, by = "Decada")


library(ggplot2)

# Criar um gráfico com dois eixos y
ggplot(analise_decada, aes(x = Decada)) +
  geom_line(aes(y = media_metodos, group = 1), colour = "blue") +
  geom_point(aes(y = media_metodos), colour = "blue") +
  geom_bar(aes(y = numero_publicacoes), stat = "identity", fill = "lightblue", alpha = 0.5) +
  labs(title = "Número de Métodos e Publicações por Década",
       x = "Década",
       y = "Média de Métodos (linha e pontos)",
       fill = "Número de Publicações (barras)") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Número de Publicações")) +
  theme_minimal()



# Suponha que temos um dataframe chamado 'dados' com suas colunas
dados <- data.frame(
  Decada = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
  qtd_publc = c(2, 9, 30, 86, 206, 415, 295),
  media_metodos = c(1, 2, 1.6, 1.9, 2.1, 1.9, 2),
  qtd_metodos = c(2, 10, 11, 30, 46, 68, 72)
)

# Criar o gráfico combinado
ggplot(dados, aes(x = Decada)) +
  geom_bar(aes(y = qtd_publc), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = media_metodos*100), group = 1, color = "black", size = 1) + # Multiplicado por 100 para usar a mesma escala
  geom_point(aes(y = media_metodos*100), color = "black") +
  geom_bar(aes(y = qtd_metodos), stat = "identity", fill = "darkblue", alpha = 0.5, position = "dodge") +
  scale_y_continuous(
    "Quantidade de Publicações / Métodos",
    sec.axis = sec_axis(~./100, name = "Média de Métodos") # Dividido por 100 para voltar à escala original
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exibir o gráfico
print(ggplot_object)


table(base$count_method)


#Métodos (mais comuns) por década 

# Separar os métodos em cada linha e converter para um formato longo
base_separada <- base_filtrada %>%
  mutate(metodos = strsplit(as.character(metodologia_ml), ",\\s*")) %>%
  unnest(metodos)

# Contar a frequência de cada método por década
metodos_por_decada <- base_separada %>%
  group_by(Decada, metodos) %>%
  summarise(frequencia = n()) %>%
  arrange(Decada, desc(frequencia))

# Se você quiser ver os métodos mais usados por década, você pode fazer:
metodos_mais_usados_por_decada <- metodos_por_decada %>%
  group_by(Decada) %>%
  top_n(3, frequencia)

# Visualizar os resultados
print(metodos_mais_usados_por_decada)


# Métodos por tópicos e década

topicos_metodos_decadas <- base_separada %>% 
  group_by(topic_max, Decada, metodos) %>%
  summarise(frequencia = n()) %>%
  ungroup() %>%
  arrange(topic_max, Decada, desc(frequencia))

# Agora, selecionamos os três principais métodos por tópico e década
topicos_metodos_decadas_top3 <- topicos_metodos_decadas %>% 
  group_by(topic_max, Decada) %>%
  top_n(3, frequencia) %>%
  ungroup()  


# Agrupar e somar frequências que se repetem
df_agrupado <- topicos_metodos_decadas_top3 %>%
  group_by(topic_max, Decada) %>%
  add_count(frequencia) %>%  # Adiciona uma contagem de ocorrências para cada frequência
  mutate(metodos = ifelse(n > 1 & frequencia != max(frequencia), "Outros", metodos)) %>%
  group_by(topic_max, Decada, metodos) %>%
  summarise(frequencia = sum(frequencia), .groups = "drop")  # Soma as frequências para métodos agrupados

# Visualizando o resultado
print(df_agrupado)









writexl::write_xlsx(topicos_metodos_decadas_top3, "topicos_metodos_decadas3.xlsx")

# criando os graficos separados

dados_agrupados <- topicos_metodos_decadas_top3 %>%
  group_by(Decada, topic_max) %>%
  summarise(top_metodos = paste0(sort(unique(metodos)), collapse = ", "),
            total_frequencia = sum(frequencia)) %>%
  ungroup()


ggplot(dados_agrupados, aes(x = Decada, y = total_frequencia, color = topic_max)) +
  geom_line() +
  facet_wrap(~ topic_max, scales = "free_y") +
  theme_minimal() +
  labs(x = "Década", y = "Frequência Total dos Métodos", color = "Tópico") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(topicos_metodos_decadas_top3, aes(x = Decada, y = frequencia, fill = metodos)) +
  geom_col() + 
  facet_wrap(~ topic_max, scales = "free_y", strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside", 
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(x = "Década", y = "Frequência", fill = "Métodos") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_discrete(breaks = topicos_metodos_decadas_top3$Decada)














# função para criar um gráfico para um tópico específico
plot_topic <- function(data, topic) {
  filtered_data <- data[data$topic_max == topic, ]
  p <- ggplot(filtered_data, aes(x = Decada, y = frequencia, fill = metodos)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    labs(title = paste("Métodos mais comuns no tópico", topic),
         x = "Década", y = "Frequência", fill = "Métodos") +
    theme(legend.position = "top")
  return(p)
}

# Aplicando a função a cada tópico
unique_topics <- unique(topicos_metodos_decadas_top3$topic_max)

plots <- lapply(unique_topics, function(t) plot_topic(topicos_metodos_decadas_top3, t))

plots 


