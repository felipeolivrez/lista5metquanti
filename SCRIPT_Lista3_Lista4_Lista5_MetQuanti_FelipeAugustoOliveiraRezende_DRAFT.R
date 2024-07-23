###(LISTA 03, QUESTÃO 1) Subindo e criando objetos dos DataFrames

#Rodando Pacotes

library(xlsx)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(xaringan)

#Carregando dados

read_excel("C:/Users/Felipe/Desktop/Doutorado IRel UnB/Fontes e Materiais de Pesquisa Empírica Doutorado/Base de Dados PDC 2013_MetQuanti_FelipeAugustoOliveiraRezende.xlsx")
PDC_2013 <- read_excel("C:/Users/Felipe/Desktop/Doutorado IRel UnB/Fontes e Materiais de Pesquisa Empírica Doutorado/Base de Dados PDC 2013_MetQuanti_FelipeAugustoOliveiraRezende.xlsx")

View(PDC_2013)

###(LISTA 03, QUESTÃO 2) Manipulações dplyr/tidyr 

##'N' Projetos PDC por variável de destinação geográfica e domínio cultural nas ações/eventos de difusão cultural

#Ranking dos continentes com maior número de eventos

n_continente <- PDC_2013 %>% 
  group_by(`Destinação (Continente)`) %>% 
  summarise(Num_Eventos = n()) %>% 
  arrange(desc(Num_Eventos)) %>% 
  slice(1:7)

View(n_continente)

#Ranking dos domínios culturais predominantes

n_dominiocultural <- PDC_2013 %>% 
  group_by(`Domínio Cultural`) %>% 
  summarise(Num_Eventos = n()) %>% 
  arrange(desc(Num_Eventos)) %>% 
  slice(1:15)

View(n_dominiocultural)

#Representação oficial (posto) [embaixada, consulado ou delegação] que mais implementou ações/eventos PDC 2013

ranking_embaixadas <- PDC_2013 %>% 
  group_by(`Representação Oficial (Posto) responsável`) %>% 
  summarise(Num_Eventos = n()) %>% 
  arrange(desc(Num_Eventos)) %>% 
  slice(1:15)

View (ranking_embaixadas)

##Indicadores de Paridade de Gênero e Raça nas ações/eventos PDC 2013

PDC_2013_gen_raça <- PDC_2013 %>% 
  mutate(Paridade_Gênero_Indicador = ifelse(`Paridade Gênero` == 1, "Sim", "Não"),
         Paridade_Raça_Indicador = ifelse(`Paridade Raça` == 1, "Sim", "Não"))

View(PDC_2013_gen_raça)

#Percentual de ações/eventos PDC com paridade de gênero

percent_paridade_genero <- PDC_2013_gen_raça %>% 
  summarise(Total_Eventos = n(),
            Eventos_Paridade_Genero = sum(`Paridade Gênero` == 1),
            Percent_Paridade_Genero = (Eventos_Paridade_Genero / Total_Eventos) * 100)

head(min(percent_paridade_genero))

#Percentual de ações/eventos PDC com paridade de raça

percent_paridade_raça <- PDC_2013_gen_raça %>% 
  summarise(Total_Eventos = n(),
            Eventos_Paridade_raça = sum(`Paridade Raça` == 1),
            Percent_Paridade_raça = (Eventos_Paridade_raça / Total_Eventos) * 100)

head(min(percent_paridade_raça))

#Percentual de ações/eventos PDC com paridade de gênero & raça

percent_total_genero_raça <- PDC_2013_gen_raça %>% 
  summarise(Total_Eventos = n(),
            Eventos_Paridade_Genero_raça = sum(`Paridade Gênero` == 1 & `Paridade Raça` == 1),
            Percent_Paridade_Genero_raça_Juntos = (Eventos_Paridade_Genero_raça / Total_Eventos) * 100)

head(min(percent_total_genero_raça))

###(LISTA 03, QUESTÃO 3) Saídas gráficas em ggplot

##Gráfico DONUT que relaciona o 'N' de ações/eventos PDC por continente

n_continente <- n_continente %>%
  mutate(proporcao = Num_Eventos / sum(Num_Eventos))

n_continente <- n_continente %>%
  arrange(desc(`Destinação (Continente)`)) %>%
  mutate(posicao_final = cumsum(proporcao))

n_continente <- n_continente %>%
  mutate(posicao_inicio = lag(posicao_final, default = 0))

ggplot(n_continente, aes(ymax = posicao_final, ymin = posicao_inicio, xmax = 4, xmin = 3, fill = `Destinação (Continente)`)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("#b3cde3", "#8c96c6", "#88419d", "#810f7c", "#4d004b", "#2d004b","#edf8fb")) +
  theme_void() +
  theme(legend.position = "right") +
  ggtitle("'N' Ações/Eventos PDC x Continentes")

##Gráfico BARRAS que relaciona o 'N' de ações/eventos PDC por domínio cultural

ggplot(n_dominiocultural, aes(x = reorder(`Domínio Cultural`, Num_Eventos), y = Num_Eventos)) +
  geom_bar(stat = "identity", fill = "#8B3A3A") +
  coord_flip() +
  theme_minimal() +
  labs(title = "'N' Ações/Eventos PDC x Domínio Cultural",
       x = "Domínio Cultural",
       y = "Número de Eventos") +
  scale_y_continuous(breaks = seq(0, max(n_dominiocultural$Num_Eventos), by = 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Gráfico BARRAS que relaciona os aspectos de paridade de gênenero e raça ao total 'N' de ações/eventos

ntotal_paridade_gen_raça <- data.frame(
  Total_Eventos = percent_paridade_genero$Total_Eventos,
  Percent_Paridade_Genero = percent_paridade_genero$Percent_Paridade_Genero,
  Percent_Paridade_raça = percent_paridade_raça$Percent_Paridade_raça,
  Percent_Paridade_Genero_raça_Juntos = percent_total_genero_raça$Percent_Paridade_Genero_raça_Juntos
)

head(ntotal_paridade_gen_raça)

ggplot(ntotal_paridade_gen_raça, aes(x = factor(1), y = Total_Eventos, fill = "Total de Eventos")) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Total_Eventos), vjust = -0.3, size = 3) +
  geom_bar(aes(x = factor(2), y = Percent_Paridade_Genero, fill = "Paridade de Gênero"), stat = "identity", width = 0.5) +
  geom_text(aes(x = factor(2), y = Percent_Paridade_Genero, label = paste0(round(Percent_Paridade_Genero, 1), "%")), vjust = -0.3, size = 3) +
  geom_bar(aes(x = factor(3), y = Percent_Paridade_raça, fill = "Paridade de Raça"), stat = "identity", width = 0.5) +
  geom_text(aes(x = factor(3), y = Percent_Paridade_raça, label = paste0(round(Percent_Paridade_raça, 1), "%")), vjust = -0.3, size = 3) +
  geom_bar(aes(x = factor(4), y = Percent_Paridade_Genero_raça_Juntos, fill = "Paridade de Gênero & Raça"), stat = "identity", width = 0.5) +
  geom_text(aes(x = factor(4), y = Percent_Paridade_Genero_raça_Juntos, label = paste0(round(Percent_Paridade_Genero_raça_Juntos, 1), "%")), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("#FF4500", "#CD3700", "#8B2500", "#8B0000")) +
  labs(title = "'N' Ações/Eventos PDC x Indicadores de Paridade de Gênero e Raça",
       x = NULL, y = "Percentual (%)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top")

###(LISTA 04) 

##(Questão 1)Estimativas Intervalares (Proporção de Gênero/Raça nos Eventos Culturais PDC 2013)

PDC_2013_genero <- PDC_2013 %>% filter(`Paridade Gênero` == 1)
PDC_2013_raça <- PDC_2013 %>% filter(`Paridade Raça` == 1)
PDC_2013_gen_raça <- PDC_2013 %>% filter(`Paridade Gênero` == 1, `Paridade Raça` == 1)

n_total <- nrow(PDC_2013)
  
#Teste de proporção e intervalos de confiança (estimativa intervalar)

prop_test_genero <- prop.test(n = n_total, x = nrow(PDC_2013_genero))
prop_test_raça <- prop.test(n = n_total, x = nrow(PDC_2013_raça))
prop_test_gen_raça <- prop.test(n = n_total, x = nrow(PDC_2013_gen_raça))
  
props <- c(prop_test_genero$estimate, prop_test_raça$estimate, prop_test_gen_raça$estimate)
  ci_lowers <- c(prop_test_genero$conf.int[1], prop_test_raça$conf.int[1], prop_test_gen_raça$conf.int[1])
  ci_uppers <- c(prop_test_genero$conf.int[2], prop_test_raça$conf.int[2], prop_test_gen_raça$conf.int[2])
  
prop_df <- data.frame(
    Categoria = c("Paridade Gênero", "Paridade Raça", "Paridade Gênero e Raça"),
    Proporcao = props,
    CI_lower = ci_lowers,
    CI_upper = ci_uppers
  )
  
ggplot(prop_df, aes(x = Categoria, y = Proporcao)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
    ylim(0, 1) +
    labs(title = "Estimativas Intervalares (Proporção de Gênero/Raça nos Eventos Culturais PDC 2013)",
         x = "Categorias",
         y = "Proporção") +
    theme_minimal()

##(Questão 2) Configuração de Hipótese Testável

#Hipótese Nula (H0): Não há diferença significativa entre as proporções 
#de paridade de gênero, paridade de raça e paridade de gênero e raça.

#Hipótese Alternativa (H1): Há pelo menos uma diferença significativa 
#entre as proporções de paridade de gênero, paridade de raça e paridade de gênero e raça.


###(LISTA 05) TESTE DE HIPOTESE E RELATORIO ONLINE

##(Questão 01) TESTE DE HIPOTESE USANDO QUI-QUADRADO

PDC_2013_genero <- PDC_2013 %>% filter(`Paridade Gênero` == 1)
PDC_2013_raça <- PDC_2013 %>% filter(`Paridade Raça` == 1)
PDC_2013_gen_raça <- PDC_2013 %>% filter(`Paridade Gênero` == 1 & `Paridade Raça` == 1)

n_total <- nrow(PDC_2013)

#Contagens de cada categoria
n_genero <- nrow(PDC_2013_genero)
n_raça <- nrow(PDC_2013_raça)
n_gen_raça <- nrow(PDC_2013_gen_raça)

#Tabela de contigência
contingency_table <- matrix(c(n_genero, n_total - n_genero,
                              n_raça, n_total - n_raça,
                              n_gen_raça, n_total - n_gen_raça),
                            nrow = 3, byrow = TRUE)

#Nomeando as linhas e colunas
rownames(contingency_table) <- c("Paridade Gênero", "Paridade Raça", "Paridade Gênero e Raça")
colnames(contingency_table) <- c("Sim", "Não")

#Teste qui-quadrado
chi_squared_test <- chisq.test(contingency_table)

##Resultados
chi_squared_test

#Resultados do Teste Qui-Quadrado
X-squared = 25.089
df = 2
p-value = 3.564e-06

#Exibindo os valores esperados e os resíduos padronizados
chi_squared_test$expected
chi_squared_test$residuals

## Interpretação
#O p-valor é muito menor que o nível de significância adotado de 0.05.

## Decisão
#Hipótese Nula (H0): Não há diferença significativa entre as proporções de 
#paridade de gênero, paridade de raça e paridade de gênero e raça.

#Hipótese Alternativa (H1): Há pelo menos uma diferença significativa entre 
#as proporções de paridade de gênero, paridade de raça e paridade de gênero e raça.

#Dado que o valor-p é significativamente menor que 0.05, rejeitamos a hipótese nula (H0). 
#Portanto, conclui-se que há uma diferença significativa entre 
#as proporções de paridade de gênero, paridade de raça e paridade de gênero e raça nos
#eventos PDC 2013.

## Conclusão
#A paridade de gênero tem um resíduo padronizado positivo significativo para 
#a categoria "Sim", indicando que há mais eventos com paridade de gênero do 
#que o esperado.

#A paridade de gênero e raça tem um resíduo padronizado negativo significativo 
#para a categoria "Sim", indicando que há menos eventos com paridade de gênero e raça 
#do que o esperado.

#Estes resultados mostram que existem diferenças significativas nas proporções 
#de paridade de gênero, paridade de raça e paridade de gênero e raça nos eventos 
#culturais PDC 2013.