# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )
library(dplyr)
library(ggplot2)

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?
notbuilt_products <- products %>% anti_join(insta_products, by = "product_id") %>% count()

print(notbuilt_products)
#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 
data_Frame_prd_ais_depart <- products %>% left_join(departments, by = "department_id") %>%
    left_join(aisles, by = "aisle_id")

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.
top_ais_dep <- data_Frame_prd_ais_depart %>% count(aisle, department) %>% arrange(desc(n)) %>% head(10)
print(top_ais_dep)


#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?
dataframe_prod_orders = insta_products %>% left_join(data_Frame_prd_ais_depart, by = "product_id")
dataframe_prod_orders <- dataframe_prod_orders %>% 
    mutate(in_top_ad = department %in% top_ais_dep$department & aisle %in% top_ais_dep$aisle)

total_orders = insta_orders %>% count() ##Total de pedidos
percent <- count(dataframe_prod_orders%>% filter(in_top_ad==TRUE) %>% distinct(order_id)) 
##Contagem de pedidos que estao no TOP 10 DEPARTAMENTO E CORREDOR
respost <- ((percent/total_orders)*100)

paste("Percentual de pedidos que possuem algum produto dos pares 'corredor + departamento'", respost, "%")
#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)
## Considerar Categoria = departamento
dataframe_without_cat = dataframe_prod_orders %>% filter(department != "missing")
##top_ais_dep1 <- dataframe_without_cat %>% count(aisle, department) %>% arrange(desc(n)) %>% head(10)

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   # Transforme as variáveis user_id, department e aisle em factor
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)
dataframe_full = insta_orders %>% left_join( dataframe_prod_orders, by = "order_id") %>%
    mutate(user_id=factor(user_id), 
           department = factor(department),
           aisle = factor(aisle)) %>%
    mutate(order_hour_of_day = factor(order_hour_of_day, ordered = TRUE))

   # Este dataframe deverá ser utilizado em todas as atividades seguintes


#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
top5hours <- dataframe_full %>% distinct(user_id, order_hour_of_day) %>% count(order_hour_of_day) %>% arrange(desc(n)) %>% head(5)

top5hours
#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)

top_products <- dataframe_full %>% filter(order_hour_of_day %in% pull(top5hours, order_hour_of_day)) %>% 
    count(product_name) %>% arrange(desc(n)) %>% head(15)

top_products
#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 

dataframe_full %>% 
    filter(product_name %in% top_products$product_name) %>%
    count(product_name, order_hour_of_day) %>%
    group_by(product_name, order_hour_of_day) %>%
    summarise(media = mean(n)) %>%
    ungroup() %>%
    ggplot(aes(x = order_hour_of_day, y = media, group = product_name, colour = product_name))+
    geom_line() + 
    ggtitle("Média de Vendas por Hora") +
    xlab("Horas") +
    ylab("Média de vendas")

paste("OS 15 produtos seguem o mesmo padrão de média, aumentando a partir das 5 horas, tendo a maior media entre os horários de 10 as 16.")

#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 
dataframe_full %>% 
    count(order_dow, order_hour_of_day) %>%
    group_by(order_hour_of_day) %>%
    summarise(media = mean(n), desvio_padrao = sd(n), mediana = median(n), minimo = min(n), maximo = max(n))  %>%
    ungroup() -> products_by_hour

products_by_hour %>% View()

print("Sim, pelo valores é possivel inferir que a distribuição é gaussiana, devido o maximo ser bem superior a média, e ")
#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda
products_by_hour %>%
    ggplot(aes(x = order_hour_of_day, y = media, group = "order_hour_of_day", ymin = media - desvio_padrao, ymax = media + desvio_padrao)) +
    geom_ribbon(fill = "blue", alpha = 0.8) +
    geom_line(color = "red") +
    ggtitle("Média de Quantidade de Produtos por Hora") +
    xlab("Horas") +
    ylab("Média")


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.
dataframe_full %>%
    distinct(order_id,order_dow,order_hour_of_day) %>%
    count(order_dow, order_hour_of_day) %>%
    ggplot(aes(x = order_dow, y = n , group = order_dow)) +
    geom_boxplot() + 
    scale_x_continuous( breaks = 0:6 ) +
    scale_y_continuous( breaks = seq(from = 0, to = 2300, by = 100 )) +
    labs( x = "Dias da Semana"
          , y = "Quantidade de Pedidos por Hora"
          , title = "Boxplot da Quantidade de Pedidos por Hora") +
    theme_bw()

#13 # Identifique, por usuário, o tempo médio entre pedidos

tempo_medio <- dataframe_full %>% 
    distinct(order_id, user_id,days_since_prior_order) %>%
    group_by(user_id) %>%
    summarise(media_tempo = mean(days_since_prior_order))

#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado

tempo_medio %>%
    ggplot(aes(x=media_tempo))+
    geom_bar(fill="green", color = "blue", alpha=0.75)+
    scale_x_continuous(breaks = seq(from=0, to=30, by=1))+
    ggtitle("Quantidade de Usuários por Tempo Medio")+
    xlab("Tempo Médio (em dias)")+
    ylab("Quantidade de usuários")

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 
dataframe_full %>%
    distinct(user_id, days_since_prior_order) %>%
    ggplot(aes(x = days_since_prior_order)) +
    geom_bar(fill="blue", color="red", alpha=0.7)+
    scale_x_continuous(breaks = seq(from=0, to=30, by=1))+
    scale_y_continuous(breaks = seq(from=0, to=40000, by=4000))+
    xlab("Tempo (em dias)")+
    ylab("Quantidade de usuários")

print("Os graficos possuem os mesmo valores.")
#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?
tempo_medio <- dataframe_full %>% 
    distinct(order_id, user_id,days_since_prior_order) %>%
    count(user_id) %>%
    group_by(user_id) %>%
    filter(n()>5) %>%
    summarise(media_tempo = mean(days_since_prior_order))
tempo_medio %>% 
    ggplot(aes(x=media_tempo))+
    geom_bar(fill="green", color = "blue", alpha=0.75)+
    scale_x_continuous(breaks = seq(from=0, to=30, by=1))+
    ggtitle("Quantidade de Usuários por Tempo Medio")+
    xlab("Tempo Médio (em dias)")+
    ylab("Quantidade de usuários")

print("Não Há usuários com 5 ou mais pedidos")
#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
bananas <- c(24852, 13176, 39276, 37067, 29259)

orders_many_bananas <- dataframe_full %>%
    filter(product_id %in% bananas) %>%
    count(order_id) %>%
    filter(n>1) %>%
    select(order_id)

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

top3bananas <- dataframe_full %>%
    filter(order_id %in% orders_many_bananas$order_id) %>%
    filter(product_id %in% bananas) %>%
    group_by(product_id) %>%
    count(product_id) %>%
    arrange(desc(n)) %>%
    head(3) %>%
    select(product_id)
    

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 
media_top3bananas <- dataframe_full %>%
    filter(product_id %in% top3bananas$product_id) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(media_bananas = mean(n()))

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora

media_top3bananas %>%
    ggplot(aes(x=order_dow, y=order_hour_of_day, size=media_bananas))+
    geom_point(color="gold")+
    scale_x_continuous(breaks = 0:6)+
    ggtitle("Medida de Venda de Bananas por dia e Hora")+
    xlab("Dias da Semana")+
    ylab("Hora do Dia")
#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana

media_top3bananas %>%
    ggplot(aes(x=media_bananas))+
    geom_histogram(bins = 200)+
    facet_wrap(~ order_dow, ncol = 3) +
    theme_bw()
#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

wilcox.test( media_bananas ~ order_hour_of_day,
             data = media_top3bananas,
             alternative = "two.sided",
             subset = order_dow %in% c(3,4),
             conf.int=TRUE)
