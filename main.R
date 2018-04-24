
library(dplyr)

#Rotinas auxiliares, como a de normalizacao
source("C:/Users/caetano/Desktop/CognitiveProfiles/util.R")

# Nome: calPropRespostasConcluisivasTrabalhador
# Parâmetros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
#             Conclusivos: um data frame com a quantidade de respostas conclusivas dada por cada um dos avaliadores
# Retorno: a Proporção de repostas Concluisivas por trabalhor em relação a proporção de respostas conclusivas no sistema 
calPropRespConclTrabEmRelacaoPropResConclSistema <- function(Todos, Conclusivos) {

  #Merge feito para Conclusivos e Resultado para a métrica Pwy.conclusivos
  Conclusivos.Pwy <- merge(Todos, Conclusivos, by="rater")
  #Para cada trabalhador, divide a quantidade de respostas conclusivas dele pela quantidade total de respostas dele
  Conclusivos.Pwy$Tryw <- Conclusivos.Pwy$x.y/Conclusivos.Pwy$x.x
  
  #Proporção de respostas conclusivas por trabalhador divido pala proporção de respostas conclusivas no sistema
  Conclusivos.Pwy$pwy<-Conclusivos.Pwy$Tryw/(sum(Conclusivos$x)/length(base$judgment))
  
  Conclusivos.Pwy
}
# Nome: calPropRespInConclTrabEmRelacaoPropResInConclSistema
# Parâmetros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
#             Inconclusivos: um data frame com a quantidade de respostas inconclusivas dada por cada um dos avaliadores
# Retorno: a Proporção de repostas Inconclusivos por trabalhor em relação a proporção de respostas Inconclusivos no sistema 
calPropRespInConclTrabEmRelacaoPropResInConclSistema <- function(Todos, Inconclusivos) {
  
    #Merge feito para Inconclusivos e Resultado para a métrica Pwi.inconclusivos
    Pwy.inconclusivos <-merge(Todos,Inconclusivos,by ="rater")
    Pwy.inconclusivos$Tryw <- Pwy.inconclusivos[,3]/Pwy.inconclusivos[,2]
    Pwy.inconclusivos$pwi <- Pwy.inconclusivos$Tryw/(sum(Conclusivos$x)/length(base$judgment))
    Pwy.inconclusivos
}
# Nome: calPerRespPorTrabalhador
# Parâmetros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
# Retorno: 0 percentual de repostas por trabalhor no sistema 
calPerRespPorTrabalhador <- function (Todos) {
    Todos$Percentual <- (Todos$x / sum(Todos$x))*100
    Todos
}
# Nome: calDistVariaRespPorTrabEmRelVariaNoSistema
# Parâmetros: base: um data frame orignal com os dados completos
# Retorno: distancias: a distacia euclidiana entre o padrão de tipo de resposta do avaliador em relação a esse mesmo padrão com os tipo de resposta no sistema 

calDistVariaRespPorTrabEmRelVariaNoSistema <- function (base){
  
  baseDerivada <- data.frame(rater = c(),
                             QTD_0 = c(),
                             QTD_1 = c(),
                             QTD_2 = c(),
                             QTD_3 = c(),
                             QTD_4 = c(),
                             stringsAsFactors = FALSE)
  nrow(base) 
  # Começa o loop
  
  while(nrow(base) > 0){
    #converte a coluna rater par string
    base$rater <- as.character(base$rater)
    # Quarda todos os registro do primeiro da fila para um dataframe
    baseUno <- as.data.frame(subset(base, rater == base[1,2]))
    #Separa o nome do rater
    jugamentos <- as.data.frame(baseUno$judgment)
    #cria um vetor com todas os jugamento deste rater
    jugamentoVetor <- as.vector(jugamentos$`baseUno$judgment`)
    #converte o vetor em um dataframe
    jugamentoData <- data.frame(judgment=c(jugamentoVetor))
    #Realiza a contagem de repetiçoes para cada jugamento do rater e quarda em quadroQTD
    quadroQTD <- jugamentoData %>%
      group_by(judgment) %>%
      summarise(repetido = n()) %>%
      arrange(repetido)
    #Converte a coluna repetido do quadroQTD em um vetor de quantidades de jugamento do rater
    vetorQTD=c(quadroQTD$repetido)
    #Quarda na variavel rater o nome que esta em dataframe
    rater = baseUno[1,2]
    #cria um dataframe suas coluna de nomes e colunas de cada tipo de quantidade 
    RaterDerivada <- data.frame(rater=c(rater))
    RaterDerivada$QTD_0 <- c(vetorQTD[2])
    RaterDerivada$QTD_1 <- c(vetorQTD[5])
    RaterDerivada$QTD_2 <- c(vetorQTD[3])
    RaterDerivada$QTD_3 <- c(vetorQTD[4])
    RaterDerivada$QTD_4 <- c(vetorQTD[1])
    #Deleta do ambiente a dataframe baseUno
    remove(baseUno)
    #Adiciona o RaterDerivada na baseDerivada
    baseDerivada <- rbind.data.frame(baseDerivada, RaterDerivada)
    #Deleta do ambiente a dataframe RaterDerivada
    remove(RaterDerivada)
    #Exclui totalemte o rater da que foi derivado da base inicia(Isso se repete até a base ter zero rows)
    base <- as.data.frame(subset(base, rater != base[1,2]))
  }
  #Para as colunas em que nao houve repetição o sistema atribui NA nesse comando substituimos pelo o valor Zero
  baseDerivada[is.na(baseDerivada)] <- 0
  
  Szero = sum(baseDerivada$QTD_0)
  Sum = sum(baseDerivada$QTD_1)
  Sdois = sum(baseDerivada$QTD_2)
  Stres = sum(baseDerivada$QTD_3)
  Squatro = sum(baseDerivada$QTD_4)
  
  max <- 0
  
  distancia = c()
  trabalhador = c()
  #distancias <- as.data.frame()
  distancias <- data.frame(trabalhador,distancia)
  
  while(nrow(baseDerivada) > 0){
    primeiro = baseDerivada[1,]
    zero = primeiro$QTD_0
    um = primeiro$QTD_1
    dois = primeiro$QTD_2
    tres = primeiro$QTD_3
    quatro = primeiro$QTD_4
    
    distancia<-sqrt(sum((zero-Szero)^2,(um-Sum)^2,(dois-Sdois)^2,(tres-Stres)^2,(quatro-Squatro)^2))
    
    distancias <- rbind.data.frame(distancias, data.frame(trabalhador=primeiro$rater,distancia))
    
    baseDerivada <- as.data.frame(subset(baseDerivada, rater != primeiro$rater))
    if(distancia > max){
      max <- distancia
    }
  }
  
  distancias$X <- NULL
  distancias
}

analiseDoMelhorK(Metricas)

analiseDoMelhorK <- function(Metricas){
  
  #Verificando o número ideal de clusters dado os dados com o Método Elbow  
  wss <- (nrow(Metricas)-1)*sum(apply(Metricas,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(Metricas,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Numero de Clusters",
       ylab="Soma dos quadrados dentro dos grupos ",
       main="",
       pch=20, cex=2)
  
}

# Carregamento da base de dados 
base <- as.data.frame(read.csv("C:\Users\caetano\Desktop\cf-sentiment-basic.csv"))

#Seleciona respostas inconclusivas e conta a quantidade de repostas inconclusivas por avaliador
baseI <- as.data.frame(subset(base,judgment == 4))
Inconclusivos <-aggregate(baseI$rater, by = list(rater = baseI$rater),length)

#Seleciona respostas conclusivas e conta a quantidade de repostas onclusivas por avaliador
baseC <- as.data.frame(subset(base,judgment != 4))
Conclusivos <-aggregate(baseC$rater,by = list(rater = baseC$rater),length)

#conta a quantidade de repostas por avaliador
Todos <-aggregate(base$rater,by = list(rater = base$rater),length)

Pwy.conclusivos <- calPropRespConclTrabEmRelacaoPropResConclSistema(Todos, Conclusivos)

Pwy.inconclusivos <- calPropRespInConclTrabEmRelacaoPropResInConclSistema(Todos,Inconclusivos)

Pwy.todos <- calPerRespPorTrabalhador(Todos)

distancias <- calDistVariaRespPorTrabEmRelVariaNoSistema(base)

# cria um subset com a metrica e os rater de Pwy.conclusivos
MetricaPwyConclusivos <- subset(Pwy.conclusivos, select = c("rater","pwy"))
min(MetricaPwyConclusivos$pwy)
max(MetricaPwyConclusivos$pwy)
MetricaPwyConclusivosNormalizada <- Normaliza(MetricaPwyConclusivos)
min(MetricaPwyConclusivosNormalizada$pwy)
max(MetricaPwyConclusivosNormalizada$pwy)

# cria um subset com a metrica e os rater de Pwy.Inconclusivos
MetricaPwyInconclusivos <- subset(Pwy.inconclusivos, select = c("rater","pwi"))
min(MetricaPwyInconclusivos$pwi)
max(MetricaPwyInconclusivos$pwi)
MetricaPwyInconclusivosNormalizada <- Normaliza(MetricaPwyInconclusivos)
min(MetricaPwyInconclusivosNormalizada$pwi)
max(MetricaPwyInconclusivosNormalizada$pwi)

# cria um subset com a metrica e os rater de Pwy.Todos
MetricaPwycTodos <- subset(Pwy.todos, select = c("rater","Percentual"))
min(MetricaPwycTodos$Percentual)
max(MetricaPwycTodos$Percentual)
MetricaPwycTodosNormalizada <- Normaliza(MetricaPwycTodos)
min(MetricaPwycTodosNormalizada$Percentual)
max(MetricaPwycTodosNormalizada$Percentual)

# cria um dataframe que reuni as colunas em uma so tabela de nome 'ColunaDeMétricas'.
ColunaDeMétricas <- join(MetricaPwycTodosNormalizada, MetricaPwyConclusivosNormalizada, by="rater", type="left")
ColunaDeMétricas <- join(ColunaDeMétricas, MetricaPwyInconclusivosNormalizada, by="rater", type="left")
nrow(ColunaDeMétricas)

#Atribui 0 as colunas que possui linha com valor 'NA'
ColunaDeMétricas[is.na(ColunaDeMétricas)] <- 0

#renomeia as colunas da tabela com os nomes das métricas
names(ColunaDeMétricas) = c("raters","Pwc", "Pwi", "Pw")


#Renomeia a coluna trabalhadores par raters
names(distancias) = c("raters", "distancia")
min(distancias$distancia)
max(distancias$distancia)
distanciasNormalizada <- Normaliza(distancias)
min(distanciasNormalizada$distancia)
max(distanciasNormalizada$distancia)

#Realiao o join da tabela de metrica com a nova metrica que foi derivada
ColunaDeMétricasCompleta <- join(ColunaDeMétricas,distanciasNormalizada, by="raters", type="left")

#renomeia as colunas da tabela com os nomes das métricas
names(ColunaDeMétricasCompleta) = c("raters","Pwc", "Pwi", "Pw", "DEw")

#Dataframe com puramente dados numericos sem a lista de raters
Metricas <- data.frame(ColunaDeMétricasCompleta[,2:5])

boxplot(Metricas, las=1)

set.seed(7)

analiseDoMelhorK(Metricas)


#Execute K-Means com o número ótimo de clusters identificados a partir do método Elbow

Resultado = kmeans(Metricas, 5, nstart=100)
Resultado$centers

#
plot(Metricas, col =(Resultado$cluster +1) , main="K-Means resultados com 5 
     clusters", pch=20, cex=2)

a <- seq(1:5)

ndf <- as.data.frame(Resultado$centers)

ndf$Centroids <- a

MetricasVertival <- as.data.frame(rbind(cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pwc,"Metrica"=rep("Pwc",5)),
                                        cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pwi,"Metrica"=rep("Pwi",5)),
                                        cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pw,"Metrica"=rep("Pw",5)),
                                        cbind("Centroid"=ndf$Centroids,"Valor"=ndf$DEw,"Metrica"=rep("DEw",5))))


# Grouped barplot
barplot(Resultado$centers, col=colors()[c(617,23,89,12,148)] , border="white", font.axis=2, beside=T, xlab="group", font.lab=2)

## Grouped barplot
#barplot(MetricasVertival, col=colors()[c(617,23,89,12,148)] , border="white", font.axis=2, beside=T, xlab="group", font.lab=2)
