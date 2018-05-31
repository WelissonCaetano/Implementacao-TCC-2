
library(dplyr)
library(ggplot2)


#Rotinas auxiliares, como a de normalizacao
source("C:/Users/caetano/Desktop/CognitiveProfiles/Implementacao-TCC-2/util.R")

# Nome: calPropRespostasConcluisivasTrabalhador
# Par?metros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
#             Conclusivos: um data frame com a quantidade de respostas conclusivas dada por cada um dos avaliadores
# Retorno: a Propor??o de repostas Concluisivas por trabalhor em rela??o a propor??o de respostas conclusivas no sistema 
calPropRespConclTrabEmRelacaoPropResConclSistema <- function(Todos, Conclusivos) {

  #Merge feito para Conclusivos e Resultado para a m?trica Pwy.conclusivos
  Conclusivos.Pwy <- merge(Todos, Conclusivos, by="rater")
  #Para cada trabalhador, divide a quantidade de respostas conclusivas dele pela quantidade total de respostas dele
  Conclusivos.Pwy$Tryw <- Conclusivos.Pwy$x.y/Conclusivos.Pwy$x.x
  
  #Propor??o de respostas conclusivas por trabalhador divido pala propor??o de respostas conclusivas no sistema
  Conclusivos.Pwy$pwy<-Conclusivos.Pwy$Tryw/(sum(Conclusivos$x)/length(base$judgment))
  
  Conclusivos.Pwy
}
# Nome: calPropRespInConclTrabEmRelacaoPropResInConclSistema
# Par?metros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
#             Inconclusivos: um data frame com a quantidade de respostas inconclusivas dada por cada um dos avaliadores
# Retorno: a Propor??o de repostas Inconclusivos por trabalhor em rela??o a propor??o de respostas Inconclusivos no sistema 
calPropRespInConclTrabEmRelacaoPropResInConclSistema <- function(Todos, Inconclusivos) {
  
    #Merge feito para Inconclusivos e Resultado para a m?trica Pwi.inconclusivos
    Pwy.inconclusivos <-merge(Todos,Inconclusivos,by ="rater")
    Pwy.inconclusivos$Tryw <- Pwy.inconclusivos[,3]/Pwy.inconclusivos[,2]
    Pwy.inconclusivos$pwi <- Pwy.inconclusivos$Tryw/(sum(Conclusivos$x)/length(base$judgment))
    Pwy.inconclusivos
}
# Nome: calPerRespPorTrabalhador
# Par?metros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
# Retorno: 0 percentual de repostas por trabalhor no sistema 
calPerRespPorTrabalhador <- function (Todos) {
    Todos$Percentual <- (Todos$x / sum(Todos$x))*100
    Todos
}
# Nome: calDistVariaRespPorTrabEmRelVariaNoSistema
# Par?metros: base: um data frame orignal com os dados completos
# Retorno: distancias: a distacia euclidiana entre o padr?o de tipo de resposta do avaliador em rela??o a esse mesmo padr?o com os tipo de resposta no sistema 

calDistVariaRespPorTrabEmRelVariaNoSistema <- function (base){
  
  baseDerivada <- data.frame(rater = c(),
                             QTD_0 = c(),
                             QTD_1 = c(),
                             QTD_2 = c(),
                             QTD_3 = c(),
                             QTD_4 = c(),
                             stringsAsFactors = FALSE)
  nrow(base) 
  # Come?a o loop
  
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
    #Realiza a contagem de repeti?oes para cada jugamento do rater e quarda em quadroQTD
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
    #Exclui totalemte o rater da que foi derivado da base inicia(Isso se repete at? a base ter zero rows)
    base <- as.data.frame(subset(base, rater != base[1,2]))
  }
  #Para as colunas em que nao houve repeti??o o sistema atribui NA nesse comando substituimos pelo o valor Zero
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


analiseDoMelhorK <- function(Metricas){
  
  #Verificando o n?mero ideal de clusters dado os dados com o M?todo Elbow  
  wss <- (nrow(Metricas)-1)*sum(apply(Metricas,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(Metricas,
                                       centers=i)$withinss)
  
  png("ErroPorNumeroDeGrupos.png")
  plot(1:15, wss, type="b", xlab="Numero de Clusters",
       ylab="Soma dos quadrados dentro dos grupos ",
       main="",
       pch=20, cex=2)
  dev.off()
  
}



# Carregamento da base de dados 
base <- as.data.frame(read.csv("C:/Users/caetano/Desktop/cf-sentiment-basic.csv"))

#Seleciona respostas inconclusivas e conta a quantidade de repostas inconclusivas por avaliador
baseI <- as.data.frame(subset(base,judgment == 4))
Inconclusivos <-aggregate(baseI$rater, by = list(rater = baseI$rater),length)

#Seleciona respostas conclusivas e conta a quantidade de repostas onclusivas por avaliador
baseC <- as.data.frame(subset(base,judgment != 4))
Conclusivos <-aggregate(baseC$rater,by = list(rater = baseC$rater),length)

#conta a quantidade de repostas por avaliador
Todos <- aggregate(base$rater,by = list(rater = base$rater),length)



#Calcula metricas 

Pwy.todos <- calPerRespPorTrabalhador(Todos)
# cria um subset com a metrica e os rater de Pwy.Todos
MetricaPwycTodos <- subset(Pwy.todos, select = c("rater","Percentual"))
MetricaPwycTodosNormalizada <- Normaliza(MetricaPwycTodos)

Pwy.conclusivos <- calPropRespConclTrabEmRelacaoPropResConclSistema(Todos, Conclusivos)
# cria um subset com a metrica e os rater de Pwy.conclusivos
MetricaPwyConclusivos <- subset(Pwy.conclusivos, select = c("rater","pwy"))
MetricaPwyConclusivosNormalizada <- Normaliza(MetricaPwyConclusivos)

Pwy.inconclusivos <- calPropRespInConclTrabEmRelacaoPropResInConclSistema(Todos,Inconclusivos)
# cria um subset com a metrica e os rater de Pwy.Inconclusivos
MetricaPwyInconclusivos <- subset(Pwy.inconclusivos, select = c("rater","pwi"))
MetricaPwyInconclusivosNormalizada <- Normaliza(MetricaPwyInconclusivos)

distancias <- calDistVariaRespPorTrabEmRelVariaNoSistema(base)
#Renomeia a coluna trabalhadores par raters
names(distancias) = c("rater", "distancia")
distanciasNormalizada <- Normaliza(distancias)


df.metrics.naoNormalizadas <- AgregaDFByRater(MetricaPwycTodos,MetricaPwyConclusivos,MetricaPwyInconclusivos,distancias)
print(head(ColunaDeMetricasCompleta))
names(df.metrics.naoNormalizadas) = c("rater","Pwc", "Pwi", "Pw", "DEw")

ColunaDeMetricasCompleta <- AgregaDFByRater(MetricaPwycTodosNormalizada,MetricaPwyConclusivosNormalizada,MetricaPwyInconclusivosNormalizada,distanciasNormalizada)
print(head(ColunaDeMetricasCompleta))
names(ColunaDeMetricasCompleta) = c("rater","Pwc", "Pwi", "Pw", "DEw")

#Atribui 0 no valor da metrica de algum rater que não valor naquela metrica
#IMPORTANTE:lesandrop(comentei essa linha para ver efeitos nos resultados e checar a necessidade dela)
ColunaDeMetricasCompleta[is.na(ColunaDeMetricasCompleta)] <- 0
df.metrics.naoNormalizadas[is.na(df.metrics.naoNormalizadas)] <- 0

df.metrics.naoNormalizadas$Pw <- NULL #seja apenas as tres metricas relevantes
head(df.metrics.naoNormalizadas)

png("boxPlotMetricasNaoNormalizadas.png")
boxplot(df.metrics.naoNormalizadas[,2:4], las=1)
dev.off()


ColunaDeMetricasCompleta$Pw <- NULL #seja apenas as tres metricas relevantes
head(ColunaDeMetricasCompleta)

png("boxPlotMetricasNormalizadas.png")
boxplot(ColunaDeMetricasCompleta[,2:4], las=1)
dev.off()

##
##Analise do Agrapamentos
##

set.seed(7)

#Dataframe com puramente dados numericos sem a lista de raters
Metricas <- data.frame(ColunaDeMetricasCompleta[,2:4])
head(Metricas)


##
##Analise grafica do melhor k
##
analiseDoMelhorK(Metricas)



##
##Execucao do k-means para o melhor k
##
Resultado <- kmeans(Metricas, 4, nstart=100)


##
##Exibicao dos centroids resultantes do k-means
##
plot(Metricas, col =(Resultado$cluster +1) , main="K-Means resultados com 4 
     clusters", pch=20, cex=2)

a <- seq(1:4)

ndf <- as.data.frame(Resultado$centers)

ndf$Centroids <- a

MetricasVertival <- as.data.frame(rbind(cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pwc,"Metrica"=rep("Pwc",4)),
                                        cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pwi,"Metrica"=rep("Pwi",4)),
  #                                      cbind("Centroid"=ndf$Centroids,"Valor"=ndf$Pw,"Metrica"=rep("Pw",5)),
                                        cbind("Centroid"=ndf$Centroids,"Valor"=ndf$DEw,"Metrica"=rep("DEw",4))))


p <- ggplot(data=MetricasVertival, aes(x=Centroid, y=Valor, fill=Metrica)) + 
  geom_bar(stat="identity", position="dodge")

png("centroids.png")
print(p)
dev.off()

# Grouped barplot
#barplot(Resultado$centers, col=colors()[c(617,23,89,12,148)] , border="white", font.axis=2, beside=T, xlab="group", font.lab=2)

## Grouped barplot
#barplot(MetricasVertival, col=colors()[c(617,23,89,12,148)] , border="white", font.axis=2, beside=T, xlab="group", font.lab=2)
