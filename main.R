
library(plyr)
library(ggplot2)

#Rotinas auxiliares, como a de normalizacao
source("C:/Users/caetano/Desktop/CognitiveProfiles/Implementacao-TCC-2/util.R")
source("C:/Users/caetano/Desktop/CognitiveProfiles/Implementacao-TCC-2/class.R")

# Nome: calPropRespostasConcluisivasTrabalhador
# Parametros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
#             Conclusivos: um data frame com a quantidade de respostas conclusivas dada por cada um dos avaliadores
# Retorno: a Proporcao de repostas Concluisivas por trabalhor em relacao a proporcao de respostas conclusivas no sistema 
calPropRespConclTrabEmRelacaoPropResConclSistema <- function(Todos, Conclusivos) {

  #Merge feito para Conclusivos e Resultado para a metrica Pwy.conclusivos
  Conclusivos.Pwy <- merge(Todos, Conclusivos, by="rater")
  #Para cada trabalhador, divide a quantidade de respostas conclusivas dele pela quantidade total de respostas dele
  Conclusivos.Pwy$Tryw <- Conclusivos.Pwy$x.y/Conclusivos.Pwy$x.x
  
  #Proporcao de respostas conclusivas por trabalhador divido pala proporcao de respostas conclusivas no sistema
  Conclusivos.Pwy$pwy<-Conclusivos.Pwy$Tryw/(sum(Conclusivos$x)/length(base$judgment))
  
  Conclusivos.Pwy
}
# Nome: calPropRespInConclTrabEmRelacaoPropResInConclSistema
# Parametros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
#             Inconclusivos: um data frame com a quantidade de respostas inconclusivas dada por cada um dos avaliadores
# Retorno: a Proporcao de repostas Inconclusivos por trabalhor em relacao a proporcao de respostas Inconclusivos no sistema 
calPropRespInConclTrabEmRelacaoPropResInConclSistema <- function(Todos, Inconclusivos) {
  
    #Merge feito para Inconclusivos e Resultado para a metrica Pwi.inconclusivos
    Pwy.inconclusivos <-merge(Todos,Inconclusivos,by ="rater")
    Pwy.inconclusivos$Tryw <- Pwy.inconclusivos[,3]/Pwy.inconclusivos[,2]
    Pwy.inconclusivos$pwi <- Pwy.inconclusivos$Tryw/(sum(Conclusivos$x)/length(base$judgment))
    Pwy.inconclusivos
}
# Nome: calPerRespPorTrabalhador
# Paremetros: Todos: um data frame com a quantidade de resposta dada por cada um dos avaliadores
# Retorno: 0 percentual de repostas por trabalhor no sistema 
calPerRespPorTrabalhador <- function (Todos) {
    Todos$Percentual <- (Todos$x / sum(Todos$x))*100
    Todos
}
# Nome: analiseDoMelhorK
# Paremetros: Metricas: um data frame orignal com os dados completos de Metricas
# Retorno: plot: gera um grafico com o melhor K e envia para a pasta documentos ou para pasta do projeto

analiseDoMelhorK <- function(Metricas){
  
  #Verificando o numero ideal de clusters dado os dados com o Metodo Elbow  
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

names(df.metrics.naoNormalizadas) = c("rater","Pwc", "Pwi", "Pw", "DEw")

ColunaDeMetricasCompleta <- AgregaDFByRater(MetricaPwycTodosNormalizada,MetricaPwyConclusivosNormalizada,MetricaPwyInconclusivosNormalizada,distanciasNormalizada)
print(head(ColunaDeMetricasCompleta))
names(ColunaDeMetricasCompleta) = c("rater","Pwc", "Pwi", "Pw", "DEw")

#Atribui 0 no valor da metrica de algum rater que não valor naquela metrica
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
                                        cbind("Centroid"=ndf$Centroids,"Valor"=ndf$DEw,"Metrica"=rep("DEw",4))))


p <- ggplot(data=MetricasVertival, aes(x=Centroid, y=Valor, fill=Metrica)) + 
  geom_bar(stat="identity", position="dodge")

png("centroids.png")
print(p)
dev.off()
