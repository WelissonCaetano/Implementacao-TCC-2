library(dplyr)
calDistVariaRespPorTrabEmRelVariaNoSistema <- function (base){
  
  baseDerivada <- data.frame(rater = c(),
                             QTD_0 = c(),
                             QTD_1 = c(),
                             QTD_2 = c(),
                             QTD_3 = c(),
                             QTD_4 = c(),
                             stringsAsFactors = FALSE)
  nrow(base) 
  # Comeca o loop
  
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
    #Realiza a contagem de repeticoes para cada jugamento do rater e quarda em quadroQTD
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
    #Exclui totalemte o rater da que foi derivado da base inicia(Isso se repete ate a base ter zero rows)
    base <- as.data.frame(subset(base, rater != base[1,2]))
  }
  #Para as colunas em que nao houve repeticao o sistema atribui NA nesse comando substituimos pelo o valor Zero
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
remove.packages()