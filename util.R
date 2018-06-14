#Funcao para normalizar as metrica com valores entre 0 e 1

Normaliza <- function (dados) {
  maior <- max(dados[,2])
  menor <- min(dados[,2])
  dados[,2] <- (dados[,2] - menor) / (maior - menor)
  return (dados)
}

AgregaDFByRater <- function(Df1, Df2, Df3, df4){
 
  df.joined <- join(Df1, Df2, by="rater", type="left")
  df.joined <- join(df.joined, Df3, by="rater", type="left")
  df.joined <- join(df.joined,df4, by="rater", type="left")
  df.joined
  
}