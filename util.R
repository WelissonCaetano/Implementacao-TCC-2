#Função para normalizar as metrica com valores entre 0 e 1
Normaliza = function (dados) {
  maior = max(dados[,2])
  menor = min(dados[,2])
  dados[,2] = (dados[,2] - menor) / (maior - menor)
  return (dados)
}
