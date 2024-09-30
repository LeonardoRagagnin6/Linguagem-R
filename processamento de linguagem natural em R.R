library(tidyverse)
library(tokenizers)

texto <- paste("Também entendo que, pelo fato de estarmos em temporada eleitoral, as expectativas quanto ao que vamos realizar este ano são baixas. Mesmo assim, senhor presidente da Câmara, aprecio a atitude construtiva que o senhor e os outros líderes assumiram no final do ano passado para aprovar o orçamento e perpetuar a redução dos impostos sobre as famílias trabalhadoras. Desse modo, espero que possamos colaborar este ano sobre questões que são prioritárias para ambos os partidos, como a reforma da justiça criminal e a assistência às pessoas dependentes de drogas vendidas com receita médica. Quem sabe possamos surpreender os cínicos novamente.")

palavras <- tokenize_words(texto)
palavras

length(palavras)
length(palavras[[1]])

tabela <- table(palavras[[1]])
tabela <- data_frame(palavra = names(tabela), contagem = as.numeric(tabela))
tabela

arrange(tabela, desc(contagem))

# Análise de Frases
frases <- tokenize_sentences(texto)
frases

frases_palavras <- tokenize_words(frases[[1]])
frases_palavras

length(frases_palavras)

length(frases_palavras[[1]])
length(frases_palavras[[2]])
length(frases_palavras[[3]])
length(frases_palavras[[4]])

sapply(frases_palavras, length)

# Análise exploratória do discurso sobre o Estado da União de Barack Obama em 2016
base_url <- "https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/basic-text-processing-in-r/"
url <- sprintf("%s/sotu_text/236.txt", base_url)
texto <- paste(readLines(url), collapse = "\n")

palavras <- tokenize_words(texto)
length(palavras[[1]])

tabela <- table(palavras[[1]])
tabela <- data_frame(word = names(tabela), count = as.numeric(tabela))
tabela <- arrange(tabela, desc(count))
tabela

# Verificação de palavras mais frequentes e filtragem de palavras
palavras_frequentes <- read_csv(sprintf("%s/%s", base_url, "word_frequency.csv"))
palavras_frequentes

tabela <- inner_join(tabela, palavras_frequentes)
tabela

filter(tabela, frequency < 0.1)
print(filter(tabela, frequency < 0.002), n = 15)

# Sumarizar documentos
metadados <- read_csv(sprintf("%s/%s", base_url, "metadata.csv"))
metadados

tabela <- filter(tabela, frequency < 0.002)
resultado <- c(metadados$president[236], metadados$year[236], tabela$word[1:5])
paste(resultado, collapse = "; ")

# Análise dos discursos do Estado da União de 1970 a 2016
# Carregar corpus
ficheiros <- sprintf("%s/sotu_text/%03d.txt", base_url, 1:236)
texto <- c()
for (f in ficheiros) {
  
  texto <- c(texto, paste(readLines(f), collapse = "\n"))
  
}

# Outra forma de carregar um corpus
input_loc <- "/Users/USER/Documents/sotu_text"
ficheiros <- dir(input_loc, full.names = TRUE)
texto <- c()
for (f in ficheiros) {
  
  texto <- c(texto, paste(readLines(f), collapse = "\n"))
  
}

#Análise Exploratória
palavras <- tokenize_words(texto)
sapply(palavras, length)

qplot(metadados$year, sapply(palavras, length)) + labs(x = "Ano", y = "Número de palavras")
qplot(metadados$year, sapply(palavras, length), color = metadados$sotu_type) + labs(x = "Ano", y = "Número de palavras", color = "Modalidade do discurso")

#Análise Estilométrica
frases <- tokenize_sentences(texto)
frases_palavras <- sapply(frases, tokenize_words)

comprimento_frases <- list()
for (i in 1:nrow(metadados)) {
  
  comprimento_frases[[i]] <- sapply(frases_palavras[[i]], length)
  
}                             
mediana_comprimento_frases <- sapply(comprimento_frases, median)
qplot(metadados$year, mediana_comprimento_frases) + labs(x = "Ano", y = "Mediana do comprimento das frases")
qplot(metadados$year, mediana_comprimento_frases) + geom_smooth() + labs(x = "Ano", y = "Mediana do comprimento das frases")

# Resumo do Documento
description <- c()
for (i in 1:length(palavras)) {
  
  tabela <- table(palavras[[i]])
  
  tabela <- data_frame(word = names(tabela), count = as.numeric(tabela))
  
  tabela <- arrange(tabela, desc(count))
  
  tabela <- inner_join(tabela, palavras_frequentes)
  
  tabela <- filter(tabela, frequency < 0.002)
  
  resultado <- c(metadados$president[i], metadados$year[i], tabela$word[1:5])
  
  description <- c(description, paste(resultado, collapse = "; "))
  
}
cat(description, sep = "\n")