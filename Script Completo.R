{
#### IMPORTANDO RECURSOS ####

# instalando/importando pacotes
install.packages('lexiconPT')
install.packages('stringr')
install.packages("rtweet")
install.packages("writexl")
install.packages('caret')
install.packages('tidyverse')
install.packages('vctrs')
install.packages('ROCR')

# carregando pacotes
{ library(writexl)
  library(dplyr)
  library(stringr)
  library(rtweet)
  library(readxl)
  library(caret)
  library(tidyverse)
  library(ROCR)}}
  
  
# importando e explorando os lexicos
{sentilex = lexiconPT::sentiLex_lem_PT02
oplexicon = lexiconPT::oplexicon_v3.0
  
positivas=filter(oplexicon,polarity==1)
negativas=filter(oplexicon,polarity==-1)
  
unique(oplexicon$type)
names(oplexicon)
}
  
# dicionario de lemas
lemma_dic <- read.delim(file = "https://raw.githubusercontent.com/michmech/lemmatization-lists/master/lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE, encoding = 'UTF-8')
names(lemma_dic) <- c("stem", "Palavras")
  
# dicionario de stop words
#stop_words1 = filter(stopwordslangs, lang=="pt")
{stop_words2 = read.delim(file="https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  names(stop_words2) = c('Palavras')
  
# removendo espaCos em branco da lista importada
for (i in 1:length(stop_words2$stop_words)){
  stop_words2$stop_words[i]=trimws(stop_words2$stop_words[i])
  }
}

#### API TWITTER ####

## configurando acesso a API do twitter
{api_key = "PREENCHER"
api_secret = "PREENCHER"

## authentication via web browser
token <- create_token(
app = "PREENCHER",
consumer_key = api_key, 
consumer_secret = api_secret)}

# extraindo tweets
  
{rt <- search_tweets(
  "Anitta -filter:replies 
  -filter:links 
  -filter:retweets 
  -filter:quote 
  -filter:verified", 
  n = 18000, 
  include_rts = FALSE, 
  token = token, 
  lang="pt", )}
  
  
write_xlsx(rt,"tweets(Anitta).xlsx")
  
  
#### CRIACAO DE FUNCOES BASICAS #### 

#funcao para remover acentos

tirar_acentos <- function(str,pattern="all") {
  # Rotinas e funcoes uteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Funcao que tira todos os acentos e pontua??es de um vetor de strings.
  # Parametros:
  # str - vetor de strings que ter?o seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos dever?o ser retirados.
  #            Para indicar quais acentos dever?o ser retirados, um vetor com os s?mbolos dever?o ser passados.
  #            Exemplo: pattern = c("?", "^") retirar? os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que s?o "?", "`", "^", "~", "?", "?")
  if(!is.character(str))
    str <- as.character(str)
    
    pattern <- unique(pattern)
    
    if(any(pattern=="?"))
      pattern[pattern=="?"] <- "?"
    
    symbols <- c(
      acute = "????????????",
      grave = "??????????",
      circunflex = "??????????",
      tilde = "??????",
      umlaut = "???????????",
      cedil = "??"
    )
    
    nudeSymbols <- c(
      acute = "aeiouAEIOUyY",
      grave = "aeiouAEIOU",
      circunflex = "aeiouAEIOU",
      tilde = "aoAOnN",
      umlaut = "aeiouAEIOUy",
      cedil = "cC"
    )
    
    accentTypes <- c("?","`","^","~","?","?")
    
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
      return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    
    for(i in which(accentTypes%in%pattern))
      str <- chartr(symbols[i],nudeSymbols[i], str)
    
    return(str)
  }
  
  
  
  
# LER BASE DE TWEETS ROTULADOS
  
dados = read_xlsx('Tweets Rotulados.xlsx')
  
dados = data.frame(dados) %>% select(c(text,Polaridade))
  
dados = filter(dados,Polaridade==-1 || Polaridade==1)
dados$text = tirar_acentos(dados$text)
  

  
# TRATAMENTOS DE TEXTO
{dados$text = str_to_lower(dados$text)
  
  dados$text = str_replace_all(dados$text,'aaaa','a')
  dados$text = str_replace_all(dados$text,'aaa','a')
  dados$text = str_replace_all(dados$text,'aa','a')
  dados$text = str_replace_all(dados$text,'eeee','e')
  dados$text = str_replace_all(dados$text,'eee','e')
  dados$text = str_replace_all(dados$text,'ee','e')
  dados$text = str_replace_all(dados$text,'iiii','i')
  dados$text = str_replace_all(dados$text,'iii','i')
  dados$text = str_replace_all(dados$text,'ii','i')
  dados$text = str_replace_all(dados$text,'oooo','o')
  dados$text = str_replace_all(dados$text,'ooo','o')
  dados$text = str_replace_all(dados$text,'oo','o')
  dados$text = str_replace_all(dados$text,'uuuu','u')
  dados$text = str_replace_all(dados$text,'uuu','u')
  dados$text = str_replace_all(dados$text,'uu','u')
  dados$text = str_replace_all(dados$text,'@anitta','Anitta')
  dados$text = str_replace_all(dados$text,'anita','Anitta')
  dados$text = str_replace_all(dados$text,'anira','Anitta')
  dados$text = str_replace_all(dados$text,"[[:punct:]]", "")
  
  dados$text = str_replace_all(dados$text,'0','')
  dados$text = str_replace_all(dados$text,'1','')
  dados$text = str_replace_all(dados$text,'2','')
  dados$text = str_replace_all(dados$text,'3','')
  dados$text = str_replace_all(dados$text,'4','')
  dados$text = str_replace_all(dados$text,'5','')
  dados$text = str_replace_all(dados$text,'6','')
  dados$text = str_replace_all(dados$text,'7','')
  dados$text = str_replace_all(dados$text,'8','')
  dados$text = str_replace_all(dados$text,'9','')
}
  
#criando coluna identificador

dados = mutate(dados, RecordID = c(1:length(dados$text)))
dados = select(dados,c(RecordID,everything()))

# CRIACAO DO BAG OF WORDS
{
  dados = mutate(dados, Palavras = strsplit(as.character(text)," ")) %>% unnest(Palavras)
  dados$Palavras = str_replace_all(dados$Palavras,' ','')
  dados$Palavras = str_replace_all(dados$Palavras,' ','')
  dados$Palavras = str_replace_all(dados$Palavras,'
','')
  
  dados = na.exclude(dados)
  
  dados = filter(dados,Palavras!=' ')
  dados = filter(dados,Palavras!='')
  dados = filter(dados,Palavras!=' ')
  dados = filter(dados,Palavras!='
')
  dados$Palavras =  str_squish(dados$Palavras) 
  
  
  dados = filter(dados,RecordID!=2134 & RecordID!=2890 & 
                   RecordID!=2897 & RecordID!=3185 & 
                   RecordID!=3244 & RecordID!=3874 & 
                   RecordID!=4032 & RecordID!=2174 &
                   RecordID!=2897 & RecordID!=3800 &
                   RecordID!=4394 & RecordID!=2108)
}
  


# REMOVENDO STOP WORDS
dados = anti_join(dados, stop_words2, by='Palavras')
lemma_dic$stem = tirar_acentos(lemma_dic$stem)
lemma_dic$Palavras = tirar_acentos(lemma_dic$Palavras)
# LEMATIZANDO OS TERMOS
{dados_lematizados = left_join(dados, lemma_dic, by='Palavras')
  dados_lematizados$Palavras = ifelse(is.na(dados_lematizados$stem),dados_lematizados$Palavras,dados_lematizados$stem)
  dados_lematizados = select(dados_lematizados,-stem)}

# contando a frequencia das palavras

{dados_lematizados = group_by(dados_lematizados,Palavras)
  freq = summarize(dados_lematizados, frequencia = n())
  freq = arrange(freq,desc(frequencia))
  dados_lematizados = left_join(dados_lematizados,freq,'Palavras')
  min_freq = 16
  dados_lematizados = ungroup(dados_lematizados)
  }
  


# filtrando palavras com a frequencia minima
{dados_lematizados = filter(dados_lematizados,frequencia>=min_freq)
  dados_lematizados = select(dados_lematizados,-frequencia)}



# contando frequencia das palavras dentro do tweet
{dados_lematizados = dados_lematizados %>% group_by(RecordID,Palavras,Polaridade)
  freq_palavra = summarize(dados_lematizados,Palavras,Polaridade,frequencia=n())
  freq_palavra = unique(freq_palavra)
  dados_lematizados = ungroup(dados_lematizados)
  dados_lematizados = group_by(dados_lematizados,RecordID)
  
  total_palavras = summarize(dados_lematizados,TotalPalavras = n())
  freq_palavra = left_join(freq_palavra,total_palavras,by='RecordID')
  freq_palavra = mutate(freq_palavra,FreqPercentual = frequencia/TotalPalavras)
  
  dados_lematizados = select(freq_palavra,-c(frequencia,TotalPalavras))
}


# transformando na matriz termo-documento

teste_bow = pivot_wider(dados_lematizados, id_cols = c(RecordID,Polaridade), names_from = Palavras, values_from = FreqPercentual)

bow = teste_bow %>% replace(is.na(.), 0)
  
# calculando scores basicos com o lexico rotulado

{names(oplexicon) = c('Palavras','Classe','Polaridade Palavra','Polaridade Revisada')
  oplexicon = select(oplexicon,c(Palavras,`Polaridade Palavra`))
  
  dados_lematizados = left_join(dados_lematizados,oplexicon,by='Palavras')
  dados_lematizados = dados_lematizados %>% replace(is.na(.), 0)
  dados_lematizados = group_by(dados_lematizados, RecordID)
  scores = summarize(dados_lematizados,score=sum(Polaridade))
  ungroup(dados_lematizados)}
  
# incluindo coluna do score no Bag Of Words
  
bow = left_join(bow,scores,by='RecordID')
bow = select(bow,c(Polaridade,score,everything()))

# removendo coluna de RecordID
bow = ungroup(bow)
bow = select(bow,-RecordID)
bow = select(bow,-Anitta)
  
  

# removendo objetos que nao serao mais utilizados

{rm('freq')
  rm('freq_palavra')
  rm('teste_bow')
  rm('stop_words2')
  rm('total_palavras')
  rm('lemma_dic')
  rm('oplexicon')
  rm('scores')}
  



################################################################################

{
  require(readxl)
  require(caret)
  require(dplyr)
  require(gbm)

  
  bow = read_xlsx('Tweets Rotulados.xlsx')
}



{bow = data.frame(bow)
  
  bow = dplyr::select(bow,-c(anitta,nao))
  bow = dplyr::select(bow,-c(score))
  
  bow$Polaridade = ifelse(bow$Polaridade==1,1,0)
  
  set.seed(13)
  indice_treino = createDataPartition(y=bow$Polaridade, p=0.75, list=FALSE)
  treino = bow[indice_treino, ]
  teste = bow[-indice_treino, ]
}

write_xlsx(bow,'bow_final.xlsx')

# fazendo o balanceamento de classe
  
library(ggplot2)
library(dplyr)
library(themis)
library(recipes)

treino %>% ggplot(aes(x=Polaridade)) + geom_bar(stat='count')
table(treino$Polaridade)

treino$Polaridade=as.factor(treino$Polaridade)
treino2 = recipe(Polaridade~.,data=treino)
treino2 = step_downsample(treino2,Polaridade)
treino2 = prep(treino2)
treino2 = juice(treino2)

treino2 %>% ggplot(aes(x=Polaridade)) + geom_bar(stat='count')
table(treino2$Polaridade)


modelFit1 = gbm(Polaridade~.,data=treino,
                distribution="bernoulli",
                n.trees = 50,
                interaction.depth = 5,
                shrinkage=0.1,
                bag.fraction =1,
                cv.fold=10)

head(summary(modelFit1))


n_trees = c(5000)
shrink = c(0.1,0.01,0.001)
depth = c(1,5,10)



model_testing <- function(treino, n_trees,shrink,depth){
  models = list()
  for (i in n_trees){
    for (j in shrink){
      for (k in depth){
        models[[length(models)+1]] = gbm(Polaridade~.,data=treino,
                           distribution="bernoulli",
                           n.trees = i,
                           interaction.depth = k,
                           shrinkage=j,
                           bag.fraction =1,
                           cv.fold=10)
      }
    }
  }
  return(models)
}

getwd()  

{models = model_testing(treino,n_trees,shrink,depth)
save(models,file='models_10k.rdata')}


load("~/Downloads/models.rdata")
predictions_ = predict(models[[2]],newdata = teste)
predictions = predict(modelFit1,newdata = teste)

gbm.perf(modelFit1)
gbm.perf(models[[2]])

predicao<-predict(models[[2]],teste, type = 'response',
                  n.trees=1000)

#AUC
gbm.roc.area(teste$Polaridade,predicao)

#ROC e AUC c/ pROC
par(pty = "s")
pROC::roc(teste$Polaridade,predicao,plot=T)
par(pty = "m")

#Devemos criar regra de classificação. 
#Exemplo: prob < 0.5: tipo 0; prob >= 0.5: tipo 1
classificacao<-predicao>=0.5

confusionMatrix(data=as.factor(classificacao),
                reference=as.factor(teste$Polaridade))



#Utilizando curva ROC
#predicao=predict aplicado na amostra teste
#resposta e a var. resposta do teste
otimizador<-function(predicao,resposta){
  require(ROCR)
  predicao2 = prediction(predicao, resposta)
  performance = performance(predicao2,"tpr","fpr")
  funcao <- attr(performance, "y.values")[[1]] -
    (attr(performance, "x.values")[[1]])
  c <-  performance@alpha.values[[1]][which.max(funcao)]
  plot(performance, colorize=T, lwd=2)
  c
}

#Realizo a predição na amostra treino, e aplico na 
#na funcao otimizador()
predicao_treino<-predict(models[[2]],treino, 
                         type = 'response',
                         n.trees=666)

c<-otimizador(predicao_treino,treino$Polaridade)

#Com o c definido, aplico o modelo na amostra teste
predicao<-predict(models[[2]],teste, 
                  type = 'response',
                  n.trees=666)

classificacao2<-predicao>=c

confusionMatrix(data=as.factor(classificacao2),
                reference=as.factor(teste$Polaridade))
