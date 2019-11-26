######################################################

## Definindo o wd e carregando os pacotes necessários.

setwd("~/")

library(ffbase)


## Definindo os anos do Censo que serão observados e o loop responsável pelo carregamento dos dados.

anos_censo <- c("2018", "2017", "2016", "2015", "2014",
                "2013", "2012", "2011", "2010","2009")

tempo_inicial <- Sys.time()

for(ano in anos_censo){
  
  ## Carregando a base de dados do Censo da Educação Superior.
  DM_ALUNO <- read.csv2.ffdf(file = paste0("~/CENSO", ano, "/DADOS/DM_ALUNO.CSV"), sep="|", first.rows=1000000, colClasses=NA)
  
  ##               
  save.ffdf(DM_ALUNO, dir = paste0("~/ALUNOS", ano))
  
}

tempo_final <- Sys.time()