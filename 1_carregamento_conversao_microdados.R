#### 01 _ Carregamento e conversão para RDS dos microdados do Censo da Educação superior.
#### Os microdados foram baixados do site http://portal.inep.gov.br/microdados

#### PACOTES E SETWD() ----

setwd("~/")

library(ffbase)
library(ffbase2)
library(dplyr)
library(stringr)
library(tibble)

####

#### LOOP FFDF -> TIBBLE ----

## Definindo os anos do Censo que serão observados e o loop responsável pelo carregamento dos dados.

anos_censo <-
  c("2018",
    "2017",
    "2016",
    "2015",
    "2014",
    "2013",
    "2012",
    "2011",
    "2010",
    "2009")

## Para acompanhar o tempo gasto no loop:
tempo_inicio <- Sys.time()

## Loop responsável por:
## carregar o ffdf -> transformar em tibble -> selecionar colunas de interesse
## -> filtrar para manter graduação presencial -> nomear as colunas com nomes padrão 
## -> criar tibbles para cada ano -> salvar tibble em RDS (etapa feita fora do loop).
for (ano in anos_censo) {
  ## Carregando a base de dados do Censo da Educação Superior em ffdf.
  DM_ALUNO <- load.ffdf(paste0("~/ALUNOS", ano))
  
  ## Selecionando o ffdf dentro do envir e usando a tbl_ffdf para poder usar o dplyr::select.
  DM_ALUNO <- tbl_ffdf(DM_ALUNO[["DM_ALUNO"]])
  
  ## Reduzindo as colunas, transformando em tibble e filtrando para graduação presencial.
  if (ano == "2018") {
    DM_ALUNO <- DM_ALUNO %>% select(
      ID_ALUNO,
      CO_ALUNO_CURSO,
      CO_CURSO,
      CO_IES,
      TP_CATEGORIA_ADMINISTRATIVA,
      TP_ORGANIZACAO_ACADEMICA,
      TP_SITUACAO,
      NU_ANO_INGRESSO,
      TP_NIVEL_ACADEMICO,
      TP_MODALIDADE_ENSINO
    ) %>% as_tibble() %>% filter(TP_NIVEL_ACADEMICO == 1) %>% filter(TP_MODALIDADE_ENSINO == 1)
  } else if (ano == "2017") {
    DM_ALUNO <- DM_ALUNO %>% select(
      CO_ALUNO,
      CO_ALUNO_CURSO,
      CO_CURSO,
      CO_IES,
      TP_CATEGORIA_ADMINISTRATIVA,
      TP_ORGANIZACAO_ACADEMICA,
      TP_SITUACAO,
      NU_ANO_INGRESSO,
      TP_NIVEL_ACADEMICO,
      TP_MODALIDADE_ENSINO
    ) %>% as_tibble() %>% filter(TP_NIVEL_ACADEMICO == 1) %>% filter(TP_MODALIDADE_ENSINO == 1)
  } else if (ano == "2009") {
    DM_ALUNO <- DM_ALUNO %>% select(
      CO_ALUNO,
      CO_VINCULO_ALUNO_CURSO,
      CO_CURSO,
      CO_IES,
      CO_CATEGORIA_ADMINISTRATIVA,
      CO_ORGANIZACAO_ACADEMICA,
      CO_ALUNO_SITUACAO,
      ANO_INGRESSO,
      CO_NIVEL_ACADEMICO,
      CO_MODALIDADE_ENSINO
    ) %>% as_tibble() %>% filter(CO_NIVEL_ACADEMICO == 1) %>% filter(CO_MODALIDADE_ENSINO == 1)
  } else {
    DM_ALUNO <- DM_ALUNO %>% select(
      CO_ALUNO,
      CO_ALUNO_CURSO,
      CO_CURSO,
      CO_IES,
      CO_CATEGORIA_ADMINISTRATIVA,
      CO_ORGANIZACAO_ACADEMICA,
      CO_ALUNO_SITUACAO,
      ANO_INGRESSO,
      CO_NIVEL_ACADEMICO,
      CO_MODALIDADE_ENSINO
    ) %>% as_tibble() %>% filter(CO_NIVEL_ACADEMICO == 1) %>% filter(CO_MODALIDADE_ENSINO == 1)
  }
  
  
  ## Ajustando nome das colunas para que todos os anos tenham colunas de mesmos nomes.
  colnames(DM_ALUNO) <-
    c(
      "CO_ALUNO",
      "CO_ALUNO_CURSO",
      "CO_CURSO",
      "CO_IES",
      "TP_CATEGORIA_ADMINISTRATIVA",
      "TP_ORGANIZACAO_ACADEMICA",
      "TP_SITUACAO",
      "NU_ANO_INGRESSO",
      "TP_NIVEL_ACADEMICO",
      "TP_MODALIDADE_ENSINO"
    )
  
  ## Criando um objeto específico para cada ano e removando o objeto usado no loop.
  assign(paste0("ALUNOS", ano), DM_ALUNO)
  rm(DM_ALUNO)
}

tempo_final <- Sys.time()

####

#### SALVANDO TIBBLE EM RDS r----

saveRDS(ALUNOS2009, "ALUNOS2009.RDS")
saveRDS(ALUNOS2010, "ALUNOS2010.RDS")
saveRDS(ALUNOS2011, "ALUNOS2011.RDS")
saveRDS(ALUNOS2012, "ALUNOS2012.RDS")
saveRDS(ALUNOS2013, "ALUNOS2013.RDS")
saveRDS(ALUNOS2014, "ALUNOS2014.RDS")
saveRDS(ALUNOS2015, "ALUNOS2015.RDS")
saveRDS(ALUNOS2016, "ALUNOS2016.RDS")
saveRDS(ALUNOS2017, "ALUNOS2017.RDS")
saveRDS(ALUNOS2018, "ALUNOS2018.RDS")

####