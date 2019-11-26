#### 2 _ Elaboração dos resultados dos indicadores de variação anual (IEA, NEA)
#### Para maximizar e minimizar o código das seções a seguir, basta 
#### clicar na ponta de seta/no triângulo ao lado do número da linha onde começa a seção.
#### Ex: para ver a seção 02.0.1, clique no triângulo ao lado do número 8 (começo da linha 8)

#### 2.0.0 - Passos prévios necessários. ----
### 2.0.1 - PACOTES E SETWD() ----

setwd("~/")

"%ni%" <- Negate("%in%")

library(dplyr)
library(stringr)
library(tidyr)

### 2.0.2 - CARREGANDO OS RDS ANUAIS ====
ALUNOS2009 <- readRDS("ALUNOS2009.RDS")
ALUNOS2010 <- readRDS("ALUNOS2010.RDS")
ALUNOS2011 <- readRDS("ALUNOS2011.RDS")
ALUNOS2012 <- readRDS("ALUNOS2012.RDS")
ALUNOS2013 <- readRDS("ALUNOS2013.RDS")
ALUNOS2014 <- readRDS("ALUNOS2014.RDS")
ALUNOS2015 <- readRDS("ALUNOS2015.RDS")
ALUNOS2016 <- readRDS("ALUNOS2016.RDS")
ALUNOS2017 <- readRDS("ALUNOS2017.RDS")
ALUNOS2018 <- readRDS("ALUNOS2018.RDS")

#### 2.1.0 - Cálculo dos indicadores gerais. ----
### 2.1.1 - LOOP PARA COMPONENTES DO IEA/NEA ----
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

## Loop responsável por calcular, para cada agrupamento de interesse
## (neste caso, para cada categoria administrativa), os componentes 
## do cálculo dos indicadores de variação anual.

for (ano in anos_censo) {
  
  if (ano == "2018"){
    ALUNOS <- ALUNOS2018
  } else if (ano == "2017") {
    ALUNOS <- ALUNOS2017
  } else if (ano == "2016") {
    ALUNOS <- ALUNOS2016
  } else if (ano == "2015") {
    ALUNOS <- ALUNOS2015
  } else if (ano == "2014") {
    ALUNOS <- ALUNOS2014
  } else if (ano == "2013") {
    ALUNOS <- ALUNOS2013
  } else if (ano == "2012") {
    ALUNOS <- ALUNOS2012
  } else if (ano == "2011") {
    ALUNOS <- ALUNOS2011
  } else if (ano == "2010") {
    ALUNOS <- ALUNOS2010
  } else if (ano == "2009") {
    ALUNOS <- ALUNOS2009
  }
  
  ano_num <- as.numeric(ano)
  
  ## Criando a tabela com os dados de interesse.
  ALUNOS_1B <- ALUNOS %>% 
    mutate("CO_CATEGORIA_ADMINISTRATIVA" = case_when(
      TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
      TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
      TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
      TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
    )) %>% 
    group_by(CO_CATEGORIA_ADMINISTRATIVA) %>%
    summarize(
      CURn = sum(TP_SITUACAO == "2"),
      FORn = sum(TP_SITUACAO == "6" |
                   TP_SITUACAO == "1"),
      MATn = sum(TP_SITUACAO == "2" |
                   TP_SITUACAO == "6"),
      INGTn = sum(NU_ANO_INGRESSO == ano_num)
    )
  
  colnames(ALUNOS_1B) <- c(
    "CO_CATEGORIA_ADMINISTRATIVA",
    paste0("A", ano, "_CURn"),
    paste0("A", ano, "_FORn"),
    paste0("A", ano, "_MATn"),
    paste0("A", ano, "_INGTn")
  )
  
  RENAME <- paste0("ALUNOS", ano, "_1B")
  assign(RENAME, ALUNOS_1B)
}

### 2.1.2 - UNINDO OS RESULTADOS DE CADA ANO DO LOOP ----
ALUNOSFINAL_1B <- merge(ALUNOS2018_1B, ALUNOS2017_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2016_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2015_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2014_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2013_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2012_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2011_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2010_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2009_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")

### 2.1.3 - PRODUZINDO IEA/NEA ----
## Carregando RDS com os resultados dos indicadores para as 
## diferentes categorias administrativas.
ALUNOSFINAL_1B <- readRDS("BASE_IEANEA_CATEGORIASADM.RDS")

## Produzindo o IEA e o NEA
ALUNOSFINAL_IEANEA <-
  ALUNOSFINAL_1B %>% group_by(CO_CATEGORIA_ADMINISTRATIVA) %>% mutate(
    IEA2010 = 1 - ((A2010_MATn - A2010_INGTn) / (A2009_CURn)),
    IEA2011 = 1 - ((A2011_MATn - A2011_INGTn) / (A2010_CURn)),
    IEA2012 = 1 - ((A2012_MATn - A2012_INGTn) / (A2011_CURn)),
    IEA2013 = 1 - ((A2013_MATn - A2013_INGTn) / (A2012_CURn)),
    IEA2014 = 1 - ((A2014_MATn - A2014_INGTn) / (A2013_CURn)),
    IEA2015 = 1 - ((A2015_MATn - A2015_INGTn) / (A2014_CURn)),
    IEA2016 = 1 - ((A2016_MATn - A2016_INGTn) / (A2015_CURn)),
    IEA2017 = 1 - ((A2017_MATn - A2017_INGTn) / (A2016_CURn)),
    IEA2018 = 1 - ((A2018_MATn - A2018_INGTn) / (A2017_CURn)),
    NEA2010 = -((A2010_MATn - A2010_INGTn) - (A2009_CURn)),
    NEA2011 = -((A2011_MATn - A2011_INGTn) - (A2010_CURn)),
    NEA2012 = -((A2012_MATn - A2012_INGTn) - (A2011_CURn)),
    NEA2013 = -((A2013_MATn - A2013_INGTn) - (A2012_CURn)),
    NEA2014 = -((A2014_MATn - A2014_INGTn) - (A2013_CURn)),
    NEA2015 = -((A2015_MATn - A2015_INGTn) - (A2014_CURn)),
    NEA2016 = -((A2016_MATn - A2016_INGTn) - (A2015_CURn)),
    NEA2017 = -((A2017_MATn - A2017_INGTn) - (A2016_CURn)),
    NEA2018 = -((A2018_MATn - A2018_INGTn) - (A2017_CURn))
  )

#### 2.2.0 - Indicadores para cada momento da trajetória acadêmica. ----
### 2.2.1 - LOOP PARA COMPONENTES DO IEAC/NEAC ----
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

## Loop responsável por calcular, para cada agrupamento de interesse
## (neste caso, para cada categoria administrativa), os componentes 
## do cálculo dos indicadores de variação anual ao longo da trajetória acadêmica.

for (ano in anos_censo) {
  
  if (ano == "2018"){
    ALUNOS <- ALUNOS2018
  } else if (ano == "2017") {
    ALUNOS <- ALUNOS2017
  } else if (ano == "2016") {
    ALUNOS <- ALUNOS2016
  } else if (ano == "2015") {
    ALUNOS <- ALUNOS2015
  } else if (ano == "2014") {
    ALUNOS <- ALUNOS2014
  } else if (ano == "2013") {
    ALUNOS <- ALUNOS2013
  } else if (ano == "2012") {
    ALUNOS <- ALUNOS2012
  } else if (ano == "2011") {
    ALUNOS <- ALUNOS2011
  } else if (ano == "2010") {
    ALUNOS <- ALUNOS2010
  } else if (ano == "2009") {
    ALUNOS <- ALUNOS2009
  }
  
  ano_num <- as.numeric(ano)
  
  ## Criando a tabela com os dados de interesse.
  ALUNOS_1B <- ALUNOS %>% 
    mutate("CO_CATEGORIA_ADMINISTRATIVA" = case_when(
      TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
      TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
      TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
      TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
    )) %>% 
    group_by(CO_CATEGORIA_ADMINISTRATIVA) %>%
    summarize(
      CURn = sum(TP_SITUACAO == "2"),
      FORn = sum(TP_SITUACAO == "6" |
                   TP_SITUACAO == "1"),
      MATn = sum(TP_SITUACAO == "2" |
                   TP_SITUACAO == "6"),
      INGTn = sum(NU_ANO_INGRESSO == ano_num),
      CURn1a=sum(NU_ANO_INGRESSO == ano_num & TP_SITUACAO == "2"),
      CURn2a=sum(NU_ANO_INGRESSO == ano_num-1 & TP_SITUACAO == "2"),
      CURn3a=sum(NU_ANO_INGRESSO == ano_num-2 & TP_SITUACAO == "2"),
      CURn4a=sum(NU_ANO_INGRESSO == ano_num-3 & TP_SITUACAO == "2"),
      CURn5a=sum(NU_ANO_INGRESSO == ano_num-4 & TP_SITUACAO == "2"),
      CURn6a=sum(NU_ANO_INGRESSO == ano_num-5 & TP_SITUACAO == "2"),
      CURn7a=sum(NU_ANO_INGRESSO == ano_num-6 & TP_SITUACAO == "2"),
      CURn8a=sum(NU_ANO_INGRESSO == ano_num-7 & TP_SITUACAO == "2"),
      CURn9a=sum(NU_ANO_INGRESSO == ano_num-8 & TP_SITUACAO == "2"),
      CURn10a=sum(NU_ANO_INGRESSO == ano_num-9 & TP_SITUACAO == "2"),
      CURn11a=sum(NU_ANO_INGRESSO == ano_num-10 & TP_SITUACAO == "2"),
      FORn1a=sum(NU_ANO_INGRESSO ==  ano & TP_SITUACAO == "6"),
      FORn2a=sum(NU_ANO_INGRESSO == ano_num-1 & TP_SITUACAO == "6"),
      FORn3a=sum(NU_ANO_INGRESSO == ano_num-2 & TP_SITUACAO == "6"),
      FORn4a=sum(NU_ANO_INGRESSO == ano_num-3 & TP_SITUACAO == "6"),
      FORn5a=sum(NU_ANO_INGRESSO == ano_num-4 & TP_SITUACAO == "6"),
      FORn6a=sum(NU_ANO_INGRESSO == ano_num-5 & TP_SITUACAO == "6"),
      FORn7a=sum(NU_ANO_INGRESSO == ano_num-6 & TP_SITUACAO == "6"),
      FORn8a=sum(NU_ANO_INGRESSO == ano_num-7 & TP_SITUACAO == "6"),
      FORn9a=sum(NU_ANO_INGRESSO == ano_num-8 & TP_SITUACAO == "6"),
      FORn10a=sum(NU_ANO_INGRESSO == ano_num-9 & TP_SITUACAO == "6"),
      FORn11a=sum(NU_ANO_INGRESSO == ano_num-10 & TP_SITUACAO == "6"),
      MATn1a=sum(NU_ANO_INGRESSO == ano & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn2a=sum(NU_ANO_INGRESSO == ano_num-1 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn3a=sum(NU_ANO_INGRESSO == ano_num-2 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn4a=sum(NU_ANO_INGRESSO == ano_num-3 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn5a=sum(NU_ANO_INGRESSO == ano_num-4 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn6a=sum(NU_ANO_INGRESSO == ano_num-5 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn7a=sum(NU_ANO_INGRESSO == ano_num-6 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn8a=sum(NU_ANO_INGRESSO == ano_num-7 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn9a=sum(NU_ANO_INGRESSO == ano_num-8 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn10a=sum(NU_ANO_INGRESSO == ano_num-9 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")),
      MATn11a=sum(NU_ANO_INGRESSO == ano_num-10 & (TP_SITUACAO == "2" | TP_SITUACAO == "6")))

  
  colnames(ALUNOS_1B) <- c(
    "CO_CATEGORIA_ADMINISTRATIVA",
    paste0("A", ano, "_CURn"),
    paste0("A", ano, "_FORn"),
    paste0("A", ano, "_MATn"),
    paste0("A", ano, "_INGTn"),
    paste0("A", ano, "_CURn1a"),
    paste0("A", ano, "_CURn2a"),
    paste0("A", ano, "_CURn3a"),
    paste0("A", ano, "_CURn4a"),
    paste0("A", ano, "_CURn5a"),
    paste0("A", ano, "_CURn6a"),
    paste0("A", ano, "_CURn7a"),
    paste0("A", ano, "_CURn8a"),
    paste0("A", ano, "_CURn9a"),
    paste0("A", ano, "_CURn10a"),
    paste0("A", ano, "_CURn11a"),
    paste0("A", ano, "_FORn1a"),
    paste0("A", ano, "_FORn2a"),
    paste0("A", ano, "_FORn3a"),
    paste0("A", ano, "_FORn4a"),
    paste0("A", ano, "_FORn5a"),
    paste0("A", ano, "_FORn6a"),
    paste0("A", ano, "_FORn7a"),
    paste0("A", ano, "_FORn8a"),
    paste0("A", ano, "_FORn9a"),
    paste0("A", ano, "_FORn10a"),
    paste0("A", ano, "_FORn11a"),
    paste0("A", ano, "_MATn1a"),
    paste0("A", ano, "_MATn2a"),
    paste0("A", ano, "_MATn3a"),
    paste0("A", ano, "_MATn4a"),
    paste0("A", ano, "_MATn5a"),
    paste0("A", ano, "_MATn6a"),
    paste0("A", ano, "_MATn7a"),
    paste0("A", ano, "_MATn8a"),
    paste0("A", ano, "_MATn9a"),
    paste0("A", ano, "_MATn10a"),
    paste0("A", ano, "_MATn11a")
  )
  
  RENAME <- paste0("ALUNOS", ano, "_1B")
  assign(RENAME, ALUNOS_1B)
}

### 2.2.2 - UNINDO OS RESULTADOS DE CADA ANO DO LOOP ----
ALUNOSFINAL_1B <- merge(ALUNOS2018_1B, ALUNOS2017_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2016_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2015_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2014_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2013_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2012_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2011_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2010_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2009_1B, by = "CO_CATEGORIA_ADMINISTRATIVA")


### 2.2.3 - PRODUZINDO IEAC/NEAC ----
## Carregando a base de dados salva previamente.
ALUNOSFINAL_1B <- readRDS("BASE_IEANEA_CATEGORIASADM_COORTES.RDS")
ALUNOSFINAL_1B <-
  ALUNOSFINAL_1B %>% group_by(CO_CATEGORIA_ADMINISTRATIVA)

## Loop para produzir o IEA e o NEA de cada posição
## da trajetória acadêmica por ano.


anos_de_graduacao <- c("1a", "2a", "3a", "4a", "5a",
                       "6a", "7a", "8a", "9a", "10a", "11a")


ALUNOSFINAL_IEACNEAC_MERGE <-
  data.frame(CO_CATEGORIA_ADMINISTRATIVA = c(
    "Privada",
    "Pública Federal",
    "Pública Estadual",
    "Pública Municipal"
  ))

## Loop que compara os agregados do  IEA/NEA mas considerando o tempo de graduação.
## Ex: O IEA dos alunos no 2º ano de graduação em 2013 é calculado pelo quociente
## Entre os matriculados de 2013 que estavam no 2º ano (ano de ingresso em 2012)
## e os matriculados não concluintes de 2012 que estavam no 1º ano.
for (ano_graduacao in anos_de_graduacao) {
  
  ano_anterior_de_graduacao <- ifelse(ano_graduacao == "1a",
                                      NA,
                                      ifelse(
                                        ano_graduacao == "2a",
                                        "1a",
                                        ifelse(
                                          ano_graduacao == "3a",
                                          "2a",
                                          ifelse(
                                            ano_graduacao == "4a",
                                            "3a",
                                            ifelse(
                                              ano_graduacao == "5a",
                                              "4a",
                                              ifelse(
                                                ano_graduacao == "6a",
                                                "5a",
                                                ifelse(
                                                  ano_graduacao == "7a",
                                                  "6a",
                                                  ifelse(
                                                    ano_graduacao == "8a",
                                                    "7a",
                                                    ifelse(
                                                      ano_graduacao == "9a",
                                                      "8a",
                                                      ifelse(
                                                        ano_graduacao == "10a",
                                                        "9a",
                                                        ifelse(ano_graduacao == "11a", "10a", NA)
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      ))
  
  for (ano_anterior_graduacao in ano_anterior_de_graduacao) {
    
    ifelse(
      ano_graduacao == "1a",
      ALUNOSFINAL_IEACNEAC <- transmute(
        ALUNOSFINAL_1B,
        IEAC2009 = 1 - ((get(
          paste0("A2009_MATn", ano_graduacao)
        ) / (A2009_INGTn))),
        IEAC2010 = 1 - ((get(
          paste0("A2010_MATn", ano_graduacao)
        ) / (A2010_INGTn))),
        IEAC2011 = 1 - ((get(
          paste0("A2011_MATn", ano_graduacao)
        ) / (A2011_INGTn))),
        IEAC2012 = 1 - ((get(
          paste0("A2012_MATn", ano_graduacao)
        ) / (A2012_INGTn))),
        IEAC2013 = 1 - ((get(
          paste0("A2013_MATn", ano_graduacao)
        ) / (A2013_INGTn))),
        IEAC2014 = 1 - ((get(
          paste0("A2014_MATn", ano_graduacao)
        ) / (A2014_INGTn))),
        IEAC2015 = 1 - ((get(
          paste0("A2015_MATn", ano_graduacao)
        ) / (A2015_INGTn))),
        IEAC2016 = 1 - ((get(
          paste0("A2016_MATn", ano_graduacao)
        ) / (A2016_INGTn))),
        IEAC2017 = 1 - ((get(
          paste0("A2017_MATn", ano_graduacao)
        ) / (A2017_INGTn))),
        IEAC2018 = 1 - ((get(
          paste0("A2018_MATn", ano_graduacao)
        ) / (A2018_INGTn))),
        NEAC2009 = -((get(
          paste0("A2009_MATn", ano_graduacao)
        ) - (A2009_INGTn))),
        NEAC2010 = -((get(
          paste0("A2010_MATn", ano_graduacao)
        ) - (A2010_INGTn))),
        NEAC2011 = -((get(
          paste0("A2011_MATn", ano_graduacao)
        ) - (A2011_INGTn))),
        NEAC2012 = -((get(
          paste0("A2012_MATn", ano_graduacao)
        ) - (A2012_INGTn))),
        NEAC2013 = -((get(
          paste0("A2013_MATn", ano_graduacao)
        ) - (A2013_INGTn))),
        NEAC2014 = -((get(
          paste0("A2014_MATn", ano_graduacao)
        ) - (A2014_INGTn))),
        NEAC2015 = -((get(
          paste0("A2015_MATn", ano_graduacao)
        ) - (A2015_INGTn))),
        NEAC2016 = -((get(
          paste0("A2016_MATn", ano_graduacao)
        ) - (A2016_INGTn))),
        NEAC2017 = -((get(
          paste0("A2017_MATn", ano_graduacao)
        ) - (A2017_INGTn))),
        NEAC2018 = -((get(
          paste0("A2018_MATn", ano_graduacao)
        ) - (A2018_INGTn)))
      ),
      
      ALUNOSFINAL_IEACNEAC <- transmute(
        ALUNOSFINAL_1B,
        IEAC2010 = 1 - ((get(
          paste0("A2010_MATn", ano_graduacao)
        ) / (get(
          paste0("A2009_CURn", ano_anterior_graduacao)
        )))),
        IEAC2011 = 1 - ((get(
          paste0("A2011_MATn", ano_graduacao)
        ) / (get(
          paste0("A2010_CURn", ano_anterior_graduacao)
        )))),
        IEAC2012 = 1 - ((get(
          paste0("A2012_MATn", ano_graduacao)
        ) / (get(
          paste0("A2011_CURn", ano_anterior_graduacao)
        )))),
        IEAC2013 = 1 - ((get(
          paste0("A2013_MATn", ano_graduacao)
        ) / (get(
          paste0("A2012_CURn", ano_anterior_graduacao)
        )))),
        IEAC2014 = 1 - ((get(
          paste0("A2014_MATn", ano_graduacao)
        ) / (get(
          paste0("A2013_CURn", ano_anterior_graduacao)
        )))),
        IEAC2015 = 1 - ((get(
          paste0("A2015_MATn", ano_graduacao)
        ) / (get(
          paste0("A2014_CURn", ano_anterior_graduacao)
        )))),
        IEAC2016 = 1 - ((get(
          paste0("A2016_MATn", ano_graduacao)
        ) / (get(
          paste0("A2015_CURn", ano_anterior_graduacao)
        )))),
        IEAC2017 = 1 - ((get(
          paste0("A2017_MATn", ano_graduacao)
        ) / (get(
          paste0("A2016_CURn", ano_anterior_graduacao)
        )))),
        IEAC2018 = 1 - ((get(
          paste0("A2018_MATn", ano_graduacao)
        ) / (get(
          paste0("A2017_CURn", ano_anterior_graduacao)
        )))),
        NEAC2010 = -((get(
          paste0("A2010_MATn", ano_graduacao)
        ) - (get(
          paste0("A2009_CURn", ano_anterior_graduacao)
        )))),
        NEAC2011 = -((get(
          paste0("A2011_MATn", ano_graduacao)
        ) - (get(
          paste0("A2010_CURn", ano_anterior_graduacao)
        )))),
        NEAC2012 = -((get(
          paste0("A2012_MATn", ano_graduacao)
        ) - (get(
          paste0("A2011_CURn", ano_anterior_graduacao)
        )))),
        NEAC2013 = -((get(
          paste0("A2013_MATn", ano_graduacao)
        ) - (get(
          paste0("A2012_CURn", ano_anterior_graduacao)
        )))),
        NEAC2014 = -((get(
          paste0("A2014_MATn", ano_graduacao)
        ) - (get(
          paste0("A2013_CURn", ano_anterior_graduacao)
        )))),
        NEAC2015 = -((get(
          paste0("A2015_MATn", ano_graduacao)
        ) - (get(
          paste0("A2014_CURn", ano_anterior_graduacao)
        )))),
        NEAC2016 = -((get(
          paste0("A2016_MATn", ano_graduacao)
        ) - (get(
          paste0("A2015_CURn", ano_anterior_graduacao)
        )))),
        NEAC2017 = -((get(
          paste0("A2017_MATn", ano_graduacao)
        ) - (get(
          paste0("A2016_CURn", ano_anterior_graduacao)
        )))),
        NEAC2018 = -((get(
          paste0("A2018_MATn", ano_graduacao)
        ) - (get(
          paste0("A2017_CURn", ano_anterior_graduacao)
        ))))
      )
      
    )
    
    ifelse(
      ano_graduacao == "1a",
      colnames(ALUNOSFINAL_IEACNEAC) <-
        c(
          "CO_CATEGORIA_ADMINISTRATIVA",
          paste0("IEAC2009", ano_graduacao),
          paste0("IEAC2010", ano_graduacao),
          paste0("IEAC2011", ano_graduacao),
          paste0("IEAC2012", ano_graduacao),
          paste0("IEAC2013", ano_graduacao),
          paste0("IEAC2014", ano_graduacao),
          paste0("IEAC2015", ano_graduacao),
          paste0("IEAC2016", ano_graduacao),
          paste0("IEAC2017", ano_graduacao),
          paste0("IEAC2018", ano_graduacao),
          paste0("NEAC2009", ano_graduacao),
          paste0("NEAC2010", ano_graduacao),
          paste0("NEAC2011", ano_graduacao),
          paste0("NEAC2012", ano_graduacao),
          paste0("NEAC2013", ano_graduacao),
          paste0("NEAC2014", ano_graduacao),
          paste0("NEAC2015", ano_graduacao),
          paste0("NEAC2016", ano_graduacao),
          paste0("NEAC2017", ano_graduacao),
          paste0("NEAC2018", ano_graduacao)
        ),
      
      colnames(ALUNOSFINAL_IEACNEAC) <-
        c(
          "CO_CATEGORIA_ADMINISTRATIVA",
          paste0("IEAC2010", ano_graduacao),
          paste0("IEAC2011", ano_graduacao),
          paste0("IEAC2012", ano_graduacao),
          paste0("IEAC2013", ano_graduacao),
          paste0("IEAC2014", ano_graduacao),
          paste0("IEAC2015", ano_graduacao),
          paste0("IEAC2016", ano_graduacao),
          paste0("IEAC2017", ano_graduacao),
          paste0("IEAC2018", ano_graduacao),
          paste0("NEAC2010", ano_graduacao),
          paste0("NEAC2011", ano_graduacao),
          paste0("NEAC2012", ano_graduacao),
          paste0("NEAC2013", ano_graduacao),
          paste0("NEAC2014", ano_graduacao),
          paste0("NEAC2015", ano_graduacao),
          paste0("NEAC2016", ano_graduacao),
          paste0("NEAC2017", ano_graduacao),
          paste0("NEAC2018", ano_graduacao)
        )
    )
    
    ALUNOSFINAL_IEACNEAC_MERGE <-
      merge(ALUNOSFINAL_IEACNEAC_MERGE, ALUNOSFINAL_IEACNEAC, by = "CO_CATEGORIA_ADMINISTRATIVA")
    
  }
}



#### 2.3.0 - Cálculo dos indicadores gerais por IES. ----
### 2.3.1 - LOOP PARA COMPONENTES DO IEA/NEA POR IES ----
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

## Loop responsável por calcular, para cada agrupamento de interesse
## (neste caso, para cada categoria administrativa), os componentes 
## do cálculo dos indicadores de variação anual.

for (ano in anos_censo) {
  
  if (ano == "2018"){
    ALUNOS <- ALUNOS2018
  } else if (ano == "2017") {
    ALUNOS <- ALUNOS2017
  } else if (ano == "2016") {
    ALUNOS <- ALUNOS2016
  } else if (ano == "2015") {
    ALUNOS <- ALUNOS2015
  } else if (ano == "2014") {
    ALUNOS <- ALUNOS2014
  } else if (ano == "2013") {
    ALUNOS <- ALUNOS2013
  } else if (ano == "2012") {
    ALUNOS <- ALUNOS2012
  } else if (ano == "2011") {
    ALUNOS <- ALUNOS2011
  } else if (ano == "2010") {
    ALUNOS <- ALUNOS2010
  } else if (ano == "2009") {
    ALUNOS <- ALUNOS2009
  }
  
  ano_num <- as.numeric(ano)
  
  ## Criando a tabela com os dados de interesse.
  ALUNOS_1B <- ALUNOS %>% 
    mutate("CO_CATEGORIA_ADMINISTRATIVA" = case_when(
      TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
      TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
      TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
      TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
    )) %>% 
    group_by(CO_IES) %>%
    summarize(
      CURn = sum(TP_SITUACAO == "2"),
      FORn = sum(TP_SITUACAO == "6" |
                   TP_SITUACAO == "1"),
      MATn = sum(TP_SITUACAO == "2" |
                   TP_SITUACAO == "6"),
      INGTn = sum(NU_ANO_INGRESSO == ano_num)
    )
  
  colnames(ALUNOS_1B) <- c(
    "CO_IES",
    paste0("A", ano, "_CURn"),
    paste0("A", ano, "_FORn"),
    paste0("A", ano, "_MATn"),
    paste0("A", ano, "_INGTn")
  )
  
  RENAME <- paste0("ALUNOS", ano, "_1B")
  assign(RENAME, ALUNOS_1B)
}
### 2.3.2 - UNINDO OS RESULTADOS DE CADA ANO DO LOOP ----
ALUNOSFINAL_1B <- merge(ALUNOS2018_1B, ALUNOS2017_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2016_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2015_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2014_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2013_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2012_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2011_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2010_1B, by = "CO_IES")
ALUNOSFINAL_1B <-
  merge(ALUNOSFINAL_1B, ALUNOS2009_1B, by = "CO_IES")

### 2.3.3 - PRODUZINDO IEA/NEA POR IES ----
## Carregando RDS com os resultados dos indicadores para as 
## diferentes IES.
ALUNOSFINAL_1B <- readRDS("BASE_IEANEA_IES.RDS")

## Produzindo o IEA e o NEA
ALUNOSFINAL_IEANEA_IES <-
  ALUNOSFINAL_1B %>% group_by(CO_IES) %>% mutate(
    IEA2010 = 1 - ((A2010_MATn - A2010_INGTn) / (A2009_CURn)),
    IEA2011 = 1 - ((A2011_MATn - A2011_INGTn) / (A2010_CURn)),
    IEA2012 = 1 - ((A2012_MATn - A2012_INGTn) / (A2011_CURn)),
    IEA2013 = 1 - ((A2013_MATn - A2013_INGTn) / (A2012_CURn)),
    IEA2014 = 1 - ((A2014_MATn - A2014_INGTn) / (A2013_CURn)),
    IEA2015 = 1 - ((A2015_MATn - A2015_INGTn) / (A2014_CURn)),
    IEA2016 = 1 - ((A2016_MATn - A2016_INGTn) / (A2015_CURn)),
    IEA2017 = 1 - ((A2017_MATn - A2017_INGTn) / (A2016_CURn)),
    IEA2018 = 1 - ((A2018_MATn - A2018_INGTn) / (A2017_CURn)),
    NEA2010 = -((A2010_MATn - A2010_INGTn) - (A2009_CURn)),
    NEA2011 = -((A2011_MATn - A2011_INGTn) - (A2010_CURn)),
    NEA2012 = -((A2012_MATn - A2012_INGTn) - (A2011_CURn)),
    NEA2013 = -((A2013_MATn - A2013_INGTn) - (A2012_CURn)),
    NEA2014 = -((A2014_MATn - A2014_INGTn) - (A2013_CURn)),
    NEA2015 = -((A2015_MATn - A2015_INGTn) - (A2014_CURn)),
    NEA2016 = -((A2016_MATn - A2016_INGTn) - (A2015_CURn)),
    NEA2017 = -((A2017_MATn - A2017_INGTn) - (A2016_CURn)),
    NEA2018 = -((A2018_MATn - A2018_INGTn) - (A2017_CURn))
  )

## Complementando a tabela com os dados de categoria adm e org acadêmica das IES.
## Passo para o CALCULO PARA IES.

IES <- read.csv("DM_IES.csv", sep = "|")
IES <- IES %>% filter(CO_IES %in% ALUNOSFINAL_IEANEA_IES$CO_IES)

complemento_ies <- data.frame(
    "CO_IES" = IES$CO_IES,
    "NO_IES" = IES$NO_IES,
    "SG_IES" = IES$SG_IES,
    "CO_CATEGORIA_ADMINISTRATIVA" = IES$TP_CATEGORIA_ADMINISTRATIVA,
    "CO_REGIAO" = IES$CO_REGIAO,
    "CO_UF" = IES$CO_UF,
    "CO_MUNICIPIO" = IES$CO_MUNICIPIO
  )

ALUNOSFINAL_IEANEA_IES <-
  merge(ALUNOSFINAL_IEANEA_IES, complemento_ies, by = "CO_IES")
ALUNOSFINAL_IEANEA_IES <-
  ALUNOSFINAL_IEANEA_IES %>% select(CO_CATEGORIA_ADMINISTRATIVA,
                                CO_REGIAO,
                                CO_UF,
                                CO_MUNICIPIO,
                                CO_IES,
                                NO_IES,
                                SG_IES,
                                everything())
