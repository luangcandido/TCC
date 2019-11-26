#### 4 _ Elaboração dos resultados dos indicadores de fluxo (TMC, TDC, TEC)
#### Para maximizar e minimizar o código das seções a seguir, basta
#### clicar na ponta de seta/no triângulo ao lado do número da linha onde começa a seção.
#### Ex: para ver a seção 02.0.1, clique no triângulo ao lado do número 8 (começo da linha 8)

#### 4.0.0 - Passos prévios necessários. ----
### 4.0.1 - PACOTES E SETWD() ----

library(dplyr)
library(stringr)
library(ffbase)
"%ni%" <- Negate("%in%")

### 4.0.2 - CARREGANDO OS RDS ANUAIS ====
# Seguindo a sugestão de INEP (2017), começamos o acompanhamento a partir de 2010.
# Devido a mudança no padrão da variável CO_ALUNO em 2018, a presente série histórica vai até 2017.
ALUNOS2010 <- readRDS("ALUNOS2010.RDS")
ALUNOS2011 <- readRDS("ALUNOS2011.RDS")
ALUNOS2012 <- readRDS("ALUNOS2012.RDS")
ALUNOS2013 <- readRDS("ALUNOS2013.RDS")
ALUNOS2014 <- readRDS("ALUNOS2014.RDS")
ALUNOS2015 <- readRDS("ALUNOS2015.RDS")
ALUNOS2016 <- readRDS("ALUNOS2016.RDS")
ALUNOS2017 <- readRDS("ALUNOS2017.RDS")

C10_REP <- readRDS("C10_REP.RDS")
C11_REP <- readRDS("C11_REP.RDS")
C12_REP <- readRDS("C12_REP.RDS")
C13_REP <- readRDS("C13_REP.RDS")
C14_REP <- readRDS("C14_REP.RDS")
C15_REP <- readRDS("C15_REP.RDS")
C16_REP <- readRDS("C16_REP.RDS")
C17_REP <- readRDS("C17_REP.RDS")


#### 4.1.0 - Definição e acompanhamento das coortes. ----
### 4.1.1 - Criação do registro-chave e identificação dos que se repetem. ----

## Filtrando as bases de dados.
ALUNOS2010_1 <- ALUNOS2010 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES)))
ALUNOS2011_1 <- ALUNOS2011 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES)))
ALUNOS2012_1 <- ALUNOS2012 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES))) 
ALUNOS2013_1 <- ALUNOS2013 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES))) 
ALUNOS2014_1 <- ALUNOS2014 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES))) 
ALUNOS2015_1 <- ALUNOS2015 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES))) 
ALUNOS2016_1 <- ALUNOS2016 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES))) 
ALUNOS2017_1 <- ALUNOS2017 %>% 
  mutate("REGCHAVE" = as.numeric(paste0(CO_ALUNO, CO_CURSO, CO_IES))) 

rm(ALUNOS2010)
rm(ALUNOS2011)
rm(ALUNOS2012)
rm(ALUNOS2013)
rm(ALUNOS2014)
rm(ALUNOS2015)
rm(ALUNOS2016)
rm(ALUNOS2017)

# Identificando e filtrando vínculos que se repetem.
# Os registros-chave (CO_ALUNO+CO_CURSO+CO_IES) que tem frequência 
# maior que 1 no ano de ingresso são filtrados da base de dados.
# Os registros-chave serem calculados dessa forma é coerente com a opção
# de mensurar a evasão dos cursos. 

# Repetições de ingressantes e não ingressantes.
# Já estão salvos na pasta com os mesmos nomes e são carregados junto com os RDS anuais.
#C10_REP <-
#  as.data.frame(table(ALUNOS2010_1$REGCHAVE)) %>% filter(Freq > 1)
#C10_REP <- as.numeric(C10_REP$Var1)
#
#C11_REP <-
#  as.data.frame(table(ALUNOS2011_1$REGCHAVE)) %>% filter(Freq > 1)
#C11_REP <- as.numeric(C11_REP$Var1)
#
#C12_REP <-
#  as.data.frame(table(ALUNOS2012_1$REGCHAVE)) %>% filter(Freq > 1)
#C12_REP <- as.numeric(C12_REP$Var1)
#
#C13_REP <-
#  as.data.frame(table(ALUNOS2013_1$REGCHAVE)) %>% filter(Freq > 1)
#C13_REP <- as.numeric(C13_REP$Var1)
#
#C14_REP <-
#  as.data.frame(table(ALUNOS2014_1$REGCHAVE)) %>% filter(Freq > 1)
#C14_REP <- as.numeric(C14_REP$Var1)
#
#C15_REP <-
#  as.data.frame(table(ALUNOS2015_1$REGCHAVE)) %>% filter(Freq > 1)
#C15_REP <- as.numeric(C15_REP$Var1)
#
#C16_REP <-
#  as.data.frame(table(ALUNOS2016_1$REGCHAVE)) %>% filter(Freq > 1)
#C16_REP <- as.numeric(C16_REP$Var1)
#
#C17_REP <-
#  as.data.frame(table(ALUNOS2017_1$REGCHAVE)) %>% filter(Freq > 1)
#C17_REP <- as.numeric(C17_REP$Var1)
#
#
#
## Juntando os REGCHAVE que se repetem nos anos.
REGISTROS_REPETIDOS <- bind_rows(rename(as.data.frame(C10_REP), "REGCHAVE" = "C10_REP"), 
                                 rename(as.data.frame(C11_REP), "REGCHAVE" = "C11_REP"), 
                                 rename(as.data.frame(C12_REP), "REGCHAVE" = "C12_REP"), 
                                 rename(as.data.frame(C13_REP), "REGCHAVE" = "C13_REP"), 
                                 rename(as.data.frame(C14_REP), "REGCHAVE" = "C14_REP"), 
                                 rename(as.data.frame(C15_REP), "REGCHAVE" = "C15_REP"), 
                                 rename(as.data.frame(C16_REP), "REGCHAVE" = "C16_REP"), 
                                 rename(as.data.frame(C17_REP), "REGCHAVE" = "C17_REP")) %>% distinct()


REGISTROS_REPETIDOS <- as.numeric(REGISTROS_REPETIDOS$REGCHAVE)

### 4.1.2 - Definindo e acompanhando as coortes ----
## Primeiramente, Define-se a coorte. Para a coorte dos ingressantes de 2010, por exemplo, são considerados os estudantes cujo ANO_INGRESSO == 2010,
## excluindo aquele que aparecem no registro de vínculos que aparecem duplicados em qualquer ano entre 2010-2017 (df REGISTROS_REPETIDOS).
## Coorte 2010

Coorte2010_Acompanhamento <-
  ALUNOS2010_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2010_Acompanhamento_Registros <- as.numeric(Coorte2010_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2010_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2010
Coorte2010_A2010 <-
  ALUNOS2010_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)

Coorte2010_A2010 <-
  Coorte2010_A2010 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2010, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2010" = TP_SITUACAO)
rm(Coorte2010_A2010)

## Ano 2011
Coorte2010_A2011 <-
  ALUNOS2011_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)
Coorte2010_A2011 <-
  Coorte2010_A2011 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2011, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2011" = TP_SITUACAO)
Coorte2010_Acompanhamento <-
  Coorte2010_Acompanhamento %>% mutate("SITUACAO_2011" = ifelse(is.na(SITUACAO_2011),
                                                                SITUACAO_2010,
                                                                SITUACAO_2011))



## Ano 2012
Coorte2010_A2012 <-
  ALUNOS2012_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)
Coorte2010_A2012 <-
  Coorte2010_A2012 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2012, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2012" = TP_SITUACAO)
Coorte2010_Acompanhamento <-
  Coorte2010_Acompanhamento %>% mutate("SITUACAO_2012" = ifelse(is.na(SITUACAO_2012),
                                                                SITUACAO_2011,
                                                                SITUACAO_2012))

## Ano 2013
Coorte2010_A2013 <-
  ALUNOS2013_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)
Coorte2010_A2013 <-
  Coorte2010_A2013 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2013, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2013" = TP_SITUACAO)
Coorte2010_Acompanhamento <-
  Coorte2010_Acompanhamento %>% mutate("SITUACAO_2013" = ifelse(is.na(SITUACAO_2013),
                                                                SITUACAO_2012,
                                                                SITUACAO_2013))

## Ano 2014
Coorte2010_A2014 <-
  ALUNOS2014_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)
Coorte2010_A2014 <-
  Coorte2010_A2014 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2014, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2014" = TP_SITUACAO)
Coorte2010_Acompanhamento <-
  Coorte2010_Acompanhamento %>% mutate("SITUACAO_2014" = ifelse(is.na(SITUACAO_2014),
                                                                SITUACAO_2013,
                                                                SITUACAO_2014))

## Ano 2015
Coorte2010_A2015 <-
  ALUNOS2015_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)
Coorte2010_A2015 <-
  Coorte2010_A2015 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2015, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2015" = TP_SITUACAO)
Coorte2010_Acompanhamento <-
  Coorte2010_Acompanhamento %>% mutate("SITUACAO_2015" = ifelse(is.na(SITUACAO_2015),
                                                                SITUACAO_2014,
                                                                SITUACAO_2015))

## Ano 2016
Coorte2010_A2016 <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)
Coorte2010_A2016 <-
  Coorte2010_A2016 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2016, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2016" = TP_SITUACAO)
Coorte2010_Acompanhamento <-
  Coorte2010_Acompanhamento %>% mutate("SITUACAO_2016" = ifelse(is.na(SITUACAO_2016),
                                                                SITUACAO_2015,
                                                                SITUACAO_2016))

## Ano 2017
Coorte2010_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2010) %>%
  filter(REGCHAVE %in% Coorte2010_Acompanhamento_Registros)
Coorte2010_A2017 <-
  Coorte2010_A2017 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2010_Acompanhamento <-
  left_join(Coorte2010_Acompanhamento, Coorte2010_A2017, by = "REGCHAVE")
Coorte2010_Acompanhamento <-
  rename(Coorte2010_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
Coorte2010_Acompanhamento <-
  Coorte2010_Acompanhamento %>% mutate("SITUACAO_2017" = ifelse(is.na(SITUACAO_2017),
                                                                SITUACAO_2016,
                                                                SITUACAO_2017))


saveRDS(Coorte2010_Acompanhamento, "Coorte2010_Acompanhamento.RDS")



## Coorte 2011

Coorte2011_Acompanhamento <-
  ALUNOS2011_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2011_Acompanhamento_Registros <- as.numeric(Coorte2011_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2011_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2011
Coorte2011_A2011 <-
  ALUNOS2011_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %in% Coorte2011_Acompanhamento_Registros)

Coorte2011_A2011 <-
  Coorte2011_A2011 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2011_Acompanhamento <-
  left_join(Coorte2011_Acompanhamento, Coorte2011_A2011, by = "REGCHAVE")
Coorte2011_Acompanhamento <-
  rename(Coorte2011_Acompanhamento, "SITUACAO_2011" = TP_SITUACAO)
rm(Coorte2011_A2011)


## Ano 2012
Coorte2011_A2012 <-
  ALUNOS2012_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %in% Coorte2011_Acompanhamento_Registros)
Coorte2011_A2012 <-
  Coorte2011_A2012 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2011_Acompanhamento <-
  left_join(Coorte2011_Acompanhamento, Coorte2011_A2012, by = "REGCHAVE")
Coorte2011_Acompanhamento <-
  rename(Coorte2011_Acompanhamento, "SITUACAO_2012" = TP_SITUACAO)
Coorte2011_Acompanhamento <-
  Coorte2011_Acompanhamento %>% mutate("SITUACAO_2012" = ifelse(is.na(SITUACAO_2012),
                                                                SITUACAO_2011,
                                                                SITUACAO_2012))

## Ano 2013
Coorte2011_A2013 <-
  ALUNOS2013_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %in% Coorte2011_Acompanhamento_Registros)
Coorte2011_A2013 <-
  Coorte2011_A2013 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2011_Acompanhamento <-
  left_join(Coorte2011_Acompanhamento, Coorte2011_A2013, by = "REGCHAVE")
Coorte2011_Acompanhamento <-
  rename(Coorte2011_Acompanhamento, "SITUACAO_2013" = TP_SITUACAO)
Coorte2011_Acompanhamento <-
  Coorte2011_Acompanhamento %>% mutate("SITUACAO_2013" = ifelse(is.na(SITUACAO_2013),
                                                                SITUACAO_2012,
                                                                SITUACAO_2013))

## Ano 2014
Coorte2011_A2014 <-
  ALUNOS2014_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %in% Coorte2011_Acompanhamento_Registros)
Coorte2011_A2014 <-
  Coorte2011_A2014 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2011_Acompanhamento <-
  left_join(Coorte2011_Acompanhamento, Coorte2011_A2014, by = "REGCHAVE")
Coorte2011_Acompanhamento <-
  rename(Coorte2011_Acompanhamento, "SITUACAO_2014" = TP_SITUACAO)
Coorte2011_Acompanhamento <-
  Coorte2011_Acompanhamento %>% mutate("SITUACAO_2014" = ifelse(is.na(SITUACAO_2014),
                                                                SITUACAO_2013,
                                                                SITUACAO_2014))

## Ano 2015
Coorte2011_A2015 <-
  ALUNOS2015_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %in% Coorte2011_Acompanhamento_Registros)
Coorte2011_A2015 <-
  Coorte2011_A2015 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2011_Acompanhamento <-
  left_join(Coorte2011_Acompanhamento, Coorte2011_A2015, by = "REGCHAVE")
Coorte2011_Acompanhamento <-
  rename(Coorte2011_Acompanhamento, "SITUACAO_2015" = TP_SITUACAO)
Coorte2011_Acompanhamento <-
  Coorte2011_Acompanhamento %>% mutate("SITUACAO_2015" = ifelse(is.na(SITUACAO_2015),
                                                                SITUACAO_2014,
                                                                SITUACAO_2015))

## Ano 2016
Coorte2011_A2016 <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %in% Coorte2011_Acompanhamento_Registros)
Coorte2011_A2016 <-
  Coorte2011_A2016 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2011_Acompanhamento <-
  left_join(Coorte2011_Acompanhamento, Coorte2011_A2016, by = "REGCHAVE")
Coorte2011_Acompanhamento <-
  rename(Coorte2011_Acompanhamento, "SITUACAO_2016" = TP_SITUACAO)
Coorte2011_Acompanhamento <-
  Coorte2011_Acompanhamento %>% mutate("SITUACAO_2016" = ifelse(is.na(SITUACAO_2016),
                                                                SITUACAO_2015,
                                                                SITUACAO_2016))

## Ano 2017
Coorte2011_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2011) %>%
  filter(REGCHAVE %in% Coorte2011_Acompanhamento_Registros)
Coorte2011_A2017 <-
  Coorte2011_A2017 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2011_Acompanhamento <-
  left_join(Coorte2011_Acompanhamento, Coorte2011_A2017, by = "REGCHAVE")
Coorte2011_Acompanhamento <-
  rename(Coorte2011_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
Coorte2011_Acompanhamento <-
  Coorte2011_Acompanhamento %>% mutate("SITUACAO_2017" = ifelse(is.na(SITUACAO_2017),
                                                                SITUACAO_2016,
                                                                SITUACAO_2017))


saveRDS(Coorte2011_Acompanhamento, "Coorte2011_Acompanhamento.RDS")


## Coorte 2012

Coorte2012_Acompanhamento <-
  ALUNOS2012_1 %>%
  filter(NU_ANO_INGRESSO == 2012) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2012_Acompanhamento_Registros <- as.numeric(Coorte2012_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2012_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2012
Coorte2012_A2012 <-
  ALUNOS2012_1 %>%
  filter(NU_ANO_INGRESSO == 2012) %>%
  filter(REGCHAVE %in% Coorte2012_Acompanhamento_Registros)

Coorte2012_A2012 <-
  Coorte2012_A2012 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2012_Acompanhamento <-
  left_join(Coorte2012_Acompanhamento, Coorte2012_A2012, by = "REGCHAVE")
Coorte2012_Acompanhamento <-
  rename(Coorte2012_Acompanhamento, "SITUACAO_2012" = TP_SITUACAO)
rm(Coorte2012_A2012)

## Ano 2013
Coorte2012_A2013 <-
  ALUNOS2013_1 %>%
  filter(NU_ANO_INGRESSO == 2012) %>%
  filter(REGCHAVE %in% Coorte2012_Acompanhamento_Registros)
Coorte2012_A2013 <-
  Coorte2012_A2013 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2012_Acompanhamento <-
  left_join(Coorte2012_Acompanhamento, Coorte2012_A2013, by = "REGCHAVE")
Coorte2012_Acompanhamento <-
  rename(Coorte2012_Acompanhamento, "SITUACAO_2013" = TP_SITUACAO)
Coorte2012_Acompanhamento <-
  Coorte2012_Acompanhamento %>% mutate("SITUACAO_2013" = ifelse(is.na(SITUACAO_2013),
                                                                SITUACAO_2012,
                                                                SITUACAO_2013))

## Ano 2014
Coorte2012_A2014 <-
  ALUNOS2014_1 %>%
  filter(NU_ANO_INGRESSO == 2012) %>%
  filter(REGCHAVE %in% Coorte2012_Acompanhamento_Registros)
Coorte2012_A2014 <-
  Coorte2012_A2014 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2012_Acompanhamento <-
  left_join(Coorte2012_Acompanhamento, Coorte2012_A2014, by = "REGCHAVE")
Coorte2012_Acompanhamento <-
  rename(Coorte2012_Acompanhamento, "SITUACAO_2014" = TP_SITUACAO)
Coorte2012_Acompanhamento <-
  Coorte2012_Acompanhamento %>% mutate("SITUACAO_2014" = ifelse(is.na(SITUACAO_2014),
                                                                SITUACAO_2013,
                                                                SITUACAO_2014))

## Ano 2015
Coorte2012_A2015 <-
  ALUNOS2015_1 %>%
  filter(NU_ANO_INGRESSO == 2012) %>%
  filter(REGCHAVE %in% Coorte2012_Acompanhamento_Registros)
Coorte2012_A2015 <-
  Coorte2012_A2015 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2012_Acompanhamento <-
  left_join(Coorte2012_Acompanhamento, Coorte2012_A2015, by = "REGCHAVE")
Coorte2012_Acompanhamento <-
  rename(Coorte2012_Acompanhamento, "SITUACAO_2015" = TP_SITUACAO)
Coorte2012_Acompanhamento <-
  Coorte2012_Acompanhamento %>% mutate("SITUACAO_2015" = ifelse(is.na(SITUACAO_2015),
                                                                SITUACAO_2014,
                                                                SITUACAO_2015))

## Ano 2016
Coorte2012_A2016 <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2012) %>%
  filter(REGCHAVE %in% Coorte2012_Acompanhamento_Registros)
Coorte2012_A2016 <-
  Coorte2012_A2016 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2012_Acompanhamento <-
  left_join(Coorte2012_Acompanhamento, Coorte2012_A2016, by = "REGCHAVE")
Coorte2012_Acompanhamento <-
  rename(Coorte2012_Acompanhamento, "SITUACAO_2016" = TP_SITUACAO)
Coorte2012_Acompanhamento <-
  Coorte2012_Acompanhamento %>% mutate("SITUACAO_2016" = ifelse(is.na(SITUACAO_2016),
                                                                SITUACAO_2015,
                                                                SITUACAO_2016))

## Ano 2017
Coorte2012_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2012) %>%
  filter(REGCHAVE %in% Coorte2012_Acompanhamento_Registros)
Coorte2012_A2017 <-
  Coorte2012_A2017 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2012_Acompanhamento <-
  left_join(Coorte2012_Acompanhamento, Coorte2012_A2017, by = "REGCHAVE")
Coorte2012_Acompanhamento <-
  rename(Coorte2012_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
Coorte2012_Acompanhamento <-
  Coorte2012_Acompanhamento %>% mutate("SITUACAO_2017" = ifelse(is.na(SITUACAO_2017),
                                                                SITUACAO_2016,
                                                                SITUACAO_2017))


saveRDS(Coorte2012_Acompanhamento, "Coorte2012_Acompanhamento.RDS")

## Coorte 2013

Coorte2013_Acompanhamento <-
  ALUNOS2013_1 %>%
  filter(NU_ANO_INGRESSO == 2013) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2013_Acompanhamento_Registros <- as.numeric(Coorte2013_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2013_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2013
Coorte2013_A2013 <-
  ALUNOS2013_1 %>%
  filter(NU_ANO_INGRESSO == 2013) %>%
  filter(REGCHAVE %in% Coorte2013_Acompanhamento_Registros)

Coorte2013_A2013 <-
  Coorte2013_A2013 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2013_Acompanhamento <-
  left_join(Coorte2013_Acompanhamento, Coorte2013_A2013, by = "REGCHAVE")
Coorte2013_Acompanhamento <-
  rename(Coorte2013_Acompanhamento, "SITUACAO_2013" = TP_SITUACAO)
rm(Coorte2013_A2013)

## Ano 2014
Coorte2013_A2014 <-
  ALUNOS2014_1 %>%
  filter(NU_ANO_INGRESSO == 2013) %>%
  filter(REGCHAVE %in% Coorte2013_Acompanhamento_Registros)
Coorte2013_A2014 <-
  Coorte2013_A2014 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2013_Acompanhamento <-
  left_join(Coorte2013_Acompanhamento, Coorte2013_A2014, by = "REGCHAVE")
Coorte2013_Acompanhamento <-
  rename(Coorte2013_Acompanhamento, "SITUACAO_2014" = TP_SITUACAO)
Coorte2013_Acompanhamento <-
  Coorte2013_Acompanhamento %>% mutate("SITUACAO_2014" = ifelse(is.na(SITUACAO_2014),
                                                                SITUACAO_2013,
                                                                SITUACAO_2014))

## Ano 2015
Coorte2013_A2015 <-
  ALUNOS2015_1 %>%
  filter(NU_ANO_INGRESSO == 2013) %>%
  filter(REGCHAVE %in% Coorte2013_Acompanhamento_Registros)
Coorte2013_A2015 <-
  Coorte2013_A2015 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2013_Acompanhamento <-
  left_join(Coorte2013_Acompanhamento, Coorte2013_A2015, by = "REGCHAVE")
Coorte2013_Acompanhamento <-
  rename(Coorte2013_Acompanhamento, "SITUACAO_2015" = TP_SITUACAO)
Coorte2013_Acompanhamento <-
  Coorte2013_Acompanhamento %>% mutate("SITUACAO_2015" = ifelse(is.na(SITUACAO_2015),
                                                                SITUACAO_2014,
                                                                SITUACAO_2015))

## Ano 2016
Coorte2013_A2016 <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2013) %>%
  filter(REGCHAVE %in% Coorte2013_Acompanhamento_Registros)
Coorte2013_A2016 <-
  Coorte2013_A2016 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2013_Acompanhamento <-
  left_join(Coorte2013_Acompanhamento, Coorte2013_A2016, by = "REGCHAVE")
Coorte2013_Acompanhamento <-
  rename(Coorte2013_Acompanhamento, "SITUACAO_2016" = TP_SITUACAO)
Coorte2013_Acompanhamento <-
  Coorte2013_Acompanhamento %>% mutate("SITUACAO_2016" = ifelse(is.na(SITUACAO_2016),
                                                                SITUACAO_2015,
                                                                SITUACAO_2016))

## Ano 2017
Coorte2013_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2013) %>%
  filter(REGCHAVE %in% Coorte2013_Acompanhamento_Registros)
Coorte2013_A2017 <-
  Coorte2013_A2017 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2013_Acompanhamento <-
  left_join(Coorte2013_Acompanhamento, Coorte2013_A2017, by = "REGCHAVE")
Coorte2013_Acompanhamento <-
  rename(Coorte2013_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
Coorte2013_Acompanhamento <-
  Coorte2013_Acompanhamento %>% mutate("SITUACAO_2017" = ifelse(is.na(SITUACAO_2017),
                                                                SITUACAO_2016,
                                                                SITUACAO_2017))


saveRDS(Coorte2013_Acompanhamento, "Coorte2013_Acompanhamento.RDS")

## Coorte 2014

Coorte2014_Acompanhamento <-
  ALUNOS2014_1 %>%
  filter(NU_ANO_INGRESSO == 2014) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2014_Acompanhamento_Registros <- as.numeric(Coorte2014_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2014_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2014
Coorte2014_A2014 <-
  ALUNOS2014_1 %>%
  filter(NU_ANO_INGRESSO == 2014) %>%
  filter(REGCHAVE %in% Coorte2014_Acompanhamento_Registros)

Coorte2014_A2014 <-
  Coorte2014_A2014 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2014_Acompanhamento <-
  left_join(Coorte2014_Acompanhamento, Coorte2014_A2014, by = "REGCHAVE")
Coorte2014_Acompanhamento <-
  rename(Coorte2014_Acompanhamento, "SITUACAO_2014" = TP_SITUACAO)
rm(Coorte2014_A2014)

## Ano 2015
Coorte2014_A2015 <-
  ALUNOS2015_1 %>%
  filter(NU_ANO_INGRESSO == 2014) %>%
  filter(REGCHAVE %in% Coorte2014_Acompanhamento_Registros)
Coorte2014_A2015 <-
  Coorte2014_A2015 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2014_Acompanhamento <-
  left_join(Coorte2014_Acompanhamento, Coorte2014_A2015, by = "REGCHAVE")
Coorte2014_Acompanhamento <-
  rename(Coorte2014_Acompanhamento, "SITUACAO_2015" = TP_SITUACAO)
Coorte2014_Acompanhamento <-
  Coorte2014_Acompanhamento %>% mutate("SITUACAO_2015" = ifelse(is.na(SITUACAO_2015),
                                                                SITUACAO_2014,
                                                                SITUACAO_2015))

## Ano 2016
Coorte2014_A2016 <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2014) %>%
  filter(REGCHAVE %in% Coorte2014_Acompanhamento_Registros)
Coorte2014_A2016 <-
  Coorte2014_A2016 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2014_Acompanhamento <-
  left_join(Coorte2014_Acompanhamento, Coorte2014_A2016, by = "REGCHAVE")
Coorte2014_Acompanhamento <-
  rename(Coorte2014_Acompanhamento, "SITUACAO_2016" = TP_SITUACAO)
Coorte2014_Acompanhamento <-
  Coorte2014_Acompanhamento %>% mutate("SITUACAO_2016" = ifelse(is.na(SITUACAO_2016),
                                                                SITUACAO_2015,
                                                                SITUACAO_2016))

## Ano 2017
Coorte2014_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2014) %>%
  filter(REGCHAVE %in% Coorte2014_Acompanhamento_Registros)
Coorte2014_A2017 <-
  Coorte2014_A2017 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2014_Acompanhamento <-
  left_join(Coorte2014_Acompanhamento, Coorte2014_A2017, by = "REGCHAVE")
Coorte2014_Acompanhamento <-
  rename(Coorte2014_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
Coorte2014_Acompanhamento <-
  Coorte2014_Acompanhamento %>% mutate("SITUACAO_2017" = ifelse(is.na(SITUACAO_2017),
                                                                SITUACAO_2016,
                                                                SITUACAO_2017))


saveRDS(Coorte2014_Acompanhamento, "Coorte2014_Acompanhamento.RDS")

## Coorte 2015

Coorte2015_Acompanhamento <-
  ALUNOS2015_1 %>%
  filter(NU_ANO_INGRESSO == 2015) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2015_Acompanhamento_Registros <- as.numeric(Coorte2015_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2015_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2015
Coorte2015_A2015 <-
  ALUNOS2015_1 %>%
  filter(NU_ANO_INGRESSO == 2015) %>%
  filter(REGCHAVE %in% Coorte2015_Acompanhamento_Registros)

Coorte2015_A2015 <-
  Coorte2015_A2015 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2015_Acompanhamento <-
  left_join(Coorte2015_Acompanhamento, Coorte2015_A2015, by = "REGCHAVE")
Coorte2015_Acompanhamento <-
  rename(Coorte2015_Acompanhamento, "SITUACAO_2015" = TP_SITUACAO)
rm(Coorte2015_A2015)

## Ano 2016
Coorte2015_A2016 <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2015) %>%
  filter(REGCHAVE %in% Coorte2015_Acompanhamento_Registros)
Coorte2015_A2016 <-
  Coorte2015_A2016 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2015_Acompanhamento <-
  left_join(Coorte2015_Acompanhamento, Coorte2015_A2016, by = "REGCHAVE")
Coorte2015_Acompanhamento <-
  rename(Coorte2015_Acompanhamento, "SITUACAO_2016" = TP_SITUACAO)
Coorte2015_Acompanhamento <-
  Coorte2015_Acompanhamento %>% mutate("SITUACAO_2016" = ifelse(is.na(SITUACAO_2016),
                                                                SITUACAO_2015,
                                                                SITUACAO_2016))

## Ano 2017
Coorte2015_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2015) %>%
  filter(REGCHAVE %in% Coorte2015_Acompanhamento_Registros)
Coorte2015_A2017 <-
  Coorte2015_A2017 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2015_Acompanhamento <-
  left_join(Coorte2015_Acompanhamento, Coorte2015_A2017, by = "REGCHAVE")
Coorte2015_Acompanhamento <-
  rename(Coorte2015_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
Coorte2015_Acompanhamento <-
  Coorte2015_Acompanhamento %>% mutate("SITUACAO_2017" = ifelse(is.na(SITUACAO_2017),
                                                                SITUACAO_2016,
                                                                SITUACAO_2017))


saveRDS(Coorte2015_Acompanhamento, "Coorte2015_Acompanhamento.RDS")

## Coorte 2016

Coorte2016_Acompanhamento <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2016) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2016_Acompanhamento_Registros <- as.numeric(Coorte2016_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2016_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2016
Coorte2016_A2016 <-
  ALUNOS2016_1 %>%
  filter(NU_ANO_INGRESSO == 2016) %>%
  filter(REGCHAVE %in% Coorte2016_Acompanhamento_Registros)

Coorte2016_A2016 <-
  Coorte2016_A2016 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2016_Acompanhamento <-
  left_join(Coorte2016_Acompanhamento, Coorte2016_A2016, by = "REGCHAVE")
Coorte2016_Acompanhamento <-
  rename(Coorte2016_Acompanhamento, "SITUACAO_2016" = TP_SITUACAO)
rm(Coorte2016_A2016)

## Ano 2017
Coorte2016_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2016) %>%
  filter(REGCHAVE %in% Coorte2016_Acompanhamento_Registros)
Coorte2016_A2017 <-
  Coorte2016_A2017 %>% select(REGCHAVE, TP_SITUACAO)
Coorte2016_Acompanhamento <-
  left_join(Coorte2016_Acompanhamento, Coorte2016_A2017, by = "REGCHAVE")
Coorte2016_Acompanhamento <-
  rename(Coorte2016_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
Coorte2016_Acompanhamento <-
  Coorte2016_Acompanhamento %>% mutate("SITUACAO_2017" = ifelse(is.na(SITUACAO_2017),
                                                                SITUACAO_2016,
                                                                SITUACAO_2017))


saveRDS(Coorte2016_Acompanhamento, "Coorte2016_Acompanhamento.RDS")

## Coorte 2017

Coorte2017_Acompanhamento <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2017) %>%
  filter(REGCHAVE %ni% REGISTROS_REPETIDOS) %>%
  select(CO_ALUNO, CO_CURSO, CO_IES, REGCHAVE)

Coorte2017_Acompanhamento_Registros <- as.numeric(Coorte2017_Acompanhamento$REGCHAVE)

## Agora, adicionaremos ao Coorte2017_Acompanhamento uma variável para cada ano de trajetória acadêmica.
## Ano 2017
Coorte2017_A2017 <-
  ALUNOS2017_1 %>%
  filter(NU_ANO_INGRESSO == 2017) %>%
  filter(REGCHAVE %in% Coorte2017_Acompanhamento_Registros)

Coorte2017_A2017 <-
  Coorte2017_A2017 %>% select(REGCHAVE, TP_SITUACAO)

Coorte2017_Acompanhamento <-
  left_join(Coorte2017_Acompanhamento, Coorte2017_A2017, by = "REGCHAVE")
Coorte2017_Acompanhamento <-
  rename(Coorte2017_Acompanhamento, "SITUACAO_2017" = TP_SITUACAO)
rm(Coorte2017_A2017)

saveRDS(Coorte2017_Acompanhamento, "Coorte2017_Acompanhamento.RDS")

#### 4.2.0 - Criando o TEC, TMC e TDC das categorias administrativas. ----
### 4.2.1.1 - Coorte 2010 ----
c10 <- readRDS("Coorte2010_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c10 <- left_join(c10, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c10 <- c10 %>% mutate("SITUACAO_2010" = case_when(
  SITUACAO_2010 == 2 ~ "Cursando",
  SITUACAO_2010 == 6 ~ "Formado",
  SITUACAO_2010 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2011" = case_when(
  SITUACAO_2011 == 2 ~ "Cursando",
  SITUACAO_2011 == 6 ~ "Formado",
  SITUACAO_2011 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2012" = case_when(
  SITUACAO_2012 == 2 ~ "Cursando",
  SITUACAO_2012 == 6 ~ "Formado",
  SITUACAO_2012 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2010
nrowfed <- c10 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Federal") %>% nrow()
nrowest <- c10 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Estadual") %>% nrow()
nrowmun <- c10 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Municipal") %>% nrow()
nrowpri <- c10 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Privada") %>% nrow()


c10_a2010 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2010)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
    )
  ) %>% 
  mutate("Ano" = 2010)

## Ano 2011
c10_a2011 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2011)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2011)

## Ano 2012
c10_a2012 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2012)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2012)

## Ano 2013
c10_a2013 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2013)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2013)

## Ano 2014
c10_a2014 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2014)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2014)

## Ano 2015
c10_a2015 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2015)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2015)

## Ano 2016
c10_a2016 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2016)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2016)

## Ano 2017
c10_a2017 <- as.data.frame(table(c10$TP_CATEGORIA_ADMINISTRATIVA, c10$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c10_1017 <- bind_rows(c10_a2010,
                      c10_a2011,
                      c10_a2012,
                      c10_a2013,
                      c10_a2014,
                      c10_a2015,
                      c10_a2016,
                      c10_a2017) %>% mutate("Coorte" = "Coorte 2010")

rm(c10_a2010)
rm(c10_a2011)
rm(c10_a2012)
rm(c10_a2013)
rm(c10_a2014)
rm(c10_a2015)
rm(c10_a2016)
rm(c10_a2017)

### 4.2.1.2 - Coorte 2011 ----
c11 <- readRDS("Coorte2011_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c11 <- left_join(c11, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c11 <- c11 %>% mutate("SITUACAO_2011" = case_when(
  SITUACAO_2011 == 2 ~ "Cursando",
  SITUACAO_2011 == 6 ~ "Formado",
  SITUACAO_2011 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2012" = case_when(
  SITUACAO_2012 == 2 ~ "Cursando",
  SITUACAO_2012 == 6 ~ "Formado",
  SITUACAO_2012 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2011
nrowfed <- c11 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Federal") %>% nrow()
nrowest <- c11 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Estadual") %>% nrow()
nrowmun <- c11 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Municipal") %>% nrow()
nrowpri <- c11 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Privada") %>% nrow()

c11_a2011 <- as.data.frame(table(c11$TP_CATEGORIA_ADMINISTRATIVA, c11$SITUACAO_2011)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2011)

## Ano 2012
c11_a2012 <- as.data.frame(table(c11$TP_CATEGORIA_ADMINISTRATIVA, c11$SITUACAO_2012)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2012)

## Ano 2013
c11_a2013 <- as.data.frame(table(c11$TP_CATEGORIA_ADMINISTRATIVA, c11$SITUACAO_2013)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2013)

## Ano 2014
c11_a2014 <- as.data.frame(table(c11$TP_CATEGORIA_ADMINISTRATIVA, c11$SITUACAO_2014)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2014)

## Ano 2015
c11_a2015 <- as.data.frame(table(c11$TP_CATEGORIA_ADMINISTRATIVA, c11$SITUACAO_2015)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2015)

## Ano 2016
c11_a2016 <- as.data.frame(table(c11$TP_CATEGORIA_ADMINISTRATIVA, c11$SITUACAO_2016)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2016)

## Ano 2017
c11_a2017 <- as.data.frame(table(c11$TP_CATEGORIA_ADMINISTRATIVA, c11$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c11_1017 <- bind_rows(c11_a2011,
                      c11_a2012,
                      c11_a2013,
                      c11_a2014,
                      c11_a2015,
                      c11_a2016,
                      c11_a2017) %>% mutate("Coorte" = "Coorte 2011")

rm(c11_a2011)
rm(c11_a2012)
rm(c11_a2013)
rm(c11_a2014)
rm(c11_a2015)
rm(c11_a2016)
rm(c11_a2017)
### 4.2.1.3 - Coorte 2012 ----
c12 <- readRDS("Coorte2012_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c12 <- left_join(c12, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c12 <- c12 %>% mutate("SITUACAO_2012" = case_when(
  SITUACAO_2012 == 2 ~ "Cursando",
  SITUACAO_2012 == 6 ~ "Formado",
  SITUACAO_2012 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2012
nrowfed <- c12 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Federal") %>% nrow()
nrowest <- c12 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Estadual") %>% nrow()
nrowmun <- c12 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Municipal") %>% nrow()
nrowpri <- c12 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Privada") %>% nrow()

c12_a2012 <- as.data.frame(table(c12$TP_CATEGORIA_ADMINISTRATIVA, c12$SITUACAO_2012)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2012)

## Ano 2013
c12_a2013 <- as.data.frame(table(c12$TP_CATEGORIA_ADMINISTRATIVA, c12$SITUACAO_2013)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2013)

## Ano 2014
c12_a2014 <- as.data.frame(table(c12$TP_CATEGORIA_ADMINISTRATIVA, c12$SITUACAO_2014)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2014)

## Ano 2015
c12_a2015 <- as.data.frame(table(c12$TP_CATEGORIA_ADMINISTRATIVA, c12$SITUACAO_2015)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2015)

## Ano 2016
c12_a2016 <- as.data.frame(table(c12$TP_CATEGORIA_ADMINISTRATIVA, c12$SITUACAO_2016)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2016)

## Ano 2017
c12_a2017 <- as.data.frame(table(c12$TP_CATEGORIA_ADMINISTRATIVA, c12$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c12_1017 <- bind_rows(c12_a2012,
                      c12_a2013,
                      c12_a2014,
                      c12_a2015,
                      c12_a2016,
                      c12_a2017) %>% mutate("Coorte" = "Coorte 2012")

rm(c12_a2012)
rm(c12_a2013)
rm(c12_a2014)
rm(c12_a2015)
rm(c12_a2016)
rm(c12_a2017)
### 4.2.1.4 - Coorte 2013 ----
c13 <- readRDS("Coorte2013_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c13 <- left_join(c13, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c13 <- c13 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2013
nrowfed <- c13 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Federal") %>% nrow()
nrowest <- c13 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Estadual") %>% nrow()
nrowmun <- c13 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Municipal") %>% nrow()
nrowpri <- c13 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Privada") %>% nrow()

c13_a2013 <- as.data.frame(table(c13$TP_CATEGORIA_ADMINISTRATIVA, c13$SITUACAO_2013)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2013)

## Ano 2014
c13_a2014 <- as.data.frame(table(c13$TP_CATEGORIA_ADMINISTRATIVA, c13$SITUACAO_2014)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2014)

## Ano 2015
c13_a2015 <- as.data.frame(table(c13$TP_CATEGORIA_ADMINISTRATIVA, c13$SITUACAO_2015)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2015)

## Ano 2016
c13_a2016 <- as.data.frame(table(c13$TP_CATEGORIA_ADMINISTRATIVA, c13$SITUACAO_2016)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2016)

## Ano 2017
c13_a2017 <- as.data.frame(table(c13$TP_CATEGORIA_ADMINISTRATIVA, c13$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c13_1017 <- bind_rows(c13_a2013,
                      c13_a2014,
                      c13_a2015,
                      c13_a2016,
                      c13_a2017) %>% mutate("Coorte" = "Coorte 2013")

rm(c13_a2013)
rm(c13_a2014)
rm(c13_a2015)
rm(c13_a2016)
rm(c13_a2017)
### 4.2.1.5 - Coorte 2014 ----
c14 <- readRDS("Coorte2014_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c14 <- left_join(c14, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c14 <- c14 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2014
nrowfed <- c14 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Federal") %>% nrow()
nrowest <- c14 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Estadual") %>% nrow()
nrowmun <- c14 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Municipal") %>% nrow()
nrowpri <- c14 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Privada") %>% nrow()

c14_a2014 <- as.data.frame(table(c14$TP_CATEGORIA_ADMINISTRATIVA, c14$SITUACAO_2014)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2014)

## Ano 2015
c14_a2015 <- as.data.frame(table(c14$TP_CATEGORIA_ADMINISTRATIVA, c14$SITUACAO_2015)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2015)

## Ano 2016
c14_a2016 <- as.data.frame(table(c14$TP_CATEGORIA_ADMINISTRATIVA, c14$SITUACAO_2016)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2016)

## Ano 2017
c14_a2017 <- as.data.frame(table(c14$TP_CATEGORIA_ADMINISTRATIVA, c14$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c14_1017 <- bind_rows(c14_a2014,
                      c14_a2015,
                      c14_a2016,
                      c14_a2017) %>% mutate("Coorte" = "Coorte 2014")

rm(c14_a2014)
rm(c14_a2015)
rm(c14_a2016)
rm(c14_a2017)
### 4.2.1.6 - Coorte 2015 ----
c15 <- readRDS("Coorte2015_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c15 <- left_join(c15, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c15 <- c15 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c15 <- c15 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c15 <- c15 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c15 <- c15 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2015
nrowfed <- c15 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Federal") %>% nrow()
nrowest <- c15 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Estadual") %>% nrow()
nrowmun <- c15 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Municipal") %>% nrow()
nrowpri <- c15 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Privada") %>% nrow()

c15_a2015 <- as.data.frame(table(c15$TP_CATEGORIA_ADMINISTRATIVA, c15$SITUACAO_2015)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2015)

## Ano 2016
c15_a2016 <- as.data.frame(table(c15$TP_CATEGORIA_ADMINISTRATIVA, c15$SITUACAO_2016)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2016)

## Ano 2017
c15_a2017 <- as.data.frame(table(c15$TP_CATEGORIA_ADMINISTRATIVA, c15$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c15_1017 <- bind_rows(c15_a2015,
                      c15_a2016,
                      c15_a2017) %>% mutate("Coorte" = "Coorte 2015")

rm(c15_a2015)
rm(c15_a2016)
rm(c15_a2017)
### 4.2.1.7 - Coorte 2016 ----
c16 <- readRDS("Coorte2016_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c16 <- left_join(c16, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c16 <- c16 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c16 <- c16 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c16 <- c16 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2016
nrowfed <- c16 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Federal") %>% nrow()
nrowest <- c16 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Estadual") %>% nrow()
nrowmun <- c16 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Pública Municipal") %>% nrow()
nrowpri <- c16 %>% filter(TP_CATEGORIA_ADMINISTRATIVA == "Privada") %>% nrow()

c16_a2016 <- as.data.frame(table(c16$TP_CATEGORIA_ADMINISTRATIVA, c16$SITUACAO_2016)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2016)

## Ano 2017
c16_a2017 <- as.data.frame(table(c16$TP_CATEGORIA_ADMINISTRATIVA, c16$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c16_1017 <- bind_rows(c16_a2016,
                      c16_a2017) %>% mutate("Coorte" = "Coorte 2016")

rm(c16_a2016)
rm(c16_a2017)
### 4.2.1.8 - Coorte 2017 ----
c17 <- readRDS("Coorte2017_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c17 <- left_join(c17, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c17 <- c17 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c17 <- c17 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2017
c17_a2017 <- as.data.frame(table(c17$TP_CATEGORIA_ADMINISTRATIVA, c17$SITUACAO_2017)) %>%
  rename("CO_CATEGORIA_ADMINISTRATIVA" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Freq_Percent" = case_when(
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Federal" ~ round(Freq/nrowfed*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Estadual" ~ round(Freq/nrowest*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Pública Municipal" ~ round(Freq/nrowmun*100, digits = 2),
    CO_CATEGORIA_ADMINISTRATIVA == "Privada" ~ round(Freq/nrowpri*100, digits = 2)
  )
  ) %>% 
  mutate("Ano" = 2017)

## Agregando os anos.
c17_1017 <- c17_a2017 %>% mutate("Coorte" = "Coorte 2017")

rm(c17_a2017)
### 4.2.1.9 - Agregando as coortes. ----
coortes_1017 <- bind_rows(
  c10_1017,
  c11_1017,
  c12_1017,
  c13_1017,
  c14_1017,
  c15_1017,
  c16_1017,
  c17_1017
)

#### 4.3.0 - Criando o TEC, TMC e TDC das IES. ----
### 4.3.1.1 - Coorte 2010 ----
c10 <- readRDS("Coorte2010_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c10 <- left_join(c10, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c10 <- c10 %>% mutate("SITUACAO_2010" = case_when(
  SITUACAO_2010 == 2 ~ "Cursando",
  SITUACAO_2010 == 6 ~ "Formado",
  SITUACAO_2010 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2011" = case_when(
  SITUACAO_2011 == 2 ~ "Cursando",
  SITUACAO_2011 == 6 ~ "Formado",
  SITUACAO_2011 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2012" = case_when(
  SITUACAO_2012 == 2 ~ "Cursando",
  SITUACAO_2012 == 6 ~ "Formado",
  SITUACAO_2012 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c10 <- c10 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2010
c10_a2010 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2010)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2010)

c10_a2010_compl <- c10_a2010 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2010 <- left_join(c10_a2010, c10_a2010_compl, by = "CO_IES", all.x = TRUE)
c10_a2010 <- c10_a2010 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2011
c10_a2011 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2011)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2011)

c10_a2011_compl <- c10_a2011 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2011 <- left_join(c10_a2011, c10_a2011_compl, by = "CO_IES", all.x = TRUE)

c10_a2011 <- c10_a2011 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2012
c10_a2012 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2012)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2012)

c10_a2012_compl <- c10_a2012 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2012 <- left_join(c10_a2012, c10_a2012_compl, by = "CO_IES", all.x = TRUE)

c10_a2012 <- c10_a2012 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2013
c10_a2013 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2013)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2013)

c10_a2013_compl <- c10_a2013 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2013 <- left_join(c10_a2013, c10_a2013_compl, by = "CO_IES", all.x = TRUE)

c10_a2013 <- c10_a2013 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2014
c10_a2014 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2014)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2014)

c10_a2014_compl <- c10_a2014 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2014 <- left_join(c10_a2014, c10_a2014_compl, by = "CO_IES", all.x = TRUE)

c10_a2014 <- c10_a2014 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2015
c10_a2015 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2015)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2015)

c10_a2015_compl <- c10_a2015 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2015 <- left_join(c10_a2015, c10_a2015_compl, by = "CO_IES", all.x = TRUE)

c10_a2015 <- c10_a2015 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2016
c10_a2016 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2016)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2016)

c10_a2016_compl <- c10_a2016 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2016 <- left_join(c10_a2016, c10_a2016_compl, by = "CO_IES", all.x = TRUE)

c10_a2016 <- c10_a2016 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2017
c10_a2017 <- as.data.frame(table(c10$CO_IES, c10$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c10_a2017_compl <- c10_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c10_a2017 <- left_join(c10_a2017, c10_a2017_compl, by = "CO_IES", all.x = TRUE)

c10_a2017 <- c10_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c10_1017 <- bind_rows(c10_a2010,
                      c10_a2011,
                      c10_a2012,
                      c10_a2013,
                      c10_a2014,
                      c10_a2015,
                      c10_a2016,
                      c10_a2017) %>% mutate("Coorte" = "Coorte 2010")

rm(c10_a2010)
rm(c10_a2011)
rm(c10_a2012)
rm(c10_a2013)
rm(c10_a2014)
rm(c10_a2015)
rm(c10_a2016)
rm(c10_a2017)
rm(c10_a2010_compl)
rm(c10_a2011_compl)
rm(c10_a2012_compl)
rm(c10_a2013_compl)
rm(c10_a2014_compl)
rm(c10_a2015_compl)
rm(c10_a2016_compl)
rm(c10_a2017_compl)


### 4.3.1.2 - Coorte 2011 ----
c11 <- readRDS("Coorte2011_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c11 <- left_join(c11, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c11 <- c11 %>% mutate("SITUACAO_2011" = case_when(
  SITUACAO_2011 == 2 ~ "Cursando",
  SITUACAO_2011 == 6 ~ "Formado",
  SITUACAO_2011 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2012" = case_when(
  SITUACAO_2012 == 2 ~ "Cursando",
  SITUACAO_2012 == 6 ~ "Formado",
  SITUACAO_2012 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c11 <- c11 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2011
c11_a2011 <- as.data.frame(table(c11$CO_IES, c11$SITUACAO_2011)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2011)

c11_a2011_compl <- c11_a2011 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c11_a2011 <- left_join(c11_a2011, c11_a2011_compl, by = "CO_IES", all.x = TRUE)

c11_a2011 <- c11_a2011 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2012
c11_a2012 <- as.data.frame(table(c11$CO_IES, c11$SITUACAO_2012)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2012)

c11_a2012_compl <- c11_a2012 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c11_a2012 <- left_join(c11_a2012, c11_a2012_compl, by = "CO_IES", all.x = TRUE)

c11_a2012 <- c11_a2012 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2013
c11_a2013 <- as.data.frame(table(c11$CO_IES, c11$SITUACAO_2013)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2013)

c11_a2013_compl <- c11_a2013 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c11_a2013 <- left_join(c11_a2013, c11_a2013_compl, by = "CO_IES", all.x = TRUE)

c11_a2013 <- c11_a2013 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2014
c11_a2014 <- as.data.frame(table(c11$CO_IES, c11$SITUACAO_2014)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2014)

c11_a2014_compl <- c11_a2014 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c11_a2014 <- left_join(c11_a2014, c11_a2014_compl, by = "CO_IES", all.x = TRUE)

c11_a2014 <- c11_a2014 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2015
c11_a2015 <- as.data.frame(table(c11$CO_IES, c11$SITUACAO_2015)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2015)

c11_a2015_compl <- c11_a2015 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c11_a2015 <- left_join(c11_a2015, c11_a2015_compl, by = "CO_IES", all.x = TRUE)

c11_a2015 <- c11_a2015 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2016
c11_a2016 <- as.data.frame(table(c11$CO_IES, c11$SITUACAO_2016)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2016)

c11_a2016_compl <- c11_a2016 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c11_a2016 <- left_join(c11_a2016, c11_a2016_compl, by = "CO_IES", all.x = TRUE)

c11_a2016 <- c11_a2016 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2017
c11_a2017 <- as.data.frame(table(c11$CO_IES, c11$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c11_a2017_compl <- c11_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c11_a2017 <- left_join(c11_a2017, c11_a2017_compl, by = "CO_IES", all.x = TRUE)

c11_a2017 <- c11_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c11_1017 <- bind_rows(c11_a2011,
                      c11_a2012,
                      c11_a2013,
                      c11_a2014,
                      c11_a2015,
                      c11_a2016,
                      c11_a2017) %>% mutate("Coorte" = "Coorte 2011")


rm(c11_a2011)
rm(c11_a2012)
rm(c11_a2013)
rm(c11_a2014)
rm(c11_a2015)
rm(c11_a2016)
rm(c11_a2017)
rm(c11_a2011_compl)
rm(c11_a2012_compl)
rm(c11_a2013_compl)
rm(c11_a2014_compl)
rm(c11_a2015_compl)
rm(c11_a2016_compl)
rm(c11_a2017_compl)
### 4.3.1.3 - Coorte 2012 ----
c12 <- readRDS("Coorte2012_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c12 <- left_join(c12, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c12 <- c12 %>% mutate("SITUACAO_2012" = case_when(
  SITUACAO_2012 == 2 ~ "Cursando",
  SITUACAO_2012 == 6 ~ "Formado",
  SITUACAO_2012 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c12 <- c12 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2012
c12_a2012 <- as.data.frame(table(c12$CO_IES, c12$SITUACAO_2012)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2012)

c12_a2012_compl <- c12_a2012 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c12_a2012 <- left_join(c12_a2012, c12_a2012_compl, by = "CO_IES", all.x = TRUE)

c12_a2012 <- c12_a2012 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2013
c12_a2013 <- as.data.frame(table(c12$CO_IES, c12$SITUACAO_2013)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2013)

c12_a2013_compl <- c12_a2013 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c12_a2013 <- left_join(c12_a2013, c12_a2013_compl, by = "CO_IES", all.x = TRUE)

c12_a2013 <- c12_a2013 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2014
c12_a2014 <- as.data.frame(table(c12$CO_IES, c12$SITUACAO_2014)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2014)

c12_a2014_compl <- c12_a2014 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c12_a2014 <- left_join(c12_a2014, c12_a2014_compl, by = "CO_IES", all.x = TRUE)

c12_a2014 <- c12_a2014 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2015
c12_a2015 <- as.data.frame(table(c12$CO_IES, c12$SITUACAO_2015)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2015)

c12_a2015_compl <- c12_a2015 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c12_a2015 <- left_join(c12_a2015, c12_a2015_compl, by = "CO_IES", all.x = TRUE)

c12_a2015 <- c12_a2015 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2016
c12_a2016 <- as.data.frame(table(c12$CO_IES, c12$SITUACAO_2016)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2016)

c12_a2016_compl <- c12_a2016 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c12_a2016 <- left_join(c12_a2016, c12_a2016_compl, by = "CO_IES", all.x = TRUE)

c12_a2016 <- c12_a2016 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2017
c12_a2017 <- as.data.frame(table(c12$CO_IES, c12$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c12_a2017_compl <- c12_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c12_a2017 <- left_join(c12_a2017, c12_a2017_compl, by = "CO_IES", all.x = TRUE)

c12_a2017 <- c12_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c12_1017 <- bind_rows(c12_a2012,
                      c12_a2013,
                      c12_a2014,
                      c12_a2015,
                      c12_a2016,
                      c12_a2017) %>% mutate("Coorte" = "Coorte 2012")


rm(c12_a2012)
rm(c12_a2013)
rm(c12_a2014)
rm(c12_a2015)
rm(c12_a2016)
rm(c12_a2017)
rm(c12_a2012_compl)
rm(c12_a2013_compl)
rm(c12_a2014_compl)
rm(c12_a2015_compl)
rm(c12_a2016_compl)
rm(c12_a2017_compl)
### 4.3.1.4 - Coorte 2013 ----
c13 <- readRDS("Coorte2013_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c13 <- left_join(c13, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c13 <- c13 %>% mutate("SITUACAO_2013" = case_when(
  SITUACAO_2013 == 2 ~ "Cursando",
  SITUACAO_2013 == 6 ~ "Formado",
  SITUACAO_2013 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c13 <- c13 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2013
c13_a2013 <- as.data.frame(table(c13$CO_IES, c13$SITUACAO_2013)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2013)

c13_a2013_compl <- c13_a2013 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c13_a2013 <- left_join(c13_a2013, c13_a2013_compl, by = "CO_IES", all.x = TRUE)

c13_a2013 <- c13_a2013 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2014
c13_a2014 <- as.data.frame(table(c13$CO_IES, c13$SITUACAO_2014)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2014)

c13_a2014_compl <- c13_a2014 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c13_a2014 <- left_join(c13_a2014, c13_a2014_compl, by = "CO_IES", all.x = TRUE)

c13_a2014 <- c13_a2014 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2015
c13_a2015 <- as.data.frame(table(c13$CO_IES, c13$SITUACAO_2015)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2015)

c13_a2015_compl <- c13_a2015 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c13_a2015 <- left_join(c13_a2015, c13_a2015_compl, by = "CO_IES", all.x = TRUE)

c13_a2015 <- c13_a2015 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2016
c13_a2016 <- as.data.frame(table(c13$CO_IES, c13$SITUACAO_2016)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2016)

c13_a2016_compl <- c13_a2016 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c13_a2016 <- left_join(c13_a2016, c13_a2016_compl, by = "CO_IES", all.x = TRUE)

c13_a2016 <- c13_a2016 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2017
c13_a2017 <- as.data.frame(table(c13$CO_IES, c13$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c13_a2017_compl <- c13_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c13_a2017 <- left_join(c13_a2017, c13_a2017_compl, by = "CO_IES", all.x = TRUE)

c13_a2017 <- c13_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c13_1017 <- bind_rows(c13_a2013,
                      c13_a2014,
                      c13_a2015,
                      c13_a2016,
                      c13_a2017) %>% mutate("Coorte" = "Coorte 2013")


rm(c13_a2013)
rm(c13_a2014)
rm(c13_a2015)
rm(c13_a2016)
rm(c13_a2017)
rm(c13_a2013_compl)
rm(c13_a2014_compl)
rm(c13_a2015_compl)
rm(c13_a2016_compl)
rm(c13_a2017_compl)
### 4.3.1.5 - Coorte 2014 ----
c14 <- readRDS("Coorte2014_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c14 <- left_join(c14, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c14 <- c14 %>% mutate("SITUACAO_2014" = case_when(
  SITUACAO_2014 == 2 ~ "Cursando",
  SITUACAO_2014 == 6 ~ "Formado",
  SITUACAO_2014 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c14 <- c14 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2014
c14_a2014 <- as.data.frame(table(c14$CO_IES, c14$SITUACAO_2014)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2014)

c14_a2014_compl <- c14_a2014 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c14_a2014 <- left_join(c14_a2014, c14_a2014_compl, by = "CO_IES", all.x = TRUE)

c14_a2014 <- c14_a2014 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2015
c14_a2015 <- as.data.frame(table(c14$CO_IES, c14$SITUACAO_2015)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2015)

c14_a2015_compl <- c14_a2015 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c14_a2015 <- left_join(c14_a2015, c14_a2015_compl, by = "CO_IES", all.x = TRUE)

c14_a2015 <- c14_a2015 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2016
c14_a2016 <- as.data.frame(table(c14$CO_IES, c14$SITUACAO_2016)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2016)

c14_a2016_compl <- c14_a2016 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c14_a2016 <- left_join(c14_a2016, c14_a2016_compl, by = "CO_IES", all.x = TRUE)

c14_a2016 <- c14_a2016 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2017
c14_a2017 <- as.data.frame(table(c14$CO_IES, c14$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c14_a2017_compl <- c14_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c14_a2017 <- left_join(c14_a2017, c14_a2017_compl, by = "CO_IES", all.x = TRUE)

c14_a2017 <- c14_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c14_1017 <- bind_rows(c14_a2014,
                      c14_a2015,
                      c14_a2016,
                      c14_a2017) %>% mutate("Coorte" = "Coorte 2014")


rm(c14_a2014)
rm(c14_a2015)
rm(c14_a2016)
rm(c14_a2017)
rm(c14_a2014_compl)
rm(c14_a2015_compl)
rm(c14_a2016_compl)
rm(c14_a2017_compl)
### 4.3.1.6 - Coorte 2015 ----
c15 <- readRDS("Coorte2015_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c15 <- left_join(c15, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c15 <- c15 %>% mutate("SITUACAO_2015" = case_when(
  SITUACAO_2015 == 2 ~ "Cursando",
  SITUACAO_2015 == 6 ~ "Formado",
  SITUACAO_2015 %ni% c(2,6) ~ "Evadido"
))

c15 <- c15 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c15 <- c15 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c15 <- c15 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2015
c15_a2015 <- as.data.frame(table(c15$CO_IES, c15$SITUACAO_2015)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2015)

c15_a2015_compl <- c15_a2015 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c15_a2015 <- left_join(c15_a2015, c15_a2015_compl, by = "CO_IES", all.x = TRUE)

c15_a2015 <- c15_a2015 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2016
c15_a2016 <- as.data.frame(table(c15$CO_IES, c15$SITUACAO_2016)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2016)

c15_a2016_compl <- c15_a2016 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c15_a2016 <- left_join(c15_a2016, c15_a2016_compl, by = "CO_IES", all.x = TRUE)

c15_a2016 <- c15_a2016 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2017
c15_a2017 <- as.data.frame(table(c15$CO_IES, c15$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c15_a2017_compl <- c15_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c15_a2017 <- left_join(c15_a2017, c15_a2017_compl, by = "CO_IES", all.x = TRUE)

c15_a2017 <- c15_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c15_1017 <- bind_rows(c15_a2015,
                      c15_a2016,
                      c15_a2017) %>% mutate("Coorte" = "Coorte 2015")


rm(c15_a2015)
rm(c15_a2016)
rm(c15_a2017)
rm(c15_a2015_compl)
rm(c15_a2016_compl)
rm(c15_a2017_compl)
### 4.3.1.7 - Coorte 2016 ----
c16 <- readRDS("Coorte2016_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c16 <- left_join(c16, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c16 <- c16 %>% mutate("SITUACAO_2016" = case_when(
  SITUACAO_2016 == 2 ~ "Cursando",
  SITUACAO_2016 == 6 ~ "Formado",
  SITUACAO_2016 %ni% c(2,6) ~ "Evadido"
))

c16 <- c16 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c16 <- c16 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2016
c16_a2016 <- as.data.frame(table(c16$CO_IES, c16$SITUACAO_2016)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2016)

c16_a2016_compl <- c16_a2016 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c16_a2016 <- left_join(c16_a2016, c16_a2016_compl, by = "CO_IES", all.x = TRUE)

c16_a2016 <- c16_a2016 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Ano 2017
c16_a2017 <- as.data.frame(table(c16$CO_IES, c16$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c16_a2017_compl <- c16_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c16_a2017 <- left_join(c16_a2017, c16_a2017_compl, by = "CO_IES", all.x = TRUE)

c16_a2017 <- c16_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c16_1017 <- bind_rows(c16_a2016,
                      c16_a2017) %>% mutate("Coorte" = "Coorte 2016")


rm(c16_a2016)
rm(c16_a2017)
rm(c16_a2016_compl)
rm(c16_a2017_compl)
### 4.3.1.8 - Coorte 2017 ----
c17 <- readRDS("Coorte2017_Acompanhamento.RDS")
ies_compl <- read.csv("DM_IES.csv", sep = "|") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA)
c17 <- left_join(c17, ies_compl, by = "CO_IES") %>% select(CO_IES, NO_IES, SG_IES, TP_CATEGORIA_ADMINISTRATIVA, everything())
## Agregando os evadidos.
c17 <- c17 %>% mutate("SITUACAO_2017" = case_when(
  SITUACAO_2017 == 2 ~ "Cursando",
  SITUACAO_2017 == 6 ~ "Formado",
  SITUACAO_2017 %ni% c(2,6) ~ "Evadido"
))

c17 <- c17 %>% mutate("TP_CATEGORIA_ADMINISTRATIVA" = case_when(
  TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  TP_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

## Realizando os cálculos.
## Ano 2017
c17_a2017 <- as.data.frame(table(c17$CO_IES, c17$SITUACAO_2017)) %>%
  rename("CO_IES" = "Var1", "Situaçao_Vínculo" = "Var2") %>% 
  mutate("Ano" = 2017)

c17_a2017_compl <- c17_a2017 %>% group_by(CO_IES) %>% 
  summarise("N_TOTAL" = sum(Freq))

c17_a2017 <- left_join(c17_a2017, c17_a2017_compl, by = "CO_IES", all.x = TRUE)

c17_a2017 <- c17_a2017 %>% 
  mutate("Freq_Percent" = round(Freq/N_TOTAL*100, digits = 2))

## Agregando os anos.
c17_1017 <- c17_a2017 %>% mutate("Coorte" = "Coorte 2017")


rm(c17_a2017)
rm(c17_a2017_compl)
### 4.3.1.9 - Agregando as coortes. ----
coortes_1017_ies <- bind_rows(
  c10_1017,
  c11_1017,
  c12_1017,
  c13_1017,
  c14_1017,
  c15_1017,
  c16_1017,
  c17_1017
)

IES <- read.csv("DM_IES.csv", sep = "|")
IES <- IES %>% filter(CO_IES %in% ALUNOSFINAL_IEANEA$CO_IES)

complemento_ies <- data.frame(
  "CO_IES" = IES$CO_IES,
  "NO_IES" = IES$NO_IES,
  "SG_IES" = IES$SG_IES,
  "CO_CATEGORIA_ADMINISTRATIVA" = IES$TP_CATEGORIA_ADMINISTRATIVA,
)

coortes_1017_ies$CO_IES <- as.numeric(coortes_1017_ies$CO_IES)

coortes_1017_ies <-
  left_join(coortes_1017_ies, complemento_ies, by = "CO_IES", all.x = TRUE)
coortes_1017_ies <-
  coortes_1017_ies %>% select(CO_CATEGORIA_ADMINISTRATIVA,
                                CO_IES,
                                NO_IES,
                                SG_IES,
                                everything())

## Mantendo apenas das IES que aparecem todos os anos.
ies_todos_anos <- as.data.frame(table(coortes_1017_ies$CO_IES)) %>% 
  filter(Freq == 108) %>% 
  select(Var1)
ies_todos_anos <- as.numeric(ies_todos_anos$Var1)

coortes_1017_ies <- coortes_1017_ies %>% 
  filter(CO_IES %in% ies_todos_anos)

## Ajustando nomes das categorias. 
coortes_1017_ies <- coortes_1017_ies %>% mutate("CO_CATEGORIA_ADMINISTRATIVA" = case_when(
  CO_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
  CO_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
  CO_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal",
  CO_CATEGORIA_ADMINISTRATIVA %ni% c(1,2,3) ~ "Privada"
))

saveRDS(coortes_1017_ies, "coortes_1017_ies.RDS")
