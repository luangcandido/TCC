#### 06 _ Outros gráficos


#### PACOTES E SETWD() ----

setwd("~/")

"%ni%" <- Negate("%in%")

library(ggplot2)
library(ggthemes)
library(ggrepel)
library(geobr)
library(ggsflabel)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)

####



#### FONTES ----
# Importando fontes previamente instaladas para usar nos gráficos.
font_import(pattern = "[M/m]ontserrat")
font_import(pattern = "[T/t]imes")
loadfonts(device = "win")
#### TEMA ----
# Definindo tema dos gráficos.
theme_tcc <- theme(
  text = element_text(family = "Montserrat"),
  legend.position = "top",
  legend.justification = "center",
  legend.key.width = unit(1.5, "lines"),
  legend.key.height = unit(0.5, "lines"),
  plot.title = element_text(face = "bold", size = 12),
  plot.subtitle = element_text(face = "italic", size = 9),
  legend.text = element_text(colour = "gray50", size = 8),
  legend.title = element_text(colour = "gray50", size = 9),
  strip.text.x = element_blank(),
  axis.text.x = element_text(colour = "gray50", size = 8),
  axis.title.x = element_blank(),
  axis.text.y = element_text(colour = "gray50", size = 8),
  axis.title.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)

theme_abnt <-   theme(
  text = element_text(family = "Times New Roman"),
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  plot.caption = element_blank(),
  legend.text = element_text(colour = "gray50", size = 10),
  legend.title = element_text(colour = "gray30", size = 11),
  axis.text.x = element_text(colour = "gray50", size = 9),
  axis.title.x = element_text(
    colour = "gray30",
    size = 11,
    margin = margin(7, 0, 0, 0)
  ),
  axis.text.y = element_text(colour = "gray50", size = 9),
  axis.title.y = element_text(
    colour = "gray30",
    size = 11,
    angle = 90,
    margin = margin(0, 7, 0, 0)
  )
)
####




#### PLOT 1 ----
## Dados extraídos de: http://s.glbimg.com/jo/g1/f/original/2011/02/04/evasao1.jpg.
plot1 <- data.frame(
  ANO = c(2000,
            2000,
            2000,
            2001,
            2001,
            2001,
            2002,
            2002,
            2002,
            2003,
            2003,
            2003,
            2004,
            2004,
            2004,
            2005,
            2005,
            2005,
            2006,
            2006,
            2006,
            2007,
            2007,
            2007,
            2008,
            2008,
            2008,
            2009,
            2009,
            2009),
  "CO_CATEGORIA_ADMINISTRATIVA" = c("Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada",
              "Pública",
              "Brasil",
              "Privada"),
  "IEA" = c(0.13, 0.189, 0.221,
            0.138,0.221,0.261,
            0.093,0.215,0.268,
            0.095,0.221,0.275,
            0.152,0.243,0.28,
            0.118,0.215,0.253,
            0.124,0.217,0.251,
            0.118,0.22,0.256,
            0.12, 0.222,0.257,
            0.105,0.209,0.245
            )
)

plot1$CO_CATEGORIA_ADMINISTRATIVA <- factor(plot1$CO_CATEGORIA_ADMINISTRATIVA,
                                            levels = 
                                              c("Brasil",
                                                "Privada",
                                                "Pública"))

# Ajustes
hline <- plot1 %>%
  group_by(CO_CATEGORIA_ADMINISTRATIVA) %>%
  summarise(MEAN = mean(IEA))

# Plot
ggplot(plot1, aes(x = ANO, y = IEA)) +
  geom_line(aes(colour = CO_CATEGORIA_ADMINISTRATIVA),
            size = 1.8,
            alpha = 0.6) +
  geom_hline(
    data = hline,
    aes(yintercept = MEAN),
    linetype = 2,
    size = 0.5,
    alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(2000, 2009, by = 3),
                     labels = c("2000",
                                "2003",
                                "2006",
                                "2009"),
                     limits = c(2000, 2009)) +
  scale_y_continuous(
    breaks = seq(0, 0.3, by = 0.05),
    labels = c("0", "5%", "10%", "15%", "20%", "25%", "30%"),
    limits = c(0,0.3)
  ) +
  scale_colour_manual(values = c("#0F0D0E", "#9986A5",
                                 "#D9D0D3")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    shape = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Série histórica entre 2000 e 2009",
    y = "Índice de evasão anual média",
    title = "Índice de Evasão Anual nas diferentes categorias\nadministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2018. Linha tracejada\nrepresenta a média no período.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  theme_tcc +
  theme_abnt

####






#### PLOT 2 ----
## Dados extraídos de: http://www.institutolobo.org.br/imagens/pdf/artigos/art_079.pdf
plot2 <- data.frame(
  ANO = c(2002,
          2002,
          2003,
          2003,
          2004,
          2004,
          2005,
          2005,
          2006,
          2006,
          2007,
          2007,
          2008,
          2008,
          2009,
          2009,
          2010,
          2010),
  "METODO" = c("Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09",
                                    "Ingressantes antes do Censo-09",
                                    "Ingressantes a partir do Censo-09"),
  "IEA" = c(0.2148, 0.1772,
            0.2213, 0.1725,
            0.2431, 0.1956,
            0.2154, 0.1809,
            0.2173, 0.1824,
            0.2204, 0.1816,
            0.2225, 0.1829,
            0.2060, 0.2060,
            0.1511, 0.1511
  )
)

plot2$METODO <- factor(plot2$METODO,
                                            levels = 
                                              c("Ingressantes antes do Censo-09",
                                                "Ingressantes a partir do Censo-09"))

# Ajustes
hline <- plot2 %>%
  group_by(METODO) %>%
  filter(ANO %in% 2002:2008) %>% 
  summarise(MEAN = mean(IEA))

# Plot
ggplot(plot2, aes(x = ANO, y = IEA)) +
  geom_line(aes(colour = METODO),
            size = 1.8,
            alpha = 0.6) +
  geom_hline(
    data = hline,
    aes(yintercept = MEAN),
    linetype = 2,
    size = 0.5,
    alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(2002, 2008, by = 2),
                     labels = c("2002",
                                "2004",
                                "2006",
                                "2008"),
                     limits = c(2002, 2008)) +
  scale_y_continuous(
    breaks = seq(0, 0.3, by = 0.1),
    labels = c("0", "10%", "20%", "30%"),
    limits = c(0,0.3)
  ) +
  scale_colour_manual(values = c("gray30", "gray70")) +
  guides(
    colour = guide_legend(
      title = "Forma de cálculo",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Série histórica entre 2002 e 2008",
    y = "Índice de Evasão Anual",
    title = "Índice de Evasão Anual nas diferentes categorias\nadministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2018. Linha tracejada\nrepresenta a média no período.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  theme_tcc +
  theme_abnt

####






#### PLOT 3 ----
## Dados extraídos de: http://www.scielo.br/pdf/cp/v37n132/a0737132.
plot3 <- data.frame(
  ANO = c(2000,
          2000,
          2000,
          2001,
          2001,
          2001,
          2002,
          2002,
          2002,
          2003,
          2003,
          2003,
          2004,
          2004,
          2004,
          2005,
          2005,
          2005),
  "CO_CATEGORIA_ADMINISTRATIVA" = c("Pública Federal",
                                    "Pública Estadual",
                                    "Pública Municipal",
                                    "Pública Federal",
                                    "Pública Estadual",
                                    "Pública Municipal",
                                    "Pública Federal",
                                    "Pública Estadual",
                                    "Pública Municipal",
                                    "Pública Federal",
                                    "Pública Estadual",
                                    "Pública Municipal",
                                    "Pública Federal",
                                    "Pública Estadual",
                                    "Pública Municipal",
                                    "Pública Federal",
                                    "Pública Estadual",
                                    "Pública Municipal"),
  "IEA" = c(0.09,0.11,0.40,
            0.14,0.12,0.18,
            0.11,0.09,-0.02,
            0.09,0.10,0.06,
            0.14,0.15,0.19,
            0.10,0.11,0.20
  )
)

plot3$CO_CATEGORIA_ADMINISTRATIVA <- factor(plot3$CO_CATEGORIA_ADMINISTRATIVA,
                                            levels = 
                                              c("Pública Federal",
                                                "Pública Estadual",
                                                "Pública Municipal"))

# Ajustes
hline <- plot3 %>%
  group_by(CO_CATEGORIA_ADMINISTRATIVA) %>%
  summarise(MEAN = mean(IEA))

# Plot
ggplot(plot3, aes(x = ANO, y = IEA)) +
  geom_line(aes(colour = CO_CATEGORIA_ADMINISTRATIVA),
            size = 1.8,
            alpha = 0.6) +
  geom_hline(
    data = hline,
    aes(yintercept = MEAN),
    linetype = 2,
    size = 0.5,
    alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(2000, 2005, by = 1),
                     labels = c("00",
                                "01",
                                "02",
                                "03",
                                "04",
                                "05")) +
  scale_y_continuous(
    breaks = seq(0, 0.4, by = 0.1),
    labels = c("0", "10%", "20%", "30%", "40%")
  ) +
  scale_colour_manual(values = c("#79402E",
                                 "#CCBA72", "#1a9850")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    shape = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Série histórica entre 2000 e 2005",
    y = "Índice de evasão anual média",
    title = "Índice de Evasão Anual nas diferentes categorias\nadministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2018. Linha tracejada\nrepresenta a média no período.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  facet_wrap(. ~ CO_CATEGORIA_ADMINISTRATIVA, nrow = 1) +
  theme_tcc +
  theme_abnt

####








#### PLOT 4 -----
## Dados extraídos de: http://www.dominiopublico.gov.br/download/texto/me002240.pdf
dados96 <- readxl::read_excel("Dados 1996.xlsx")
dados96[is.na(dados96)] <- 0

## fazendo o cálculo do resultado agregados das Federais.
dados96_agregado <- dados96 %>% 
  summarise("IES" = "Federais",
            "N_TOTAL" = sum(N_TOTAL),
            "N_DIPLOMADOS" = sum(N_DIPLOMADOS),
            "N_RETIDOS" = sum(N_RETIDOS),
            "N_EVADIDOS" = sum(N_EVADIDOS)) %>% 
  mutate("P_DIPLOMADOS" = round(N_DIPLOMADOS/N_TOTAL, digits = 4)*100,
         "P_RETIDOS" =    round(N_RETIDOS/N_TOTAL, digits = 4)*100,
         "P_EVADIDOS" =   round(N_EVADIDOS/N_TOTAL, digits = 4)*100) %>% 
  select(IES, P_DIPLOMADOS, P_RETIDOS, P_EVADIDOS) %>% 
  pivot_longer(-IES, names_to = "Situação_Vínculo", values_to = "Freq") %>% 
  mutate("Situação_Vínculo" = 
           case_when(
             Situação_Vínculo == "P_DIPLOMADOS" ~ "Formados",
             Situação_Vínculo == "P_RETIDOS" ~ "Cursandos",
             Situação_Vínculo == "P_EVADIDOS" ~ "Evadidos"
           ))

## fazendo o cálculo do resultado agregados das Federais.
dados96_agregado_ies <- dados96 %>% 
  group_by(IES) %>% 
  summarise("N_TOTAL" = sum(N_TOTAL),
            "N_DIPLOMADOS" = sum(N_DIPLOMADOS),
            "N_RETIDOS" = sum(N_RETIDOS),
            "N_EVADIDOS" = sum(N_EVADIDOS)) %>% 
  mutate("P_DIPLOMADOS" = round(N_DIPLOMADOS/N_TOTAL, digits = 4)*100,
         "P_RETIDOS" =    round(N_RETIDOS/N_TOTAL, digits = 4)*100,
         "P_EVADIDOS" =   round(N_EVADIDOS/N_TOTAL, digits = 4)*100)

dados96_agregado_ies <- dados96_agregado_ies %>% 
  select(IES, P_DIPLOMADOS, P_RETIDOS, P_EVADIDOS) %>% 
  pivot_longer(-IES, names_to = "Situação_Vínculo", values_to = "Freq") %>% 
  mutate("Situação_Vínculo" = 
           case_when(
             Situação_Vínculo == "P_DIPLOMADOS" ~ "Formados",
             Situação_Vínculo == "P_RETIDOS" ~ "Cursandos",
             Situação_Vínculo == "P_EVADIDOS" ~ "Evadidos"
           ))

## Plot
ggplot(dados96_agregado_ies, aes(x = Situação_Vínculo, y = Freq)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#79402E",
               alpha = 0.85) +
  geom_point(data = dados96_agregado,
             aes(x = Situação_Vínculo, y = Freq),
             shape = 18,
             colour = "white",
             size = 3,
             alpha = 0.9) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     labels = c("0", "20%", "40%", "60%", "80%", "100%"),
                     limits = c(0,100)) +
  labs(
    x = "Situação da matrícula (losango representa o resultado agregado das IES federais)",
    y = "Percentual de cursandos, evadidos e formados",
    title = "Percentual de cursandos, evadidos e formados nas IES Federais",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  theme_tcc +
  theme_abnt


#### PLOT 5 -----
## Dados extraídos de: http://www.dominiopublico.gov.br/download/texto/me002240.pdf
dados96 <- readxl::read_excel("Dados 1996.xlsx")
dados96[is.na(dados96)] <- 0

## fazendo o cálculo do resultado agregados das Federais.
dados96_agregado <- dados96 %>% 
  summarise("IES" = "Federais",
            "N_TOTAL" = sum(N_TOTAL),
            "N_DIPLOMADOS" = sum(N_DIPLOMADOS),
            "N_RETIDOS" = sum(N_RETIDOS),
            "N_EVADIDOS" = sum(N_EVADIDOS)) %>% 
  mutate("P_DIPLOMADOS" = round(N_DIPLOMADOS/N_TOTAL, digits = 4)*100,
         "P_RETIDOS" =    round(N_RETIDOS/N_TOTAL, digits = 4)*100,
         "P_EVADIDOS" =   round(N_EVADIDOS/N_TOTAL, digits = 4)*100) %>% 
  select(IES, P_DIPLOMADOS, P_RETIDOS, P_EVADIDOS) %>% 
  pivot_longer(-IES, names_to = "Situação_Vínculo", values_to = "Freq") %>% 
  mutate("Situação_Vínculo" = 
           case_when(
             Situação_Vínculo == "P_DIPLOMADOS" ~ "Formados",
             Situação_Vínculo == "P_RETIDOS" ~ "Cursandos",
             Situação_Vínculo == "P_EVADIDOS" ~ "Evadidos"
           ))

## fazendo o cálculo do resultado agregados das Federais.
dados96_agregado_ies <- dados96 %>% 
  group_by(IES) %>% 
  summarise("N_TOTAL" = sum(N_TOTAL),
            "N_DIPLOMADOS" = sum(N_DIPLOMADOS),
            "N_RETIDOS" = sum(N_RETIDOS),
            "N_EVADIDOS" = sum(N_EVADIDOS)) %>% 
  mutate("P_DIPLOMADOS" = round(N_DIPLOMADOS/N_TOTAL, digits = 4)*100,
         "P_RETIDOS" =    round(N_RETIDOS/N_TOTAL, digits = 4)*100,
         "P_EVADIDOS" =   round(N_EVADIDOS/N_TOTAL, digits = 4)*100)

dados96_agregado_ies <- dados96_agregado_ies %>% 
  select(IES, N_TOTAL, P_EVADIDOS)

## Plot
ggplot(dados96_agregado_ies, aes(x = N_TOTAL, y = P_EVADIDOS)) +
  geom_point(colour = "#79402E",
               alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE,
              colour = "black",
              linetype = 2) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     labels = c("0", "20%", "40%", "60%", "80%", "100%"),
                     limits = c(0,100)) +
  scale_x_continuous(breaks = seq(0, 7500, by = 2500),
                     labels = c("0",
                                "2.5\nmil",
                                "5\nmil",
                                "7.5\nmil")) +
  labs(
    x = "Tamanho total das coortes analisadas",
    y = "Percentual de evadidos",
    title = "Percentual de cursandos, evadidos e formados nas IES Federais",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  theme_tcc +
  theme_abnt


#### PLOT 6 ----
## Dados extraídos de: http://www.institutolobo.org.br/imagens/pdf/artigos/art_045.pdf

plot6 <- data.frame(
  "TT" = c(
    73,
    54,
    51,
    25,
    72,
    52,
    48,
     7,
    12,
    17,
    22,
    30,
    31,
    34,
    41,
    58,
    52
  ),
  "PAÍS" = c(
    "Bolívia",
    "Chile",
    "Colômbia",
    "Cuba",
    "Uruguai",
    "Venezuela",
    "Brasil",
    "Japão",
    "Turquia",
    "Reino Unido",
    "Coréia do Sul",
    "Alemanha",
    "México",
    "EUA",
    "França",
    "Itália",
    "Suécia"
  )
)


plot6$PAÍS <- factor(plot6$PAÍS, levels = c(
  "Bolívia",
  "Uruguai",
  "Itália",
  "Chile",
  "Venezuela",
  "Suécia",
  "Colômbia",
  "Brasil",
  "França",
  "EUA",
  "México",
  "Alemanha",
  "Cuba",
  "Coréia do Sul",
  "Reino Unido",
  "Turquia",
  "Japão"
  
))

## Plot
ggplot(plot6, aes(x = PAÍS, y = TT)) +
  geom_segment(aes(x=PAÍS, xend=PAÍS, y=0, yend=TT), colour = "#fc9272") +
  geom_point(size = 3, colour = "#fc9272") +
  geom_point(data = subset(plot6, PAÍS == "Brasil"), colour = "#de2d26", size = 3) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     labels = c("0", "20%", "40%", "60%", "80%", "100%"),
                     limits = c(0,100)) +
  labs(
    x = "",
    y = "Evasão Total Média (Brasil em destaque)",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  coord_flip() +
  theme_minimal() +
  theme_tcc +
  theme_abnt
