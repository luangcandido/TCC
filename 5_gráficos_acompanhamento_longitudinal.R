#### 05 _ Códigos dos gráficos de acompanhamento longitudinal.
#### Para maximizar e minimizar o código das seções a seguir, basta
#### clicar na ponta de seta/no triângulo ao lado do número da linha onde começa a seção.
#### Ex: para ver a seção 02.0.1, clique no triângulo ao lado do número 8 (começo da linha 8)

#### 5.0.0 - Passos prévios necessários. ----
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
library(wesanderson)

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
  
  
  
  
  
  library(dplyr)
  library(stringr)
  library(ffbase)
  "%ni%" <- Negate("%in%")
  
  
  
  ggplot(subset(coortes_1017, CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal"), aes(x = Ano, y = Freq_Percent)) + geom_col(position = "stack", aes(fill = Situaçao_Vínculo)) + facet_wrap(CO_CATEGORIA_ADMINISTRATIVA ~ Coorte, ncol = 8) + theme_minimal
#### PLOT 1 ----
## Preparado o df
coortes_1017 <- readRDS("coortes_1017.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")
  

coortes_1017 <- coortes_1017 %>%  
  mutate("CAT_SIT" = paste0(Situaçao_Vínculo, " ",
                                                            "(", CO_CATEGORIA_ADMINISTRATIVA, ")")) %>% 
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017$CAT_SIT <- factor(
  coortes_1017$CAT_SIT,
  levels = c(
    "Cursando (Privada)",
    "Cursando (Pública Federal)",
    "Cursando (Pública Estadual)",
    "Formado (Privada)",
    "Formado (Pública Federal)",
    "Formado (Pública Estadual)",
    "Evadido (Privada)",
    "Evadido (Pública Federal)",
    "Evadido (Pública Estadual)"
  )
)

coortes_1017$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    coortes_1017$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c("Privada",
               "Pública Federal",
               "Pública Estadual")
  )

coortes_1017_p1 <- subset(coortes_1017, Coorte %in% c("Coorte 2010", "Coorte 2011", "Coorte 2012", "Coorte 2013"))

## Plot
ggplot(coortes_1017_p1,
aes(x = M_TRAJ, y = Freq_Percent)) +
  geom_area(aes(fill = CAT_SIT, alpha = CAT_SIT), position = "stack") +
  scale_x_continuous(breaks = seq(1, 8, by = 1),
                     labels = c("1º",
                                "2º",
                                "3º",
                                "4º",
                                "5º",
                                "6º",
                                "7º",
                                "8º")) +
  scale_y_continuous(breaks = seq(0, 100, by = 25),
                     labels = c("0", "25%", "50%", "75%", "100%")) +
  scale_fill_manual(
    values = c(
      "#9986A5",
      "#79402E",
      "#CCBA72",
      "#9986A5",
      "#79402E",
      "#CCBA72",
      "#9986A5",
      "#79402E",
      "#CCBA72"
    )
  ) +
  scale_alpha_manual(values = c(0.2, 0.2, 0.2,
                                0.5, 0.5, 0.5,
                                0.8, 0.8, 0.8)) +
  guides(
    fill = guide_legend(
      title = "",
      title.position = "left",
      ncol = 3
    ),
    alpha = guide_legend(
      title = "",
      title.position = "left",
      ncol = 3
    )
  ) +
  labs(
    x = "Trajetória acadêmica das Coortes 10, 11, 12 e 13 (1º ano é o ano de ingresso)",
    y = "Percentual de cursandos, evadidos e formados",
    title = "Percentual de cursandos, evadidos e formados nas categorias\nadministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(Ano_ingresso ~ CO_CATEGORIA_ADMINISTRATIVA) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    legend.text = element_text(colour = "gray50", size = 7.8),
    strip.text.x = element_blank(),
    strip.text.y = element_text(colour = "gray50", size = 10)
  )

#### PLOT 2 ----
## Carregando a base de dados das categorias administrativas.
coortes_1017 <- readRDS("coortes_1017.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

## Filtrando os valores de interesse.
coortes_1017_tec <- coortes_1017 %>%
  filter(Situaçao_Vínculo == "Evadido") %>%
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA <- 
  factor(coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA,
         levels = c(
           "Privada",
           "Pública Federal",
           "Pública Estadual"
         ))

## Plot
ggplot(subset(coortes_1017_tec,
              M_TRAJ2 %ni% c("7º ano", "8º ano")), aes(x = Ano_ingresso, y = Freq_Percent)) +
  geom_col(aes(fill = CO_CATEGORIA_ADMINISTRATIVA, 
                group = interaction(M_TRAJ, CO_CATEGORIA_ADMINISTRATIVA)),
            alpha = 0.4,
           width = .8) +
  geom_smooth(
    method = "lm",
    linetype = 2,
    colour = "black",
    size = 0.5,
    alpha = 0.3,
    se = FALSE
  ) +
  scale_x_continuous(breaks = seq(2011, 2017, by = 2),
                     labels = c("11",
                                "13",
                                "15",
                                "17")) +
  scale_y_continuous(breaks = seq(0, 60, by = 20),
                     labels = c("0", "20%", "40%", "60%"),
                     limits = c(0,60)) +
  scale_colour_manual(values = c("#9986A5",
                                 "#79402E",
                                 "#CCBA72")) +
  scale_fill_manual(values = c("#9986A5",
                                 "#79402E",
                                 "#CCBA72")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Coortes dos ingressantes de 10, 11, 12, 13, 14, 15, 16 e 17",
    y = "Percentual de evadidos",
    title = "Percentual de evadidos dos anos da trajetória acadêmica\n nas categorias dministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ M_TRAJ2) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 10),
    panel.spacing.y = unit(0.8, "lines")
    
  )


#### PLOT 3 ----
## Carregando a base de dados das categorias administrativas.
coortes_1017 <- readRDS("coortes_1017.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

## Filtrando os valores de interesse.
coortes_1017_tmc <- coortes_1017 %>%
  filter(Situaçao_Vínculo == "Cursando") %>%
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017_tmc$CO_CATEGORIA_ADMINISTRATIVA <- 
  factor(coortes_1017_tmc$CO_CATEGORIA_ADMINISTRATIVA,
         levels = c(
           "Privada",
           "Pública Federal",
           "Pública Estadual"
         ))

## Plot
ggplot(subset(coortes_1017_tmc,
              M_TRAJ2 %ni% c("7º ano", "8º ano")), aes(x = Ano_ingresso, y = Freq_Percent)) +
  geom_col(aes(fill = CO_CATEGORIA_ADMINISTRATIVA, 
               group = interaction(M_TRAJ, CO_CATEGORIA_ADMINISTRATIVA)),
           alpha = 0.4,
           width = .8) +
  geom_smooth(
    method = "lm",
    linetype = 2,
    colour = "black",
    size = 0.5,
    alpha = 0.3,
    se = FALSE
  ) +
  scale_x_continuous(breaks = seq(2011, 2017, by = 2),
                     labels = c("11",
                                "13",
                                "15",
                                "17")) +
  scale_y_continuous(breaks = seq(0, 80, by = 20),
                     labels = c("0", "20%", "40%", "60%", "80%"),
                     limits = c(0,100)) +
  scale_colour_manual(values = c("#9986A5",
                                 "#79402E",
                                 "#CCBA72")) +
  scale_fill_manual(values = c("#9986A5",
                               "#79402E",
                               "#CCBA72")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Coortes dos ingressantes de 10, 11, 12, 13, 14, 15, 16 e 17",
    y = "Percentual de cursandos",
    title = "Percentual de cursandos dos anos da trajetória acadêmica\n nas categorias dministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ M_TRAJ2) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 10),
    panel.spacing.y = unit(0.8, "lines")
    
  )


#### PLOT 4 ----
## Carregando a base de dados das categorias administrativas.
coortes_1017 <- readRDS("coortes_1017.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

## Filtrando os valores de interesse.
coortes_1017_tdc <- coortes_1017 %>%
  filter(Situaçao_Vínculo == "Formado") %>%
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017_tdc$CO_CATEGORIA_ADMINISTRATIVA <- 
  factor(coortes_1017_tdc$CO_CATEGORIA_ADMINISTRATIVA,
         levels = c(
           "Privada",
           "Pública Federal",
           "Pública Estadual"
         ))

## Plot
ggplot(subset(coortes_1017_tdc,
              M_TRAJ2 %ni% c("1º ano", "2º ano", "8º ano", "7º ano")), aes(x = Ano_ingresso, y = Freq_Percent)) +
  geom_col(aes(fill = CO_CATEGORIA_ADMINISTRATIVA, 
               group = interaction(M_TRAJ, CO_CATEGORIA_ADMINISTRATIVA)),
           alpha = 0.4,
           width = .8) +
  geom_smooth(
    method = "lm",
    linetype = 2,
    colour = "black",
    size = 0.5,
    alpha = 0.3,
    se = FALSE
  ) +
  scale_x_continuous(breaks = seq(2011, 2017, by = 2),
                     labels = c("11",
                                "13",
                                "15",
                                "17")) +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     labels = c("0", "10%", "20%", "30%", "40%"),
                     limits = c(0,40)) +
  scale_colour_manual(values = c("#9986A5",
                                 "#79402E",
                                 "#CCBA72")) +
  scale_fill_manual(values = c("#9986A5",
                               "#79402E",
                               "#CCBA72")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Coortes dos ingressantes de 10, 11, 12, 13, 14 e 15",
    y = "Percentual de formados",
    title = "Percentual de formados dos anos da trajetória acadêmica\n nas categorias dministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ M_TRAJ2) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 10),
    panel.spacing.y = unit(0.8, "lines")
    
  )


#### PLOT 5 ----
## Carregando a base de dados das categorias administrativas.
coortes_1017 <- readRDS("coortes_1017_ies.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

## Filtrando os valores de interesse.
coortes_1017_tec <- coortes_1017 %>%
  filter(Situaçao_Vínculo == "Evadido") %>%
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA <- 
  factor(coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA,
         levels = c(
           "Privada",
           "Pública Federal",
           "Pública Estadual"
         ))

## Plot
ggplot(subset(coortes_1017_tec,
              M_TRAJ2 %ni% c("7º ano", "8º ano")), aes(x = Ano_ingresso, y = Freq_Percent)) +
  stat_boxplot(geom = "errorbar",
               aes(group = interaction(Ano_ingresso, CO_CATEGORIA_ADMINISTRATIVA)),
               size = 0.1) +
  geom_boxplot(aes(fill = CO_CATEGORIA_ADMINISTRATIVA, 
               group = interaction(Ano_ingresso, CO_CATEGORIA_ADMINISTRATIVA)),
               alpha = 0.85,
               size = 0.1,
               outlier.alpha = 0.1) +
  scale_x_continuous(breaks = seq(2011, 2017, by = 2),
                     labels = c("11",
                                "13",
                                "15",
                                "17")) +
  scale_y_continuous(breaks = seq(0, 100, by = 25),
                     labels = c("0", "25%", "50%", "75%", "100%"),
                     limits = c(0,100)) +
  scale_colour_manual(values = c("#9986A5",
                                 "#79402E",
                                 "#CCBA72")) +
  scale_fill_manual(values = c("#9986A5",
                               "#79402E",
                               "#CCBA72")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Coortes dos ingressantes de 10, 11, 12, 13, 14, 15, 16 e 17",
    y = "Percentual de evadidos",
    title = "Percentual de evadidos dos anos da trajetória acadêmica\n nas categorias dministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ M_TRAJ2) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 10),
    panel.spacing.y = unit(0.8, "lines")
    
  )



#### PLOT 6 ----
## Carregando a base de dados das categorias administrativas.
coortes_1017 <- readRDS("coortes_1017_ies.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

## Filtrando os valores de interesse.
coortes_1017_tdc <- coortes_1017 %>%
  filter(Situaçao_Vínculo == "Formado") %>%
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017_tdc$CO_CATEGORIA_ADMINISTRATIVA <- 
  factor(coortes_1017_tdc$CO_CATEGORIA_ADMINISTRATIVA,
         levels = c(
           "Privada",
           "Pública Federal",
           "Pública Estadual"
         ))

## Plot
ggplot(subset(coortes_1017_tdc,
              M_TRAJ2 %ni% c("1º ano", "2º ano", "8º ano", "7º ano")), aes(x = Ano_ingresso, y = Freq_Percent)) +
  stat_boxplot(geom = "errorbar",
               aes(group = interaction(Ano_ingresso, CO_CATEGORIA_ADMINISTRATIVA)),
               size = 0.1) +
  geom_boxplot(aes(fill = CO_CATEGORIA_ADMINISTRATIVA, 
                   group = interaction(Ano_ingresso, CO_CATEGORIA_ADMINISTRATIVA)),
               alpha = 0.85,
               size = 0.1,
               outlier.alpha = 0.1) +
  scale_x_continuous(breaks = seq(2011, 2017, by = 2),
                     labels = c("11",
                                "13",
                                "15",
                                "17")) +
  scale_y_continuous(breaks = seq(0, 100, by = 25),
                     labels = c("0", "25%", "50%", "75%", "100%"),
                     limits = c(0,100)) +
  scale_colour_manual(values = c("#9986A5",
                                 "#79402E",
                                 "#CCBA72")) +
  scale_fill_manual(values = c("#9986A5",
                               "#79402E",
                               "#CCBA72")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Coortes dos ingressantes de 10, 11, 12, 13, 14 e 15",
    y = "Percentual de formados",
    title = "Percentual de formados dos anos da trajetória acadêmica\n nas categorias dministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ M_TRAJ2) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 10),
    panel.spacing.y = unit(0.8, "lines")
    
  )



#### PLOT 7 ----
## Carregando a base de dados das categorias administrativas.
coortes_1017 <- readRDS("coortes_1017_ies.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

## Filtrando os valores de interesse.
coortes_1017_tec <- coortes_1017 %>%
  filter(Situaçao_Vínculo == "Cursando") %>%
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA <- 
  factor(coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA,
         levels = c(
           "Privada",
           "Pública Federal",
           "Pública Estadual"
         ))

## Plot
ggplot(subset(coortes_1017_tec,
              M_TRAJ2 %ni% c("7º ano", "8º ano")), aes(x = Ano_ingresso, y = Freq_Percent)) +
  stat_boxplot(geom = "errorbar",
               aes(group = interaction(Ano_ingresso, CO_CATEGORIA_ADMINISTRATIVA)),
               size = 0.1) +
  geom_boxplot(aes(fill = CO_CATEGORIA_ADMINISTRATIVA, 
                   group = interaction(Ano_ingresso, CO_CATEGORIA_ADMINISTRATIVA)),
               alpha = 0.85,
               size = 0.1,
               outlier.alpha = 0.1) +
  scale_x_continuous(breaks = seq(2011, 2017, by = 2),
                     labels = c("11",
                                "13",
                                "15",
                                "17")) +
  scale_y_continuous(breaks = seq(0, 100, by = 25),
                     labels = c("0", "25%", "50%", "75%", "100%"),
                     limits = c(0,100)) +
  scale_colour_manual(values = c("#9986A5",
                                 "#79402E",
                                 "#CCBA72")) +
  scale_fill_manual(values = c("#9986A5",
                               "#79402E",
                               "#CCBA72")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    ),
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 3
    )
  ) +
  labs(
    x = "Coortes dos ingressantes de 10, 11, 12, 13, 14, 15, 16 e 17",
    y = "Percentual de cursandos",
    title = "Percentual de cursandos dos anos da trajetória acadêmica\n nas categorias dministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2017.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ M_TRAJ2) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 10),
    panel.spacing.y = unit(0.8, "lines")
    
  )




#### PLOT 8 ----
## Carregando a base de dados das categorias administrativas.
coortes_1017 <- readRDS("coortes_1017_ies.RDS")
coortes_1017 <-
  coortes_1017 %>% filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

## Filtrando.
coortes_1017_tec <- coortes_1017 %>%
  mutate("Ano_ingresso" = as.numeric(str_extract(Coorte, "[0-9]+$"))) %>%
  mutate("M_TRAJ" = Ano - Ano_ingresso + 1) %>% 
  mutate("M_TRAJ2" = paste0(M_TRAJ, "º ano"))

coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA <- 
  factor(coortes_1017_tec$CO_CATEGORIA_ADMINISTRATIVA,
         levels = c(
           "Privada",
           "Pública Federal",
           "Pública Estadual"
         ))

coortes_1017_tec_p8 <- coortes_1017_tec %>% 
  filter(M_TRAJ %in% c(1,2)) %>% 
  filter(Situaçao_Vínculo == "Evadido")

## Plot
ggplot(coortes_1017_tec_p8,
       aes(x = N_TOTAL, y = Freq_Percent)) +
geom_point(aes(colour = CO_CATEGORIA_ADMINISTRATIVA), alpha = 0.5) +
  geom_smooth(
    method = "lm",
    linetype = 2,
    colour = "black",
    size = 0.5,
    alpha = 0.3
  ) +  
  scale_y_continuous(
    breaks = seq(0,100,25),
    labels = c("0", "25%", "50%", "75%", "100%"),
    limits = c(0, 100)
  ) +
  scale_x_continuous(breaks = seq(0, 150000, by = 50000),
                     labels = c(
                       "0",
                       "50\nmil",
                       "100\nmil",
                       "150\nmil"
                     )) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  guides(colour = guide_legend(
    title = "Categoria Administrativa",
    title.position = "top",
    ncol = 4
  )) +
  labs(
    x = "Tamanho total da coorte (dados das coortes 10-17)",
    y = "TEC no 1º e 2º ano de graduação",
    title = "Relação entre IEA e número de matriculados nas Inst. de\nEnsino Superior por categoria administrativa.",
    subtitle = "Para os anos entre 2010 e 2018.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  facet_grid(M_TRAJ2 ~ CO_CATEGORIA_ADMINISTRATIVA) +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.x = element_blank(),
    strip.text.y = element_text(colour = "gray50", size = 10),
    panel.spacing.y = unit(0.8, "lines")
    
  )
