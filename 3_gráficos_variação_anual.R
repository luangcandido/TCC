#### 03 _ Códigos dos gráficos de variação anual.


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
## Reorganizando o df para o plot.
ALUNOSFINAL_IEANEA <- ALUNOSFINAL_IEANEA %>% 
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA[, c(1, 42:50)] %>%
  pivot_longer(-CO_CATEGORIA_ADMINISTRATIVA,
               names_to = "INDICADOR_ANO",
               values_to = "IEA") %>%
  mutate("ANO" = str_extract(INDICADOR_ANO, "[0-9]+$")) %>%
  select(CO_CATEGORIA_ADMINISTRATIVA, ANO, IEA)

ALUNOSFINAL_IEANEA_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$ANO)
ALUNOSFINAL_IEANEA_PLOT$IEA <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$IEA)

ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )


# Últimos ajustes antes do plot (base para geom_hline e geom_text)
hline <- ALUNOSFINAL_IEANEA_PLOT %>%
  group_by(CO_CATEGORIA_ADMINISTRATIVA) %>%
  summarise(MEAN = mean(IEA))

# Plot
ggplot(ALUNOSFINAL_IEANEA_PLOT, aes(x = ANO, y = IEA)) +
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
  scale_x_continuous(breaks = seq(2010, 2018, by = 1),
                     labels = c("2010",
                                "2011",
                                "2012",
                                "2013",
                                "2014",
                                "2015",
                                "2016",
                                "2017",
                                "2018")) +
  scale_y_continuous(
    breaks = seq(0, 0.3, by = 0.05),
    labels = c("0", "5%", "10%", "15%", "20%", "25%", "30%"),
    limits = c(0,0.3)
  ) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  guides(
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 4
    )
  ) +
  labs(
    x = "Série histórica entre 2010 e 2018",
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
## Reorganizando o df para o plot.
ALUNOSFINAL_IEANEA <- ALUNOSFINAL_IEANEA %>% 
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA[, c(1, 51:59)] %>%
  pivot_longer(-CO_CATEGORIA_ADMINISTRATIVA,
               names_to = "INDICADOR_ANO",
               values_to = "NEA") %>%
  mutate("ANO" = str_extract(INDICADOR_ANO, "[0-9]+$")) %>%
  select(CO_CATEGORIA_ADMINISTRATIVA, ANO, NEA)

ALUNOSFINAL_IEANEA_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$ANO)
ALUNOSFINAL_IEANEA_PLOT$NEA <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$NEA)

ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )

# Últimos ajustes antes do plot (base para geom_hline e geom_text)
hline <- ALUNOSFINAL_IEANEA_PLOT %>%
  group_by(CO_CATEGORIA_ADMINISTRATIVA) %>%
  summarise(MEAN = mean(NEA))


# Plot
ggplot(data = ALUNOSFINAL_IEANEA_PLOT, aes(x = ANO, y = NEA)) +
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
  scale_x_continuous(breaks = seq(2010, 2018, by = 1),
                     labels = c("2010",
                                "2011",
                                "2012",
                                "2013",
                                "2014",
                                "2015",
                                "2016",
                                "2017",
                                "2018")) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  scale_y_continuous(
    
    breaks = seq(0, 1000000, by = 200000),
    labels = c("0",
               "200 mil",
               "400 mil",
               "600 mil",
               "800 mil",
               "1 milhão")
  ) +
  guides(colour = guide_legend(
    title = "Categoria Administrativa",
    title.position = "top",
    ncol = 4
  )) +
  labs(
    x = "Série histórica entre 2010 e 2018",
    y = "Número de Evasões Anuais em média",
    title = "Número de evasões anuais nas diferentes categorias\nadministrativas do Ensino Superior",
    subtitle = "Para os anos entre 2010 e 2018. Linha tracejada\nrepresenta a média no período.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  theme_tcc +
  theme_abnt

####

#### PLOT 3 ----
## Reorganizando o df para o plot.
ALUNOSFINAL_IEANEA <- ALUNOSFINAL_IEANEA %>% 
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA[, c(1, 1:41)] %>%
  pivot_longer(-CO_CATEGORIA_ADMINISTRATIVA,
               names_to = "INDICADOR_ANO",
               values_to = "NÚMERO")  %>%
  mutate("ANO" = str_extract(INDICADOR_ANO, "[0-9]+")) %>%
  mutate("INDICADOR" = str_extract(INDICADOR_ANO, "[a-zA-Z]{3}+")) %>%
  select(CO_CATEGORIA_ADMINISTRATIVA, ANO, INDICADOR, NÚMERO)

ALUNOSFINAL_IEANEA_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$ANO)
ALUNOSFINAL_IEANEA_PLOT$NÚMERO <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$NÚMERO)
ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
ALUNOSFINAL_IEANEA_PLOT$INDICADOR <-
  factor(ALUNOSFINAL_IEANEA_PLOT$INDICADOR,
         levels = c("ING",
                    "CUR",
                    "MAT",
                    "FOR"))

ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA_PLOT %>%
  filter(INDICADOR != "MAT")

## Plot
ggplot(ALUNOSFINAL_IEANEA_PLOT, aes(x = INDICADOR, y = NÚMERO)) +
  geom_col(
    position = "dodge",
    aes(colour = CO_CATEGORIA_ADMINISTRATIVA, group = ANO),
    fill = "gray90"
  ) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  scale_y_continuous(
    breaks = seq(0, 4000000, by = 1000000),
    labels = c("0",
               "1 milhão",
               "2 milhões",
               "3 milhões",
               "4 milhões")
  ) +
  guides(colour = guide_legend(
    title = "Categoria Administrativa",
    title.position = "top",
    ncol = 3
  )) +
  labs(
    x = "Série histórica entre 2009 e 2018",
    y = "N. de Ingressantes, Cursandos e Formados",
    title = "Número de ingressantes, cursandos e formandos\nnas categorias administrativas do Ensino Superior",
    subtitle = "Barras representam os resultados entre os anos de 2009 e de 2018.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  facet_wrap(. ~ CO_CATEGORIA_ADMINISTRATIVA, ncol = 3) +
  theme_tcc +
  theme_abnt +
  theme(
    legend.key.width = unit(1.2, "lines"),
    legend.key.height = unit(0.2, "lines"),
  )

####
#### PLOT 4 ----
## Reorganizando o df para o plot.
ALUNOSFINAL_IEACNEAC_MERGE <- ALUNOSFINAL_IEACNEAC_MERGE %>% 
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_MERGE %>%
  pivot_longer(-CO_CATEGORIA_ADMINISTRATIVA,
               names_to = "INDICADOR_ANO_MTRAJ",
               values_to = "RESULTADOS")

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate(
    "ANO" = str_extract(INDICADOR_ANO_MTRAJ, "[0-9]{4}"),
    "INDICADOR" = str_extract(INDICADOR_ANO_MTRAJ, "[A-Z]{3}"),
    "M_TRAJ" = str_extract(INDICADOR_ANO_MTRAJ, "[0-9]+.$")
  ) %>%
  select(-INDICADOR_ANO_MTRAJ)

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate("M_TRAJ" = str_sub(M_TRAJ, start = 5))

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate("RESULTADOS" = ifelse(
    ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR == "IEA",
    round(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS, digits = 4),
    format(
      round(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS, digits = 0),
      nsmall = 0
    )
  ))

ALUNOSFINAL_IEACNEAC_PLOT$M_TRAJ <- factor(
  ALUNOSFINAL_IEACNEAC_PLOT$M_TRAJ,
  levels = c("1a", "2a", "3a", "4a", "5a", "6a", "7a", "8a", "9a", "10a", "11a")
)

## Filtrando o ano de 2009 que não interessa para esta série histórica:
ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  filter(ANO %ni% c(2009, 2010, 2011, 2012, 2013))


## Ajustando as classes das colunas.
ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS <-
  as.numeric(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS)
ALUNOSFINAL_IEACNEAC_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEACNEAC_PLOT$ANO)
ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR <-
  factor(ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR, levels = c("IEA", "NEA"))
ALUNOSFINAL_IEACNEAC_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEACNEAC_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )

## Filtrando arquivo para IEA e para NEA
ALUNOSFINAL_IEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  filter(INDICADOR == "IEA")

## Plotando
ggplot(ALUNOSFINAL_IEAC_PLOT, aes(x = M_TRAJ, y = RESULTADOS)) +
  geom_area(aes(group = ANO, fill = CO_CATEGORIA_ADMINISTRATIVA), alpha = 0.4) +
  geom_line(aes(group = ANO, colour = CO_CATEGORIA_ADMINISTRATIVA)) +
  scale_fill_manual(values = c("#9986A5", "#79402E",
                               "#CCBA72")) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.1),
                     labels = c("0",
                                "10%",
                                "20%")) +
  scale_x_discrete(
    breaks = c("1a", "3a", "5a", "7a", "9a", "11a"),
    labels = c("1º",
               "3º",
               "5º",
               "7º",
               "9º",
               "11º")
  ) +
  guides(
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 4
    ),
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 4
    )
  ) +
  labs(
    x = "Posições (anos) da trajetória acadêmica (entre 2014 e 2018)",
    y = "Índice de evasão anual média",
    title = "IEA ao longo dos anos da trajetória acadêmica\npara as categorias administrativas do Ensino Superior",
    subtitle = "Para os anos entre 2014 e 2018.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ ANO) +
  theme_minimal() +
  theme_tcc +
  theme_abnt +
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 8)
  )


####

#### PLOT 5 ----
## Reorganizando o df para o plot.
ALUNOSFINAL_IEACNEAC_MERGE <- ALUNOSFINAL_IEACNEAC_MERGE %>% 
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_MERGE %>%
  pivot_longer(-CO_CATEGORIA_ADMINISTRATIVA,
               names_to = "INDICADOR_ANO_MTRAJ",
               values_to = "RESULTADOS")

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate(
    "ANO" = str_extract(INDICADOR_ANO_MTRAJ, "[0-9]{4}"),
    "INDICADOR" = str_extract(INDICADOR_ANO_MTRAJ, "[A-Z]{3}"),
    "M_TRAJ" = str_extract(INDICADOR_ANO_MTRAJ, "[0-9]+.$")
  ) %>%
  select(-INDICADOR_ANO_MTRAJ)

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate("M_TRAJ" = str_sub(M_TRAJ, start = 5))

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate("RESULTADOS" = ifelse(
    ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR == "IEA",
    round(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS, digits = 4),
    format(
      round(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS, digits = 0),
      nsmall = 0
    )
  ))

ALUNOSFINAL_IEACNEAC_PLOT$M_TRAJ <- factor(
  ALUNOSFINAL_IEACNEAC_PLOT$M_TRAJ,
  levels = c("1a", "2a", "3a", "4a", "5a", "6a", "7a", "8a", "9a", "10a", "11a")
)

## Filtrando o ano de 2009 que não interessa para esta série histórica:
ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  filter(ANO %ni% c(2009, 2010, 2011, 2012, 2013))

## Ajustando as classes das colunas.
ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS <-
  as.numeric(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS)
ALUNOSFINAL_IEACNEAC_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEACNEAC_PLOT$ANO)
ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR <-
  factor(ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR, levels = c("IEA", "NEA"))
ALUNOSFINAL_IEACNEAC_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEACNEAC_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )

## Filtrando arquivo para IEA e para NEA

ALUNOSFINAL_NEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  filter(INDICADOR == "NEA")


## Plotando
ggplot(ALUNOSFINAL_NEAC_PLOT, aes(x = M_TRAJ, y = RESULTADOS)) +
  geom_area(aes(group = ANO, fill = CO_CATEGORIA_ADMINISTRATIVA), alpha = 0.4) +
  geom_line(aes(group = ANO, colour = CO_CATEGORIA_ADMINISTRATIVA)) +
  scale_fill_manual(values = c("#9986A5", "#79402E",
                               "#CCBA72")) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  scale_y_continuous(breaks = seq(0, 400000, by = 200000),
                     labels = c("0", "200 mil", "400 mil")) +
  scale_x_discrete(
    breaks = c("1a", "3a", "5a", "7a", "9a", "11a"),
    labels = c("1º",
               "3º",
               "5º",
               "7º",
               "9º",
               "11º")
  ) +
  guides(
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 4
    ),
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 4
    )
  ) +
  labs(
    x = "Posições (anos) da trajetória acadêmica (entre 2014 e 2018)",
    y = "Número de evasões anuais em média",
    title = "NEA ao longo dos anos da trajetória acadêmica\npara as categorias administrativas do Ensino Superior",
    subtitle = "Para os anos entre 2014 e 2018.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_grid(CO_CATEGORIA_ADMINISTRATIVA ~ ANO) +
  theme_minimal() +
  theme_tcc +
  theme(panel.spacing.y = unit(1, "lines")) +
  theme_abnt + 
  theme(
    strip.text.y = element_blank(),
    strip.text.x = element_text(colour = "gray50", size = 8)
  )

####

#### PLOT 6 ----
## Reorganizando o df para o plot.
ALUNOSFINAL_IEACNEAC_MERGE <- ALUNOSFINAL_IEACNEAC_MERGE %>% 
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal")

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_MERGE %>%
  pivot_longer(-CO_CATEGORIA_ADMINISTRATIVA,
               names_to = "INDICADOR_ANO_MTRAJ",
               values_to = "RESULTADOS")

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate(
    "ANO" = str_extract(INDICADOR_ANO_MTRAJ, "[0-9]{4}"),
    "INDICADOR" = str_extract(INDICADOR_ANO_MTRAJ, "[A-Z]{3}"),
    "M_TRAJ" = str_extract(INDICADOR_ANO_MTRAJ, "[0-9]+.$")
  ) %>%
  select(-INDICADOR_ANO_MTRAJ)

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate("M_TRAJ" = str_sub(M_TRAJ, start = 5))

ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  mutate("RESULTADOS" = ifelse(
    ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR == "IEA",
    round(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS, digits = 4),
    format(
      round(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS, digits = 0),
      nsmall = 0
    )
  ))

ALUNOSFINAL_IEACNEAC_PLOT$M_TRAJ <- factor(
  ALUNOSFINAL_IEACNEAC_PLOT$M_TRAJ,
  levels = c("1a", "2a", "3a", "4a", "5a", "6a", "7a", "8a", "9a", "10a", "11a")
)

## Filtrando o ano de 2009 que não interessa para esta série histórica:
ALUNOSFINAL_IEACNEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  filter(ANO != 2009)


## Ajustando as classes das colunas.
ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS <-
  as.numeric(ALUNOSFINAL_IEACNEAC_PLOT$RESULTADOS)
ALUNOSFINAL_IEACNEAC_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEACNEAC_PLOT$ANO)
ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR <-
  factor(ALUNOSFINAL_IEACNEAC_PLOT$INDICADOR, levels = c("IEA", "NEA"))
ALUNOSFINAL_IEACNEAC_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEACNEAC_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )

## Filtrando arquivo para IEA e para NEA
ALUNOSFINAL_IEAC_PLOT <- ALUNOSFINAL_IEACNEAC_PLOT %>%
  filter(INDICADOR == "IEA")

## Labels das facets via annotate.
text_1a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "1a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_2a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "2a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_3a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "3a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_4a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "4a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_5a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "5a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_6a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "6a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_7a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "7a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_8a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "8a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_9a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "9a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_10a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "10a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

text_11a <- data.frame(
  RESULTADOS = 0.55,
  ANO = 2014,
  M_TRAJ = "11a",
  CO_CATEGORIA_ADMINISTRATIVA = factor(
    "Privada",
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual"
    )
  )
)

## Plotando
ggplot(ALUNOSFINAL_IEAC_PLOT, aes(x = ANO, y = RESULTADOS)) +
  geom_area(aes(group = M_TRAJ, fill = CO_CATEGORIA_ADMINISTRATIVA), alpha = 0.4) +
  geom_line(aes(group = M_TRAJ, colour = CO_CATEGORIA_ADMINISTRATIVA)) +
  geom_smooth(
    method = "lm",
    linetype = 2,
    colour = "black",
    size = 0.5,
    alpha = 0.3,
    se = FALSE
  ) +
  geom_text(
    data = text_1a,
    label = "1º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_2a,
    label = "2º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_3a,
    label = "3º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_4a,
    label = "4º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_5a,
    label = "5º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_6a,
    label = "6º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_7a,
    label = "7º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_8a,
    label = "8º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_9a,
    label = "9º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_10a,
    label = "10º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  geom_text(
    data = text_11a,
    label = "11º ano",
    colour = "gray50",
    size = 3.2,
    family = "Times New Roman"
  ) +
  scale_fill_manual(values = c("#9986A5", "#79402E",
                               "#CCBA72")) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.2),
                     labels = c("0",
                                "20%",
                                "40%")) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 2),
                     labels = c("10",
                                "12",
                                "14",
                                "16",
                                "18")) +
  guides(
    fill = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 4
    ),
    colour = guide_legend(
      title = "Categoria Administrativa",
      title.position = "top",
      ncol = 4
    )
  ) +
  labs(
    x = "Série histórica entre 2010 e 2018 (de cada posição da trajetória acadêmica)",
    y = "Índice de evasão anual média",
    title = "Variação do IEA em cada momento da trajetória acadêmica\npara as categorias administrativas do Ensino Superior",
    subtitle = "Série histórica de 2010 a 2018.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  facet_wrap(CO_CATEGORIA_ADMINISTRATIVA ~ M_TRAJ, ncol = 11) +
  theme_minimal() +
  theme_tcc +
  coord_cartesian(clip = "off") +
  theme_abnt +
  theme(axis.text.x = element_text(colour = "gray50", size = 6))


####



## OBS: CALCULAR NO PLOT 6 VARIAÇÃO AO LONGO DA SÉRIE
## DO IEA EM CADA POSIÇÃO. OU SEJA: O IEA 1A CRESCEU AO LONGO DOS ANOS?
## O OBJETIVO É PERCEBER SE O AUMENTO DO IEA NAS PRIVADAS OCORREU
## POR AUMENTO NA EVASÃO DOS PRIMEIROS ANOS OU NÃO.
#### PLOT 7 ----
## Reorganizando o df para o plot.
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA_IES[, c(1:7, 48:56)] %>%
  pivot_longer(
    -c(
      CO_CATEGORIA_ADMINISTRATIVA,
      CO_REGIAO,
      CO_UF,
      CO_MUNICIPIO,
      CO_IES,
      NO_IES,
      SG_IES
    ),
    names_to = "INDICADOR_ANO",
    values_to = "IEA"
  )  %>%
  mutate("ANO" = str_extract(INDICADOR_ANO, "[0-9]+$")) %>%
  select(
    CO_CATEGORIA_ADMINISTRATIVA,
    CO_REGIAO,
    CO_UF,
    CO_MUNICIPIO,
    CO_IES,
    NO_IES,
    SG_IES,
    ANO,
    IEA
  )

ALUNOSFINAL_IEANEA_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$ANO)
ALUNOSFINAL_IEANEA_PLOT$IEA <-
  round(as.numeric(ALUNOSFINAL_IEANEA_PLOT$IEA), digits = 4)
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA_PLOT %>%
  mutate(
    CO_CATEGORIA_ADMINISTRATIVA = case_when(
      CO_CATEGORIA_ADMINISTRATIVA == "1" ~ "Pública Federal",
      CO_CATEGORIA_ADMINISTRATIVA == "2" ~ "Pública Estadual",
      CO_CATEGORIA_ADMINISTRATIVA == "3" ~ "Pública Municipal",
      CO_CATEGORIA_ADMINISTRATIVA %ni% c(1, 2, 3) ~ "Privada"
    )
  )
ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual",
      "Pública Municipal"
    )
  )

## Adicionando a coluna com o número de matriculados.
ALUNOSFINAL_IEANEA_COMPL <- ALUNOSFINAL_IEANEA_IES %>%
  select(
    CO_CATEGORIA_ADMINISTRATIVA,
    CO_REGIAO,
    CO_UF,
    CO_MUNICIPIO,
    CO_IES,
    NO_IES,
    SG_IES,
    contains("MAT")
  ) %>%
  pivot_longer(
    -c(
      CO_CATEGORIA_ADMINISTRATIVA,
      CO_REGIAO,
      CO_UF,
      CO_MUNICIPIO,
      CO_IES,
      NO_IES,
      SG_IES
    ),
    names_to = "INDICADOR_ANO",
    values_to = "N_MAT"
  )  %>%
  mutate("ANO" = str_extract(INDICADOR_ANO, "[0-9]+")) %>%
  select(
    CO_CATEGORIA_ADMINISTRATIVA,
    CO_REGIAO,
    CO_UF,
    CO_MUNICIPIO,
    CO_IES,
    NO_IES,
    SG_IES,
    ANO,
    N_MAT
  )

ALUNOSFINAL_IEANEA_COMPL$ANO <-
  as.numeric(ALUNOSFINAL_IEANEA_COMPL$ANO)
ALUNOSFINAL_IEANEA_COMPL$N_MAT <-
  as.numeric(ALUNOSFINAL_IEANEA_COMPL$N_MAT)
ALUNOSFINAL_IEANEA_COMPL <- ALUNOSFINAL_IEANEA_COMPL %>%
  mutate(
    CO_CATEGORIA_ADMINISTRATIVA = case_when(
      CO_CATEGORIA_ADMINISTRATIVA == "1" ~ "Pública Federal",
      CO_CATEGORIA_ADMINISTRATIVA == "2" ~ "Pública Estadual",
      CO_CATEGORIA_ADMINISTRATIVA == "3" ~ "Pública Municipal",
      CO_CATEGORIA_ADMINISTRATIVA %ni% c(1, 2, 3) ~ "Privada"
    )
  )
ALUNOSFINAL_IEANEA_COMPL$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEANEA_COMPL$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual",
      "Pública Municipal"
    )
  )

ALUNOSFINAL_IEANEA_COMPL <- ALUNOSFINAL_IEANEA_COMPL %>%
  filter(ANO != 2009)

## Juntando os dois dfs.
ALUNOSFINAL_IEANEA_PLOT <- merge(
  ALUNOSFINAL_IEANEA_PLOT,
  ALUNOSFINAL_IEANEA_COMPL,
  by = c(
    "CO_CATEGORIA_ADMINISTRATIVA",
    "CO_REGIAO",
    "CO_UF",
    "CO_MUNICIPIO",
    "CO_IES",
    "NO_IES",
    "SG_IES",
    "ANO"
  )
)

## Filtrando as Municipais e valores nulos.
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA_PLOT %>%
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal") %>%
  na.omit()

## Plot
ggplot(subset(ALUNOSFINAL_IEANEA_PLOT, N_MAT > 100), aes(x = ANO, y = IEA)) +
  stat_boxplot(geom = "errorbar",
               aes(group = ANO),
               size = 0.4) +
  geom_boxplot(
    aes(group = ANO, fill = CO_CATEGORIA_ADMINISTRATIVA),
    outlier.alpha = 0.1,
    size = 0.4,
    alpha = 0.85
  ) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 2),
                     labels = c("10",
                                "12",
                                "14",
                                "16",
                                "18")) +
  scale_y_continuous(
    breaks = c(-0.4,-0.2, 0, 0.2, 0.4, 0.6),
    labels = c("-40%",
               "-20%", "0", "20%",
               "40%", "60%"),
    limits = c(-0.4, 0.6)
  ) +
  scale_fill_manual(values = c("#9986A5", "#79402E",
                               "#CCBA72")) +
  guides(fill = guide_legend(
    title = "Categoria Administrativa",
    title.position = "top",
    ncol = 4
  )) +
  labs(
    x = "Série histórica entre 2010 e 2018",
    y = "Índice de evasão anual em média",
    title = "Distribuição do IEA nas Inst. de Ensino\nSuperior por categoria administrativa",
    subtitle = "Para os anos entre 2010 e 2018.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  facet_wrap(. ~ CO_CATEGORIA_ADMINISTRATIVA, nrow = 1) +
  theme_tcc +
  theme_abnt



#### PLOT 8 ----
## Reorganizando o df para o plot.
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA_IES[, c(1:7, 48:56)] %>%
  pivot_longer(
    -c(
      CO_CATEGORIA_ADMINISTRATIVA,
      CO_REGIAO,
      CO_UF,
      CO_MUNICIPIO,
      CO_IES,
      NO_IES,
      SG_IES
    ),
    names_to = "INDICADOR_ANO",
    values_to = "IEA"
  )  %>%
  mutate("ANO" = str_extract(INDICADOR_ANO, "[0-9]+$")) %>%
  select(
    CO_CATEGORIA_ADMINISTRATIVA,
    CO_REGIAO,
    CO_UF,
    CO_MUNICIPIO,
    CO_IES,
    NO_IES,
    SG_IES,
    ANO,
    IEA
  )

ALUNOSFINAL_IEANEA_PLOT$ANO <-
  as.numeric(ALUNOSFINAL_IEANEA_PLOT$ANO)
ALUNOSFINAL_IEANEA_PLOT$IEA <-
  round(as.numeric(ALUNOSFINAL_IEANEA_PLOT$IEA), digits = 4)
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA_PLOT %>%
  mutate(
    CO_CATEGORIA_ADMINISTRATIVA = case_when(
      CO_CATEGORIA_ADMINISTRATIVA == "1" ~ "Pública Federal",
      CO_CATEGORIA_ADMINISTRATIVA == "2" ~ "Pública Estadual",
      CO_CATEGORIA_ADMINISTRATIVA == "3" ~ "Pública Municipal",
      CO_CATEGORIA_ADMINISTRATIVA %ni% c(1, 2, 3) ~ "Privada"
    )
  )
ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEANEA_PLOT$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual",
      "Pública Municipal"
    )
  )

## Adicionando a coluna com o número de matriculados.
ALUNOSFINAL_IEANEA_COMPL <- ALUNOSFINAL_IEANEA_IES %>%
  select(
    CO_CATEGORIA_ADMINISTRATIVA,
    CO_REGIAO,
    CO_UF,
    CO_MUNICIPIO,
    CO_IES,
    NO_IES,
    SG_IES,
    contains("MAT")
  ) %>%
  pivot_longer(
    -c(
      CO_CATEGORIA_ADMINISTRATIVA,
      CO_REGIAO,
      CO_UF,
      CO_MUNICIPIO,
      CO_IES,
      NO_IES,
      SG_IES
    ),
    names_to = "INDICADOR_ANO",
    values_to = "N_MAT"
  )  %>%
  mutate("ANO" = str_extract(INDICADOR_ANO, "[0-9]+")) %>%
  select(
    CO_CATEGORIA_ADMINISTRATIVA,
    CO_REGIAO,
    CO_UF,
    CO_MUNICIPIO,
    CO_IES,
    NO_IES,
    SG_IES,
    ANO,
    N_MAT
  )

ALUNOSFINAL_IEANEA_COMPL$ANO <-
  as.numeric(ALUNOSFINAL_IEANEA_COMPL$ANO)
ALUNOSFINAL_IEANEA_COMPL$N_MAT <-
  as.numeric(ALUNOSFINAL_IEANEA_COMPL$N_MAT)
ALUNOSFINAL_IEANEA_COMPL <- ALUNOSFINAL_IEANEA_COMPL %>%
  mutate(
    CO_CATEGORIA_ADMINISTRATIVA = case_when(
      CO_CATEGORIA_ADMINISTRATIVA == "1" ~ "Pública Federal",
      CO_CATEGORIA_ADMINISTRATIVA == "2" ~ "Pública Estadual",
      CO_CATEGORIA_ADMINISTRATIVA == "3" ~ "Pública Municipal",
      CO_CATEGORIA_ADMINISTRATIVA %ni% c(1, 2, 3) ~ "Privada"
    )
  )
ALUNOSFINAL_IEANEA_COMPL$CO_CATEGORIA_ADMINISTRATIVA <-
  factor(
    ALUNOSFINAL_IEANEA_COMPL$CO_CATEGORIA_ADMINISTRATIVA,
    levels = c(
      "Privada",
      "Pública Federal",
      "Pública Estadual",
      "Pública Municipal"
    )
  )

ALUNOSFINAL_IEANEA_COMPL <- ALUNOSFINAL_IEANEA_COMPL %>%
  filter(ANO != 2009)

## Juntando os dois dfs.
ALUNOSFINAL_IEANEA_PLOT <- merge(
  ALUNOSFINAL_IEANEA_PLOT,
  ALUNOSFINAL_IEANEA_COMPL,
  by = c(
    "CO_CATEGORIA_ADMINISTRATIVA",
    "CO_REGIAO",
    "CO_UF",
    "CO_MUNICIPIO",
    "CO_IES",
    "NO_IES",
    "SG_IES",
    "ANO"
  )
)

## Filtrando as Municipais e valores nulos.
ALUNOSFINAL_IEANEA_PLOT <- ALUNOSFINAL_IEANEA_PLOT %>%
  filter(CO_CATEGORIA_ADMINISTRATIVA != "Pública Municipal") %>%
  na.omit()

## Plot
ggplot(subset(ALUNOSFINAL_IEANEA_PLOT, N_MAT > 100), aes(x = N_MAT, y = IEA)) +
  geom_point(aes(colour = CO_CATEGORIA_ADMINISTRATIVA), alpha = 0.5) +
  geom_smooth(
    method = "lm",
    linetype = 2,
    colour = "black",
    size = 0.5,
    alpha = 0.3
  ) +  
  scale_y_continuous(
    breaks = c(-0.6,-0.4,-0.2, 0, 0.2, 0.4, 0.6),
    labels = c("-60%", "-40%",
               "-20%", "0", "20%",
               "40%", "60%"),
    limits = c(-0.6, 0.6)
  ) +
  scale_x_continuous(breaks = seq(0, 200000, by = 50000),
                     labels = c(
                       "0",
                       "50\nmil",
                       "100\nmil",
                       "150\nmil",
                       "200\nmil"
                     )) +
  scale_colour_manual(values = c("#9986A5", "#79402E",
                                 "#CCBA72")) +
  guides(colour = guide_legend(
    title = "Categoria Administrativa",
    title.position = "top",
    ncol = 4
  )) +
  labs(
    x = "Número de estudantes matriculados (dados de 10-18)",
    y = "Índice de evasão anual em média (dados de 10-18)",
    title = "Relação entre IEA e número de matriculados nas Inst. de\nEnsino Superior por categoria administrativa.",
    subtitle = "Para os anos entre 2010 e 2018.",
    caption = "Censo da Educação Superior. Elaboração própria."
  ) +
  theme_minimal() +
  facet_wrap(. ~ CO_CATEGORIA_ADMINISTRATIVA, ncol = 3) +
  theme_tcc +
  theme_abnt

