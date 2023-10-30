aaa <- read.csv("vendas.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600 ", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666 ")

theme_estat <- function(...) { theme <- ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(colour = "black",
                                         size = 12),
    axis.title.x = ggplot2::element_text(colour = "black",
                                         size = 12),
    axis.text = ggplot2::element_text(colour = "black", size
                                      = 9.5),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"), legend.position = "top",
    ...
  )
return( list(
  theme ,
  scale_fill_manual(values = cores_estat), scale_colour_manual(values = cores_estat)
) )
}


#Grafico por mes


aaa$Data.Venda = mdy(aaa$Data.Venda)
aaa$Meses <- month(aaa$Data.Venda)
aaa$Meses = month.name[aaa$Meses]


vendas_por_mes_categoria <- aaa %>%
  group_by(Meses, Category) %>%
  summarise(Faturamento = sum(Price, na.rm = TRUE))

custom_month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

vendas_por_mes_categoria <- vendas_por_mes_categoria %>% 
  arrange(match(Meses, custom_month_order))


vendas_por_mes_categoria <- na.omit(vendas_por_mes_categoria)


ggplot(vendas_por_mes_categoria, aes(x = factor(Meses, levels = custom_month_order), y = Faturamento, group = Category, colour = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Meses", y = "Faturamento") +
  theme_estat()



#Box splot 

Variacao <- aaa %>%
  select(Brand, Price)

Variacao <- na.omit(Variacao)

ggplot(Variacao) +
  aes(x = Brand , y = Price) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs( x = "Marca", y = "Preço") +
  theme_estat()



resumo_por_marca <- aaa %>%
  group_by(Brand) %>%
  summarise(
    Media = mean(Price, na.rm = TRUE),
    Mediana = median(Price, na.rm = TRUE),
    Minimo = min(Price, na.rm = TRUE),
    Maximo = max(Price, na.rm = TRUE),
    Variacao = Maximo - Minimo
  )
print(resumo_por_marca)



# Terceira analise

REC <- aaa %>%
  rename(Categoria = Category, Cor = Color) %>%
  mutate(Categoria = case_when(
    Categoria %>% str_detect("Men's Fashion") ~ "Moda Masculina",
    Categoria %>% str_detect("Women's Fashion") ~ "Moda Feminina",
  )) %>%
  mutate(Cor = case_when(
    Cor %>% str_detect("Black") ~ "Preto",
    Cor %>% str_detect("Blue") ~ "Azul",
    Cor %>% str_detect("Green") ~ "Verde",
    Cor %>% str_detect("Red") ~ "Vermelho",
    Cor %>% str_detect("White") ~ "Branco",
    Cor %>% str_detect("Yellow") ~ "Amarelo",
  )) %>%
  filter(Categoria %in% c("Moda Masculina", "Moda Feminina")) %>%
  group_by(Cor, Categoria) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq))
  ) %>%
  ungroup()  


REC_NA <- na.omit(REC)


porcentagens <- str_replace(REC_NA$freq_relativa, "\\.", ",")
legendas <- str_squish(paste(REC_NA$freq, " (", porcentagens, ")"))


ggplot(REC_NA) +
  aes(
    x = fct_reorder(Cor, freq, .desc = TRUE), y = freq,
    fill = Categoria, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    aes(label = legendas),  
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Cor", y = "Frequência") +
  theme_estat()





















