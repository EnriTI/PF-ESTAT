vendas <- read.csv("vendas.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(xtable)
library(scales)


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


#Analise 1 (certo)



vendas$Data.Venda = mdy(vendas$Data.Venda)

vendas$Meses <- month(vendas$Data.Venda)

# vendas$Meses = month.name[vendas$Meses]

mes_categoria <- vendas %>%
  select(Meses, Unique.ID, Category, Price) %>%
  rename(Moda = Category) %>%
  mutate(
    Moda = case_when(
      str_detect(Moda, "Men's Fashion") ~ "Masculina",
      str_detect(Moda, "Women's Fashion") ~ "Feminina",
      str_detect(Moda, "Kids' Fashion") ~ "Infantil",
      TRUE ~ as.character(Moda)
    )
  )

mes_categoria <- na.omit(mes_categoria)

mes_categoria <- distinct(mes_categoria, Unique.ID, .keep_all = TRUE)


mes_categoria$Meses <- as.factor(mes_categoria$Meses)

mes_categoria$Meses <- factor(mes_categoria$Meses,
                              levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                              labels =  c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul","Ago", "Set", "Out", "Nov", "Dez")
)




vendas_por_mes_categoria <- mes_categoria %>%
  group_by(Meses, Moda) %>%
  summarise(Faturamento = sum(Price, na.rm = TRUE))


# vendas_por_mes_categoria$Meses <- factor(vendas_por_mes_categoria$Meses, levels = month.name, ordered = TRUE)

# vendas_por_mes_categoria <- vendas_por_mes_categoria[order(vendas_por_mes_categoria$Meses), ]



ggplot( vendas_por_mes_categoria ) +
  aes(x = Meses , y = Faturamento , group = Moda , colour = Moda ) +
  geom_line(linewidth = 1) +
  geom_point (size = 2) +
  labs(x = "Mêses", y = "Faturamento") +
  theme_estat ()

ggsave("hist_uni_porc.pdf", width = 158, height = 93, units = "mm")

#Analise 2 (certo)


Variacao <- vendas %>%
  select(Unique.ID, Brand, Price)

Variacao <- na.omit(Variacao)

Variacao <- distinct(Variacao, Unique.ID, .keep_all = TRUE)


ggplot(Variacao) +
  aes(x = Brand , y = Price) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs( x = "Marca", y = "Preço") +
  theme_estat()

ggsave ("box_bi.pdf", width = 158, height = 93, units = "mm")

quadro_resumo <- Variacao %>% 
  group_by(Brand) %>% 
  summarize(Média = round(mean(Price),2),
            `Desvio Padrão` = round(sd(Price),2),
            `Variância` = round(var(Price),2),
            `Mínimo` = round(min(Price),2),
            `1º Quartil` = round(quantile(Price, probs = .25),2),
            Mediana = round(quantile(Price, probs = .5),2),
            `3º Quartil` = round(quantile(Price, probs = .75),2),
            `Máximo` = round(max(Price),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)


#Analise 3 (certo?)


Moda = vendas %>%
  select(Unique.ID,Color,Category)

Moda = na.omit(Moda)

Moda = distinct(Moda, Unique.ID, .keep_all = TRUE)


REC <- Moda %>%
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


porcentagens <- str_replace(REC$freq_relativa, "\\.", ",")
legendas <- str_squish(paste(REC$freq, " (", porcentagens, ")"))



ggplot(REC) +
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

ggsave("hist_uni_porc.pdf", width = 158, height = 93, units = "mm")

#Analise 4 (certo)


Rel <- vendas %>%
  select(Unique.ID, Price, Rating) %>%
  rename(Preço = Price, Avaliação = Rating)

Rel <- na.omit(Rel)

Rel = distinct(Rel, Unique.ID, .keep_all = TRUE)


ggplot(Rel) +
  aes(x = Preço ,y = Avaliação) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço",
    y = "Avaliação"
  ) +
  theme_estat()

ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")

cor_test_result <- cor.test(Rel$Preço, Rel$Avaliação, method = "pearson")

print(cor_test_result)

#Analise 5 (certo)


devolucao = read.csv("devolução_atualizado.csv")


devo = vendas %>%
  select(Unique.ID,Motivo.devolução,Brand)


devo = distinct(devo, Unique.ID, .keep_all = TRUE)

devo = na.omit(devo)


novo_banco <- merge(devolucao, devo, by = "Unique.ID", all = TRUE)

novo_banco <- novo_banco[, !names(novo_banco) %in% "Motivo.devolução.y"]

novo_banco <- novo_banco %>% 
  rename(Motivo.devolução = Motivo.devolução.x)

novo_banco = distinct(novo_banco, Unique.ID, .keep_all = TRUE)
 

ueto <- novo_banco %>%
  na.omit() %>%
  group_by(Brand, Motivo.devolução) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq))
  ) %>%
  ungroup()  


porcentagens <- str_replace(ueto$freq_relativa, "\\.", ",")
legendas <- str_squish(paste(ueto$freq, " (", porcentagens, ")"))


ggplot(ueto) +
  aes(
    x = fct_reorder(Brand, freq, .desc = TRUE), y = freq,
    fill = Motivo.devolução, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    aes(label = legendas),  
    position = position_dodge(width = 0.9),
    vjust = -0.09, hjust = -0.2,
    size = 3
  ) +
  labs(x = "Marca", y = "Frequência") +
  theme_estat() +
  coord_flip() +
  ylim(0, max(vapovapo$freq) * 1.2)


#Analise 6 (certo?)

momo = vendas %>%
  select(Unique.ID,Rating,Brand)

momo = distinct(momo, Unique.ID, .keep_all = TRUE)

momo = na.omit(momo)


media_por_marca <- momo %>%
  group_by(Brand) %>%
  summarise(media = mean(Rating))


ggplot(momo, aes(x = Brand, y = Rating)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#A11D21", color = "black") +
  geom_text(data = media_por_marca, aes(label = sprintf("%.2f", media), y = media + 0.1), 
            position = position_dodge(width = 0.9), vjust = 0) +
  labs(
       x = "Marca",
       y = "Avaliação Média") +
  theme_estat()

















