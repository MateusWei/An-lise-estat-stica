#imports
library(tidyverse)
library(readxl)
library(dplyr)


# recebendo os dados da planilha
df <- read_excel("sp_beaches.xlsx")


# filtrando a cidade de mongaguá e agrupando por praias
df %>% 
  filter(City == "MONGAGUÁ") %>% 
  mutate(Enterococcus = as.numeric(Enterococcus)) %>% 
  group_by(Beach) -> df

df
# ------------------------ Questao 01 ------------------------
df %>% summarise(media = mean(Enterococcus),
                 desvio = sd(Enterococcus),
                 mediana = median(Enterococcus),
                 q1 = quantile(Enterococcus,prob=c(.25), type=1),
                 q3 = quantile(Enterococcus,prob=c(.75), type=1),
                 min = min(Enterococcus),
                 max = max(Enterococcus))


# ------------------------ Questao 02 ------------------------
df %>% summarise(Amostras = sum(Enterococcus)) %>%
  mutate(Beach = fct_reorder(Beach, desc(Amostras)),porcent = prop.table(Amostras)) %>% 
  ggplot(aes(Beach, Amostras, fill=Beach)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label = scales::percent(porcent)),
            vjust = +2)


# ------------------------ Questao 03 ------------------------
df %>% summarise(Amostras = sum(Enterococcus)) %>% 
  arrange(desc(Beach)) %>%
  mutate(porcent = prop.table(Amostras)) %>% 
  mutate(newpos = porcent/sum(porcent)*7) %>% 
  mutate(ypos = cumsum(newpos)-0.5*newpos) %>% 
  ggplot(aes(x="", y="Amostras",fill=Beach)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  geom_text(aes(y=ypos,label=scales::percent(porcent)),color="black")+
  scale_fill_brewer(palette="Set1")


# ------------------------ Questao 04 ------------------------
# para ver os graficos separados por praia descomente o "facet-wrap"
df %>%  ggplot(aes(Enterococcus)) +
  geom_histogram(bins=30,color="darkblue", fill="lightblue") +
  #facet_wrap(~ Beach, scales = "free", ncol = 2) +
  theme_bw() +
  labs(fill = "")


# ------------------------ Questao 05 ------------------------
df %>% ggplot(aes(x=Beach, y=Enterococcus, color=Beach)) +
  theme_minimal() +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    dotsize = 1,
    binwidth = 2
  )


