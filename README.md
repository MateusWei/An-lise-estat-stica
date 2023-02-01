# Análise Estatística
Análise estatística da qualidade da água das praias de Mongaguá em SP


# Contextualização
Leia o relatório feito em R Markdown (ou continue lendo este sem edição e imagens), o qual apresenta as estatísticas e os gráficos obtidos apartir das informações fornecidas. Tais informações envolvem a observação de vários dias da quantidade de enterococcus presentes nas águas das praias de Mongaguá em São Paulo.


### Importando bibliotecas
Aqui estamos importando algumas bibliotecas necessárias para rodar o código e o template do markdown.

```{r, warning=FALSE, message=FALSE}
library(tidyverse)
library(readxl)
library(dplyr)
library(kableExtra)
```

### Lendo os dados da planilha
Foi nos fornecidos uma planilha contendo as informações da qualidade da água de diversas praias das cidades de São Paulo.
```{r}
df <- read_excel("sp_beaches.xlsx")
```

### Agrupando as praias de Mongaguá
Aqui estamos pegando as informações referente apenas à cidade de Mongaguá, para assim termos um nicho menor a ser estudado.
```{r}
df %>% 
  filter(City == "MONGAGUÁ") %>% 
  mutate(Enterococcus = as.numeric(Enterococcus)) %>% 
  group_by(Beach) -> df
```

# Questão 1
Apartir da analise da tabela podemos ver que o valor mínimo de enterococcus presentes nas praias foi de 1, enquanto o máximo ficou em torno dos 1000 e que a mediana ficou bastante perto do mínimo (beirando os 20). Com isso, podemos ter a certeza que possuimos um desvio muito grande pela parte dos valores máximos, os quais estão muito longe da mediana. Além disso, podemos também dizer que estas águas estão impróprias para banho :: [Praia Imprópria](https://noticias.uol.com.br/saude/ultimas-noticias/redacao/2017/12/27/sp-e-rio-tem-72-praias-improprias-desrespeitar-alertas-e-um-risco-a-saude.htm).
(obs: atribuímos o resultado a uma nova variável newDf apenas para criar a tabela no Markdown)
```{r}
df %>% summarise(media = mean(Enterococcus),
                 desvio = sd(Enterococcus),
                 mediana = median(Enterococcus),
                 q1 = quantile(Enterococcus,prob=c(.25), type=1),
                 q3 = quantile(Enterococcus,prob=c(.75), type=1),
                 min = min(Enterococcus),
                 max = max(Enterococcus)) -> newDf
```

# Questão 2
Assim como este gráfico, no gráfico da questão 3 também analisamos a relação da porcentagem de enterococcus presente entre as praias. Dessa forma, olhando para este gráfico em barra, conseguimos ver claramente que a praia de Agenor de campos é a que mais possui concentração de enterococcus, enquanto a de Flórida mirim é a que possui menos.
```{r}
df %>% summarise(Amostras = sum(Enterococcus)) %>%
  mutate(Beach = fct_reorder(Beach, desc(Amostras)),porcent = prop.table(Amostras)) %>% 
  ggplot(aes(Beach, Amostras, fill=Beach)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label = scales::percent(porcent)),
            vjust = +2)
```

# Questão 3
Comparando agora este gráfico em pizza com o gráfico em barra da questão acima, podemos ver que para essa situação este tipo de gráfico não contribui para visualização das estátisticas, uma vez que não conseguimos ver de cara qual é a que possui mais concentração e a que possui menos sem ver as legendas.
```{r}
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
```

# Questão 4 {.tabset .tabset-fade .tabset-pills}
Podemos ver aqui um gráfico no formato de histograma, tanto no total das praias como pegando cada praia e analisando ela por si própria (clicando em "individuais").

## Todos

```{r}
df %>%  ggplot(aes(Enterococcus)) +
  geom_histogram(bins=30,color="darkblue", fill="lightblue") +
  #facet_wrap(~ Beach, scales = "free", ncol = 2) +
  theme_bw() +
  labs(fill = "")
```

## Individuais

```{r}
df %>%  ggplot(aes(Enterococcus)) +
  geom_histogram(bins=30,color="darkblue", fill="lightblue") +
  facet_wrap(~ Beach, scales = "free", ncol = 2) +
  theme_bw() +
  labs(fill = "")
```

# Questão 5
Como podemos ver nos gráficos de modelo boxplot, a mediana se encontra bem próximo do primeiro quartil, ou seja, podemos dizer que os dados são assimétricos positivos. Além disso, podemos ver que todos eles possuem bastantes Outliers, ou seja, valores discrepantes em relação a mediana.
```{r}
df %>% ggplot(aes(x=Beach, y=Enterococcus, color=Beach)) +
  theme_minimal() +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    dotsize = 1,
    binwidth = 2
  )
```


