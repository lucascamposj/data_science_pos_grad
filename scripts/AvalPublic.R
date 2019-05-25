#Leitura de arquivos do publication.json
#Resumo de Scripts para leitura do arquivo

library(tidyverse); library(jsonlite); library(listviewer); 
library(scales)

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("elattes.ls2df.R")

#Definir o local onde estão os arquivos json
setwd("~/Documents/eLattes/Pacote e-Lattes/UnBPosGeral")

#Ler o arquivo profile
public <- jsonlite::fromJSON("publication.json")

#Visualizar o arquivo no formato list
listviewer::jsonedit(public)

#Dados Descritivos Gerais ####
  # Tipos de objetos armazenados
names(public)

  # Periodo analisado
names(public[[1]])

  # Número de resultados (publicações e outros) por Tipo e Ano
    # Por Tipo Fixo (PERIODICO)
public$PERIODICO %>% 
  sapply(function(x) length(x$autores))

    # Por Ano Fixo (2010)
public %>% 
  sapply(function(x) length(x$"2010"$autores))

    # Por Tipo e Ano Geral
public %>% 
  sapply(function(x) 
    sapply(x, function(x) 
      length(x$autores)))

#Periodicos ####  
  #Periodicos e o seu total geral e por ano
    # Nome dos (100 primeiros) periodicos em ordem alfabética
public$PERIODICO %>% 
  sapply(function(x)
    (x$periodico)) %>% 
  unlist() %>% unique() %>% 
  sort() %>% head(100)

    # Número total de periódicos geral
public$PERIODICO %>% 
  sapply(function(x)
    (x$periodico)) %>% 
  unlist() %>% unique() %>% length()

    # Número total de periódicos por ano
public$PERIODICO %>% 
  sapply(function(x)
    length(unique(unlist(x$periodico))))

  #Periodicos mais frequentes (10) por ano
for (i in 1:length(public$PERIODICO)){
  print(names(public$PERIODICO[i]))
  public$PERIODICO[[i]]$periodico %>% 
    table() %>% sort(decreasing = TRUE) %>% 
    head(10) %>% print()
  }

  #Periodicos mais frequentes (20) geral
public$PERIODICO %>% 
  sapply(function(x) (x$periodico)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  head(20)

  #Lista de revistas
head(sort(table(public$PERIODICO$`2011`$periodico), decreasing = TRUE), 20)

  #Análise de um periodico específico (Plos One)
public$PERIODICO %>% 
  sapply(function(x)
    (x$periodico)) %>% unlist() %in% "Plos One" %>% sum()

#Livros e Capítulos ####
  # Número total de livros por ano e tipo
public$LIVRO %>% 
  sapply(function(x)
    table(x$tipo))

  # Número total de livros por ano e natureza
public$LIVRO %>% 
  sapply(function(x)
    table(x$natureza))

  # Livros publicados por Pais por ano
public$LIVRO %>% 
  sapply(function(x)
      sort(table(x$pais_de_publicacao), decreasing = TRUE))

  # Livros publicados por autor geral
public$LIVRO %>% 
  sapply(function(x)
    (x$"autores-endogeno")) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head(40)

  # Editora de publicação
public$LIVRO %>% 
  sapply(function(x)
    (x$nome_da_editora)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head(20)

#Evento ####
  # Participação em Evento por Pais por ano
public$EVENTO %>% 
  sapply(function(x)
    sort(table(x$pais_do_evento), decreasing = TRUE))

  # Participação em Evento por Cidade (40) Geral
public$EVENTO %>% 
  sapply(function(x)
    (x$cidade_do_evento)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head()

  # Nome de evento em cidade específica (Gramado)
(public$EVENTO %>% 
  sapply(function(x)
    (x$nome_do_evento)) %>% 
  unlist())[public$EVENTO %>% 
             sapply(function(x)
               (x$cidade_do_evento)) %>% 
             unlist() %in% "Gramado"]

# Formato Data Frame  ####
  # Análise dos dados no formato DF
    #Periodico = 1; Livro = 2; Capítulo = 3; Jornais = 4; 
    #Evento = 5; Artigo aceto = 6; Demais produções = 7
public.periodico.df <- pub.ls2df(public, 1) #artigos
public.livros.df <- pub.ls2df(public, 2) #livros
public.eventos.df <- pub.ls2df(public, 5) #eventos
#Publicação por ano
public.periodico.df %>% 
  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
  theme_bw()+labs(x="Ano",y="Quantidade")+
  scale_y_continuous(labels = comma)
#Publicação Zika por ano
public.periodico.df %>% filter(grepl("zika|zkv|zikv", titulo, ignore.case = TRUE)) %>%
  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
  theme_bw()+labs(x="Ano",y="Quantidade")+
  scale_y_continuous(labels = comma)
#Publicação Dengue por ano
public.periodico.df %>% filter(grepl("dengue", titulo, ignore.case = TRUE)) %>%
  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
  theme_bw()+labs(x="Ano",y="Quantidade")+
  scale_y_continuous(labels = comma)
#Publicação Chikungunya por ano
public.periodico.df %>% filter(grepl("chikungunya", titulo, ignore.case = TRUE)) %>%
  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
  theme_bw()+labs(x="Ano",y="Quantidade")+
  scale_y_continuous(labels = comma)

#20 revistas mais publicadas
#Mesma visão que anterior mas agora trabalhando no DataFrame
public.periodico.df %>% select(periodico) %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% 
  head(20) %>% ggplot(aes(x = reorder(., (Freq)), y = Freq)) + geom_col(fill = "red4") + coord_flip() + 
  labs(title = "Os 20 Periódicos com Maior Publicação", 
       y = "Número de Publicações", x = "Revistas") + geom_text(aes(label=comma(Freq)),hjust=-0.2,vjust=0.3,size=3.5)+theme_bw()+
  scale_y_continuous(limits=c(0,400))

#publicação de livros fora do Brasil
public.livros.df %>%
  group_by(pais_de_publicacao) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_de_publicacao != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_de_publicacao, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral")+ geom_text(aes(label=comma(Quantidade)),hjust=-0.2,vjust=0.3,size=3.5) + coord_flip() +
  labs(title = "Publicação de Livros em Países Estrangeiros", x = "Países", y = "Quantidade de Livros")+
  theme_bw()

public.livros.df %>%
  filter(pais_de_publicacao %in% c("Brasil", "Estados Unidos", "Holanda",
                                   "Grã-Bretanha", "Alemanha", "Suiça")) %>%
  group_by(ano,pais_de_publicacao) %>%
  ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
  xlab("Ano") + ylab("País") + geom_point() + geom_jitter()+
  labs(color="País de publicação")

#Eventos
public.eventos.df %>%
  group_by(pais_do_evento) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_do_evento != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_do_evento, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral3")+ geom_text(aes(label=comma(Quantidade)),hjust=-0.2,vjust=0.3,size=2.5)+ coord_flip() +
  labs(title = "Participação de Eventos em Países Estrangeiros", x = "Países", y = "Quantidade de Livros")+theme_bw()

public.eventos.zika.df %>%
  filter(pais_do_evento %in% 
           c(names(head(sort(table(public.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  ggtitle("Participation on Events by Country and Year") +
  xlab("Year") + ylab("Country") + geom_point() + geom_jitter()+
  labs(color="Country of The Event")