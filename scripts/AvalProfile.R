#Leitura de arquivos do profile.json
#Resumo de Scripts para leitura do arquivo

library(tidyverse); library(jsonlite); library(listviewer)

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("scripts/elattes.ls2df.R")

#Definir o local onde estão os arquivos json
setwd("~/Documents/eLattes/Pacote e-Lattes/UnBPosGeral")

#Ler o arquivo profile
profile <- jsonlite::fromJSON("unbpos.profile.json")

#Visualizar o arquivo no formato list
listviewer::jsonedit(profile)

#número de arquivos / pessoas / pesquisadores
length(profile)

# Nome do Pesquisador #####
# Nome de um pesquisador específico ou de um conjunto de pesquisadores
profile[[1]]$nome
profile$`0000507838194708`$nome

head(sapply(profile, function(x) (x$nome)), 10)

ids <- c("0000507838194708", "0010973626622666", "0017467628165816", "0029536556461484")
sapply(profile[(names(profile) %in% ids)], function(x) (x$nome))

for (i in 1:5){
  print(names(profile[i]))
  print(profile[[i]]$nome)
}

# Resumos #####
# Resumos do currículo de um pesquisador ou de um conjunto
profile[[1]]$resumo_cv
profile$`0000507838194708`$resumo_cv

head(sapply(profile, function(x) (x$resumo_cv)), 2)

ids <- c("0000507838194708", "0010973626622666", "0017467628165816")
sapply(profile[(names(profile) %in% ids)], function(x) (x$resumo_cv))

for (i in 1:5){
  print(names(profile[i]))
  print(profile[[i]]$resumo_cv)
}

# Areas de Atuação #####
# Nome dos subitens de "areas_de_atuação"
names(profile[[1]]$areas_de_atuacao)

# Nomes e contagem dos itens armazenados em cada subitem
unique(unlist(sapply(profile, function(x) (x$areas_de_atuacao$grande_area))))
profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$grande_area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% as.data.frame() %>% filter(!. == "")

length(unique(unlist(sapply(profile, function(x) 
  x$areas_de_atuacao$area))))
profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "")

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "") %>% head(40)

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "") %>% head(40)

# Avaliação de Areas de conjunto de pesquisadores
profile[[1]]$areas_de_atuacao
    
    #com repetição de itens
head(sapply(profile, function(x) unlist(x$areas_de_atuacao$area)), 2)

ids <- c("0000507838194708", "0010973626622666", "0011938955607677", "0037568024594010")
    #sem repetição de itens
sapply(profile[(names(profile) %in% ids)], function(x) unique(x$areas_de_atuacao$grande_area))
    #em outro formato usando magrittr
profile[(names(profile) %in% ids)] %>% sapply(function(x) (x$areas_de_atuacao$grande_area)) %>% sapply(unique)

#Número de "areas_de_atuação" Especialidade... por pessoa
profile[(names(profile) %in% ids)] %>% 
  sapply(function(x) unlist(x$areas_de_atuacao$especialidade)) %>% 
  sapply(unique) %>% 
  sapply(length)
  
# Número de "areas_de_atuação" acumulado
profile %>% 
  sapply(function(x) unlist(x$areas_de_atuacao$especialidade)) %>% 
  sapply(unique) %>% 
  sapply(length) %>% 
  table()

# Endereço Profissional #####
## Nome dos subitens de "endereco_profissional"
names(profile[[1]]$endereco_profissional)

  #lista de instituições nos currículos
unique(unlist(sapply(profile, function(x) (x$endereco_profissional$instituicao))))
  #número de currículos com instituicao com Universidade de Brasília
profile %>% 
  sapply(function(x) 
    (x$endereco_profissional$instituicao)) %in% "Universidade de Brasília" %>% 
  sum()

  #Lista de Unidades
head(sort(unique(unlist(sapply(profile, function(x) 
  (x$endereco_profissional$unidade)))), decreasing = TRUE), 40)

  #Contagem de Orgãos / Institutos
head(sort(table(unlist(sapply(profile, function(x) 
  (x$endereco_profissional$orgao)))), decreasing = TRUE), 20)

# Produção Bibliográfica #####
#Análise da Produção Bibliográfica
  #Todos os tipos de produção
unique(unlist(sapply(profile, function(x) names(x$producao_bibiografica))))

  # Media de produção por pesquisador
  # Aqui é usado a media pois existem duplicações caso pesquisadores tenham escrito o mesmo artigo
(profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza)) %>% 
  sum()) / length(profile)

(profile %>% 
    sapply(function(x) 
      length(x$producao_bibiografica$EVENTO$natureza)) %>% 
    sum()) / length(profile)

(profile %>% 
    sapply(function(x) 
      length(x$producao_bibiografica$LIVRO$natureza)) %>% 
    sum()) / length(profile)

  # Número de pessoas por quantitativo de produções
profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza)) %>% 
  unlist() %>% table()

profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$EVENTO$natureza)) %>% 
  unlist() %>% table()

  # Nome de pesquisadores com quantitativo de produção específica (20)
profile[(profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza))) %in% 20] %>% 
  sapply(function(x) (x$nome))

  # Nome de pesquisadores com busca por palavra em título de publicação
profile[profile %>% 
    sapply(function(x) 
      (x$producao_bibiografica$PERIODICO$titulo)) %>% 
  grepl(pattern = "GDF")] %>% 
  sapply(function(x) (x$nome))

  # Quantitativo de eventos por pais
profile%>% 
  sapply(function(x)
    (x$producao_bibiografica$EVENTO$pais_do_evento)) %>% 
  unlist() %>% summary()
  ggplot(aes())

  # Nome de pesquisadores com busca por pais de evento
profile[profile %>% 
          sapply(function(x) 
            (x$producao_bibiografica$EVENTO$pais_do_evento)) %>% 
          sapply('%in%', "Romênia") %>% 
          sapply(any)] %>% 
  sapply(function(x) (x$nome))

  # Número de produções em eventos por ano
profile %>% 
  sapply(function(x) 
    (x$producao_bibiografica$EVENTO$ano)) %>% 
  unlist() %>% table()
    # em outro formato para livros
table(unlist(sapply(profile, function(x) (x$producao_bibiografica$LIVRO$ano))))

  # Número de produções por ano para os 100 primeiros pesquisadores da lista
profile[1:100] %>% 
  sapply(function(x) 
    (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)) %>% 
  unlist() %>% table()

# Orientações #####
#Análise das orientações
  # Tipos de orientação
profile %>% 
  sapply(function(x)
    names(x$orientacoes_academicas)) %>% 
  unlist() %>% unique()

  # Media de orientação por pesquisador
  # Aqui é usado a media pois existem duplicações caso pesquisadores tenham orientado o mesmo trabalho
(profile %>% 
   sapply(function(x) 
     length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$natureza)) %>% 
   sum()) / length(profile)

(profile %>% 
    sapply(function(x) 
      length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$natureza)) %>% 
    sum()) / length(profile)

# Número de pessoas por quantitativo de produções
profile %>% 
  sapply(function(x) 
    length(x$orientacoes_academicas$OUTRAS_ORIENTACOES_CONCLUIDAS$natureza)) %>% 
  unlist() %>% table()

# Número de pessoas por tipo de orientação
profile %>% 
  sapply(function(x) 
    names(x$orientacoes_academicas)) %>% 
  unlist() %>% table()

# Número de produções em orientações por ano
profile %>% 
  sapply(function(x) 
    (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)) %>% 
  unlist() %>% table()

# Data Frame ####
  # Análise dos dados em formato Data Frame
  # Usar as funcionalidades que estão no arquivo elatttes.ls2df.R
#Arquivo Profile por Currículo
# extrai perfis dos professores 
pr.df.pesq <- extrai.perfis(profile)
glimpse(pr.df.pesq)

# extrai producao bibliografica de todos os professores 
pr.df.pub <- extrai.producoes(profile)
glimpse(pr.df.pub)

#extrai orientacoes 
pr.df.ori <- extrai.orientacoes(profile)
glimpse(pr.df.ori)

#extrai areas de atuacao 
pr.df.areas <- extrai.areas.atuacao(profile)
glimpse(pr.df.areas)


#Eventos por país
pr.df.pub %>%
  filter(tipo_producao == 'EVENTO') %>%
  group_by(pais_do_evento) %>%
  summarise(quantidade = n()) %>%
  ggplot(aes(x = reorder(pais_do_evento, quantidade)), y = quantidade) + geom_bar(aes(color = 'blue')) + coord_flip()

str(pr.df.pub)
#cria arquivo com dados quantitativos para análise
profile.df <- data.frame()
profile.df <- pr.df.pesq %>% 
  select(idLattes, nome, resumo_cv, instituicao, orgao, unidade, cidade, senioridade) %>% 
  left_join(
    pr.df.pub %>% 
      select(tipo_producao, idLattes) %>% 
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>% 
      group_by(idLattes) %>% 
      count(tipo_producao) %>% 
      spread(key = tipo_producao, value = n), 
    by = "idLattes") %>% 
  left_join(
    pr.df.ori %>% 
      select(orientacao, idLattes) %>% 
      filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
      group_by(idLattes) %>% 
      count(orientacao) %>% 
      spread(key = orientacao, value = n), 
    by = "idLattes") %>% 
  left_join(
    pr.df.areas %>% 
      select(area, idLattes) %>% 
      group_by(idLattes) %>% 
      summarise(n_distinct(area)), 
    by = "idLattes")

glimpse(profile.df)

# Visualização ####
# Número de pessoas por area de atuação (grande área, area, sub_area, especialidade)
profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$grande_area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() + geom_text(aes(label=Freq),hjust=-0.2,vjust=0.5,size=3.5) +
  labs(title = "Número de Pessoas por Grande Área Atuação", y="Quantidade",x="Grande Área") + theme_bw() + scale_y_continuous()+
  scale_x_discrete(labels = c('CIENCIAS_DA_SAUDE' = 'Ciências da Saúde',
                              'CIENCIAS_BIOLOGICAS' = 'Ciências Biológicas',
                              'CIENCIAS_HUMANAS' = 'Ciências Humanas',
                              "CIENCIAS_EXATAS_E_DA_TERRA" = "Ciências Exatas e da Terra",
                              "CIENCIAS_SOCIAIS_APLICADAS" = "Ciências Sociais Aplicadas",
                              "CIENCIAS_AGRARIAS" = "Ciências Agrárias",
                              "OUTROS" = "Outros",
                              "ENGENHARIAS" = "Engenharias",
                              "LINGUISTICA_LETRAS_E_ARTES" = "Linguística, Letras e Artes"))

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(20) %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() +
  labs(title = "Número de Pessoas por Área Atuação",x="Área de atuação",y="Quantidade") +
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.5) +
  scale_y_continuous(limits=c(0,800))+theme_bw()

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(30) %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() + 
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.5) +
  labs(title = "Número de Pessoas por Sub Área Atuação",x="Sub Área",y="Quantidade") +
  scale_y_continuous(limits=c(0,300))+theme_bw()

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(29) %>%  
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() +
  labs(title = "Número de Pessoas por Especialidade",x="Especialidade",y="Quantidade")+
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.0)+
  theme_bw()+
  scale_y_continuous(limits=c(0,150))

# Número de áreas de atuação por pessoa (grande área, area, sub_area, especialidade)
profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$grande_area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Grande Áreas", 
       y = "Número de Pessoas", x = "Quantitativo de Grande Áreas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Áreas", 
       y = "Número de Pessoas", x = "Quantitativo de Áreas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$sub_area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Sub Áreas", 
       y = "Número de Pessoas", x = "Quantitativo de Sub Áreas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$especialidade))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange") + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Especialidades", 
       y = "Número de Pessoas", x = "Quantitativo de Especialidades") + theme_bw()

# Número de pessoas por quantitativo de produções por pessoa
profile %>% 
  sapply(function(x) length(x$producao_bibiografica$PERIODICO$ano)) %>% 
  unlist() %>% table()   %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.6) +
  labs(title = "Número de Artigos Publicados por Quantitativo de pessoas", y = "Pessoas", x = "Publicações")+
  theme_bw()+coord_flip()

profile %>% 
  sapply(function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.8) + coord_flip() + 
  labs(title = "Número de Capítulo de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publicações")+
  scale_x_discrete()+theme_bw()+geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)+scale_y_continuous(limits=c(0,500))

profile %>% 
  sapply(function(x) length(x$producao_bibiografica$LIVRO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.5) + coord_flip() + 
  labs(title = "Número de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publicações")+
  theme_bw()+geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)+scale_y_continuous(limits=c(0,400))

# Número de pessoas por quantitativo de orientações por pessoa
profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue4", width = 0.5) + coord_flip() + 
  labs(title = "Número de Orientações de Mestrado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orientaçòes") + scale_y_continuous(limits = c(0, 400))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)

profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue", width = 0.5) + coord_flip() + 
  labs(title = "Número de Orientações de Doutorado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orientaçòes") + scale_y_continuous(limits = c(0, 300))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)


profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue", width = 0.5) + coord_flip() + 
  labs(title = "Número de Orientações de Pós-Doutorado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orientaçòes") + scale_y_continuous(limits = c(0, 200))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)