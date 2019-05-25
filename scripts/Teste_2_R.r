#Teste 2 DS4A - Trabalhando com Listas
#Pacotes para serem utilizados nos scripts abaixo
library(tidyverse); library(jsonlite); library(listviewer);

#Ler o arquivo profile
profile <- jsonlite::fromJSON("comp_aplicada/profile.json")
public <- jsonlite::fromJSON("comp_aplicada/publication.json")
advise <- jsonlite::fromJSON("comp_aplicada/advise.json")

# Profile ####
# 1.Na grande área Ciências Humanas indique as duas áreas, 
# as duas sub-áreas e as duas especialidades mais frequentes.
g.area <- profile %>% 
  sapply(function(x) 
    (x$areas_de_atuacao$grande_area)) %>% 
  unlist() %in% "CIENCIAS_HUMANAS"

profile %>% 
  sapply(function(x) (x$areas_de_atuacao$area)) %>% 
  unlist() %>% subset(g.area) %>% table() %>% 
  sort(decreasing = TRUE) %>%   as.data.frame() %>% 
  filter(!. == "") %>% head(2)

profile %>% 
  sapply(function(x) (x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% subset(g.area) %>% table() %>% 
  sort(decreasing = TRUE) %>%   as.data.frame() %>% 
  filter(!. == "") %>% head(2)

profile %>% 
  sapply(function(x) (x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% subset(g.area) %>% table() %>% 
  sort(decreasing = TRUE) %>%   as.data.frame() %>% 
  filter(!. == "") %>% head(2)

#2. Quais os locais e número de eventos que as 10 pessoas com mais publicação participaram
top.pub <- profile %>% 
  sapply(function(x) 
    unlist(x$producao_bibiografica$PERIODICO$titulo)) %>% 
  sapply(unique) %>% sapply(length) %>% 
  sort(decreasing = TRUE) %>% head(10) %>% names()

profile[top.pub] %>% 
  sapply(function(x)
    (x$producao_bibiografica$EVENTO$pais_do_evento)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE)

#3. Dos professores que possuem grande área nas Engenharias qual 
# o número de trabalhos concluidos de mestrado no curso de Engenharia Elétrica
g.area <- profile %>% 
  sapply(function(x) 
    any(x$areas_de_atuacao$grande_area %in% "ENGENHARIAS")) %>% 
  which() %>% names()

profile[g.area] %>% 
  sapply(function(x) 
    (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$curso)) %>% 
  unlist() %in% "Engenharia Elétrica" %>% which() %>% length()

#4. Qual o número medio de áreas de atuação para os pesquisadores com grande área 
# Ciências da Saúde
g.area <- profile %>% 
  sapply(function(x) 
    any(x$areas_de_atuacao$grande_area %in% "CIENCIAS_DA_SAUDE")) %>% 
  which() %>% names()

profile[(names(profile) %in% g.area)] %>% 
  sapply(function(x) unlist(x$areas_de_atuacao$area)) %>% 
  sapply(unique) %>% 
  sapply(length) %>% mean()

#5. Qual as grandes áreas dos pesquisadores que publicaram algum artigo que contenha
# a palavra "saúde" no título
t.public <- profile[profile %>% 
          sapply(function(x) 
            (x$producao_bibiografica$PERIODICO$titulo)) %>% 
          grepl(pattern = "saúde")] %>% names()

profile[t.public] %>% 
  sapply(function(x) (x$areas_de_atuacao$grande_area)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>%   as.data.frame() %>% 
  filter(!. == "")

#6. Dos pesquisadores que publicaram mais de 10 artigos 
# quantos são da grande área Ciências Exatas e da Terra
top.pub <- (profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza))) >= 10

profile[top.pub] %>% 
  sapply(function(x) 
    any(x$areas_de_atuacao$grande_area %in% "CIENCIAS_EXATAS_E_DA_TERRA")) %>% 
  sum()

#7. Quantos eventos internacionais na américa do sul participaram os 
# pesquisadores da grande área Ciências da Saúde
g.area <- profile %>% 
  sapply(function(x) 
    any(x$areas_de_atuacao$grande_area %in% "CIENCIAS_DA_SAUDE"))

profile[g.area] %>% 
  sapply(function(x)
    (x$producao_bibiografica$EVENTO$pais_do_evento)) %>% 
  unlist() %>% sapply('%in%', c("Argentina","Chile","Colômbia",
                   "Uruguai","Equador", "Guiana Francesa",
                   "Peru", "Venezuela")) %>% sum()

#8. Qual o número de orientações de doutorado concluidas
# das pessoas que orientaram pos-doutorados já concluídos
t.orient <- profile %>% 
  sapply(function(x)
    any(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$natureza %in%
      "Supervisão de pós-doutorado")) %>% which() %>% names()

profile[t.orient] %>% 
  sapply(function(x) 
    (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$natureza)) %>% 
  unlist() %>% length()

#9. Qual o número de orientações de mestrado em andamento
# divididos por pesquisadores de grande área
areas <- profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$grande_area)) %>% 
  unlist() %>% unique()

for (i in areas){
  print (i)
  print(profile[profile %>% 
            sapply(function(x) 
              any(x$areas_de_atuacao$grande_area %in% i))] %>% 
      sapply(function(x)
          length(x$orientacoes_academicas$ORIENTACAO_EM_ANDAMENTO_MESTRADO$natureza)) %>% 
            sum())    
}

# Publication ####
# 1. Quais são os primeiros 5 artigos em ordem alfabética dos 2 
#periódicos mais frequentes nos anos de 2017
top.periodico <- public$PERIODICO$`2017`$periodico %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  head(2) %>% names()

top.count <- public$PERIODICO$`2017`$periodico %>%
  unlist() %in% top.periodico[1]

    (public$PERIODICO$`2017`$titulo %>% 
      unlist())[top.count] %>% sort() %>% 
      head(5)

top.count <- public$PERIODICO$`2017`$periodico %>%
  unlist() %in% top.periodico[2]

    (public$PERIODICO$`2017`$titulo %>% 
        unlist())[top.count] %>% sort() %>% 
      head(5)

#2. Qual o nome dos autores dos livros publicados em 2016 na Argentina e na França
public$LIVRO$`2016`$autores[
  public$LIVRO$`2016`$pais_de_publicacao %>% unlist() %in% c("Argentina","França")
] %>% unlist()

#3. Quais as cidades que tiveram participação de pesquisadores em 10 eventos
# e o nome dos eventos na primeira dessas cidades em ordem alfabética
cidades <- (public$EVENTO %>% 
  sapply(function(x) (x$cidade_do_evento)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) == 10) %>% 
  which() %>% names()

(public$EVENTO %>% 
  sapply(function(x) (x$nome_do_evento)) %>% unlist())[
    public$EVENTO %>% 
      sapply(function(x) (x$cidade_do_evento)) %>% unlist() %in% cidades[1]
  ]

# Advise ####
#1. Qual o número de orientações concluídas por tipo (mestrado, doutorado e pós-doutorado)
# e por ano?
concluidas <- advise %>% names() %>% grepl(pattern = "CONCLUIDA")

for (i in names(advise[concluidas])){
  print(i)
  print(advise[[i]] %>% 
    sapply(function(x) length(x$natureza)))
}

#2. Para Outras Orientações Concluídas apresente o Total de 2012 a 2014 de suas 
# subcategorias
for (i in c("2012", "2013", 2014)){
    cat(i)
    print(advise$OUTRAS_ORIENTACOES_CONCLUIDAS[[i]]$natureza %>% length)
}

#3. Com a exceção da Universidade de Brasília, qual o número total de orientações de Doutorado
# no ano de 2011 a 2014 para cada Universidade do Centro Oeste?
universidades <- advise$ORIENTACAO_CONCLUIDA_DOUTORADO %>% 
  sapply(function(x)
      (x$instituicao)) %>% 
  unlist() %>% unique() %>% sort()
  
uni <- c(universidades[grepl(universidades, pattern = "Mato Grosso")],
          universidades[grepl(universidades, pattern = "Goiás")])

for (i in c("2011", "2012", "2013", "2014")){
  for (j in uni){
    cat(i, j)
    (advise$ORIENTACAO_CONCLUIDA_DOUTORADO[[i]]$instituicao == j) %>% 
      sum() %>% print()
  }
}
