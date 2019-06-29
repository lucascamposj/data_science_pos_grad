#Leitura dos arquivos formato graph e análise dos dados

library(tidyverse); library(jsonlite); library(listviewer); library(igraph)

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("elattes.ls2df.R")

#Ler o arquivo profile e transformar no formato igraph
graph <- jsonlite::fromJSON("cic/graph.json")
g <- g.ls2ig(graph)

#Cria arquivo com quantitativo de dados para adicionar ao grafo
#Arquivo Profile por Currículo
# extrai perfis dos professores 
profile <- jsonlite::fromJSON("cic/profile.json")
str(profile)
perfil.df.professores <- extrai.perfis(profile)
glimpse(perfil.df.professores)

# extrai producao bibliografica de todos os professores 
perfil.df.publicacoes <- extrai.producoes(profile)
glimpse(perfil.df.publicacoes)

#extrai orientacoes 
perfil.df.orientacoes <- extrai.orientacoes(profile)
glimpse(perfil.df.orientacoes)

#extrai areas de atuacao 
perfil.df.areas.de.atuacao <- extrai.areas.atuacao(profile)
glimpse(perfil.df.areas.de.atuacao)

#cria arquivo com dados quantitativos para análise
perfil.df <- data.frame()
perfil.df <- perfil.df.professores %>% 
  select(idLattes, nome, resumo_cv, senioridade) %>% 
  left_join(
    perfil.df.orientacoes %>% 
      select(orientacao, idLattes) %>% 
      filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
      group_by(idLattes) %>% 
      count(orientacao) %>% 
      spread(key = orientacao, value = n), 
    by = "idLattes") %>% 
  left_join(
    perfil.df.publicacoes %>% 
      select(tipo_producao, idLattes) %>% 
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>% 
      group_by(idLattes) %>% 
      count(tipo_producao) %>% 
      spread(key = tipo_producao, value = n), 
    by = "idLattes") %>% 
  left_join(
    perfil.df.areas.de.atuacao %>% 
      select(area, idLattes) %>% 
      group_by(idLattes) %>% 
      summarise(n_distinct(area)), 
    by = "idLattes")

glimpse(perfil.df)

#Adicionando informações ao grafo
V(g)$orient_dout <- perfil.df$ORIENTACAO_CONCLUIDA_DOUTORADO
V(g)$orient_mest <- perfil.df$ORIENTACAO_CONCLUIDA_MESTRADO
V(g)$publicacao <- perfil.df$PERIODICO
V(g)$eventos <- perfil.df$EVENTO

V(g)$degree <- degree(g)
V(g)$betweenness <- betweenness(g, normalized = TRUE)
V(g)$closeness <- closeness(g, normalized = TRUE)
V(g)$eigen <- eigen_centrality(g)$vector
V(g)$cluster <- cluster_leading_eigen(g)$membership

#Escrever o grafo para leitura em outro software
write.graph(g, "graph.lattes.graphml", format = "graphml")

#Filtrando somente os pesquisadores com Grande Área Ciências Exatas e da Terra
sub.graph <- (perfil.df.areas.de.atuacao %>% 
  filter(grande_area == "CIENCIAS_EXATAS_E_DA_TERRA") %>% 
  select(idLattes) %>% unique())[,1]

g2 <- induced_subgraph(g, sub.graph)

plot(V(g))
glimpse(V(g)$orient_dout)
