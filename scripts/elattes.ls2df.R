# Funcoes para transformar arquivos do e-Lattes no formato lista em Data Frame ou igraph

#################################
#Arquivo Publication
#Entrar com dois parâmetros. 
#O primeiro é o arquivo list e o segundo o tipo de produção desejada
pub.ls2df <- function(jsonPublicacoes, tipo) {
  public.df <- data.frame()
  if (tipo == 1) x <- 'PERIODICO'
  else if (tipo == 2) x <- 'LIVRO'
  else if (tipo == 3) x <- 'CAPITULO_DE_LIVRO'
  else if (tipo == 4) x <- 'TEXTO_EM_JORNAIS'
  else if (tipo == 5) x <- 'EVENTO'
  else if (tipo == 6) x <- 'ARTIGO_ACEITO'
  else if (tipo == 7) x <- 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA'
  for (i in 1:length(jsonPublicacoes[[1]]))
    public.df <- rbind(public.df, jsonPublicacoes[[x]][[i]])
  # Limpando o data-frame de listas
  public.df$autores <- gsub("\",\"|\", \"", "; ", public.df$autores)
  public.df$autores <- gsub("\"|c\\(|\\)", "", public.df$autores)
  public.df$`autores-endogeno` <- gsub(",", ";", public.df$`autores-endogeno`)
  public.df$`autores-endogeno` <- gsub("\"|c\\(|\\)", "", public.df$`autores-endogeno`) 
  return(public.df)
}

#################################
#Arquivo Advise de Orientações
#Entrar com dois parâmetros. 
#O promeiro é o arquivo list e o segundo o tipo de orientação desejada
ori.ls2df <- function(lsOrient, tipo) {
  orient.tipo.df <- data.frame(); orient.df <- data.frame()
  if(tipo == 1) x <- 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'    
  else if(tipo == 2) x <- 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO'           
  else if(tipo == 3) x <- 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'            
  else if(tipo == 4) x <- 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO'           
  else if(tipo == 5) x <- 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'
  else if(tipo == 6) x <- 'ORIENTACAO_CONCLUIDA_POS_DOUTORADO'          
  else if(tipo == 7) x <- 'ORIENTACAO_CONCLUIDA_DOUTORADO'              
  else if(tipo == 8) x <- 'ORIENTACAO_CONCLUIDA_MESTRADO'               
  else if(tipo == 9) x <- 'OUTRAS_ORIENTACOES_CONCLUIDAS'
  for (i in 1:length(lsOrient[[1]]))
    orient.tipo.df <- rbind(orient.tipo.df, lsOrient[[x]][[i]])
  orient.df <- rbind(orient.df, orient.tipo.df); orient.tipo.df <- data.frame()
  
  #Transformar as colunas de listas em caracteres eliminando c("")
  orient.df$nome_orientadores <- gsub("\"|c\\(|\\)", "", orient.df$nome_orientadores)
  orient.df$id_lattes_orientadores <- gsub("\"|c\\(|\\)", "", orient.df$id_lattes_orientadores)
  
  #Separar as colunas com dois orientadores
  orient.df <- separate(orient.df, nome_orientadores, into = c("ori1", "ori2"), sep = ",")
  orient.df <- separate(orient.df, id_lattes_orientadores, into = c("idLattes1", "idLattes2"), sep = ",")
  
  return(orient.df)
}

#################################
#Arquivo Graph
g.ls2ig <- function(grafo) {
  g <- graph_from_data_frame(grafo$links, directed=FALSE, 
                             vertices = grafo$nodes)
  g <- delete_edge_attr(g, name = "weigth")
  g <- delete_vertex_attr(g, name = "properties")
  E(g)$weigth <- as.numeric(grafo$links$weigth)
  V(g)$label <- as.vector(grafo$nodes$properties[,1])
  graph_attr(g, "name") <- "Rede de Colaboração"
  return(g)
}

#################################
#Arquivo Researchers by area
#Transforma em um Data Frame booleano com varáveis como áreas e observações como Pesquisadores
area.ls2df <- function(jsonAreas) {
  x <- c()
  for(i in 1:length(jsonAreas$`Areas dos pesquisadores`))
    x <- c(x, jsonAreas$`Areas dos pesquisadores`[[i]])
  x <- sort(unique(x))
  df <- as.data.frame(matrix(0, nrow = length(x), ncol = length(jsonAreas$`Areas dos pesquisadores`),
                             dimnames = list(NULL, names(jsonAreas$`Areas dos pesquisadores`))),
                      row.names = x)
  for(i in 1:length(jsonAreas$`Areas dos pesquisadores`))
    df[i] <- as.numeric(x %in% jsonAreas$`Areas dos pesquisadores`[[i]])
  return(df)
}



#################################
#Arquivo Profile
# converte as colunas de um dataframe tipo lista em tipo character
cv.tplista2tpchar <- function( df  ) { 
  for( variavel in names(df)) {
    if (class(df[[variavel]]) == "list" ) {
      df[[variavel]] <- lapply(df[[variavel]] ,   function(x)   lista2texto( x  ) ) 
      df[[variavel]] <- as.character( df[[variavel]] )
    }
  }
  return(df)
}
###

# converte o conteudo de lista em array de characters
lista2texto <- function( lista  ) {
  if(is.null(lista)) {
    return ( NULL )
  }
  saida <- ""
  for( j in 1:length(lista)) { 
    for( i in 1:length(lista[[j]]) ) {
      elemento <- lista[[j]][i] 
      if( !is.null(elemento)) { 
        if( i == length(lista[[j]]) & j == length(lista)  ) { 
          # se for o ultimo elemento nao coloque o ponto e virgula no final            
          saida <- paste0( saida , elemento  )
        } else {
          # enquanto nao for o ultimo coloque ; separando os elementos concatenados 
          saida <- paste0( saida , elemento , sep = " ; ")
        }
      }  
    }
  }
  return( saida )
}

# Converte producao elattes separada por anos em um unico dataframe 
converte_producao2dataframe<- function( lista_producao ) {
  df.saida <- NULL 
  
  for( ano in names(lista_producao)) {
    df.saida <- rbind(df.saida , lista_producao[[ano]])
  }
  
  # converte tipo lista em array de character 
  df.saida <- cv.tplista2tpchar(df.saida)
  return(df.saida)
}

#concatena dois dataframes com  colunas diferentes 
concatenadf <- function( df1, df2) { 
  #cria colunas de df1 que faltam em df2
  for( coluna in names(df1 ) ) {
    if( !is.element(coluna, names(df2) )) {
      df2[coluna] <- NA
    }
  }
  #cria colunas de df2 que faltam em df1  
  for( coluna in names(df2 ) ) {
    
    if( !is.element(coluna, names(df1) )) {
      df1[coluna] <- NA
    }
  }
  #faz o rbind dos dois dataframes 
  df_final <- rbind(df1 , df2)
  return(df_final)
}

# Extracao dos perfis dos professores 

extrai.1perfil <- function( professor ) {
  idLattes <- names(professor)
  nome <- professor[[idLattes]]$nome   
  resumo_cv <- professor[[idLattes]]$resumo_cv 
  endereco_profissional <- professor[[idLattes]]$endereco_profissional #list 
  instituicao <- endereco_profissional$instituicao
  orgao <- endereco_profissional$orgao
  unidade <- endereco_profissional$unidade
  DDD <- endereco_profissional$DDD
  telefone <- endereco_profissional$telefone
  bairro <- endereco_profissional$bairro
  cep <- endereco_profissional$cep
  cidade <- endereco_profissional$cidade
  senioridade <- professor[[idLattes]]$senioridade  
  df_1perfil <- data.frame( idLattes , nome, resumo_cv ,instituicao , 
                            orgao, unidade , DDD, telefone, bairro,cep,cidade , senioridade,
                            stringsAsFactors = FALSE)
  return(df_1perfil)  
}

extrai.perfis <- function(jsonProfessores) {
  df.saida <- data.frame()
  for( i in 1:length(jsonProfessores)) {
    jsonProfessor <- jsonProfessores[i]
    df_professor <- extrai.1perfil(jsonProfessor)
    if( nrow(df.saida) > 0 ) {
      df.saida <- rbind(df.saida , df_professor)
    } else {
      df.saida <- df_professor 
    }
  }
  return(df.saida)
}

# Extracao da producao bibliografica dos professores 

extrai.1producao <- function(professor) {
  idLattes <- names(professor)
  df_1producao <<- NULL 
  producao_bibliografica <- professor[[idLattes]]$producao_bibiografica  #list
  for( tipo_producao in names(producao_bibliografica)) { 
    df.temporario <- cv.tplista2tpchar ( producao_bibliografica[[tipo_producao]]) 
    df.temporario$tipo_producao <-  tipo_producao 
    df.temporario$idLattes <-  idLattes
    df_1producao <- concatenadf( df_1producao , df.temporario  )
  }  
  return(df_1producao)
}

extrai.producoes <- function( jsonProfessores) {
  df.saida <- data.frame()
  for( i in 1:length(jsonProfessores)) {
    jsonProfessor <- jsonProfessores[i]
    df.producao <- extrai.1producao(jsonProfessor)
    if( nrow(df.saida) > 0 ) {
      df.saida <- concatenadf(df.saida , df.producao)
    } else {
      df.saida <- df.producao 
    }
  }
  df.saida <- df.saida %>% filter( !is.na(tipo_producao))
  return(df.saida)  
}

# Extracao das orientacoes dos professores 

extrai.1orientacao <- function(professor) {
  idLattes <- names(professor)
  df.1orientacao <- NULL
  orientacoes_academicas  <- professor[[idLattes]]$orientacoes_academicas  #list
  for( orientacao in names(orientacoes_academicas )) { 
    df.temporario <- cv.tplista2tpchar ( orientacoes_academicas[[orientacao]])
    df.temporario$orientacao <-  orientacao 
    df.temporario$idLattes <-  idLattes
    df.1orientacao <- concatenadf( df.1orientacao , df.temporario  )
  }  
  return(df.1orientacao) 
}

extrai.orientacoes <- function(jsonProfessores) {
  df.saida <- data.frame()
  for( i in 1:length(jsonProfessores)) {
    jsonProfessor <- jsonProfessores[i]
    df.orientacao <- extrai.1orientacao(jsonProfessor)
    if( nrow(df.saida) > 0 ) {
      df.saida <- concatenadf(df.saida , df.orientacao)
    } else {
      df.saida <- df.orientacao
    }
  }
  df.saida <- df.saida %>% filter(!is.na(idLattes))
  return(df.saida)  
}

# Extracao das areas de atuacao dos professores 

extrai.1area.de.atuacao <- function(professor){
  idLattes <- names(professor)
  df.1area <-  professor[[idLattes]]$areas_de_atuacao
  df.1area$idLattes <- idLattes
  return(df.1area)
}

extrai.areas.atuacao <- function(jsonProfessores){
  df.saida <- data.frame()
  for( i in 1:length(jsonProfessores)) {
    jsonProfessor <- jsonProfessores[i]
    df.area.atuacao <- extrai.1area.de.atuacao(jsonProfessor)
    if( nrow(df.saida) > 0 ) {
      df.saida <- concatenadf(df.saida , df.area.atuacao)
    } else {
      df.saida <- df.area.atuacao
    }
  }
  df.saida <- df.saida %>% filter( !is.na(idLattes))
  return(df.saida)   
}