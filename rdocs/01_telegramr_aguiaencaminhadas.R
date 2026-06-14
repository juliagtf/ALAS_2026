# 0. Pacotes ----

if(!require(pacman))install.packages("pacman")
p_load(tidyverse, tidytext, tidyr, dplyr, ggplot2, stringr, jsonlite, purrr,
       stringi, tidylog, widyr, igraph, ggraph, sf, countries, rnaturalearth, 
       rnaturalearthdata, viridis, tmap, writexl, gt,MetBrewer,tibble,
       wordcloud2)
seed <- 2000
salvar_figura <- function(nome_arquivo, plot = last_plot(), 
                          largura = 17, altura = 12, unidade = "cm") {
  ggsave(
    filename = paste0("figuras/aguia/", nome_arquivo, ".png"),
    plot     = plot,
    width    = largura,
    height   = altura,
    units    = unidade,
    dpi      = 300,
    bg       = "white"
  )
}
# 1. Dados ----

aguia_setembro <- fromJSON("Dados/Telegram/Grupo - Águia/Setembro/result.json") %>% 
  .[["messages"]] %>%
  select(any_of(c("id", "date", "text", "forwarded_from")))
aguia_outubro <- fromJSON ("Dados/Telegram/Grupo - Águia/Outubro/result.json") %>% 
  .[["messages"]]%>%
  select(any_of(c("id", "date", "text", "forwarded_from")))
aguia_novembro <- fromJSON ("Dados/Telegram/Grupo - Águia/Novembro/result.json") %>% 
  .[["messages"]]%>%
  select(any_of(c("id", "date", "text", "forwarded_from")))


## 1.1 Consolidando dados ----

aguia <- bind_rows(aguia_setembro, aguia_outubro, aguia_novembro)

rm(aguia_setembro, aguia_outubro, aguia_novembro)

# 2. Constantes ----

PAISES_EXCLUIR <- c("brasil", "reuniao", "irao", "malta")

# 3. ETL ----

mensagens_encaminhadas <- aguia %>%
  mutate(texto_limpo = map_chr(text, ~ paste(unlist(.x), collapse = " "))) %>%
  filter(!is.na(forwarded_from)) %>%
  filter(str_squish(texto_limpo) != "") %>%
  mutate(
    date       = as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%S"),
    texto_limpo = tolower(texto_limpo),
    texto_limpo = stri_trans_general(texto_limpo, "Latin-ASCII")
  )

rm(aguia)

# 4. Análises ----

## 4.1. Top canais ----

mensagens_encaminhadas %>%
  count(forwarded_from, name = "total_encaminhamentos", sort = TRUE)%>%
  head(10) %>%
  mutate(forwarded_from = reorder(forwarded_from, total_encaminhamentos)) %>%
  ggplot(aes(x = total_encaminhamentos, y = forwarded_from)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_text(aes(label = total_encaminhamentos), hjust = -0.2, size = 4) +
  labs(
    title = "",
    x     = "Total de Mensagens Encaminhadas",
    y     = "Canal de Origem"
  ) +
  theme_classic() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

## 4.2. Preparação do vetor de países ----

source("rdocs/source/tabela_paises_codigos.r")

paises_limpos <- stri_trans_general(paises_tabela$pais, "Latin-ASCII")
paises_limpos <- str_to_lower(paises_limpos)
padrao_busca_paises <- paste0("\\b(", paste(paises_limpos, collapse = "|"), ")\\b")

mensagens_encaminhadas_paises <- mensagens_encaminhadas %>%
  mutate(
    texto_buscavel = stri_trans_general(str_to_lower(texto_limpo), "Latin-ASCII")
  ) %>%
  filter(str_detect(texto_buscavel, padrao_busca_paises))

## 4.3. Contagem de países citados ----

contagem_paises <- mensagens_encaminhadas_paises %>%
  mutate(pais_citado = str_extract_all(texto_buscavel, padrao_busca_paises)) %>%
  unnest(pais_citado) %>%
  filter(!pais_citado %in% PAISES_EXCLUIR) %>%
  count(pais_citado, name = "total_mencoes", sort = TRUE)

write_xlsx(contagem_paises, "paises_mais_citados_grupoaguia.xlsx")

# Gráfico — 10 países mais citados
contagem_paises %>%
  slice_max(order_by = total_mencoes, n = 10) %>%
  mutate(pais_citado = reorder(pais_citado, total_mencoes, decreasing = TRUE)) %>%
  ggplot(aes(y = total_mencoes, x = pais_citado)) +
  geom_col(fill = "#4B75FF", color = "black", show.legend = FALSE) +
  scale_x_discrete(
    labels = function(x) {
      x <- str_to_title(x)
      str_replace(x, "Estados Unidos", "EUA")
    }
  )+
  theme_classic() +
  labs(
    title = "",
    x     = NULL,
    y     = "Quantidade de menções"
  )

salvar_figura("aguia_top10_paises")

## 4.4. Cruzamento canal × país ----

# canais_e_paises <- mensagens_encaminhadas_paises %>%
#   mutate(pais_citado = str_extract_all(texto_buscavel, padrao_busca_paises)) %>%
#   unnest(pais_citado) %>%
#   filter(!pais_citado %in% PAISES_EXCLUIR) %>%
#   count(forwarded_from, pais_citado, name = "total_mencoes", sort = TRUE)

# matriz_geopolitica <- canais_e_paises %>%
#   filter(total_mencoes > 5) %>%
#   pivot_wider(names_from = pais_citado, values_from = total_mencoes, values_fill = 0)

contexto_paises_por_canal <- mensagens_encaminhadas_paises %>%
  mutate(pais_citado = str_extract_all(texto_buscavel, padrao_busca_paises)) %>%
  unnest(pais_citado) %>%
  filter(!pais_citado %in% PAISES_EXCLUIR) %>%
  count(pais_citado, canal_origem = forwarded_from, texto_limpo,
        name = "total_envios", sort = TRUE) %>%
  rename(contexto_completo = texto_limpo)

write_xlsx(contexto_paises_por_canal, "paises_contexto_por_canal_grupoaguia.xlsx")

rm(contexto_paises_por_canal)

## 4.5. Mapa mundial de menções ----

contagem_com_iso <- contagem_paises %>%
  left_join(
    paises_tabela %>%
      mutate(
        pais_join = stri_trans_general(pais, "Latin-ASCII"),
        pais_join = str_to_lower(pais_join)
      ),
    by = c("pais_citado" = "pais_join")
  ) %>%
  mutate(iso3 = str_to_upper(iso3))

mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")

mapa_dados <- mapa_mundo %>%
  left_join(contagem_com_iso, by = c("iso_a3" = "iso3"))

ggplot(data = mapa_dados) +
  geom_sf(aes(fill = log10(total_mencoes + 1)), color = "gray", size = 0.2) +
  scale_fill_gradientn(
    colors   = met.brewer("Greek", direction = -1),
    na.value = "gray95",
  ) +
  theme_void() +
  labs(fill = "Menções (log10)")

salvar_figura("aguia_mapa")

rm(contagem_com_iso, mapa_mundo, mapa_dados)

## 4.6. Mensagens que citam múltiplos países ----

mensagens_multi_paises <- mensagens_encaminhadas_paises %>%
  mutate(paises_encontrados = str_extract_all(texto_buscavel, padrao_busca_paises)) %>%
  mutate(
    paises_unicos = map(paises_encontrados, ~ unique(.x[!.x %in% PAISES_EXCLUIR])),
    qtd_paises = map_int(paises_unicos, length),
    nomes_dos_paises = map_chr(paises_unicos, ~ paste(.x, collapse = " x "))
  ) %>%
  filter(qtd_paises >= 2) %>%
  select(forwarded_from, qtd_paises, nomes_dos_paises, texto_limpo) %>%
  arrange(desc(qtd_paises))

write_xlsx(mensagens_multi_paises, "mensagens_multi_paises_grupoaguia.xlsx")

rm(mensagens_multi_paises)

## 4.7. Contexto por país específico ---- 

exportar_contexto_pais <- function(dados, pais, arquivo) {
  dados %>%
    mutate(pais_citado = str_extract_all(texto_buscavel, padrao_busca_paises)) %>%
    unnest(pais_citado) %>%
    filter(pais_citado == pais) %>%
    select(!text) %>%
    count(
      pais_citado,
      canal_origem = forwarded_from,
      texto_limpo,
      name = "total_envios",
      sort = TRUE
    ) %>%
    write_xlsx(arquivo)
}
# exportar_contexto_pais(mensagens_encaminhadas_paises, "china", "china_contexto.xlsx")

## 4.8. Grafo de co-ocorrência países × países ----

### 4.8.1. Construção da matriz de co-ocorrência ----

# pares não-ordenados com contagem de co-ocorrência

pares_paises <- mensagens_encaminhadas_paises %>%
  mutate(
    paises = map(
      texto_buscavel,
      ~ str_extract_all(.x, padrao_busca_paises)[[1]] %>%
        unique() %>%
        .[!. %in% PAISES_EXCLUIR]
    )
  ) %>%
  filter(map_int(paises, length) >= 2) %>%    
  select(id_msg = id, paises) %>%
  unnest(paises) %>%
  rename(pais = paises) %>%
  pairwise_count(pais, id_msg, sort = TRUE, upper = FALSE)

### 4.8.2. Grafo com threshold mínimo ----

min_coocorrencias <- 3

grafo_paises <- pares_paises %>%
  filter(n >= min_coocorrencias) %>%
  graph_from_data_frame(directed = FALSE)

# peso das arestas proporcional à co-ocorrência
E(grafo_paises)$weight <- pares_paises %>%
  filter(n >= min_coocorrencias) %>%
  pull(n)

# grau de cada nó (quantos países diferentes ele aparece junto)
V(grafo_paises)$grau <- degree(grafo_paises)

### 4.8.3. Visualização ----

set.seed(seed)
ggraph(grafo_paises, layout = "fr") + 
  geom_edge_link(
    aes(width = weight, alpha = weight),
    color = "steelblue"
  ) +
  geom_node_point(
    aes(size = grau),
    color = "tomato"
  ) +
  geom_node_text(
    aes(label = name),
    repel     = TRUE,
    size      = 3.5,
    fontface  = "bold"
  ) +
  scale_edge_width(range = c(0.4, 3)) +
  scale_edge_alpha(range = c(0.3, 0.9)) +
  scale_size(range = c(3, 10)) +
  labs(
    title    = "Co-ocorrência de países nas mensagens encaminhadas",
    subtitle = paste0("Arestas com ≥ ", min_coocorrencias, " co-ocorrências"),
    size     = "Grau do nó",
    edge_width = "Co-ocorrências"
  ) +
  theme_graph(base_family = "sans")

salvar_figura(paste0("aguia_coocorrencia_",palavra))

write_xlsx(pares_paises, "coocorrencia_paises_grupoaguia.xlsx")

## 4.9. Wordclouds ----

### 4.9.1. Stopwords em português ----

source("rdocs/source/stopwords.r")
if (is.data.frame(stopwords_pt)) {
  if ("word" %in% names(stopwords_pt)) {
    stopwords_pt <- stopwords_pt$word
  } else if ("palavra" %in% names(stopwords_pt)) {
    stopwords_pt <- stopwords_pt$palavra
  } else {
    stopwords_pt <- unlist(stopwords_pt, use.names = FALSE)
  }
}

stopwords_pt <- stopwords_pt %>%
  as.character() %>%
  str_to_lower() %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish() %>%
  unique()

#declarar lista de palavras_compostas - aumentar conforme necessidade
palavras_compostas <- c(
  "estados unidos",
  "coreia do norte",
  "coreia do sul",
  "reino unido",
  "arabia saudita",
  "emirados arabes",
  "africa do sul",
  "nova zelandia",
  "papua nova guine",
  "guinea bissau",
  "burkina faso",
  "serra leoa",
  "costa do marfim",
  "republica tcheca",
  "republica dominicana",
  "trinidad e tobago",
  "bosnia herzegovina",
  "timor leste",
  "joe biden",
  "donald trump",
  "habeas corpus",
  "marco rubio",
  "guine equatorial",
  "greta thunberg",
  "benjamin netanyahu",
  "xi jinping",
  "gustavo petro",
  "miguel uribe"
)

colapsar_palavras <- function(texto) {
  texto_norm <- texto %>%
    str_to_lower() %>%
    stri_trans_general("Latin-ASCII")
  
  for (palavras in palavras_compostas) {
    padrao  <- str_replace_all(palavras, " ", "\\\\s+")
    slug    <- str_replace_all(palavras, " ", "_")
    texto_norm <- str_replace_all(texto_norm, padrao, slug)
  }
  texto_norm
}

palavras_limpas <- mensagens_encaminhadas_paises %>%
  mutate(
    id_mensagem  = row_number(),
    texto_tratado = colapsar_palavras(texto_limpo)
  ) %>%
  unnest_tokens(palavra, texto_tratado) %>%
  filter(!palavra %in% stopwords_pt) %>%
  filter(str_detect(palavra, "^[a-z_çáàãâéêíóôõú]+$")) %>%
  mutate(palavra = case_when(
    palavra == 'americanos' ~ 'americano',
    palavra == 'americanas' ~ 'americana',
    palavra == 'eua' ~ 'estados_unidos',
    palavra == 'trump' ~ 'donald_trump',
    palavra == 'palestinos' ~ 'palestino',
    palavra == 'israelenses' ~ 'israelense',
    palavra == 'netanyahu' ~ 'benjamin_netanyahu',
    palavra == 'greta' ~ 'greta_thunberg',
    palavra == 'petro' ~ 'gustavo_petro',
    palavra %in% c("chines","chineses","chinesa","chinesas") ~ 'chines',
    .default = palavra
  ))

correlacao_palavras <- palavras_limpas %>%
  group_by(palavra) %>%
  filter(n() >= 10) %>% 
  ungroup() %>%
  pairwise_cor(item = palavra, feature = id_mensagem, sort = TRUE)

# obs: usar underscore ao inves de espaço (ex: "donald_trump")
palavra = "estados_unidos"
palavra_titulo = "Estados Unidos"
# correlacao_alvo <- correlacao_palavras %>%
#   filter(item1 == palavra)

# head(correlacao_alvo, 15)

# correlacao_palavras %>%
#   filter(item1 == palavra) %>%
#   top_n(15, correlation)

palavras_alvo <- correlacao_palavras %>%
  filter(item1 == palavra) %>%
  top_n(15, correlation) %>%
  pull(item2)

palavras_grafo <- c(palavra, palavras_alvo)

dados_grafo <- correlacao_palavras %>%
  filter(item1 %in% palavras_grafo, item2 %in% palavras_grafo) %>%
  filter(correlation > 0.05) %>% 
  graph_from_data_frame()

set.seed(seed) 
ggraph(dados_grafo, layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), 
                 edge_colour = "firebrick", show.legend = FALSE) +
  geom_node_point(color = "dodgerblue4", size = 4) +
  
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 4.5, repel = TRUE) +
  
  theme_void() +
  labs(title = paste0("Teia Discursiva: ",palavra_titulo),
       subtitle = "Termos mais correlacionados no grupo Aguia")

salvar_figura(paste0("aguia2_teia_discursiva_",palavra))

### 4.9.2. Wordcloud geral ----

freq_geral <- palavras_limpas %>%
  count(palavra, name = "freq", sort = TRUE)

wordcloud2(
  data        = freq_geral %>% head(200),
  size        = 0.6,
  color       = "random-dark",
  backgroundColor = "white"
)

### 4.9.3. Wordcloud por país citado ----

wordcloud_pais <- function(pais_alvo, top_n = 150) {
  
  freq <- mensagens_encaminhadas_paises %>%
    mutate(pais_citado = str_extract_all(texto_buscavel, padrao_busca_paises)) %>%
    unnest(pais_citado) %>%
    filter(pais_citado == pais_alvo) %>%
    mutate(texto_limpo = colapsar_palavras(texto_limpo)) %>%
    unnest_tokens(word, texto_limpo)  %>%
    mutate(palavra = case_when(
      palavra == 'americanos' ~ 'americano',
      palavra == 'americanas' ~ 'americana',
      palavra == 'eua' ~ 'estados_unidos',
      palavra == 'trump' ~ 'donald_trump',
      palavra == 'palestinos' ~ 'palestino',
      palavra == 'israelenses' ~ 'israelense',
      palavra == 'netanyahu' ~ 'benjamin_netanyahu',
      palavra == 'greta' ~ 'greta_thunberg',
      palavra == 'petro' ~ 'gustavo_petro',
      palavra %in% c("chines","chineses","chinesa","chinesas") ~ 'chines',
      .default = palavra
    )) %>%
    filter(
      !word %in% stopwords_pt,
      !word %in% str_replace_all(paises_limpos, " ", "_"),         
      str_length(word) > 2,
      !str_detect(word, "^[0-9]+$")
    ) %>%
    count(word, name = "freq", sort = TRUE) %>%
    head(top_n)
  
  if (nrow(freq) == 0) {
    message("Nenhum token encontrado para o país: ", pais_alvo)
    return(invisible(NULL))
  }
  
  wordcloud2(
    data            = freq,
    size            = 0.5,
    color           = "random-dark",
    backgroundColor = "white"
  )
}
wordcloud_pais("colombia", top_n = 100)

