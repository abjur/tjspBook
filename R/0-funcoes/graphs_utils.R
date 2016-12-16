cria_bn <- function (edges) {
  nm <- unique(as.character(edges))
  g <- bnlearn::empty.graph(nm)
  bnlearn::arcs(g) <- edges
  g
}

leafs <- function(tabela_cnj_0){
  tabela_cnj_0 %>% 
    select(Cód..Pai, Código) %>% 
    filter(!is.na(Cód..Pai)) %>% 
    distinct(Cód..Pai, Código) %>% 
    as.matrix() %>% 
    apply(2, as.character) %>% 
    cria_bn %>% 
    leaf.nodes()
}