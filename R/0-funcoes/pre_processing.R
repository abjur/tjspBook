arruma_nomes <- function(d){
  d %>%
    setNames(tolower(abjutils::rm_accent(names(.)))) %>%
    dplyr::rename(n_processo = numero_processo,
                  mov = dsc_movimento,
                  data_mov = dta_movimento,
                  data_dist = dta_distribuicao)

}

monta_relacional <- function(d){

  d_infos <- d %>%
    ungroup() %>%
    distinct(n_processo, comarca,
             foro, vara, classe,
             assunto, data_dist)

  d_movs <- d %>%
    distinct(n_processo,
             data_mov,
             mov)

  d_partes <- d %>%
    distinct(n_processo,
             autor,
             reu)

  return(list(d_movs = d_movs,
              d_infos = d_infos,
              d_partes = d_partes))

}

substitui_na_com_ultimo_nao_nulo <- function(d, ...){
  
  n_nodes <- lazyeval::lazy_dots(...) %>%
    length()
  
  d_aux <- d %>%
    select(...) %>%
    mutate_each(funs(cumsum(!is.na(.))))
  
  for(ii in 2:ncol(d_aux)){
    d_aux[,ii] = d_aux[,ii] +
      cumsum(c(0,diff(d_aux[,ii-1])))
  }
  
  d_aux <- d_aux %>%
    gather(variavel, grupo)
  
  d_aux <- d %>%
    select(...) %>%
    gather(variavel, valor) %>%
    select(-variavel) %>%
    bind_cols(d_aux)
  
  d_2 <- d_aux %>%
    group_by(variavel, grupo) %>%
    mutate(valor = valor[1]) %>%
    group_by(variavel) %>%
    mutate(id = 1:n()) %>%
    ungroup() %>%
    select(-grupo) %>%
    spread(variavel, valor) %>%
    select(...) %>%
    setNames(paste0('N',1:n_nodes))
  
  return(d_2)
}

