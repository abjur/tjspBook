source("R/0-load.R")

d_fcc <- readRDS("data/hot_deck/d_fcc.rds")
d_guaru <- readRDS("data/hot_deck/d_guaru.rds")
d_tidy_p <- readRDS("data/d_tidy_p.rds")

classificar <- function(d){

  tabela_assuntos_w %>% 
    filter((str_detect(Dispositivo.legal, regex("11\\.101")))) %>% 
    with(c(N3, N4)) %>% 
    unique -> falencia

  d <- d %>%
    left_join(d_tidy_p, by = 'n_processo', copy = T) %>% 
    mutate(
      tipo = ifelse(assunto %in% falencia, "RJ", "Comum"),
      tipo = ifelse(p == 1, "Empresarial", tipo)) %>% 
    filter(!is.na(tipo))
  
  return(d)
}

d_fcc <- classificar(d_fcc)