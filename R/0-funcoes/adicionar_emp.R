adicionar_emp <- function(d_tidy, cnj_empresarial, cnj_completa) {
  # clean
  cnj_empresarial_clean <- cnj_empresarial %>% 
    ungroup() %>% 
    janitor::clean_names() %>% 
    set_names(abjutils::rm_accent(names(.)))
  cnj_completa_clean <- cnj_completa %>% 
    ungroup() %>% 
    janitor::clean_names() %>% 
    set_names(abjutils::rm_accent(names(.)))
  # roll
  cnj_roll_empresarial <- cnj_empresarial_clean %>% 
    filter(folha | n4 == 'Propriedade Intelectual / Industrial') %>% 
    select(n1:n6) %>% 
    mutate(n2 = if_else(n2 == '', n1, n2),
           n3 = if_else(n3 == '', n2, n3),
           n4 = if_else(n4 == '', n3, n4),
           n5 = if_else(n5 == '', n4, n5),
           n6 = if_else(n6 == '', n5, n6)) %>% 
    rename(folha = n6) %>% 
    mutate_all(abjutils::rm_accent) %>% 
    mutate(emp = TRUE)
  cnj_roll <- cnj_completa_clean %>% 
    filter(folha | n4 == 'Propriedade Intelectual / Industrial') %>%
    select(n1:n6) %>% 
    mutate(n2 = if_else(n2 == '', n1, n2),
           n3 = if_else(n3 == '', n2, n3),
           n4 = if_else(n4 == '', n3, n4),
           n5 = if_else(n5 == '', n4, n5),
           n6 = if_else(n6 == '', n5, n6)) %>% 
    rename(folha = n6) %>% 
    mutate_all(abjutils::rm_accent) %>%
    left_join(cnj_roll_empresarial, 
              c('n1', 'n2', 'n3', 'n4', 'n5', 'folha')) %>% 
    mutate(emp = !is.na(emp))
  # empilhado    
  cnj_roll_empilhado <- cnj_roll %>% 
    gather(nivel, assunto, n1:folha) %>% 
    mutate(nivel = forcats::lvls_reorder(factor(nivel), c(2:6, 1))) %>% 
    arrange(emp, nivel, assunto) %>% 
    distinct(emp, nivel, assunto) %>% 
    mutate(nivel = as.character(nivel)) %>% 
    filter(!emp | (emp & nivel == 'folha'))
  # modelo
  d_modelo <- d_tidy %>% 
    mutate(assunto = abjutils::rm_accent(assunto)) %>% 
    # está duplicando nos casos com ambiguidade
    inner_join(cnj_roll, c('assunto' = 'folha')) %>% 
    select(vara, n1:n5, assunto, emp) %>% 
    mutate_all(as.factor) %>% 
    as.data.frame()
  # modelo redes bayesianas
  g <- bnlearn::empty.graph(names(d_modelo))
  edges <- matrix(c(
    # 'vara', 'n1', 'vara', 'n2', 'vara', 'n3', 'vara', 'n4', 'vara', 'n5',
    # 'vara', 'assunto', 'vara', 'emp',
    'n1', 'n2', 'n2', 'n3', 'n3', 'n4', 'n4', 'n5',
    'n5', 'assunto', 'assunto', 'emp'
  ), ncol = 2, byrow = TRUE)
  bnlearn::arcs(g) <- edges
  fit <- bnlearn::bn.fit(g, d_modelo)
  
  cnj_roll_join <- cnj_roll_empilhado %>% 
    filter(nivel == 'folha') %>% 
    # considerando folhas duplicadas como empresariais!
    arrange(desc(emp)) %>%
    # arrange(emp) %>%
    distinct(assunto, .keep_all = TRUE) %>% 
    select(assunto, emp)
  
  d_tidy_p <- d_tidy %>% 
    mutate(assunto_clean = abjutils::rm_accent(assunto)) %>%
    # achei alguns assuntos do tidy que nao tem na tabela CNJ (sao poucos, 11)
    semi_join(cnj_roll_empilhado, c('assunto_clean' = 'assunto')) %>% 
    # isso é para identificar se é ou nao empresarial
    left_join(cnj_roll_join, c('assunto_clean' = 'assunto')) %>% 
    mutate(emp = if_else(is.na(emp), '?', if_else(emp, 'Empresarial', 'Cível'))) %>% 
    group_by(emp, assunto_clean) %>% 
    do({
      d <- .
      if (d$emp[1] == 'Cível') d$p <- 0
      if (d$emp[1] == 'Empresarial') d$p <- 1
      if (d$emp[1] == '?') d$p <- prob_emp(d$assunto_clean[1], vara = NULL,
                                           cnj_roll_empilhado = cnj_roll_empilhado,
                                           fit = fit)
      d
    }) %>% 
    ungroup()
  d_tidy_p %>% 
    select(n_processo, p)
}

prob_emp <- function(ass, vara = NULL, cnj_roll_empilhado, fit) {
  nn <- cnj_roll_empilhado %>%
    filter(assunto == ass, !emp, nivel != 'folha') %>%
    with(nivel)
  if (is.null(vara)) {
    bnlearn::cpquery(fit, (emp == 'TRUE'),
                     eval(substitute((X == Y),
                                     list(X = as.name(nn), Y = ass))))
  } else {
    bnlearn::cpquery(fit, (emp == 'TRUE'),
                     eval(substitute((X == Y & vara == Z),
                                     list(X = as.name(nn),
                                          Y = ass,
                                          Z = vara))))
  }
}
roll <- function(cnj_empresarial) {
  cnj_empresarial_clean <- cnj_empresarial %>% 
    ungroup() %>% 
    janitor::clean_names() %>% 
    set_names(abjutils::rm_accent(names(.)))
  cnj_roll_empresarial <- cnj_empresarial_clean %>% 
    filter(folha | n4 == 'Propriedade Intelectual / Industrial') %>% 
    select(-folha) %>% 
    mutate(n2 = if_else(n2 == '', n1, n2),
           n3 = if_else(n3 == '', n2, n3),
           n4 = if_else(n4 == '', n3, n4),
           n5 = if_else(n5 == '', n4, n5),
           n6 = if_else(n6 == '', n5, n6)) %>% 
    rename(folha = n6) %>% 
    mutate_all(abjutils::rm_accent) %>% 
    mutate(emp = TRUE)
  cnj_roll_empresarial
}
