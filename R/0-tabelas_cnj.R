tabela_classes_0 <- openxlsx::read.xlsx('data/tabelas_cnj/9_Tabela_Classes_Justica_Estadual_1_Grau.xlsx',
                                        skipEmptyRows = T) %>%
  mutate(X1 = `Classes.processuais.do.1º.Grau.da.Justiça.Estadual`,
         `Classes.processuais.do.1º.Grau.da.Justiça.Estadual` = ifelse(is.na(Cód..Pai),NA, X1),
         X1 = ifelse(is.na(Cód..Pai),X1,NA)) %>%
  select(X1, everything()) %>% 
  filter(!is.na(Código) & (X1 != 'Legenda' | (is.na(X1))))

leafs(tabela_classes_0) -> folhas

tabela_classes_w <- tabela_classes_0 %>%
  substitui_na_com_ultimo_nao_nulo(X1,`Classes.processuais.do.1º.Grau.da.Justiça.Estadual`,
                                   X2,
                                   X3,
                                   X4,
                                   X5) %>%
  bind_cols(select(tabela_classes_0,-X1, -`Classes.processuais.do.1º.Grau.da.Justiça.Estadual`,
                   -X2,
                   -X3,
                   -X4,
                   -X5)) %>%
  mutate_each(funs(ifelse(is.na(.),'',.))) %>%
  group_by_(.dots = grep('N', names(.), value = T)) %>%
  summarise_each(funs(paste(., collapse = '\n'))) %>% 
  mutate(folha = ifelse(Código %in% as.numeric(folhas), T, F))

tabela_movs_0 <- openxlsx::read.xlsx('data/tabelas_cnj/9_Tabela_Movimentos_Justica_Estadual_1_Grau.xlsx',
                                     skipEmptyRows = T) %>%
  mutate(X1 = `Movimentos.processuais.do.1º.Grau.da.Justiça.Estadual`,
         `Movimentos.processuais.do.1º.Grau.da.Justiça.Estadual` = ifelse(is.na(Cód..Pai),NA, X1),
         X1 = ifelse(is.na(Cód..Pai),X1,NA)) %>%
  select(X1, everything()) %>% 
  filter(!is.na(Código) & (X1 != 'Legenda' | (is.na(X1))))

leafs(tabela_movs_0) -> folhas

tabela_movs_w <- tabela_movs_0 %>%
  substitui_na_com_ultimo_nao_nulo(X1,
                                   `Movimentos.processuais.do.1º.Grau.da.Justiça.Estadual`,
                                   X2,
                                   X3,
                                   X4) %>%
  bind_cols(select(tabela_movs_0, -X1,
                   -`Movimentos.processuais.do.1º.Grau.da.Justiça.Estadual`,
                   -X2,
                   -X3,
                   -X4)) %>%
  mutate_each(funs(ifelse(is.na(.),'',.))) %>%
  group_by_(.dots = grep('N', names(.), value = T)) %>%
  summarise_each(funs(paste(., collapse = '\n'))) %>%
  filter(N1 != '') %>% 
  mutate(folha = ifelse(Código %in% as.numeric(folhas), T, F),
         valor = ifelse(N2!='' & N3==''&N4 == ''&N5 == '', N2,
                        ifelse(N3!=''&N4==''&N5=='',N3,
                               ifelse(N4!=''&N5=='',N4,
                                      ifelse(N5!='',N5,'')))))

lista_movs <- tabela_movs_w %>% 
  filter(folha) %>% 
  ungroup() %>% 
  select(N1, N2, valor) %>%
  rename(mov = valor)

lista_movs_n2 <- lista_movs %>% 
  distinct(N2, .keep_all = T)

tabela_assuntos_0 <- openxlsx::read.xlsx('data/tabelas_cnj/9_Tabela_Assuntos_Justica_Estadual_1_Grau.xlsx',
                                         skipEmptyRows = T) %>%
  mutate(X1 = `Assuntos.processuais.do.1º.Grau.da.Justiça.Estadual`,
         `Assuntos.processuais.do.1º.Grau.da.Justiça.Estadual` = ifelse(is.na(Cód..Pai),NA, X1),
         X1 = ifelse(is.na(Cód..Pai),X1,NA)) %>%
  select(X1, everything()) %>% 
  filter(!is.na(Código) & (X1 != 'Legenda' | (is.na(X1)))) %>% 
  mutate(Artigo = gsub("\\.","",Artigo))

leafs(tabela_assuntos_0) -> folhas

tabela_assuntos_w <- tabela_assuntos_0 %>%
  substitui_na_com_ultimo_nao_nulo(X1,
                                   `Assuntos.processuais.do.1º.Grau.da.Justiça.Estadual`,
                                   X2,
                                   X3,
                                   X4,
                                   X5) %>%
  bind_cols(select(tabela_assuntos_0, -X1, -`Assuntos.processuais.do.1º.Grau.da.Justiça.Estadual`,
                   -X2,
                   -X3,
                   -X4,
                   -X5)) %>%
  mutate_each(funs(ifelse(is.na(.),'',.))) %>%
  group_by_(.dots = grep('N', names(.), value = T)) %>%
  summarise_each(funs(stri_trim(paste(., collapse = '\n')))) %>% 
  mutate(folha = ifelse(Código %in% as.numeric(folhas), T, F))

tabela_movs_l <- tabela_movs_w %>% 
  select(N1, N2, N3, N4, N5, Movimento) %>% 
  gather(no, chave, -Movimento, -N1) %>% 
  distinct(chave, Movimento, no, .keep_all = T) %>% 
  filter(chave != "" & Movimento != "")

tabela_movs_chave_mov <- tabela_movs_l %>% 
  select(N1, Movimento) %>% 
  distinct(Movimento, .keep_all = T) %>% 
  rename(chave = Movimento)

tabela_movs_chave_no <- tabela_movs_l %>% 
  select(N1, chave) %>% 
  distinct(chave, .keep_all = T)

tabela_movs_tipo <- bind_rows(tabela_movs_chave_mov,
                              tabela_movs_chave_no)

 tabela_assuntos_w %>% 
   mutate(Dispositivo.legal = str_c(Dispositivo.legal, " "),
          Artigo = str_c(Artigo, " ")) %>% 
   filter((str_detect(Dispositivo.legal, regex("10\\.?406"))&
             str_detect(Artigo, regex(str_c(966:1195, collapse = '[^0-9]+|'))))|
            (str_detect(Dispositivo.legal, regex("6\\.?404")))|
            (str_detect(Dispositivo.legal, regex("9\\.?279")))|
            (str_detect(Dispositivo.legal, regex("8\\.?955")))|
            (str_detect(N4, "Agêncie e Distribuição"))|
            (str_detect(N4, "Assembléia"))|
            (str_detect(N4, "Em comum / De fato"))) -> cnj_empresarial
# 
 saveRDS(cnj_empresarial, "relatorio/cnj_empresarial.rds")
 saveRDS(tabela_assuntos_w, "relatorio/tabela_cnj_completa.rds")

#gather(select(tabela_movs_w, N2,N3,N4,N5), variavel, valor2, -N1) %>% distinct(variavel, valor2, .keep_all = T) %>% filter(valor2 != "")

rm(tabela_assuntos_0)
rm(tabela_classes_0)
rm(tabela_movs_0)