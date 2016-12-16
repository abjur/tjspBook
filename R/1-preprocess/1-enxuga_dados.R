source("./R/0-le_csv.R")

# for(ii in 1:41){
#   aux <- readRDS(sprintf("./data/hot_deck/d_movs_%s.rds", ii)) %>% 
#     group_by(n_processo, data_mov) %>% 
#     summarise(mov = str_c(mov, collapse = ", "))
#   
#   saveRDS(aux, sprintf("./data/hot_deck/d_movs_redux_%s.rds", ii))
# }

arquivos_sp <- c("./data/Foro Central Cível/2013.rds",
              "./data/Foro Central Cível/2014.rds",
              "./data/Foro Central Cível/2015.rds")

arquivos_gua <- c("./data/Guarulhos/2013-2015.rds")

enxugar <- function(files, dire){
  
  dir.create(sprintf("%s/processed",dire), showWarnings = F)
  
  for(ff in files){
  
    d <- readRDS(ff)
    
    d_infos <- d %>% 
      with(d_infos)
  
    d_movs <- d %>% 
      with(d_movs) %>% 
      filter(!str_detect("Agravo", mov)) %>% 
      # remove os agravos pois eles podem dificultar a identificação
      # de processos finalizados
      mutate(data_mov = dmy(data_mov)) %>% 
      group_by(n_processo, data_mov) %>% 
      summarise(mov = str_c(mov, collapse = ", "))
  
    resumo_movs <- d_movs %>% 
      arrange(n_processo, data_mov) %>% 
      group_by(n_processo) %>% 
      summarise(n_deci = sum(str_detect(mov, regex("- Decisão", ignore_case = T))),
              n_desp = sum(str_detect(mov, regex("- Despacho", ignore_case = T))),
              n_mov = length(data_mov),
              tempo_total = max(data_mov) - min(data_mov),
              julgado = sum(str_detect(mov, regex("Trânsito|Julgado", ignore_case = T))),
              baixa = sum(str_detect(mov, regex("Baixa", ignore_case = T))),
              recurso = sum(str_detect(mov, regex("Recurso", ignore_case = T))),
              sentenca = sum(str_detect(mov, regex("- Sentença", ignore_case = T))),
              prim_mov = min(data_mov),
              ult_mov = max(data_mov),
              data_julgado = min(data_mov[str_detect(mov, regex("Trânsito|Julgado", ignore_case = T))]),
              data_baixa = min(data_mov[str_detect(mov, regex("Baixa", ignore_case = T))]),
              data_recurso = min(data_mov[str_detect(mov, regex("Recurso", ignore_case = T))]),
              data_sentenca = min(data_mov[str_detect(mov, regex("- Sentença", ignore_case = T))]))
  
    decis <- d_movs %>% 
      arrange(n_processo, data_mov) %>% 
      mutate(l_data_mov = lag(data_mov),
           l_mov = lag(mov)) %>% 
      filter(str_detect(mov, regex("- Decisão| - Despacho", ignore_case = T))) %>% 
      mutate()
  
    resumo_decis <- decis %>% 
      group_by(n_processo) %>% 
      summarise(n_deci_e_desp = length(mov), 
              t_deci = sum((1-str_detect(mov, "- Decisão| - Despacho")*str_detect(mov, "Concluso"))*
                       (data_mov - l_data_mov), na.rm = T),
              n_deci_n_nula = sum((1-str_detect(mov, "- Decisão| - Despacho")*str_detect(mov, "Concluso"))))
  
    resumo_movs <- resumo_movs %>% 
      left_join(resumo_decis)
  
    ff_token <- gsub("\\.rds","",basename(ff))
    
    saveRDS(d_movs, sprintf("%s/processed/d_movs_%s.rds", dire, ff_token))
  
    saveRDS(decis, sprintf("%s/processed/d_deci_%s.rds", dire, ff_token))
  
    saveRDS(resumo_decis, sprintf("%s/processed/resumo_deci_%s.rds", dire, ff_token))
  
    saveRDS(resumo_movs, sprintf("%s/processed/resumo_movs_%s.rds", dire, ff_token))
    
    saveRDS(d_infos, sprintf("%s/processed/d_infos_%s.rds", dire, ff_token))
  }
}

compilar <- function(word, dire, label){
  
  files <- grep(word, list.files(dire, full.names = T), value = T)
  
  resumao_movs <- bind_rows(lapply(files, readRDS))
  
  saveRDS(resumao_movs, sprintf("%s/%s.rds", dire, label))
}

enxugar(arquivos_sp, dire = "./data/Foro Central Cível")

compilar("resumo_movs", "./data/Foro Central Cível/processed", "resumao_movs")
compilar("resumo_movs", "./data/Guarulhos/processed", "resumao_movs")

compilar("d_infos_", "./data/Foro Central Cível/processed", "d_infos")
compilar("d_infos_", "./data/Guarulhos/processed", "d_infos")

enxugar(arquivos_gua, dire = "./data/Guarulhos")




