#source("R/preprocess/2-enxuga_dados.R")

source('R/load.R')

tidy_data <- function(path, label){
  
  file_resumao <- grep("resumao", list.files(path, full.names = T), value = T)
  
  d_resumo_movs <- readRDS(file_resumao)
  
  d_infos <- readRDS(sprintf("%s/d_infos.rds", path)) %>% 
    mutate(data_dist = dmy(data_dist)) %>% 
    filter(year(data_dist) >= 2013,
           str_detect(vara, "Vara Cível|Falência")) %>% 
    left_join(d_resumo_movs) %>% 
    mutate(t_deci = t_deci/n_deci_e_desp)
  
  saveRDS(d_infos, sprintf("data/hot_deck/d_%s.rds", label))

}

tidy_data("data/Foro Central Cível/processed",
          "fcc")

tidy_data("data/Guarulhos/processed",
          "guaru")

