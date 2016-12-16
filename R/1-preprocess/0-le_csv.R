source('R/load.R')

base_relacional <- function(path, dire){

  # Lê os dados
  full_dataset <- fread(path)

  # Arruma o encoding fudido do windows
  full_dataset <- full_dataset %>%
    mutate_each(
      funs(iconv(., from = 'latin1', to = 'utf-8'))
    ) %>%
    arruma_nomes()

  # Separa a base em duas tabelas (infos e movs)
  base_relacional <- full_dataset %>%
    monta_relacional()

  rm(full_dataset)

  saveRDS(base_relacional,
          sprintf('%s/%s',
                  dire,
                  gsub("\\.csv","\\.rds",basename(path))))

}

# arquivos <- list.files(
#   './data/Foro Central Cível/',
#   full.names = T) %>%
#   grep('20', ., value = T)
# 
# sapply(arquivos, base_relacional, dire = "./data/Foro Central Cível")

 arquivos <- list.files(
   './data/Guarulhos/',
   full.names = T) %>%
   grep('\\.csv', ., value = T)

 sapply(arquivos[2], base_relacional, dire = "./data/Guarulhos")

