source("R/2-analysis/1-classificar.R")

tabela_resumo <- function(d){
  d %>%
  filter(year(prim_mov) >= 2013) %>% 
  select(tipo, n_deci, n_desp, n_mov, tempo_total, t_deci, n_deci_e_desp,
         n_deci_n_nula) %>% 
  group_by(tipo) %>% 
  mutate(tempo_total_2 = tempo_total^2) %>% 
  summarise_each(funs(mean(., na.rm = T))) %>% 
  mutate(std_tempo_total = sqrt(tempo_total_2 - tempo_total^2)) %>% 
  select(-tempo_total_2)
}

avg_t_deci_surv <- function(d){
  
  d %>% 
    filter(year(prim_mov) >= 2013) %>% 
    filter(!is.na(t_deci)) %>% 
    arrange(tipo, t_deci) %>% 
    group_by(tipo) %>% 
    mutate(surv = (1:n())/n()) %>% 
    ggplot(aes(x = t_deci, y = 1-surv, color = tipo)) +
      geom_smooth(alpha = 0) + 
      scale_x_continuous(limits = c(0, 20)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_bw(15) +
      ylab("Sobrevivência") +
      xlab("Tempo médio entre conclusos e decisões")
}


total_t_deci_surv <- function(d, ymax = 30){
  
  d %>% 
    filter(year(prim_mov) >= 2013) %>%
    mutate(t_deci = t_deci*n_deci_e_desp) %>% 
    filter(!is.na(t_deci)) %>% 
    arrange(tipo, t_deci) %>% 
    group_by(tipo) %>% 
    mutate(surv = (1:n())/n()) %>% 
    ggplot(aes(x = t_deci, y = 1-surv, color = tipo)) +
    geom_smooth(alpha = 0) + 
    scale_x_continuous(limits = c(0, ymax)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw(15) +
    ylab("Sobrevivência") +
    xlab("Tempo total entre conclusos e decisões")
  
}

n_deci_e_desp <- function(d){
  
  d %>% 
    filter(year(prim_mov) >= 2013) %>% 
    filter(!is.na(n_deci_e_desp)) %>% 
    group_by(tipo) %>% 
    count(n_deci_e_desp) %>% 
    mutate(nn = n/sum(n)) %>% 
    ggplot(aes(x = n_deci_e_desp, y = nn*100, color = tipo)) +
      geom_smooth(alpha = 0) + 
      scale_x_continuous(limits = c(0, 20)) +
      theme_bw(15) +
      ylab("Proporção (%)") +
      xlab("Número de despachos e decisões")

}

tempo_total <- function(d){
 
  d %>% 
    
  
}