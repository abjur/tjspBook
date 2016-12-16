source("R/2-analysis/1-classificar.R")

library(survival)

d_fcc <- d_fcc %>% 
  group_by(n_processo) %>% 
  mutate(censura = as.numeric(max(julgado, baixa, recurso, sentenca)),
         censura = if_else(censura > 0, 1, 0)) %>% 
  mutate(t_deci_total = n_deci_e_desp * t_deci)

d_fcc %>% 
with(survfit(Surv(t_deci_total, censura) ~ tipo)) %>% 
  ggfortify:::autoplot.survfit(conf.int = F, surv.geom = 'line', censor = F) +
  scale_x_continuous(limits = c(0,250)) +
  theme_bw(15) +
  xlab("Total de tempo acumulado\ngasto em decisões e depaschos ") +
  ylab("Probabilidade de sobrevivência") -> tempo_deci

d_fcc %>% 
  with(survfit(Surv(n_deci_e_desp, censura) ~ tipo)) %>% 
  autoplot(conf.int = F, surv.geom = 'line') +
  scale_x_continuous(limits = c(0,20)) +
  theme_bw(15) +
  xlab("Número de decisões e despacho") +
  ylab("Probabilidade de sobrevivência") -> nro_decisoes

d_fcc %>% 
  with(survfit(Surv(tempo_total, censura) ~ tipo)) %>% 
  autoplot(conf.int = F, surv.geom = 'line') +
  scale_x_continuous(limits = c(0,500)) +
  theme_bw(15) +
  xlab("Número de decisões e despacho") +
  ylab("Probabilidade de sobrevivência") -> tempo_total


