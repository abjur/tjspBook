source("R/0-funcoes/cortar.R")

source("R/2-analysis/1-classificar.R")

media <- (d_fcc$p %>% sum)/3

d_fcc_sem_rj <- d_fcc %>% 
  filter(tipo != "RJ")

intercepto <- d_fcc %>% 
  filter(p == 1) %>% 
  with(sum(p))

angular <- d_fcc %>% 
  filter(p > 0, p < 1) %>% 
  nrow

X <- seq(0.0, 0.131, 0.01)
Y <- (intercepto + X*angular)/3

df <- data_frame(X, Y)

ggplot(df, aes(x = X*100, y = Y)) +
  theme_bw(15)+
  geom_line(color = 'darkblue') +
  geom_hline(yintercept = media, color = 'red') +
  scale_x_continuous(breaks = seq(0,13,3)) +
  scale_y_continuous(breaks = c(floor(media),
                                seq(600, 1800, 300))) +
  xlab("Proporção de processos empresariais\nclassificados erroneamente (%)") +
  ylab("Número estimado de processos empresariais") ->
  regressao_plot
  

