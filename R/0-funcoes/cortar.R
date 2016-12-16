cortar <- function(d_tidy, corte){
  d_tidy %>% 
    mutate(new_tipo = if_else(corte <= p|corte == 1, 'Empresarial', 'Comum'))
}
# 
# d_fcc_sem_rj %>% 
#   cortar(0.0001) %>% 
#   filter(p != 1) %>% 
#   count(assunto, tipo, new_tipo)

nro_de_empresariais <- function(d, p_min){
  
  base <- sum(d$p == 1)
  
  acrescimo <- d %>% 
    filter(p != 1) %>% 
    cortar(p_min) %>% 
    count(assunto, tipo, new_tipo) %>% 
    group_by(assunto) %>% 
    mutate(nn = n/sum(n)) %>% 
    filter(tipo != new_tipo) %>% 
    with(sum(n))
  
  return(base+acrescimo)
  
}

maior_p <- function(d, p_min){
  
  max_change <- d %>% 
    filter(p != 1) %>% 
    cortar(p_min) %>% 
    count(assunto, tipo, new_tipo) %>% 
    group_by(assunto) %>% 
    mutate(nn = n/sum(n)) %>% 
    filter(tipo != new_tipo) %>% 
    with(max(nn))
  
  return(max_change)
  
}

tab <- function(d, p_min){

    max_change <- d %>% 
      filter(p != 1) %>% 
      cortar(p_min) %>% 
      count(assunto, tipo, new_tipo) %>% 
      group_by(assunto) %>% 
      mutate(nn = n/sum(n)) %>% 
      filter(tipo != new_tipo) %>% 
      mutate(p = p_min)
    
  return(max_change)
}

# plyr::ldply(seq(0.009,0.012,length.out = 10), 
#                    tab, d = d_fcc_sem_rj) %>% 
#   group_by(p) %>% 
#   summarise(ymin = min(nn), ymax = max(nn),
#             y = mean(nn)) %>% 
#   ggplot(aes(x = p, y = y, ymax = ymax, ymin = ymin)) +
#   geom_errorbar() +
#   theme_bw(15)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}