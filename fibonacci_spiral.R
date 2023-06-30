#----------PRESENTATION BLOCK----------

df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(df) <- c('x', 'y', 'id','color_code')

goldenratio <- 1.618
#ratios <- c(seq(0.6165,0.6180,0.0001))
ratios <- c(seq(0.544,0.548,0.002))

#ratios <- c(seq(0.54,0.60,0.2),seq(0.604,0.618,0.2))
ratios
gamma <- 2*pi*(1-goldenratio)   #change in angle(radian)

t=0
gamma_n <- 0
for(j in 1:length(ratios)){
  gamma_n <- 2*pi*ratios[j]
  #print(gamma_n)
  id <- c() #shape id
  x<- c()   #x-loc
  y <- c() #y-loc
  ccode <-c()
  df_temp <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df_temp) <- c('x', 'y', 'id','color_code') 
  #print(paste0("this is j",j))
  #print(df_temp)
  for (i in 1:10000){
    x<- c(x,sqrt(i)*cos(i*gamma_n))
    y<- c(y,sqrt(i)*sin(i*gamma_n))
    id <- c(id,j)
    
  }
  #print("this is df")
  #print(df_temp)
  ccode<- c(rep(1,1000),rep(2,1000),rep(3,1000),
            rep(4,1000),rep(5,1000),rep(6,1000),
            rep(7,1000),rep(8,1000),rep(9,1000),
            rep(10,1000))
  df_temp <- data.frame(x = x, y = y, id = id, color_code = ccode)
  df<- rbind(df,df_temp)
  #print(df)
}

df

#make plot
length(ratios)
nrow(df[which(df$id==1),])
library(ggplot2)
g_list <- list()
for (k in 1:length(ratios)){
  df_subset <- df[which(df$id==k),]
  p<- ggplot(data=df_subset, aes(x=x,y=y,color = factor(color_code)))+
    geom_point(data=df_subset,aes(x=x,y=y))+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill='transparent'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
    ggtitle(k)
    
   plot(p) 
  #g_list[[i]] = p
  #ggsave(paste0(k,".png"))
}


library(gganimate)
pv<- ggplot(data=df, aes(x=x,y=y, fill = factor(color_code)))+
  geom_point(size=0.5, alpha=0.2)+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill='transparent'),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  labs(title = "{closest_state}") +
  transition_states(id,transition_length = 1, state_length = 1, wrap = FALSE)
  #ease_aes('linear')+
  #enter_fade()+
  #exit_fade()

gganimate::animate(pv, fps = 1)
