library(ggplot2)
library(gganimate)

#dataframe for storing x, y and the plot id
df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df) <- c('x', 'y', 'id')
df
# list of angles for which we'll be making plots
ratios <- c(seq(0.622,0.632,0.002))
ratios

gamma_n <- 0
for(j in 1:length(ratios)){
  gamma_n <- 2*pi*ratios[j]
  
  id <- c() #shape id
  x<- c()   #x-loc
  y <- c() #y-loc
  df_temp <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_temp) <- c('x', 'y', 'id') 
  
  for (i in 1:8000){
    #determining x and y as the polar equivalents
    #using the formula for phyllotaxis
    #https://blog.wolfram.com/2011/07/28/how-i-made-wine-glasses-from-sunflowers/
    
    x<- c(x,sqrt(i)*cos(i*gamma_n))
    y<- c(y,sqrt(i)*sin(i*gamma_n))
    id <- c(id,j)
  } 
  df_temp <- data.frame(x = x, y = y, id = id)
  df<- rbind(df,df_temp)

  }

#df

#animation using gganimate
f<- ggplot(data=df, aes(x=x,y=y))+
  geom_point(size=0.5, alpha=0.2, color='white')+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill='black',colour = "grey50"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  #labs(title = "{closest_state}") +
  transition_states(id,transition_length = 1, state_length = 0, wrap = TRUE)+
  ease_aes('cubic-in-out')

gganimate::animate(f,fps = 1)
anim_save("C:\\Users\\amber\\Documents\\Masters_degree\\GEOM_ART\\flowers2.gif", f,fps = 2,
          height = 430, width = 430)