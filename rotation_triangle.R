library(ggplot2)
library("viridis") 
library(gganimate)


#---------------------------------------------------------------
# Equilateral triangle - all three sides and angles are equal
#xc <- c(a/2,-a/2,0) 
#yc <- c(0, 0,h)


# Isosceles triangle - Two sides and angles are equal
#xc <- c(a/2,-a/2,0) 
#yc <- c(0, 0,h+1)

#Scalene - all three sides and angles are unequal
#xc <- c(a/2,-a/2,0.5) 
#yc <- c(0, 0,h+1)

#---------------------------------------------------------------
a <- 2  #side of a triangle
h <- (sqrt(3)/2)*a # height of triangle
h
id <- c() #shape id
x<- c()   #x-loc
y <- c()  #y-loc

delta <- pi/12  #change in angle(radian)


for (i in 1:100){
  id_t<- c()  #temp vars for storing id, x and y for each iter
  x_t<- c()
  y_t <- c()
  
  xc <- c(a/2,-a/2,0.75) 
  yc <- c(0, 0,0.3)
  xy_mat <- rbind(xc,yc)
  
  ifelse(i==1, t<-0,t <- t+delta) # change in angle (t for theta)
  #print(t)
  #
  ##2d rotation matrix, source wikipedia, link above
  #
  rot_matrix <- matrix(c(cos(t), -sin(t), sin(t), cos(t)), ncol=2) 
  id_t <- c(i,i,i)
  
  conv <- rot_matrix%*%xy_mat #actual rotation
  
  #rotated x,y coordinates and id, extraction and storage
  x_t<- conv[1,] 
  y_t<- conv[2,]
  x<- c(x,x_t)
  y<- c(y,y_t)
  id<- c(id,id_t)
}

df <- data.frame(x = x, y = y, id = id)
df

plt <-  ggplot(data=df, aes(x=x,y=y, fill=factor(id))) +
  geom_polygon(data = df,aes(alpha = 0.2),alpha = 0.2)+
  #scale_color_viridis(discrete = TRUE, option = "magma")+
  #scale_fill_viridis(discrete = TRUE) +
  #theme(legend.position = "none")+
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
  transition_states(id) +shadow_mark()+
  ease_aes()
#enter_fade() + enter_grow()

plt