# phyllotaxis equations: https://blog.wolfram.com/2011/07/28/how-i-made-wine-glasses-from-sunflowers/
# (r,theta) = (sqrt(i), i*gamma)
# where gamma = 2*pi*(1-goldenratio)
# goldenratio <- 1.618
#
# conversion factor, polar to Cartesian co-ordinates:
# x = rcos(theta), y = rsin(theta), theta is same as phi above
#
#------------------------------------------------------------------------------------------
#
#
#
id <- c() #shape id
x<- c()   #x-loc
y <- c()  #y-loc
z<- c()
goldenratio <- 1.618
gamma <- 2*pi*(1-goldenratio)   #change in angle(radian)
t=0

for (i in 1:1000){
  x<- c(x,sqrt(i)*cos(i*gamma))
  y<- c(y,sqrt(i)*sin(i*gamma))
  #z <- c(z,i)
}

df <- data.frame(x = x, y = y, z = c(rep(1,100),rep(2,100),rep(3,100),
                                     rep(4,100),rep(5,100),rep(6,100),
                                     rep(7,100),rep(8,100),rep(9,100),
                                     rep(10,100)))
df



p <- ggplot(data = df, aes(x=x,y=y, color = factor(z)))+
  geom_point(data=df, aes(x=x,y=y))

p

#----------PRESENTATION BLOCK----------
#id <- c() #shape id
#x<- c()   #x-loc
#y <- c()  #y-loc

df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df) <- c('x', 'y', 'id')
df
goldenratio <- 1.618
ratios <- c(seq(0.54,0.62,0.002))
#ratios <- c(seq(0.54,0.60,0.2),seq(0.604,0.618,0.2))
ratios
gamma <- 2*pi*(1-goldenratio)   #change in angle(radian)

t=0
gamma_n<-0
for(j in 1:length(ratios)){
  gamma_n <- 2*pi*ratios[j]
  #print(gamma_n)
  id <- c() #shape id
  x<- c()   #x-loc
  y <- c()  #y-loc
df_temp <-data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_temp) <- c('x', 'y', 'id') 
#print(paste0("this is j",j))
#print(df_temp)
for (i in 1:1000){
  x<- c(x,sqrt(i)*cos(i*gamma_n))
  y<- c(y,sqrt(i)*sin(i*gamma_n))
  id <- c(id,j)
}
#print("this is df")
#print(df_temp)
df_temp <- data.frame(x = x, y = y, id = id)
df<- rbind(df,df_temp)
#print(df)
}

df


length(ratios)
nrow(df[which(df$id==1),])
library(ggplot2)

for (k in 1:length(ratios)){
  df_subset <- df[which(df$id==k),]
  p<- ggplot(data=df_subset, aes(x=x,y=y))+
    geom_point(data=df_subset,aes(x=x,y=y))+
    ggtitle(k)
  plot(p)
}

for (k in 1:length(ratios)){
  df_subset <- df[which(df$id==k),]
  pg<- ggplot(data=df_subset, aes(x=x,y=y))+
    geom_polygon(data=df_subset)+
    ggtitle(k)
  plot(pg)
}
ratios
#5 point star
ratios[31]
#many point stars
ratios[21]
ratios[11]
ratios[6]
plt <-  ggplot(data=df, aes(x=x,y=y, fill=factor(id))) +
  geom_polygon(data = df,aes(alpha = 0.2),alpha = 0.2)+
  #scale_color_viridis(discrete = TRUE, option = "A")+
  #scale_fill_viridis(discrete = TRUE) +
  #theme(legend.position = "none")+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill='transparent'),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
  #transition_states(id) +shadow_mark()
#enter_fade() + enter_grow()

plt


p <- ggplot(data = df, aes(x=x,y=y, fill = factor(id)))+
  geom_point(data=df, aes(y=y))

p