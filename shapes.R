library(ggplot2)
id <- c() #shape id
x<- c()   #x-loc
y <- c()  #y-loc
z<- c()
goldenratio <- 1.618
gamma <- 2*pi*(1-goldenratio)   #change in angle(radian)

#-------------------------------------------------
#UNIT CIRCLE
# 
r <- 1
for (i in seq(0,2*pi,0.01)){
#for (i in seq(0,1000,1)){
  x<- c(x,cos(i))
  y<- c(y,sin(i))
}


df <- data.frame(x = x, y = y)


p <- ggplot(data = df, aes(x=x,y=y))+
  geom_point(data=df, aes(x=x,y=y))

p

#------------------------------------------------------------

# Swirling circles


for (i in seq(0,2*pi,0.01)){
  x<- c(x,(i)*cos(i*gamma))
  y<- c(y,(i)*sin(i*gamma))
}

df <- data.frame(x = x, y = y)
                 

p <- ggplot(data = df, aes(x=x,y=y))+
  geom_point(data=df, aes(x=x,y=y))
  #coord_polar()

p


#---------------------------------------------------------

#swirling rhombus

for (i in seq(0,2*pi,0.01)){
  x<- c(x,(i)*cos(i*gamma)*cos(i*gamma)*cos(i*gamma))
  y<- c(y,(i)*sin(i*gamma)*sin(i*gamma)*sin(i*gamma))
}

df <- data.frame(x = x, y = y)


p <- ggplot(data = df, aes(x=x,y=y))+
  geom_point(data=df, aes(x=x,y=y))
#coord_polar()

p


