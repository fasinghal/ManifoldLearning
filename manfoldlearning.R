library(smotefamily)
library(plotly)

#Swiss Roll
swissroll<-read.table("http://people.cs.uchicago.edu/~dinoj/manifold/swissroll.dat")

p<-plot_ly(swissroll, x = ~V1, y = ~V2, z = ~V3, 
  marker = list(size = 2, color='rgba(0, 0, 0, 1)')) %>%
  layout(title = "Swissroll Dataset")      
p

data<-swissroll
data$class<-factor("Original")

dbscan <- DBSMOTE(X = data[,-4],target = data$class,dupSize = 1)
smote<-SMOTE(X = data[,-4], target = data$class, dup_size = 1)
sls<- SLS(X =data[,-4], target=data$class, K = 5, C = 5,dupSize = 1)


syn<-dbscan$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")
total<-rbind(data,syn)
total.plot<-plot_ly(data = total, x = ~V1, y = ~V2, z = ~V3,marker = list(size = 3),color = ~class,colors = c("black", "red")) %>%
  layout(title = "Swissroll Dataset + DBSMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h'))
total.plot

syn<-sls$syn_data
syn$class<-NULL
syn$class<-factor("SLS")
total<-rbind(data,syn)
total.plot<-plot_ly(data = total, x = ~V1, y = ~V2, z = ~V3,marker = list(size = 3),color = ~class,colors = c("black", "darkturquoise")) %>%
  layout(title = "Swissroll Dataset + SLS",showlegend = TRUE, legend = list(size=5, orientation = 'h'))
total.plot

syn<-smote$syn_data
syn$class<-NULL
syn$class<-factor("SMOTE")
total<-rbind(data,syn)
total.plot<-plot_ly(data = total, x = ~V1, y = ~V2, z = ~V3,marker = list(size = 3),color = ~class,colors = c("black", "deeppink")) %>%
  layout(title = "Swissroll Dataset + SMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h'))
total.plot

# S shape data

#Generate Data
n = 2000
t = runif(n = n ,min = 0, max = 1)
y = runif(n = n ,min = 0, max = 1)* 50
theta = pi * (t -0.5)
x = sin(theta)
z = sin(theta)* (cos(theta)-0.5)
S.data<-as.data.frame(cbind(x,y,z))

#Plot the original Data
S.plot<-plot_ly(data = S.data, x = ~z, y = ~y, z = ~x, 
  marker = list(size = 2, color='rgba(0, 0, 0, 1)'))  %>%
  layout(title = "S-Curve Dataset")      
S.plot

S.data$class<-factor("Original")

#Oversampling
S.dbscan <- DBSMOTE(X = S.data[,-4],target = S.data$class,dupSize = 1)
S.smote <- SMOTE(X=S.data[,-4],target = S.data$class, dup_size = 1,K = 5)
S.SLS <- SLS(X = S.data[,-4], target =S.data$class, K = 5, C = 5,dupSize = 1)

syn<-S.dbscan$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")
S.total<-rbind(S.data,syn)
S.total.plot<-plot_ly(data = S.total, x = ~z, y = ~y, z = ~x, 
                      marker = list(size = 3),color = ~class,colors = c("black", "firebrick1")) %>%
  layout(title = "S Curve Dataset + DBSMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
S.total.plot

syn<-S.smote$syn_data
syn$class<-NULL
syn$class<-factor("SMOTE")

S.total<-rbind(S.data,syn)
S.total.plot<-plot_ly(data = S.total, x = ~z, y = ~y, z = ~x, 
                    marker = list(size = 3),color = ~class,colors = c("black", "darkturquoise")) %>%
  layout(title = "S Curve Dataset + SMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
S.total.plot


syn<-S.SLS$syn_data
syn$class<-NULL
syn$class<-factor("SLS")

S.total<-rbind(S.data,syn)
S.total.plot<-plot_ly(data = S.total, x = ~z, y = ~y, z = ~x, 
                      marker = list(size = 3),color = ~class,colors = c("black", "darkolivegreen1")) %>%
  layout(title = "S Curve Dataset + SLS",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
S.total.plot


#toroidal helix data

u<-runif(n = 1000, min =0, max=2000)
k<-0.05
v<-u*k
a<-3
b<-1
x<-(a+b*cos(u))*cos(v)
y<-(a+b*cos(u))*sin(v)
z<-b*sin(u)

toroidal <- as.data.frame(cbind(x,y,z))
toroidal.plot <-plot_ly(data = toroidal, x = ~x, y = ~y, z = ~z, 
                        marker = list(size = 2, color='rgba(0, 0, 0, 1)'))  %>%
  layout(title = "Toroidal Helix Dataset")      
toroidal.plot

toroidal$class<-factor("Original")


toroidal.db <-DBSMOTE(X =toroidal[,-4],target = toroidal$class,dupSize = 1 )
toroidal.smote<-SMOTE(X = toroidal[,-4], target = toroidal$class, dup_size = 1)
toroidal.sls<-SLS(X = toroidal[,-4], target = toroidal$class, K = 5, C = 5,dupSize = 1)

syn<-toroidal.db$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")
total<- rbind(toroidal, syn)
toroidal.plot<-plot_ly(data = total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 3),color = ~class,colors = c("black", "red")) %>%
  layout(title = "Toroidal Helix + DBSMOTE" ,showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
toroidal.plot



syn<-toroidal.smote$syn_data
syn$class<-NULL
syn$class<-factor("SMOTE")
total<- rbind(toroidal, syn)
toroidal.plot<-plot_ly(data = total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 3),color = ~class,colors = c("black", "darkorchid2")) %>%
  layout(title = "Toroidal Helix + SMOTE" ,showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
toroidal.plot

syn<-toroidal.sls$syn_data
syn$class<-NULL
syn$class<-factor("SLS")
total<- rbind(toroidal, syn)
toroidal.plot<-plot_ly(data = total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 3),color = ~class,colors = c("black", "lawngreen")) %>%
  layout(title = "Toroidal Helix + SLS" ,showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
toroidal.plot

# Manifold 2

n = 2000
t = runif(n = n ,min = 0, max = 1)
y = runif(n = n ,min = 0, max = 1)* 50
theta = pi * (t -0.5)
x = sin(theta*theta)
z = sin(theta)* (cos(theta)-0.5)
m2.data<-as.data.frame(cbind(x,y,z))
m2.data$class<-factor("Original")

m2.plot<-plot_ly(data = m2.data, x = ~x, y = ~y, z = ~z, 
                marker = list(size = 2, color='rgba(0, 0, 0, 1)')) %>%
  layout(title = "Manifold 1")
m2.plot

m2.dbscan <- DBSMOTE(X =m2.data[,-4],target = m2.data$class,dupSize = 1)
syn<-m2.dbscan$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")
m2.total<-rbind(m2.data,syn)
m2.total.plot<-plot_ly(data = m2.total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 2),color = ~class,colors = c("black", "red")) %>%
  layout(title = "Manifold 1 + DBSMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
m2.total.plot


m2.smote <- SMOTE(X=m2.data[,-4],target = m2.data$class, dup_size = 1,K = 5)
syn<-m2.smote$syn_data
syn$class<-NULL
syn$class<-factor("SMOTE")
m2.total<-rbind(m2.data,syn)
m2.total.plot<-plot_ly(data = m2.total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 2),color = ~class,colors = c("black", "dodgerblue")) %>%
  layout(title = "Manifold 1 + SMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
m2.total.plot


m2.SLS <- SLS(X = m2.data[,-4], target =m2.data$class, K = 5, C = 5,dupSize = 1)
syn<-m2.SLS$syn_data
syn$class<-NULL
syn$class<-factor("SLS")
m2.total<-rbind(m2.data,syn)
m2.total.plot<-plot_ly(data = m2.total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 2),color = ~class,colors = c("black", "gold")) %>%
  layout(title = "Manifold 1 + SLS",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
m2.total.plot

#Sphere data

x<-rnorm(n = 2000, mean = 0, sd = 1)
y<-rnorm(n = 2000, mean = 0, sd = 1)
z<-rnorm(n = 2000, mean = 0, sd = 1)
norm <- 1/sqrt(x*x + y*y + z*z)

x<-x*norm
y<-y*norm
z<-z*norm

sphere<-as.data.frame(cbind(x,y,z))
sphere$class<-factor("Original")

sphere.plot<-plot_ly(data = sphere, x = ~x, y = ~y, z = ~z, 
                     marker = list(size = 1, color='rgba(0, 0, 0, 1)'))  %>%
  layout(title = "Sphere Dataset")
sphere.plot

sphere.db<-DBSMOTE(X = sphere[,-4],target =sphere$class, dupSize = 1 )
syn<-sphere.db$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")

sphere.total<-rbind(sphere,syn)
sphere.os.plot<-plot_ly(data = sphere.total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 1),color = ~class,colors = c("black", "red")) %>%
  layout(title = "Sphere Dataset + DBSMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
sphere.os.plot

#Lets examine the points
# Unit square with R =1 
sqrt(x*x + y*y + z*z)

#Check the synthetic points
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)>1)
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)==1)
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)<0.99)
sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)


# SMote on sphere

sphere.smote<-SMOTE(X = sphere[,-4],target =sphere$class,K = 5,dup_size = 1)
syn<-sphere.smote$syn_data
syn$class<-NULL
syn$class<-factor("SMOTE")

sphere.total<-rbind(sphere,syn)
sphere.os.plot<-plot_ly(data = sphere.total, x = ~x, y = ~y, z = ~z, 
                        marker = list(size = 1),color = ~class,colors = c("black", "lawngreen")) %>%
  layout(title = "Sphere Dataset + SMOTE",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
sphere.os.plot

#Diagonostics
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)>1)
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)==1)
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)<0.99)
sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)

# SLS on sphere

sphere.sls<-SLS(X = sphere[,-4],target =sphere$class,K = 5,C = 5,dupSize = 1)
syn<-sphere.sls$syn_data
syn$class<-NULL
syn$class<-factor("SLS")

sphere.total<-rbind(sphere,syn)
sphere.os.plot<-plot_ly(data = sphere.total, x = ~x, y = ~y, z = ~z, 
                        marker = list(size = 1),color = ~class,colors = c("black", "mediumorchid1")) %>%
  layout(title = "Sphere Dataset + SLS",showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
sphere.os.plot

#Diagonostics
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)>1)
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)==1)
sum(sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)<0.99)
sqrt(syn$x*syn$x + syn$y*syn$y + syn$z*syn$z)
