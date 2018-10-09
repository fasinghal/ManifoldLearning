library(smotefamily)
library(plotly)

#Swiss Roll
swissroll<-read.table("http://people.cs.uchicago.edu/~dinoj/manifold/swissroll.dat")

p<-plot_ly(swissroll, x = ~V1, y = ~V2, z = ~V3, 
           marker = list(size = 2, color='rgba(0, 0, 0, 1)'))
p

data<-swissroll
data$class<-factor("Original")

dbscan <- DBSMOTE(X = data[,-4],target = data$class,dupSize = 1)
smote<-SMOTE(X = data[,-4], target = data$class, dup_size = 1)
sls<- SLS(X =data[,-4], target=data$class, K = 5, C = 5,dupSize = 1)


syn<-dbscan$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")

syn<-sls$syn_data
syn$class<-NULL
syn$class<-factor("SLS")

syn<-smote$syn_data
syn$class<-NULL
syn$class<-factor("SMOTE")

total<-rbind(data,syn)
total.plot<-plot_ly(data = total, x = ~V1, y = ~V2, z = ~V3,marker = list(size = 3),color = ~class,colors = c("black", "darkturquoise")) %>%
  layout(showlegend = TRUE, legend = list(size=5, orientation = 'h'))

total.plot


# S shape data

n = 2000

t = runif(n = n ,min = 0, max = 1)
y = runif(n = n ,min = 0, max = 1)* 50
theta = pi * (t -0.5)
x = sin(theta)
z = sin(theta)* (cos(theta)-0.5)
S.data<-as.data.frame(cbind(x,y,z))
S.plot<-plot_ly(data = S.data, x = ~x, y = ~y, z = ~z, 
                marker = list(size = 2, color='rgba(0, 0, 0, 1)')) 
S.plot
S.data$class<-factor("Original")

S.dbscan <- DBSMOTE(X = S.data[,-4],target = S.data$class,dupSize = 1)

S.smote <- SMOTE(X=S.data[,-4],target = S.data$class, dup_size = 1,K = 5)

syn<-S.dbscan$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")

syn<-S.smote$syn_data
syn$class<-NULL
syn$class<-factor("SMOTE")

S.total<-rbind(S.data,syn)

S.total.plot<-plot_ly(data = S.total, x = ~x, y = ~y, z = ~z, 
                    marker = list(size = 3),color = ~class,colors = c("black", "red")) %>%
  layout(showlegend = TRUE, legend = list(size=5, orientation = 'h')) 

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
                        marker = list(size = 3)) %>%
  layout(showlegend = TRUE, legend = list(size=5, orientation = 'h'))

toroidal.plot

toroidal$class<-factor("Original")
toroidal.db <-DBSMOTE(X =toroidal[,-4],target = toroidal$class,dupSize = 1 )

syn<-toroidal.db$syn_data
syn$class<-NULL
syn$class<-factor("DBSMOTE")

total<- rbind(toroidal, syn)

toroidal.plot<-plot_ly(data = total, x = ~x, y = ~y, z = ~z, 
                       marker = list(size = 3),color = ~class,colors = c("black", "red")) %>%
  layout(showlegend = TRUE, legend = list(size=5, orientation = 'h')) 
toroidal.plot

#-- Cloud experiment # Fail

indx<- sample(x = 1:1000,size = 1000,replace = FALSE)
rand<-runif(n = 1000,min = 0,max = 0.005)
neg<-sample(x = c(-1,1),size = 1000,replace = TRUE)

x_1<-x[indx]+rand[indx]*neg

indx<- sample(x = 1:1000,size = 1000,replace = FALSE)
rand<-runif(n = 1000,min = 0,max = 0.005)
y_1<-y[indx]+neg*rand[indx]

indx<- sample(x = 1:1000,size = 1000,replace = FALSE)
rand<-runif(n = 1000,min = 0,max = 0.005)
z_1<-z[indx]+neg*rand[indx]

toroidal_1<-as.data.frame(cbind(x_1,y_1,z_1))
toroidal.plot_1 <-plot_ly(data = toroidal_1, x = ~x_1, y = ~y_1, z = ~z_1, 
                          marker = list(size = 2, color='rgba(0, 0, 0, 1)'))

toroidal.plot_1


# Manifold 2

n = 2000

t = runif(n = n ,min = 0, max = 1)
y = runif(n = n ,min = 0, max = 1)* 50
theta = pi * (t -0.5)
x = sin(theta*theta)
z = sin(theta)* (cos(theta)-0.5)
S.data<-as.data.frame(cbind(x,y,z))
S.plot<-plot_ly(data = S.data, x = ~x, y = ~y, z = ~z, 
                marker = list(size = 2, color='rgba(0, 0, 0, 1)')) 
S.plot