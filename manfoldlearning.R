library(smotefamily)
library(plotly)

swissroll<-read.table("http://people.cs.uchicago.edu/~dinoj/manifold/swissroll.dat")

p<-plot_ly(swissroll, x = ~V1, y = ~V2, z = ~V3, 
           marker = list(size = 2),colors = c("black"))
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

total<-rbind(data,syn)
total.plot<-plot_ly(data = total, x = ~V1, y = ~V2, z = ~V3, 
                    marker = list(size = 3),color = ~class,colors = c("black", "red")) %>%
  layout(showlegend = TRUE, legend = list(size=5, orientation = 'h')) 

total.plot


