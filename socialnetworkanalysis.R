library(igraph) ##install package igraph
g<-graph(c(1,2,2,3,3,4,4,1),directed=F,n=7)
plot(g,vertex.color="green") ##plots from node 1 to node 2
g[] ##gives the matrix of how the connections run
##with names
g1<-graph(c("amy","ram","ram","lee","lee","amy"))
plot(g1,vertex.color="green",vertex.size=40)
g1
degree(g1,mode='all')##how many arrows are going in and out
degree(g1,mode='in')##how many arrows are going in
degree(g1,mode='out')##how many arrows are going out
diameter(g1,directed=F,weights=NA)
ecount(g1)/(vcount(g1)*(vcount(g1)-1))##ratio of number of edges to the number of possible edges
reciprocity(g1)
closeness(g1,mode='all',weights=NA)
betweenness(g1,directed=T,weights=NA)
edge_betweenness(g1,directed=T,weights=NA)
##READ DATA FILE
data<-read.csv("networkdata.csv",header=T)
y<-data.frame(data$first,data$second)
net<-graph.data.frame(y,directed=T)
V(net)
E(net)
V(net)$label<-V(net)$name
V(net)$degree<-degree(net)
V(net)$degree
hist(V(net)$degree,col='green',main='histogram of node degree',ylab='frequency',xlab='degree of vertices')
set.seed(200)
plot(net,vertex.color='green',vertex.size=2,edge.arrow.size=0.4,vertext.label.cex=0.8,layout=layout.fruchterman.reingold)
#hubs and authorities, hubs have outgoing links, authorities have incoming links
hs<-hub_score(net)$vector
as<-authority.score(net)$vector
hs #for each hub there is a score
as #for each authority there is a score
par(mfrow=c(1,2))
set.seed(13)
plot(net,vertex.size=hs*30,main='Hubs',vertex.color=rainbow(52),edge.arrow.size=0.1,layout=layout.kamada.kawai)
set.seed(13)
plot(net,vertex.size=as*30,main='Authorities',vertex.color=rainbow(52),edge.arrow.size=0.1,layout=layout.kamada.kawai)
##community detection of the dataset
net<-graph.data.frame(y,directed=F)
cnet<-cluster_edge_betweenness(net)
plot(cnet,net,vertex.label.cex=0.1)










