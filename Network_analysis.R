#load librray
library(igraph)
#make a edge list of all the connected edges
el <- cbind(c("a","a","a","b","e"),c("b","c","d","e","c"))
#mkae a igraph object
g <- graph.edgelist(as.matrix(el),directed = F)
#print i graph object
g
#UN-- 5 5 -- first 5 is no. of nodes second 5 is no of edges
#plot g from graph visualization
plot(g)
#Adding nodes attributes
g <- set_vertex_attr(g,"age",value = c(18,16,20,22,21))

g
#see the change now it says attr: name (v/c), age (v/n) which means age is now a attribute
vertex_attr(g)
#adding edges attributes like how many times people call each other
g <- set_edge_attr(g,"freq",value = c(3,5,6,2,11))

g
#hear it says attr: name (v/c), age (v/n), freq (e/n) now freq is a attribute
edge_attr(g)
# now we see v/n is for vertices or nodes and e/n is for edges 

plot(g)

#Adding atributes from data frame is also eazy
ver <- cbind(c("a","b","c","d","e"),c(16,18,22,20,21)) %>% as.data.frame()
edge <- cbind(el,c(3,5,6,8,9)) %>% as.data.frame()
graph_from_data_frame(d=edge,vertices = ver,directed = F)

#find info
#find all where nodes (inc)LUDES "a" 
E(g)[[inc("a")]]
#find all where edges freq >5
E(g)[[freq>5]]

#adding plot parameters
V(g)$color <- ifelse(V(g)$age>19,"white","red")
plot(g,vertex.label.color="black")
E(g)$color <- ifelse(E(g)$freq>6,"green","blue")
plot(g)

#layout parameters
plot(g,layout=layout.davidson.harel(g))
plot(g,layout=layout.circle(g))
plot(g,layout=layout.fruchterman.reingold(g))
#assign properties to edges
w1 <- E(g)$freq
plot(g,edge.color="black",edge.width=w1,layout=layout_nicely(g))
#layout nicely for default selection of a good layout

#Directed networks
email_from <- c("A","B","C","D","E","E","E")
email_to <- c("F","A","A","A","F","G","A")
emails <- cbind(email_from,email_to) %>% as.data.frame()
g <- graph.edgelist(as.matrix(emails),directed = T)
g
plot(g,layout=layout_nicely(g))
#check the no of edges between any two
g["A","E"]
g["E","A"]
#show all edges to or from A
incident(g,"A",mode=c("all"))
incident(g,"A",mode=c("out"))
incident(g,"A",mode=c("in"))
incident(g,"A",mode=c("total"))
#to get neighbors
neighbors(g,"A",mode=c("all"))
#for intersections
x <- neighbors(g,"A",mode = c("all"))
y <- neighbors(g,"F",mode = c("all"))
intersection(x,y)
#to get the maximum distance
farthest_vertices(g)
#to get sequence of diamater
get_diameter(g)
#identify nodes that can be reached by n steps by F
ego(g,2,"F",mode = c("all"))
#to calculate degree
degree(g,mode=c("in"))
degree(g,mode=c("out"))
degree(g,mode=c("all"))
#to calculate betweenness
betweenness(g,directed = T)
betweenness(g,directed = F)
betweenness(g,directed = T,normalized = T)
?make_ego_graph
make_ego_graph(g,diameter(g),nodes = V(g),mode = c("all"))[[1]]

#network structure

eigen_centrality(g)
?eigen_centrality

#edge density
edge_density(g)
#####will end here but will update in future