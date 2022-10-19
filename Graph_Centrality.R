library(networkDynamic)
library(viridis)

####### READ IN DATA #######

edges_dynamic <-  read.csv("Fourregion-PHDynamicEdges.csv")
nodes_dynamic <- as.data.frame(read.csv("PHDynamicNodes-all.csv"))
vertex_attributes <- read.csv("PHVertexAttributes-All-total.csv", stringsAsFactors = FALSE)

###############################################################################

#### create static network data set
edges_static <- subset(edges_dynamic, select = c(tail,head))

### STATIC NETWORK (all connections at any time)

thenetwork <- network(
  edges_static,
  vertices = vertex_attributes,
  vertex.attrnames = c("vertex.id", "name", "region" ),
  directed = FALSE,
  bipartite = FALSE,
  multiple = TRUE
)
network.vertex.names(thenetwork)<-vertex_attributes$name ### allows names placed on graphs

summary(thenetwork)
class(thenetwork)

# This plots static network:
plot(thenetwork, displaylabels=TRUE)

############ TEMPORAL NETWORK ############

dynamicCollabs <- networkDynamic(
  thenetwork,
  edge.spells = edges_dynamic,
  vertex.spells =  nodes_dynamic
)
summary(dynamicCollabs)
# Check the temporal network
network.dynamic.check(dynamicCollabs)

##### deactivate multiple edge times for Belize Valley analysis

deactivate.edges(dynamicCollabs, e=1, onset=799, terminus = 800) 
deactivate.edges(dynamicCollabs, e=1, onset=695, terminus = 736) 
deactivate.edges(dynamicCollabs, e=5, onset=485, terminus = 486) 
deactivate.edges(dynamicCollabs, e=5, onset=695, terminus = 736)
deactivate.edges(dynamicCollabs, e=83, onset=485, terminus = 486) 
deactivate.edges(dynamicCollabs, e=83, onset=799, terminus = 800)

deactivate.edges(dynamicCollabs, e=9, onset=475, terminus = 849)
deactivate.edges(dynamicCollabs, e=10, onset=475, terminus = 849)
deactivate.edges(dynamicCollabs, e=17, onset=475, terminus = 849)
deactivate.edges(dynamicCollabs, e=22, onset=475, terminus = 849)

deactivate.edges(dynamicCollabs, e=30, onset=598, terminus = 820)
deactivate.edges(dynamicCollabs, e=33, onset=598, terminus = 820)
deactivate.edges(dynamicCollabs, e=34, onset=598, terminus = 820)
deactivate.edges(dynamicCollabs, e=35, onset=598, terminus = 820)
deactivate.edges(dynamicCollabs, e=69, onset=598, terminus = 820)


################################################################
##### NODE LEVEL METRICS 

source("NodeLevelFunctions.R")

myresults<-degreeCent(350,900)
plot(myresults, plot.type = "single",xy.labels = TRUE,xy.lines = TRUE, 
     col=viridis(14,alpha = 1), 
     lty=1:14,
     ylab = "",
     lwd=3,
     main="Degree Centrality")
summary(myresults)
plot(myresults[,c(8,12,7,2)],plot.type = "multiple",xy.lines = TRUE, yax.flip = TRUE, 
     main="Degree Centrality")









