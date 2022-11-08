library(networkDynamic)
library(viridis)

####### READ IN DATA #######

edges_dynamic <-  read.csv("Fourregion-PHDynamicEdges-new.csv")
nodes_dynamic <- as.data.frame(read.csv("PHDynamicsNodes-all-new.csv"))
vertex_attributes <- read.csv("PHVertexAttributes-All-total-new.csv", stringsAsFactors = FALSE)

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
plot(myresults[,c(48,60,15,8)],plot.type = "multiple",xy.lines = TRUE, yax.flip = TRUE, 
     main="Degree Centrality")









