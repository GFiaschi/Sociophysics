#EVALUATION OF NETWORK IN UK

library(ggplot2)
library(rgl)
library(igraph)
library(dplyr)
library(leaflet) #useful to create interactive_map


#data importing for population extract from ....

Data_import<-read.csv("C:\\Users\\fiasc\\Downloads\\country-cities-data.csv")

#filter data_population 
Data<-Data_import %>% 
  filter(Data_import$city %in% c(Data_import$city) & Data_import$pop2024>=50000)


#edges and nodes from reference, I use movements from reference 3. Gallotti, R. & Barthelemy, M. Dryad. http://dx.doi.org/10.5061/dryad.pc8m3 (2014).
edges_data_frame<-read.csv("C:\\Users\\fiasc\\OneDrive\\Desktop\\edges.csv", sep=",")

nodes_data_frame<-read.csv("C:\\Users\\fiasc\\OneDrive\\Desktop\\nodes.csv", sep=",")

area_data_frame<-read.csv("C:\\Users\\fiasc\\OneDrive\\Desktop\\Admin Areas.csv", sep=",")

#nodes with all information
nodes_merge<-merge(nodes_data_frame, 
                   area_data_frame, 
                   by.x="zone",
                   by.y="ATCO.Code",
                   all.x=T
  
)

nodes<-merge(nodes_merge, 
             Data_import, 
             by.x="Admin.Area.Name",
             by.y="city",
             all.x=T)

nodes<-nodes%>%select(-c( lat, lon, Call.Centre.ID, zone, atcocode, country, Issue.Version,Admin.Area.ID, Traveline.Region.ID))


edges_data_merged<-merge(nodes,
                         edges_data_frame, 
                         by.x="node",
                         by.y="ori_node", 
                         all.x=T
  
)

#edge_list 
edges_data_merged<-edges_data_merged%>%select(-c(layer, Admin.Area.Name, pop2024, minutes, latitude, longitude))


colnames(edges_data_merged)[colnames(edges_data_merged)=="node"]<-"ori_node"


#plotting movements on layer 0
#selection of the layer in edge_list and association with longitude and latitude 

edges_air_transport<-edges_data_merged[edges_data_merged$ori_layer==0 & edges_data_merged$des_layer==0,]
edges_air_transport<-na.omit(edges_air_transport)

edges_with_coords <- edges_air_transport %>%
  left_join(nodes_data_frame, by = c("ori_node" = "node"), relationship = "many-to-many") %>%
  rename(ori_lon = lon, ori_lat = lat)


edges_with_coords <- edges_with_coords %>%
  left_join(nodes_data_frame, by = c("des_node" = "node"), relationship ="many-to-many") %>%
  rename(des_lon = lon, des_lat = lat)

edges_with_coords$weight<-rep(1, nrow(edges_with_coords)

#visualization of map 
m <- leaflet() %>%
  addTiles() %>%
  setView(lng =-1.10784 , lat =50.79695, zoom = 6)



for (i in 1:nrow(edges_with_coords)) {
  m <- m %>%
    addPolylines(lng = c(edges_with_coords$ori_lon[i], edges_with_coords$des_lon[i]),
                 lat = c(edges_with_coords$ori_lat[i], edges_with_coords$des_lat[i]),
                 color = "black",
                 weight = 1)
}

m

#Analysis of mixing patterns in connections 
g<-graph_from_data_frame(edges_with_coords, directed= T)


#degree_distribution 
degree<-degree(g, mode="all")
degree_distribution<-degree.distribution(g)
LCC<-components(g)
LCC$csize[which.max(LCC$csize)]
#degree correlation, I can use it to establish how the system is

ori_nodes<-ends(g, E(g))[,1]
des_nodes<-ends(g, E(g))[,2]

degree_from<-degree[ori_nodes]
degree_to<-degree[des_nodes]


degree_data_frame<-data.frame(
  degree_from<-degree_from,
  degree_to<-degree_to
)

#evaluation of mixing pattern 
degree_correlation<-cor(degree_data_frame$degree_from, degree_data_frame$degree_to)
if(degree_correlation<0){
  cat("disassortative mixing_pattern ", degree_correlation)
}

#evaluation of failures on this graph 


#extraction of the degree with high value 
degree_ordened<-degree[order(degree, decreasing = T)]
degree_highest<-degree_ordened[1]
high<-which(degree==degree_highest)

#graph
new_graph<-delete_vertices(g, high)

#evaluation LCC sizes 
LCC_degree<-components(new_graph)

largest_degree<-LCC_degree$csize[which.max(LCC_degree$csize)]
cat("LCC_size", largest_degree)

#extractions of data.frame 
node_remaining_frame<-as.data.frame(igraph::as_data_frame(new_graph, what="vertices"))

edges_remaining_frame<-as.data.frame(igraph::as_data_frame(new_graph, what="edges"))


#visualization of map with remove connection 
m_1<- leaflet() %>%
  addTiles() %>%
  setView(lng =-1.10784 , lat =50.79695, zoom = 6)



for (i in 1:nrow(edges_remaining_frame)) {
  m_1<- m_1 %>%
    addPolylines(lng = c(edges_remaining_frame$ori_lon[i], edges_remaining_frame$des_lon[i]),
                 lat = c(edges_remaining_frame$ori_lat[i], edges_remaining_frame$des_lat[i]),
                 color = "red",
                 weight = 0.1)
}

m_1


#Analysis of the networks for train-transports 


edges_train_transport<-edges_data_merged[edges_data_merged$ori_layer==2 & edges_data_merged$des_layer==2,]
edges_train_transport<-na.omit(edges_train_transport)


edges_train_with_coords <- edges_train_transport %>%
  left_join(nodes_data_frame, by = c("ori_node" = "node"), relationship = "many-to-many") %>%
  rename(ori_lon = lon, ori_lat = lat)


edges_train_with_coords <- edges_train_with_coords %>%
  left_join(nodes_data_frame, by = c("des_node" = "node"), relationship ="many-to-many") %>%
  rename(des_lon = lon, des_lat = lat)


#visualization of effective movements with train-line system (da cambiare perchè non è sostenibile nel numero di connessioni)

library(sf)
library(rnaturalearth)
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)

#get UK_map
uk_map <- ne_countries(scale = "medium", returnclass = "sf", country = "United Kingdom")

#for batter visualization I prefer using this kind of map visualization without text reference of nodes due the fruibility of map 
ggplot(data = uk_map) +
  geom_sf(fill = "lightgray", color = "black") +
  geom_segment(data = edges_train_with_coords, 
               aes(x = ori_lon, y = ori_lat, xend = des_lon, yend = des_lat), 
               color = "blue", linewidth = 1, alpha = 0.5) +  # Archi
  geom_point(data = subset(nodes_merge, layer==2), aes(x = lon, y = lat), 
             color = "red", size = 0.1) +
  labs(title = "Network overlapping to UK",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 2), ylim = c(50, 60), expand = FALSE)  # Limiti per inquadrare la Gran Bretagna


#Analysis of mixing patterns in connections 
train_map_patterns<-graph_from_data_frame(edges_train_with_coords, directed= T)


#degree_distribution 
degree_train<-degree(train_map_patterns, mode="all")
degree_distribution_train<-degree.distribution(train_map_patterns)
LCC_train<-components(train_map_patterns)
LCC_train$csize[which.max(LCC_train$csize)]
#degree correlation, I can use it to establish how the system is


ori_train_nodes<-ends(train_map_patterns, E(train_map_patterns))[,1]
des_train_nodes<-ends(train_map_patterns, E(train_map_patterns))[,2]

degree_train_from<-degree_train[ori_train_nodes]
degree_train_to<-degree_train[des_train_nodes]


degree_train_data_frame<-data.frame(
  degree_train_from<-degree_train_from,
  degree_train_to<-degree_train_to
)


#evaluation of mixing pattern 
degree_train_correlation<-cor(degree_train_data_frame$degree_train_from, degree_train_data_frame$degree_train_to)
#printing of result
cat("disassortative mixing_pattern ", degree_train_correlation)
if (degree_train_correlation >0 ){
  print("The network is Assortative")
}
if(degree_train_correlation<0){
  print("the Network is disassortative")
}
if (degree_train_correlation == 0) {
  print("the network is neutral")
} else {
  print("The network is neutral")
}


#extraction of the degree with high value 
degree_train_ordened<-degree_train[order(degree_train, decreasing = T)]
degree_train_highest<-degree_train_ordened[1]
high_train<-which(degree_train==degree_train_highest)

#graph
new_graph_train<-delete_vertices(train_map_patterns, high_train)

#evaluation LCC sizes 
LCC_degree_train<-components(new_graph_train)

largest_degree_train<-LCC_degree_train$csize[which.max(LCC_degree_train$csize)]
cat("LCC_size", largest_degree_train)

#extractions of data.frame 
node_train_remaining_frame<-as.data.frame(igraph::as_data_frame(new_graph_train, what="vertices"))
node_train_remaining_frame<- merge( nodes_merge, 
                                    node_train_remaining_frame, 
                                    by.x="node", 
                                    by.y="name") %>% select(node, lat, lon) 

edges_train_remaining_frame<-as.data.frame(igraph::as_data_frame(new_graph_train, what="edges"))

edges_train_remaining_frame <- na.omit(edges_train_remaining_frame)
node_train_remaining_frame <- na.omit(node_train_remaining_frame)



#visualization of graph
ggplot(data = uk_map) +
  geom_sf(fill = "lightgray", color = "black") +
  geom_segment(data = edges_train_remaining_frame, 
               aes(x = ori_lon, y = ori_lat, xend = des_lon, yend = des_lat), 
               color = "blue", linewidth = 1, alpha = 0.5) +  # Archi
  geom_point(data = node_train_remaining_frame, aes(x = lon, y = lat), 
             color = "red", size = 0.1) +
labs(title = "Network with percolation on Uk",
     x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 2), ylim = c(50, 60), expand = FALSE)  





#controllare eventuali pesi, riplotta il primo degree su train 





















































































