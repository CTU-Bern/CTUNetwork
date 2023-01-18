nodes <- data.frame(id = 0:20, label = LETTERS[1:21])
edges <- data.frame(from = 0, to = 1:20, value = seq(0.35, 0.5, length.out = 20))
network <- visNetwork(nodes, edges) %>%
  visIgraphLayout(layout = "layout_on_sphere", physics = TRUE, randomSeed = 1234) %>%
  visNetwork::visOptions(highlightNearest = T, nodesIdSelection = list(enabled = TRUE, main = "Select Worker/Project"),
                         selectedBy = list(variable="group", main = "Select State")) %>% #, multiple=T
  visNetwork::visPhysics(solver = "hierarchicalRepulsion",
                         hierarchicalRepulsion = list(springLength = 850, nodeDistance = 90),
                         stabilization = list(enabled = F,
                                              iterations = 3000,
                                              updateInterval = 1)) %>%
  visNetwork::visNodes(shapeProperties = list(interpolation = F)) %>%
  visNetwork::visEdges(smooth = F, color=list(color = "#848484", highlight = "#000000"))

custom_network <- visNetworkEditor(object = network, filter = "physics")
custom_network
