edges <- read.csv("edges.csv")
users <- read.csv("users.csv")

292/59

table(users[users$school != "", ]$locale)

table(users$school, users$gender)

install.packages("igraph")
library(igraph)
?graph.data.frame
g <- graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)

degree(g)[degree(g) >= 10]

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
