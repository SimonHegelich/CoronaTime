##### Simple Simulation of Virus Spread in a Small World Network

library(igraph)
G <- sample_smallworld(1, 100, 5, 0.2)
plot(G)

# function for virus spread
# n = Number of initial infected, p = probability of infection, st = timesteps
spreadVirus <- function(G, n=3, p=0.2, st=10){
  infected <- c()
  infected <- c(infected, sample(length(V(G)), n))
  V(G)[infected]$color <- "red"
  for (b in 1:st){
    for (a in infected){
      ng <- neighbors(G, a)
      newInf <- sample(ng, round(p * length(ng)))
      V(G)[newInf]$color <- "red"
      infected <- c(infected, newInf)
      
    }
  }
  return(G)
}

G1 <- spreadVirus(G, st= 5)
d <- 0.3 # percentage of edges to be randomly removed
G2 <- spreadVirus(delete_edges(G, sample(E(G), round(0.3  * length(E(G))))), st = 5)
plot(G1)
# number of infected after st timesteps
length(which(V(G1)$color=="red"))
plot(G2)
length(which(V(G2)$color=="red"))
