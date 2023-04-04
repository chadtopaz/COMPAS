library(dagitty)
library(SEMgraph)
library(tidyverse)


mydag <- dagitty('dag {
  bb="0,0,1,1"
  "court type" [pos="0.272,0.378"]
  "public defender" [pos="0.541,0.384"]
  "social factors" [pos="0.670,0.043"]
  age [pos="0.391,0.049"]
  charges [pos="0.388,0.297"]
  compas [exposure,pos="0.888,0.445"]
  gender [pos="0.500,0.055"]
  judge [pos="0.190,0.548"]
  plea [pos="0.328,0.539"]
  prison [outcome,pos="0.607,0.956"]
  "court type" -> "public defender"
  "court type" -> judge
  "court type" -> plea
  "public defender" -> plea
  "public defender" -> prison
  "social factors" -> "public defender"
  "social factors" -> charges
  "social factors" -> compas
  "social factors" -> prison
  age -> charges
  age -> compas
  age -> prison
  charges -> "court type"
  charges -> compas
  compas -> plea
  compas -> prison
  gender -> charges
  gender -> compas
  gender -> prison
  judge -> plea
  judge -> prison
  plea -> prison
}')

g <- mydag %>% dagitty2graph()
mylayout <- layout_with_sugiyama(g)
plot(g, layout = mylayout)