library(arules)
control_complete = read.csv("control complete.csv", sep = ",", header = TRUE)
for(j in 2: ncol(control_complete)){
  order = order(control_complete[,j], decreasing = TRUE)
  control_complete[,j] = 0
  for(i in 1:(floor(nrow(control_complete)*0.1)))
    {control_complete[order[i],j]=1
  }
}

case_complete = read.csv("case complete.csv", sep = ",", header = TRUE)
for(j in 2: ncol(case_complete)){
  order = order(case_complete[,j], decreasing = TRUE)
  case_complete[,j] = 0
  for(i in 1:(floor(nrow(case_complete)*0.1)))
  {case_complete[order[i],j]=1
  }
}
  
case_complete_withoutid = case_complete[,-1]
case_complete_matrix = as.matrix(case_complete_withoutid, nrow = nrow(case_complete), ncol=800)
case_AR = as(case_complete_matrix,"transactions")

for(i in seq(from=0.06, to=0.1, by=0.01)){
  frequentItems_case <- eclat (case_AR, parameter = list(supp = i, maxlen = 800))
  num_freq_case = paste(i, length(frequentItems_case), sep=" ")
  write.table(num_freq_case,"number of frequent items for case.txt", row.names = FALSE, col.names = FALSE, append = TRUE)
}

for(i in seq(from=0.06, to=0.1, by=0.01)){
  for(j in seq(from=0.5, to=0.9, by=0.1) ){
    rules_case <- apriori (case_AR, parameter = list(supp = i, conf = j))
    num_rules_case = paste(i, j,length(rules_case), sep=" ")
    write.table(num_rules_case,"number of rules for case.txt", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
}

control_complete_withoutid = control_complete[,-1]
control_complete_matrix = as.matrix(control_complete_withoutid, nrow = nrow(control_complete), ncol=800)
control_AR = as(control_complete_matrix,"transactions")

for(i in seq(from=0.06, to=0.1, by=0.01)){
  frequentItems_control <- eclat (control_AR, parameter = list(supp = i, maxlen = 800))
  num_freq_control = paste(i, length(frequentItems_control), sep=" ")
  write.table(num_freq_control,"number of frequent items for control.txt", row.names = FALSE, col.names = FALSE, append = TRUE)
}

for(i in seq(from=0.06, to=0.1, by=0.01)){
  for(j in seq(from=0.5, to=0.9, by=0.1) ){
    rules_control <- apriori (control_AR, parameter = list(supp = i, conf = j))
    num_rules_control = paste(i, j,length(rules_control), sep=" ")
    write.table(num_rules_control,"number of rules for control.txt", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
}

# Draw 3D plots, for AD and controls separately, showing the total number of frequent itemsets and ARs discovered 
# (in separate figures) as a function of the min support and min confidence.

install.packages("plot3D")
library("plot3D")
n_case = read.csv("number of ARs for case.csv", header = TRUE, sep = ",")
names(n_case)=c("minimal_support","minimal_confidence", "number_of_ARs")
scatter3D(n_case$minimal_support, n_case$minimal_confidence, n_case$number_of_ARs, bty = "g", type = "h", 
                pch = 16, cex = 1.5, xlab = "minimal_support", ylab = "minimal_confidence",
                zlab = "number_of_ARs", main= "number of ARs for case data",theta = 20, ticktype = "detailed")

n_control = read.csv("number of ARs for control.csv", header = TRUE, sep = ",")
names(n_control)=c("minimal_support","minimal_confidence", "number_of_ARs")
scatter3D(n_control$minimal_support, n_control$minimal_confidence, n_control$number_of_ARs, bty = "g", type = "h", 
          pch = 16, cex = 1.5, xlab = "minimal_support", ylab = "minimal_confidence",
          zlab = "number_of_ARs", main= "number of ARs for control data",theta = 20, ticktype = "detailed")

n_case = read.csv("number of frequent itemsets for case.csv", header = FALSE, sep = ",")
names(n_case)=c("minimal_support","minimal_confidence", "number_of_frequent_itemsets")
scatter3D(n_case$minimal_support, n_case$minimal_confidence, n_case$number_of_frequent_itemsets, bty = "g", type = "h", 
          pch = 16, cex = 1.5, xlab = "minimal_support", ylab = "minimal_confidence",
          zlab = "number_of_frequent_itemsets", main= "number of frequent itemsets for case data",theta = 20,ticktype = "detailed")

n_case = read.csv("number of frequent itemsets for control.csv", header = FALSE, sep = ",")
names(n_case)=c("minimal_support","minimal_confidence", "number_of_frequent_itemsets")
scatter3D(n_case$minimal_support, n_case$minimal_confidence, n_case$number_of_frequent_itemsets, bty = "g", type = "h", 
          pch = 16, cex = 1.5, xlab = "minimal_support", ylab = "minimal_confidence",
          zlab = "number_of_frequent_itemsets", main= "number of frequent itemsets for control data",theta = 20,ticktype = "detailed")

# Find top 50 frequent itemsets containing at least two items
frequentItems_control <- eclat (control_AR, parameter = list(supp = 0.05, maxlen = 800))
frequentItems_case <- eclat (case_AR, parameter = list(supp = 0.05, maxlen = 800))
top_frequentItems_control = sort(frequentItems_control, by="count", decreasing = TRUE)[c(801:850)]
top_frequentItems_case = sort(frequentItems_case, by="count", decreasing = TRUE)[c(801:850)]
inspect(top_frequentItems_control)
inspect(top_frequentItems_case)

# Find top confident ARs whose confidences are greater than the min confidence level of 80%, using 50 most frequent itemsets with support 
AR_80_control = ruleInduction(top_frequentItems_control,control_AR,confidence = 0.5)
AR_80_case = ruleInduction(top_frequentItems_case,case_AR,confidence = 0.8)
write(AR_80_control, "AR_80_control.csv", sep = ",", quote=TRUE)
write(AR_80_case, "AR_80_case.csv", sep = ",", quote=TRUE)