library(arules)
library(plyr)

df_user= read.csv("./user.csv")
df_user = ddply(df_user,c("id"),function(dfl)paste(dfl$milestone, collapse = ","))
df_user$id = NULL
write.table(df_user,"./Milestones2.csv", quote=FALSE, row.names = FALSE, col.names = FALSE)
tr = read.transactions("./Milestones2.csv",format="basket",sep=",")
summary(tr)
itemFrequencyPlot(tr, topN=10)
rules = apriori(tr,parameter = list(supp=0.4,conf=0.6))
inspect(rules)
inspect(sort(rules,by='lift'))
itemsets=unique(generatingItemsets(rules))
itemsets
inspect(itemsets)


rules = apriori(tr,parameter = list(supp=0.3,conf=0.9))
rules=sort(rules,by='lift')
subset.matrix = is.subset(rules, rules)
precolumn=colSums(subset.matrix, na.rm=T) 

#REMOVE SUB RULES RULE, Consider only Main rule
x=subset.matrix@Dimnames[[1]]
subset.matrix[lower.tri(subset.matrix, diag=T)] = 0 
column=colSums(subset.matrix, na.rm=T) 
notredundant=c()
for(i in 1:length(column)){
  if(column[i]<1){
    if(precolumn[i]-column[i]==1){
      print(i)
      notredundant[i]=TRUE
    }
    else{
      notredundant[i]=FALSE
    }
  }
  else{
      notredundant[i]=TRUE
  }
}
rules=rules[notredundant]
rules=sort(rules,by='lift')
inspect(rules)

