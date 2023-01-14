
############################################################################
#
#       Market Basket Analysis 
#
############################################################################

library(arules)
library(arulesViz)


grocerieslist = read.csv("C:/Users/thami/OneDrive/Desktop/Pastas Git/Market_Basket_Analysis/grocerieslist.csv")


GL=grocerieslist 
dim(GL); class(GL);  head(GL)

write.csv(GL, "GLx.csv",quote=F, row.names = T) 
GLITENS=read.transactions("GLx.csv",format = "basket", sep = ",")
summary(GLITENS)
sort(itemFrequency(GLITENS), decreasing = T)
itemFrequency(GLITENS)
reg=apriori(GLITENS, parameter = list(supp=.1,conf=0.5, minlen=1, maxlen=5, maxtime=25, target="rules"))
reg=apriori(GLITENS, parameter = list(supp=.1,conf=0.2, minlen=1, maxlen=5, maxtime=25, target="rules"))
reg=apriori(GLITENS, parameter = list(supp=.05,conf=0.1, minlen=1, maxlen=5, maxtime=25, target="rules"))
inspect(reg) 


data(Groceries) 
head(Groceries@itemInfo,15) 

GL2=aggregate(Groceries, by="level2")
head(GL2@itemInfo)
inspect(GL2)

######################################################################################


write(GL2, file = "grocerieslevel2.csv", format="basket", sep = ",", quote=F)
#############################################################################################

sort(itemFrequency(GL2), decreasing = T)

Grules=apriori(GL2, parameter = list(supp=.05,conf=0.5, minlen=2, maxlen=5, maxtime=25, target="rules"), 
               control = list(verbose=F),)
Grules
Grules.sorted=sort(Grules, by="confidence")
inspect(Grules.sorted[1:20])


inspect(Grules.sorted[is.redundant(Grules.sorted)]) 
Grules.pruned=Grules.sorted[!is.redundant(Grules.sorted, measure="confidence")];Grules.pruned
inspect(Grules.pruned)

sink( "Gregras.txt") 
inspect(Grules.sorted) 
sink() 

######################################################################################

#gerando arquivo Excel com as transações
write(GL2, file = "grocerieslevel2.csv", format="basket", sep = ",", quote=F)
#############################################################################################