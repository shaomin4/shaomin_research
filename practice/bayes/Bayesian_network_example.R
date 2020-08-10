library(bnlearn)
data(coronary)

bn_df <- data.frame(coronary)
#use hill-climbing greedy search
res <- hc(bn_df)
plot(res)

#delete from "M..Work" to  "Family" 
res$arcs <- res$arcs[-which((res$arcs[,'from'] == "M..Work" & res$arcs[,'to'] == "Family")),]

#Training
#bm.fit runs the EM algorithm to learn CPT for different nodes in the above graph
fittedbn <- bn.fit(res, data = bn_df)
print(fittedbn$Proteins)
print(fittedbn$Smoking)

#find event's probability
#Proteins 蛋白質
cpquery(fittedbn, event = (Proteins=="<3"), evidence = ( Smoking=="no") )
cpquery(fittedbn, event = (Proteins=="<3"), evidence = ( Smoking=="no" & Pressure==">140" ) )
cpquery(fittedbn, event = (Pressure==">140"), evidence = ( Proteins=="<3" ) )