recipedata <- read.csv("RecipeData.csv")

fix(recipeData)
recipe.x= recipedata[,2:91] #recipedata[,-1]

fix(recipe.x)

#recipe.x[1:20,]

#######K-means

set.seed(12345)

km.out = kmeans(x=recipe.x,center = 4, nstart = 20,options(max.print = nrow(recipe.x)))
km.out
table(km.out$cluster, RecipeData$cuisine)
#plot(recipe.x,col=(km.out$cluster))

sort(km.out$centers[1,1:90])
sort(km.out$centers[2,1:90])
sort(km.out$centers[3,1:90])
sort(km.out$centers[4,1:90])
km.out$tot.withinss


#cluster =3

km.out1= kmeans(x=recipe.x,center = 3, nstart = 20,options(max.print = nrow(recipe.x)))
km.out1
km.out1$tot.withinss


##############Hierarchical clustering
set.seed(1001)

sample_data = sample(nrow(recipedata), 0.0076*nrow(recipedata))
hc_sample_data =recipedata[sample_data,]

hc.complete = hclust(d=dist(scale((hc_sample_data[,-1]))), method ="complete")
plot(hc.complete,main="Complete Linkage",labels =hc_sample_data[,1], xlab = "",sub="")
abline(h=16.5, col="red")


###########Association Rule
library(arules)
library(arulesViz)
library(datasets)

americanrecipe_association = data.frame(apply(recipedata[1:641,2:11],2,as.factor))
rules<-apriori(americanrecipe_association, parameter=list(supp=0.001,conf=0.8))
inspect(rules[1:10])


rules<-sort(rules, by="lift", decreasing = TRUE)
inspect(rules[1:10])
summary(rules[1:10])
