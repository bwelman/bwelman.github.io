dataset <- data.frame(x1 = c(1,3,4,4,5,10,11,13,14,16),
				   x2 = c(12,17,19,13,18,4,3,5,3,4))

library(factoextra) # Voor visualisatie van de clustering

# Om gelijke clusternummering als in het excelmodel te krijgen worden de
# beginwaarden voor centra gespecificeerd

# Model k=3
model3 <- kmeans(dataset,
				 centers = array(c(10.5, 4.0, 14.3, 8.0, 6.0, 4.0), dim = c(3,2)),
				 iter.max = 100,
				 algorithm = "Lloyd",
				 nstart = 5)
model3$centers
fviz_cluster(model3, dataset, ggtheme = theme_bw())


# Model k=2
model2 <- kmeans(dataset,
				 centers = array(c(5,15,15,5), dim = c(2,2)),
				 iter.max = 100,
				 algorithm = "Lloyd",
				 nstart = 5)
model2$centers
fviz_cluster(model2, dataset, ggtheme = theme_bw())
