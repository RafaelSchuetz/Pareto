library(partition)
library(ggplot2)
library(dplyr)
library(tidyselect)

#https://cran.r-project.org/web/packages/partition/vignettes/introduction-to-partition.html

#partition merged Data 

#excludeNAs

PartitionedData <- mergedDataImputeMode[ ,colSums(is.na(mergedDataImputeMode)) == 0]
PartitionedData <- PartitionedData %>% 
  dplyr::select(!contains('scaled'))


#threshold .1
#Each reduced variable explains at least 10% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .1.

prt1 <- partition(PartitionedData, threshold = .1) #creates a tibble with the newly reduced variables

str(partition_scores(prt1))

str(mapping_key(prt1)) #the information each variable explains

mapping_key1 <- as.data.frame(mapping_key(prt1))
view(mapping_key1)

unnested1 <- as.data.frame(unnest_mappings(prt1))
view(unnested1)#to see each individual mapping

View(prt1)

#Each variable in the original data set maps to one reduced variable;
#in this partition, there are three reduced variables,as well as nine of the original variables that did not get reduced because too much information would have been lost.


#plot the raw feautures & the observed information & save them in workspace

rawfeatures1 <- plot_ncluster(prt1) + theme_minimal(14)

saveRDS(rawfeatures1, "./ANALYSIS/GRAPHS/PAPER/rawfeat1.Rds")

observedinfo1 <- plot_information(prt1, geom = geom_histogram) +
  theme_minimal(14) #obeservedinformation

saveRDS(observedinfo1, "./ANALYSIS/GRAPHS/PAPER/obsinf1.Rds")



#threshold .2 
#Each reduced variable explains at least 20% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .2.

prt2 <- partition(PartitionedData, threshold = .2)

str(partition_scores(prt2))

str(mapping_key(prt2))

mapping_key2 <- as.data.frame(mapping_key(prt2))

unnested2 <- as.data.frame(unnest_mappings(prt2))
view(unnested2) #individualmappings

View(prt2)

#plot the raw feautures & the observed information & save them in workspace

observedinfo2 <- plot_information(prt2, geom = geom_histogram) +
  theme_minimal(14)
saveRDS(observedinfo2, "./ANALYSIS/GRAPHS/PAPER/obsinf2.Rds")

rawfeatures2 <- plot_ncluster(prt2) + theme_minimal(14)
saveRDS(rawfeatures2, "./ANALYSIS/GRAPHS/PAPER/rawfeat2.Rds")




#threshold .3
#Each reduced variable explains at least 30% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .3.

prt3 <- partition(PartitionedData, threshold = .3) #creates a tibble with the newly reduced variables

str(partition_scores(prt3))

str(mapping_key(prt3)) #the information each variable explains

mapping_key3 <- as.data.frame(mapping_key(prt3))

unnest3 <- as.data.frame(unnest_mappings(prt3))
view(unnest3) #to see each individual mapping

View(prt3)

#plot the raw feautures & the observed information & save them in workspace

rawfeatures3 <- plot_ncluster(prt3) + theme_minimal(14) 
saveRDS(rawfeatures3, "./ANALYSIS/GRAPHS/PAPER/rawfeat3.Rds")

observedinformation3 <- plot_information(prt3, geom = geom_histogram) +
  theme_minimal(14) 
saveRDS(observedinformation3, "./ANALYSIS/GRAPHS/PAPER/obsinf3.Rds")




#threshold .4
#Each reduced variable explains at least 40% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .4.
prt4 <- partition(PartitionedData, threshold = .4)

str(partition_scores(prt4))

str(mapping_key(prt4))

mapping_key4 <- as.data.frame(mapping_key(prt4))

unnest4 <- as.data.frame(unnest_mappings(prt4)) 
view(unnest4) #individualmappings

View(prt4)

#plot the raw feautures & the observed information & save them in workspace

rawfeatures4 <- plot_ncluster(prt4) + theme_minimal(14)
saveRDS(rawfeatures4, "./ANALYSIS/GRAPHS/PAPER/rawfeat4.Rds")

observedinfo4 <- plot_information(prt4, geom = geom_histogram) +
  theme_minimal(14)
saveRDS(observedinfo4, "./ANALYSIS/GRAPHS/PAPER/obsinf4.Rds")






#threshold .5
#Each reduced variable explains at least 50% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .5.

prt5 <- partition(PartitionedData, threshold = .5)

str(partition_scores(prt5))

str(mapping_key(prt5))

mapping_key5 <- as.data.frame(mapping_key(prt5))

unnest5 <- as.data.frame(unnest_mappings(prt5)) 
view(unnest5) #individualmappings

View(prt5)

#plot the raw feautures & the observed information & save them in workspace

rawfeatures5 <- plot_ncluster(prt5) + theme_minimal(14)
saveRDS(rawfeatures5, "./ANALYSIS/GRAPHS/PAPER/rawfeat5.Rds")

observedinformation5 <- plot_information(prt5, geom = geom_histogram) +
  theme_minimal(14)
saveRDS(observedinformation5, "./ANALYSIS/GRAPHS/PAPER/obsinf5.Rds")

