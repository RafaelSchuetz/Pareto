library(partition)
library(ggplot2)
library(dplyr)

#https://cran.r-project.org/web/packages/partition/vignettes/introduction-to-partition.html

#partition merged Data 

#excludeNAs

PartitionedData <- mergedDataImputeMode[ ,colSums(is.na(mergedDataImputeMode)) == 0]


#threshold .1
#Each reduced variable explains at least 10% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .1.

prt1 <- partition(PartitionedData, threshold = .1) #creates a tibble with the newly reduced variables

str(partition_scores(prt1))

str(mapping_key(prt1)) #the information each variable explains

mapping_key1 <- as.data.frame(mapping_key(prt1))
view(mapping_key1)

#values
mapping_key1[10, 2]
mapping_key1[11, 2]
mapping_key1[12, 2]
#....

unnest_mappings(prt1) #to see each individual mapping

View(prt1)

#Each variable in the original data set maps to one reduced variable;
#in this partition, there are nine reduced variables,as well as nine of the original variables that did not get reduced because too much information would have been lost.

plot_ncluster(prt1) + theme_minimal(14)

plot_information(prt1, geom = geom_histogram) +
  theme_minimal(14) #obeservedinformation





#threshold .2 
#Each reduced variable explains at least 20% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .2.

prt2 <- partition(PartitionedData, threshold = .2)

str(partition_scores(prt2))

str(mapping_key(prt2))

unnest_mappings(prt2)

View(prt2)

plot_information(prt2, geom = geom_histogram) +
  theme_minimal(14)

plot_ncluster(prt2) + theme_minimal(14)

mapping_key2 <- as.data.frame(mapping_key(prt2))



#threshold .3
#Each reduced variable explains at least 30% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .3.

prt3 <- partition(PartitionedData, threshold = .3) #creates a tibble with the newly reduced variables

str(partition_scores(prt3))

str(mapping_key(prt3)) #the information each variable explains

unnest_mappings(prt3) #to see each individual mapping

View(prt3)

plot_ncluster(prt3) + theme_minimal(14) 

plot_information(prt3, geom = geom_histogram) +
  theme_minimal(14) 

mapping_key3 <- as.data.frame(mapping_key(prt3))


#threshold .4
#Each reduced variable explains at least 40% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .4.
prt4 <- partition(PartitionedData, threshold = .4)

str(partition_scores(prt4))

str(mapping_key(prt4))

unnest_mappings(prt4)

View(prt4)

plot_ncluster(prt4) + theme_minimal(14)

plot_information(prt4, geom = geom_histogram) +
  theme_minimal(14)

mapping_key4 <- as.data.frame(mapping_key(prt4))




#threshold .5
#Each reduced variable explains at least 50% of the information of the original variables from which it was created.
#The distribution of information has a lower limit of our threshold, .5.

prt5 <- partition(PartitionedData, threshold = .5)

str(partition_scores(prt5))

str(mapping_key(prt5))

unnest_mappings(prt5)

View(prt5)

plot_ncluster(prt5) + theme_minimal(14)

plot_information(prt5, geom = geom_histogram) +
  theme_minimal(14)

mapping_key5 <- as.data.frame(mapping_key(prt5))