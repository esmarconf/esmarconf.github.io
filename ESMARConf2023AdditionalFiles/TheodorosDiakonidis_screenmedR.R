#Install screenmedR from github
# Need to install devtools first
#devtools::install_github('thdiakon/screenmedR')

#load screenmed
library(screenmedR)

# csv-randomized-set.csv. By default in screenmedR package (You can use your own)
initial_search <- read.csv(system.file("extdata", "csv-randomized-set.csv", package = "screenmedR"))
initialPMID<-initial_search$PMID
knownPMID<-c("18822428","8276025","16452355","17329276","8346957")

# removesparseterms function in tm package (numbers should be in the (0.99, 1))
a_cluster<-screenmed(initialPMID,knownPMID,0.995,2)

# Cosine similarity measured
a_cluster$cosine_similarity

# In which group each abstract belongs to 
a_cluster$clustering

# Total number of abstracts 
table(a_cluster$clustering)

a_cluster$missing_abstracts

# Choose the group with the most relevant abstracts
secondPMID<-abstractsofgroup(a_cluster$clustering,1)


# Apply screenmed once again
b_cluster<-screenmed(secondPMID,knownPMID,0.998,2)

b_cluster$cosine_similarity

table(b_cluster$clustering)


# mesh_clean_bq function 

initialPMID<-initial_search$PMID
knownPMID<-c("18822428","8276025","16452355","17329276","8346957")
a<-mesh_clean_bq(initialPMID,knownPMID,11,2)


# mesh_by_name_bq function

Descriptors<-c("Blood Pressure","Dobutamine","Humans","Infant, Newborn")
Qualifiers<-c("administration & dosage")
b<-mesh_by_name_bq(initialPMID,Descriptors,Qualifiers)

