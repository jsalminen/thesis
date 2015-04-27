# This script was used to anonymize the published data. It loads the three 
# OpenIDEO datasets and changes user names and concept names to generic versions
# to hide the identities of users. 

# Load data
ewaste <- read.csv("Data/Raw data/How can we manage e-waste & discarded electronics to safeguard human health & protect our environment?/OpenIDEO_ewaste.csv", 
                   header = T, sep = ";")

unemployment <- read.csv("Data/Raw data/How can we equip young people with the skills, information and opportunities to succeed in the world of work?/OpenIDEO_unemployment.csv", 
                         header = T, sep = ";")

celebrate <- read.csv("Data/Raw data/How might we identify and celebrate businesses that innovate for world benefit – and inspire other companies to do the same?/OpenIDEO_celebrate.csv", header = T, sep = ";")

# Anonymize concepts

concepts <- c(1:nlevels(ewaste$Concept))
concepts <- paste("concept_", concepts, sep = "")
levels(ewaste$Concept) = concepts

concepts <- c(1:nlevels(unemployment$Concept))
concepts <- paste("concept_", concepts, sep = "")
levels(unemployment$Concept) = concepts

concepts <- c(1:nlevels(celebrate$Concept))
concepts <- paste("concept_", concepts, sep = "")
levels(celebrate$Concept) = concepts

# Save data
write.table(ewaste, "Data/Processed data/ewaste_anon.csv", sep = ";")
write.table(unemployment, "Data/Processed data/unemployment_anon.csv", sep = ";")
write.table(celebrate, "Data/Processed data/celebrate_anon.csv", sep = ";")



