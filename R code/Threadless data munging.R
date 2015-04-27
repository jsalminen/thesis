
# Function to combine data in one file
combineData <- function(startfile, stopfile, filepath, filename){
  df <- read.csv(paste(filepath, startfile, '.csv', sep = ''), header = T)
  for(i in (startfile+1):stopfile){
    df2 <- read.csv(paste(filepath, i, '.csv', sep = ''), header = T)
    df <- rbind(df,df2)
  }
  write.table(df, file = filename, sep = ',', row.names = FALSE)
}

# Combine crawled designs in one file
combineData(startfile=1, stopfile=1133, 
            filepath = 'Data/Raw data/Crawled designs/', 
            filename = 'Data/Processed data/combinedData.csv')

# Combine printed designs in one file
combineData(startfile=1, stopfile=9, 
            filepath = 'Data/Raw data/Printed submissions/', 
            filename = 'Data/Processed data/printed submissions.csv')

# Read data to be munged
combined_data <- read.csv('Data/Processed data/combinedData.csv', header = T)
printed_designs <- read.csv('Data/Raw data/printed designs.csv', header = T)
printed_submissions <- read.csv('Data/Processed data/printed submissions.csv', header = T)

################################################################################

# Helper functions
formatPrinted <- function(printed){
  printedsplit <- strsplit(as.character(printed), '/')
  cleanprinted <- printedsplit[[1]][length(printedsplit[[1]])]
  cleanprinted <- tolower(cleanprinted)
  cleanprinted <- gsub('_', '-', cleanprinted)
  return(cleanprinted)
}

formatDesign <- function(design){
  designsplit <- strsplit(design, '/')
  cleandesign <- designsplit[[1]][length(designsplit[[1]])]
  cleandesign <- tolower(cleandesign)
  return(cleandesign)
}

compareDesigns <- function(design, printlist){
    for(i in 1:length(printlist)){
        if(design == printlist[[i]]){return(1)}
    }
    return(0)
}

################################################################################

# Create lists of printed designs and submissions and 
# change them in the same format, remove NAs
printed_submissionsF <- as.list(apply(printed_submissions, 1, formatPrinted))
printed_submissionsF <- printed_submissionsF[!is.na(printed_submissionsF)]
printed_designsF <- as.list(apply(printed_designs, 1, formatPrinted))
printed_designsF <- printed_designsF[!is.na(printed_designsF)]
designs <- as.character(combined_data$design)
designs <- as.list(designs)
designsF <- lapply(designs, formatDesign)

# Compare submitted designs to lists of printed designs
comparison1 <- combined_data$printed
comparison1[is.na(comparison1)] <- 0
comparison2 <- lapply(designsF, compareDesigns, printlist = printed_submissionsF)
comparison3 <- lapply(designsF, compareDesigns, printlist = printed_designsF)

# Combine comparisons
combined_comparison <- comparison1 + unlist(comparison2) + unlist(comparison3)
combined_comparisonTF <- combined_comparison
combined_comparisonTF[combined_comparisonTF >= 1] <- 1

# Add comparisons to data frame and save as .csv file
combined_data$printed2 <- as.factor(unlist(comparison2))
combined_data$printed3 <- as.factor(unlist(comparison3))
combined_data$printed.all <- as.factor(combined_comparison)
combined_data$printed.allTF <- as.factor(combined_comparisonTF)

################################################################################

# Helper function for extracting challenges
getChallenge <- function(design){
  designsplit <- strsplit(design, '/')
  challenge <- designsplit[[1]][[2]]
  return(challenge)
}

# Get challenge of each design
designs <- combined_data$design
designs <- as.character(designs)
challenges <- lapply(designs, getChallenge)
challenges <- unlist(challenges)
challenges <- as.factor(challenges)

combined_data$challenge <- challenges

# Save final data as csv
write.table(combined_data, file = 'Data/Processed data/threadless_data.csv', 
            sep = ',', row.names = FALSE)

################################################################################

# Anonymize data and save anonymized version

# Create new levels for user names
user_levels <- c(1:nlevels(combined_data$user))
user_levels <- paste("user_", user_levels, sep = '')
levels(combined_data$user) <- user_levels

# Create new levels for designs
design_levels <- c(1:nlevels(combined_data$design))
design_levels <- paste("design_", design_levels, sep = "")
levels(combined_data$design) <- design_levels

# Remove exactime from approval dates
dates <- strsplit(as.character(combined_data$approved_date), ' ')
dateExtract <- function(x) {
    return (paste(x[1:3], collapse = ' '))
}
dates <- sapply(dates, dateExtract)
combined_data$approved_date <- dates

write.table(combined_data, file = 'Data/Processed data/threadless_data_anon.csv', 
            sep = ',', row.names = FALSE)





