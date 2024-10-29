############IMPORT DATASETS#############
#ii. bring in the database for GA and SA and evaluate together
print("Importing databases...")

results_dir <- paste(base_dir, PC, "Databases", sep = "/")

#import single FLA database
if (PC == "1DBPP"){
  import_pc <- "BPP"
}else{
  import_pc <- PC
}

GA_database <- readRDS(paste(results_dir, 
                             paste(paste(import_pc, MHs[1], "FLA", "Database", sep="_"), ".rds", sep=""),
                             sep="/"))
SA_database <- readRDS(paste(results_dir, 
                             paste(paste(import_pc, MHs[2], "FLA", "Database", sep="_"), ".rds", sep=""),
                             sep="/"))
print("ML databases imported and constructed...")