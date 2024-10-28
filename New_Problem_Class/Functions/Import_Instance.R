#script for importing the problem instance based on the new problem class file convention
import_instance <- function(prob_file, dir_import){
  #extract instance depending on directory, file name, and format
  inst <- read.csv(file = paste(dir_import, "/", prob_file, sep=''))

  #extract values from file
  variable_1 <- <...>
  variable_2 <- <...>
  ...elt()
    
  #return listed elements for problem instance
  return(list(<variable_1, variable_2, ...>))
}
