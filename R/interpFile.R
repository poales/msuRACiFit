#'interpFile
#'
#' Attempts to read in the data file provided, if fails returns NULL so that the program won't break...
#' @param l Location of the file to be interpreted
#' @name interpFile

interpFile <- function(l){
  #interpret the extension!
  #l <- "E:/OneDrive/Michigan State University/PRL - Sharkey Lab - Documents/Alan/2019_05_07 fast ql measurements right after co2/2019-05-07 fast ql changes"
  ext <- tools::file_ext(l)
  if(ext==""){
    #it's probably the text tsv file...
    df <- tryCatch({
      readLicorData::licorData(l,purgeComments=T,makeCommentsCol = F)
    },
    error=function(cond){
      message("Attempted interpreting file as licor tsv, but it failed")
      message(cond)
      return(NULL)
    })
  }else if(ext=="csv"){
    df <- readr::read_csv(l)
  } else if(ext=="xlsx" | ext=="xls"){
    #we need to read it in and test the shape to decide if i should use licorData or just readxl::readxl
    x <- readxl::read_excel(l)
    flag <- F
    if(nrow(x) > 6){
      if(sum(is.na(x[1,])) > 3 & sum(is.na(x[2,])) > 3){ #then it's probably a licor data file - this means it's not a rectangular data file
        df <- tryCatch(
          {
            readLicorData::licorData(l,purgeComments = T,makeCommentsCol = F)
          },
          error=function(cond){
            message("Attempted interpreting file as licor xls, but it failed")
            message(cond)
            return(NULL)
          })
      } else flag <- T
    } else flag <- T
    if(flag){
      df <- readxl::read_excel(l,col_names = T)
    }
    
  } else {
    message("Attempted to interpret file, but failed")
    df <- NULL
  }
  return(df)
}



