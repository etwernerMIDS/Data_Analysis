#' Parses through documents
#'
#' @author Erin Werner
#' 
#' @param doc_list  documents to parse
#'
#' @return parsed documents
#' 

wsk_parse_raw <- function(doc_list
                          ){
  if(!is.atomic(doc_list)){
    warning("Object provided is not a list, use singledoc for single documents.")
    return(NULL)
  }
  if(!is.character(doc_list)){
    warning("Object provided is not a list, use singledoc for single documents.")
    return(NULL)
  }
  #assertthat::assert_that(is.atomic(doc_list), msg = "List not provided, use doclist for single list and singledoc for singledoc")
  outputList <- list()
  count <- 1
  word <- "document = "
  a <- character(0)
  for(index in 1:length(doc_list)){
    input <- doc_list[[index]]
    for (i in 1:length(input)){
    #find first appearance of "document"
      if (identical(a,input[[i]])) next
      pos <- regexpr(word, input[i]) # Returns position of 1st match in a string
      keep = substr(input[i], pos, 1000000L) 
      
      #parse out the characters between quotes
      output <- sub('[^\"]+\"([^\"]+).*', '\\1', keep)
      
      #decode from base64 and write into xml file
      output <- RCurl::base64Decode(output)
      outputList[[count]] <- output
      count <- count + 1
      }
    }

  #outputList
  return(outputList)
}
