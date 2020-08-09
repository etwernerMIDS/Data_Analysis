#' Gets searched documents by range.
#' 
#' Retrieves up to 10 documents using a previously generated binarySecurityToken
#' and searchID. Begin and end must be specified by user; invalid ranges will
#' not allow retrieval. 
#' 
#' @author Erin Werner
#'
#' @param client SOAP client.
#' @param binarySecurityToken Token used to access search and retrieve functions.
#' @param searchID String generated from a search query
#' @param docformat format of how text are desired. Available options: Cite,ExpandedCite,FullText,FullTextWithTerms
#' @param begin Beginning index of a set of documents to retrieve. Must be between 1 and 10.
#' @param end Ending index of a set of documents to retrieve. Must be between 1 and 10.
#'
#' @examples 
#' \dontrun{ 
#' documents <- wsk_getdocumentsbyrange(client = initialized_client, binarySecurityToken = authenticatedToken,
#'                                      searchId = returned_searchIds, docformat = 'FullTextWithTerms',
#'                                      begin = "1", end = "10")
#' }
#' 

wsk_getdocumentsbyrange <- function(client, 
                                    binarySecurityToken, 
                                    searchId,
                                    docformat = 'FullTextWithTerms',
                                    begin="1", 
                                    end="10"
                                    ) {
  
  assertthat::assert_that(typeof(client)=="environment", msg = "client parameter is not of the environment type.")
  assertthat::assert_that(is.character(binarySecurityToken), msg = "binarySecurityToken paramater is not of the character type.")
  assertthat::assert_that(is.character(searchId), msg = "searchId paramater is not of the character type.")
  assertthat::assert_that(is.character(docformat), msg = "docformat paramater is not of the character type.")
  assertthat::assert_that(is.character(begin), msg = "begin paramater is not of the character type.")
  assertthat::assert_that(is.character(end), msg = "end paramater is not of the character type.")

  # Log retrieve documents event
  params_passed <- list(binarySecurityToken = binarySecurityToken,
                        searchID = searchId,
                        docformat = docformat,
                        begin = begin,
                        end = end)
  eventID <- log_function_begin(params_passed)
  
  #TODO: Check valid search range
  
  #TODO: wsk_check_if_transaction_allowed
  # Retrieval limit
  documentoptions <- c('Cite','ExpandedCite','FullText','FullTextWithTerms')
  if (!(docformat %in% documentoptions)){stop("Doc Format not possible")}

  client$set_options(port='Retrieval')
  
  retOp <- reticulate::dict('documentView' = docformat,
                            'documentMarkup' = 'Display',
                            'documentRange' = reticulate::dict('begin'=begin,'end'=end))
  
  response <- tryCatch({
    client$service$GetDocumentsByRange(binarySecurityToken=binarySecurityToken,
                                       searchId=searchId,
                                       retrievalOptions=retOp)
  }, warning = function(w) {
    # TODO: Change if LN can actually throw warnings
  }, error = function(e) {
    log_function_transaction(eventID = eventID, params_passed = params_passed, fail = TRUE)
    return(NULL)
  }
  )
  
  assertthat::assert_that(typeof(response)=="environment", msg="response is not of the environment type.")
  
  #Log successful get documents event
  log_function_success(eventID = eventID, params_passed = params_passed, fail = FALSE)

  # b <- as.numeric(begin)
  # e1 <- length(response$documentContainerList$documentContainer)
  # e <- as.numeric(end)
  # if (e1 < e) e = e1
  
  
  list <- list()
  count <- 1
  
  for(index in 1:length(response$documentContainerList$documentContainer)){
    doc <- as.character(response$documentContainerList$documentContainer[[index]])
    list[[count]] <- doc
    count <- count + 1
  }
  
  response <- list
  
  #as.character(response$documentContainerList$documentContainer[b:e])
  #response
  
  return(response)
}
