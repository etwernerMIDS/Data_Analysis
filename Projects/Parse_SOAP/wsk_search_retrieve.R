#' Obtains the searches
#' 
#' @author Erin Werner
#'
#' @param client The SOAP client
#' @param binarySecurityToken Token used to access search and retrieve functions.
#' @param queryList list of keywords to query
#' @param sourceId  indicates the type of source to retrieve from LN (ie Time)
#' @param startDate the day to start retrieving sources from
#' @param endDate  the day to end retrieving sources from
#' @param begin first item to retreive
#' @param end  last item to retreive
#' 

wsk_search_retrieve <- function(client,
                                binarySecurityToken,
                                queryList,
                                sourceId="6742",
                                startDate="2016-06-29",
                                endDate="2016-06-29",
                                begin="1",
                                end="10"
                                ){
  
  docList <- list()
  count <- 1
  
  for(query in queryList){
    search <- wsk_search(client = client,
                         binarySecurityToken = binarySecurityToken,
                         query = query,
                         sourceId = sourceId,
                         startDate = startDate,
                         endDate = endDate)
    
    #ID <- search$searchId
    docs <- wsk_getdocumentsbyrange(client = client,
                                    binarySecurityToken = binarySecurityToken,
                                    #searchId = ID,
                                    searchId = search,
                                    begin = begin,
                                    end = end)
    
    output_raw_doc_responses(query = query,
                             docs = docs,
                             startDate=startDate,
                             #endDate=endDate,
                             begin=begin,
                             end=end)
    
    output_parsed_doc_responses(query = query,
                                docs = docs,
                                startDate=startDate,
                                #endDate=endDate,
                                begin=begin,
                                end=end)
    
    docList[[count]] <- docs
    count <- count + 1
  }
  
  return(docList)
}
