#' Initialize client.
#' 
#' Takes a wsdl url in order to initialize a client with the LexisNexis server.
#' Once initialized, a client may be authorized with the LexisNexis server and then
#' retrieve documents from the server.
#' 
#' @author Erin Werner
#' 
#' @param wsdl_url WSDL url.
#' @return client used to access authenticate and perform functions.
#' 
#' @examples 
#' \dontrun{
#' wsk_client_initialize()
#' wsk_client_initialize(simulated=TRUE)
#' }
#' 

wsk_client_initialize <- function(
  wsdl_url= paste0('file:',here::here(),'/data/WSK1.41-General-Prod/WSAPI-1_0.wsdl'), #switched to no slashes
  simulated = FALSE
  ){
  
  assertthat::assert_that(is.logical(simulated), msg = "Simulated paramater is not of the logical type.")
  assertthat::assert_that(is.character(wsdl_url), msg = "wsdl_url parameter is not of the character type.")
  
  if (!simulated)
  {
    authID <- keyring::key_get(service = "ln", username = "username", 
                               keyring = "LexisNexis")
    password <- keyring::key_get(service = "ln", username = "password", 
                                 keyring = "LexisNexis")
  }
  else 
  {
    authID <- "rexdouglass@wsk"
    password <- "testing78"
  }
  
  header <- reticulate::dict(authId=authID, password=password)

  suds <- reticulate::import("suds")
  
  if(simulated){
    #use testing wsdl
    #msslLexisNexisRed::launch_plumber_api()
    wsdl_url <- paste0('file:',here::here(),'/data/Testing_WSK/RedTeam.wsdl')
  }
  else{
    client <- suds$client$Client('https://www.lexisnexis.com/wsapi/v1/services/Authentication')
    assertthat::assert_that(typeof(client)=="environment", msg = "client is not of the environment type.")
  }
  
  imp <- suds$xsd$doctor$Import('http://www.w3.org/2001/XMLSchema',
                                location='http://www.w3.org/2001/XMLSchema.xsd')
  imp$filter$add('http://security.common.services.v1.wsapi.lexisnexis.com')
  client <- suds$client$Client(wsdl_url, doctor=suds$xsd$doctor$ImportDoctor(imp),headers=header) #no send first to experiment
  
  assertthat::assert_that(typeof(client)=="environment", msg = "client is not of the environment type.")
  
  return(client)
}
