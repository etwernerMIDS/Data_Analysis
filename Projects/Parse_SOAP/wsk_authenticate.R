#' Authenticate user.
#' 
#' Uses a previously initialized client with a username and password to authenticate
#' a user with the LexisNexis server. Once authenticated, a user may search and
#' retrieve documents from the server.
#' 
#' @author Erin Werner
#' 
#' @param client SOAP client.
#' @param authID Username. If null uses default credentials
#' @param password User password. If null uses default credentials
#' @param simulated Boolean to determine whether testing header should be used.
#' @return binarySecurityToken used to access search and retrieve functions.
#' 
#' @examples 
#' \dontrun{
#' binarySecurityToken <- wsk_authenicate(client = msslLexisNexisBlue:::wsk_client_initialize())
#' }
#' 
wsk_authenticate <- function(client, 
                             authID=NULL, 
                             password=NULL, 
                             simulated=FALSE
                             ) {

  assertthat::assert_that(typeof(client)=="environment", msg = "client parameter is not of the environment type.")
  #assertthat::assert_that(is.character(authID), msg = "authID paramater is not of the character type.")
  #assertthat::assert_that(is.character(password), msg = "password paramater is not of the character type.")
  
  # gets the username and password from the keyring
  if (is.null(authID) && is.null(password) && simulated==FALSE)
  {
    authID <- keyring::key_get(service = "ln", username = "username", 
                               keyring = "LexisNexis")
    password <- keyring::key_get(service = "ln", username = "password", 
                                 keyring = "LexisNexis")
  }
  else if (simulated==TRUE)
  {
    authID <- "rexdouglass@wsk"
    password <- "testing78"
  }
  # TODO -- we'll figure out what to reactivate and all that fun jazz tomorrow. 
  # For now, we have a more "hacky-ish" type solution
  
  # Log authentication event
  params_passed <- NA
  #eventID <- log_function_begin(params_passed)

  client$set_options(port='Authentication')

  response <- tryCatch({
    client$service$Authenticate(authId=authID, password=password)
  }, warning = function(w) {
    # TODO: Can LN even raise warnings?
  }, error = function(e) {
    log_function_transaction(eventID = eventID, params_passed = params_passed, fail = TRUE)
    return(e)
  }
  )

  assertthat::assert_that(typeof(response)=="environment", msg="response is not of the environment type.")

  #Log successful authentication event
  #log_function_transaction(eventID = eventID, params_passed = params_passed, fail = FALSE)

  binarySecurityToken <- response$binarySecurityToken

  binarySecurityToken <- as.character(binarySecurityToken)

  
  
  
  return(binarySecurityToken)
}
