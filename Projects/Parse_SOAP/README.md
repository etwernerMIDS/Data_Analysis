# Parse SOAP Messages

## Erin Werner

Web services can be regarded as functions of systems exposed over the Web using standardised message formats. They can be interfaced to other software platforms through "message-centric" usage of such services. This is accomplished by interacting through SOAP messages, as SOAP is a messaging protocol specification for exchanging structured information in the implementation of web services in computer networks. The goal of this project was to use SOAP messages to interact with a web-service through R.

More specifically, in this repository you will find:

* wsk_client_initialize()

This function initializes a client with the web service.

* wsk_authenticate()

This function authenticates the client and returns a binary security token.

* wsk_seacrh_retrieve()

This function performs a search within the web-service and stores the documents that are returned from the search.

* wsk_parse_raw()

This function parses the returned xml-formatted documents and extracts the text.

*This project was completed for the Center for Peace and Security Studies (CPASS). These were completed using RStudio and the respective web-service interaction libraries.*




