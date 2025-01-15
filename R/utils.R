#' Stores your API key in your keyring. 
#'
#' @param api_key Your personal linear API key. See README.md for information.  
#' 
#' @return nothing
#' @export
store_linear_api_key <- function(api_key){
  keyring::key_set_with_value("linear_api", password = api_key)
}

get_linear_api_key <- function(){
  keyring::key_get("linear_api")
}
