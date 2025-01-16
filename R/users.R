#' Returns a tibble with your id, name, and email. Useful for checking if you have successfully authenticated with the linear API.
#'
#' @return tibble
#' @importFrom methods new
#' @export
get_linear_current_user <- function(){
  
  resp <- make_linear_api_request(
    '{ "query": "{ viewer { id name email } }" }'
  )

  return(dplyr::as_tibble(resp$data$viewer))
}