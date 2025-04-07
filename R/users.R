#' Returns a tibble with your id, name, and email. Useful for checking if you have successfully authenticated with the linear API.
#'
#' @param api_url Defaults to the current linear api url, but can be changed.
#'
#' @return tibble
#' @export
get_linear_current_user <- function(api_url = "https://api.linear.app/graphql"){
  resp <- make_linear_api_request(
    "{ viewer { id name email } }",
    api_url
  )

  dplyr::as_tibble(httr2::resp_body_json(resp)) |> tidyr::unnest_wider(data)
}
