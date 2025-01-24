#' Returns a tibble with the issues for a given team_id
#'
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want. 
#' @param api_url Defaults to the current linear api url, but can be changed 
#' 
#' @return tibble
#' @importFrom methods new
#' @export
get_linear_issues <- function(team_id, api_url = "https://api.linear.app/graphql"){
  graphql_query <- paste0('{
  team(id: "',  team_id, '") {
    id
    name

    issues {
      nodes {
        id
        title
        description
        assignee {
          id
          name
        }
        createdAt
        archivedAt
      }
    }
  }
  }')

  body <- list(query = graphql_query)

  response <- httr2::request(api_url) |> 
    httr2::req_headers(
      "Authorization" = get_linear_api_key(),
      "Content-Type" = "application/json"
    ) |> 
    httr2::req_body_json(body) |> 
    httr2::req_perform()

  response_body <- httr2::resp_body_json(response)
  return(dplyr::as_tibble(response_body))
}

#' Creates a new issue within a team specified using the team's id. 
#'
#' @param title The issue title
#' @param description The issue description
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want. 
#' @param api_url Defaults to the current linear api url, but can be changed 
#' 
#' @return tibble
#' @export
create_linear_issue <- function(
  title, 
  description, 
  team_id, 
  api_url = "https://api.linear.app/graphql"){
  
  graphql_query <- paste0('mutation {
    issueCreate(input: {
      title: "', title, '",
      description: "', description, '",
      teamId: "', team_id, '"
    }) {
      issue {
        id
        url
      }
    }
  }')

  body <- list(query = graphql_query)

  response <- httr2::request() |> 
    httr2::req_headers(
      "Authorization" = get_linear_api_key(),
      "Content-Type" = "application/json"
    ) |> 
    httr2::req_body_json(body) |> 
    httr2::req_perform()

  response_body <- httr2::resp_body_json(response)
  return(dplyr::as_tibble(response_body))
}
