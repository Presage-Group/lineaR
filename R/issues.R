#' Builds a query for use in `get_linear_issues`
#'
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want.
#' @param cursor The id of the `after` cursor, obtained with `page_info$endCursor`.
#' @param year If specified, all the issues (except the archived issues) since that year are returned. Default is `NULL` that returns all the issues to date, except the archived issues.
#'
#' @returns character
build_graphql_query <- function(team_id, cursor = NULL, year = NULL) {
  cursor_part <- if (!is.null(cursor)) glue::glue(', after: "{cursor}"') else ""
  year_filter <- if (!is.null(year)) glue::glue(', filter: { createdAt: { gte: "[year]" } }', .open = "[", .close = "]") else ""

  glue::glue('
    {
      team(id: "[team_id]") {
        id
        name
        issues(first: 50[cursor_part][year_filter]) {
          nodes {
            id
            title
            description
            assignee { id name }
            createdAt
            updatedAt
            completedAt
            project {
              id
              name
            }
            state { id name }
            priority
            priorityLabel
            estimate
          }
          pageInfo {
            hasNextPage
            endCursor
          }
        }
      }
    }
  ', .open = "[", .close = "]")
}


#' Returns a tibble with the issues for a given team_id
#'
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want.
#' @param year If specified, all the issues (except the archived issues) since that year are returned. Default is `NULL` that returns all the issues to date, except the archived issues.
#' @param api_url Defaults to the current linear api url, but can be changed
#'
#' @return tibble
#' @export
get_linear_issues <- function(team_id, year = NULL, api_url = "https://api.linear.app/graphql"){
  all_nodes <- list()
  cursor <- NULL
  has_next <- TRUE

  while (has_next) {
    query <- build_graphql_query(team_id, cursor = cursor, year = year)
    resp <- make_linear_api_request(query, api_url)
    resp_data <- httr2::resp_body_json(resp)

    nodes <- resp_data$data$team$issues$nodes
    page_info <- resp_data$data$team$issues$pageInfo

    all_nodes <- append(all_nodes, nodes)
    has_next <- page_info$hasNextPage
    cursor <- page_info$endCursor
  }

  nodes_df <- tibble::tibble(nodes = all_nodes) |>
    tidyr::unnest_wider(nodes)
  nodes_df$state <- vapply(nodes_df$state, function(x) x$name, character(1))

  nodes_df
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
  title="New issue via lineaR",
  description=" ",
  team_id,
  api_url = "https://api.linear.app/graphql"){

  graphql_query <- glue::glue('mutation {
    issueCreate(input: {
      title: "[title]",
      description: "[description]",
      teamId: "[team_id]"
    }) {
      issue {
        id
        url
      }
    }
  }', .open = "[", .close = "]")

  resp <- make_linear_api_request(graphql_query, api_url = api_url)

  return(httr2::resp_body_json(resp) |>
    dplyr::as_tibble() |>
    tidyr::unnest_wider(data) |>
    tidyr::unnest_wider(issue))
}
