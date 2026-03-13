setMethod(f = "show",
          signature = "LoginStudio",
          definition = function(object) {
            cli_setting()

            cli::cli_text("You have access to the following workspace groups ({.wsg-id wsg_id}: {.wsg wsg_label}) with their respective workspaces ({.ws-id ws_id}: {.ws ws_label}):")

            ul <- cli::cli_ul()
            object@wsg_list %>%
              purrr::map(function(wsg) {
                cli::cli_li(glue::glue("{{.wsg-id {wsg$wsg_id}}}: {{.wsg {wsg$wsg_label}}}"))

                ul_nest <- cli::cli_ul()
                wsg$ws_list %>%
                  purrr::map(function(ws) {
                    cli::cli_li(items = glue::glue("{{.ws-id {ws$ws_id}}}: {{.ws {ws$ws_label}}}"))
                  })
                cli::cli_end(ul_nest)
              })
            cli::cli_end(ul)
          })

setMethod(f = "show",
          signature = "LoginTestcenter",
          definition = function(object) {
            cli_setting()

            cli::cli_text("You have access to the following workspaces ({.ws-id ws_id}: {.ws ws_label}):")

            ul <- cli::cli_ul()
            object@ws_list %>%
              purrr::map(function(ws) {
                cli::cli_li(glue::glue("{{.ws-id {ws$ws_id}}}: {{.ws {ws$ws_label}}}"))
              })
            cli::cli_end(ul)
          })

setMethod(f = "show",
          signature = "WorkspaceStudio",
          definition = function(object) {
            cli_setting()

            cli::cli_text("You have access to the following workspaces ({.ws-id ws_id}: {.ws ws_label}):")

            ul <- cli::cli_ul()
            list(ws_id = object@ws_id, ws_label = object@ws_label) %>%
              purrr::list_transpose() %>%
              purrr::map(function(ws) {
                cli::cli_li(items = glue::glue("{{.ws-id {ws$ws_id}}}: {{.ws-label {ws$ws_label}}}"))
              })
            cli::cli_end(ul)
          })

setMethod(f = "show",
          signature = "WorkspaceTestcenter",
          definition = function(object) {
            cli_setting()

            cli::cli_text("You have access to the workspace {.ws-label {object@ws_label}} (id {.ws-id {object@ws_id}}).")

          })
