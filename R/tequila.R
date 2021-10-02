requireNamespace(c("httr", "shiny", "shinyjs", "shinylogin"))

#' @export
login <- function() {
    id <- .ids$nextId()
    ns <- shiny::NS(id)

    list(
        id = id,
        loginUI  = function(title = "Log in with Tequila") {
            shiny::actionButton(ns("button_login"), title, class = "btn-primary", style = "color: white;")
        },

        logoutUI = function(label = "Log out", icon = NULL, class = "btn-danger",
                            style = "color: white;") {
            shinylogin::core.logoutUI(id, label, icon, class, style)
        },

        loginServer = function(reload_on_logout = FALSE) {
            shiny::moduleServer(
                id,
                function(input, output, session) {
                    user <- serve(input, output, session, reload_on_logout)
                    ## Grant app code a low-privilege facet:
                    list(user = user$state, logout = user$logout)
                })
        })
}

.ids <- shinylogin::core.newIDSequence("tequila")

serve <- function(input, output, session, reload_on_logout = FALSE) {
    user <- shinylogin::core.serve(input, output, session, reload_on_logout)

    ## Synchronize visibility of the login button
    shiny::observe({
        shinyjs::toggle(id = "button_login",
                        condition = user$state()$logged_in == FALSE)
    })

    shiny::observeEvent(input$button_login, {
        ## Wow, that was an easy login!
        user$addLoginDetails(list(username = "joe", permissions = "standard"))
    })

    user
}

teq_encode <- function(params) {
    body_line <- function(k, v) sprintf("%s=%s\n", k, v)
    paste0(Map((function(k) body_line(k, params[[k]])), names(params)))
}

teq_decode <- function(s) {
    lines <- strsplit(s, "\n")[[1]]
    parsed <- regmatches(lines, regexec("^(.*?)=(.*)$", lines, perl = TRUE))
    kv <- Map(function(v) v[[3]], parsed)
    names(kv) <- Map(function(v) v[[2]], parsed)
    kv
}

.tequila_prod_url <- "https://tequila.epfl.ch/cgi-bin/tequila"

call_tequila <- function(tequila_base_url = .tequila_prod_url, method, ...) {
    method <- match.arg(method, c("createrequest", "fetchattributes"), several.ok = FALSE)
    uri <- paste0(tequila_base_url, "/", method)
    res <- httr::POST(uri, body = teq_encode(rlang::dots_list(...)))
    if (res$status_code == 200) {
        teq_decode(httr::content(res))
    } else {
        rlang::warn(sprintf("Bad status code %s from Tequila\n", res$status_code), httr::content(res))
        NULL
    }
}

createrequest <- function(tequila_base_url = .tequila_prod_url, ...) {
    call_tequila(tequila_base_url, "createrequest", ...)
}

fetchattributes <- function(tequila_base_url = .tequila_prod_url, key, ...) {
    call_tequila(tequila_base_url, "fetchattributes", key = key, ...)
}
