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

    ## Tequila step 1: ask Tequila server for a session, then redirect browser
    shiny::observeEvent(input$button_login, {
        createrequest_and_redirect(redirect_back_here_url(session$clientData))
    })

    ## Tequila step 2: back from Tequila with a ?key= query param
    shiny::observeEvent(session$clientData$url_search, {
        back_from_tequila(session$clientData$url_search, user)
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

redirect_back_here_url <- function(clientData) {
    is_standard_port <- (clientData$url_protocol == "http:" && clientData$url_port == 80) ||
        (clientData$url_protocol == "https:" && clientData$url_port == 443)
    ## Don't re-send the ?key= of a previous session to Tequila! It will not remove it for us.
    url_search <- sub("^[?](key=.*?)(&|$)", "?\\2", clientData$url_search, perl = TRUE)
    sprintf(
        "%s//%s%s%s",
        clientData$url_protocol,
        `if`(is_standard_port, clientData$url_hostname,
             sprintf("%s:%s", clientData$url_hostname, clientData$url_port)),
        clientData$url_pathname, url_search)
}

#' Create a Tequila session, then redirect browser to Tequila for authentication
#'
#' The user will hopefully come back either immediately (if they are already logged in to Tequila),
#' or after typing in their credentials.
createrequest_and_redirect <- function(app_url, tequila_base_url = .tequila_prod_url, ...) {
    req <- createrequest(tequila_base_url = tequila_base_url, urlaccess=app_url, ...)
    if (is.null(req)) return()
    redirect <- paste0(tequila_base_url, "/requestauth?requestkey=", req$key)
    shinyjs::runjs(sprintf('window.location.href = "%s";', redirect))
}

#' Validate Tequila credentials in the form of a `?key=` query parameter.
#'
#' @param url_search The current `session$clientData$url_search`. If
#'     it contains a currently-valid Tequila session key, then login is successful.
#' @param user The shinylogin `user` object
back_from_tequila <- function(url_search, user, tequila_base_url = .tequila_prod_url, ...) {
    key <- regmatches(url_search, regexec("^[?]key=(.*?)(&|$)", url_search, perl = TRUE))[[1]][2]
    if (is.na(key)) return()

    teq_attributes <- fetchattributes(tequila_base_url = tequila_base_url, key, ...)
    if (is.null(teq_attributes)) return()

    if (teq_attributes$status != "ok") {
        print("Bad Tequila response")
        print(teq_attributes)
        return()
    }

    username <- teq_attributes$user
    teq_attributes$user <- NULL
    teq_attributes$key <- NULL

    user$addLoginDetails(list(
             username = username,
             tequila = teq_attributes))
}
