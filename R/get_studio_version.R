#' Get current app version of IQB Studio Lite
#'
#' @description
#' Retrieves the current app version of a hosted IQB Studio Lite instance
#' by inspecting its HTML page and JavaScript bundle.
#'
#' @param base_url Character. Base URL of the hosted IQB Studio Lite instance.
#'
#' @return Character string containing the app version.
#'
#' @export

get_studio_version <- function(base_url) {

  # HTML-Abruf
  html <- httr2::request(base_url) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string()

  # JavaScript-Dateien finden
  js_files <- xml2::read_html(html) %>%
    rvest::html_elements("script[src]") %>%
    rvest::html_attr("src")

  # Hauptdatei auswählen (hier ist app-version enthalten)
  main_files <- js_files[grepl("main-", basename(js_files))]

  if (length(main_files) == 0) {
    stop("Keine JavaScript-Hauptdatei gefunden.", call. = FALSE)
  }

  # Vollständige URLs erzeugen
  main_files_url <- xml2::url_absolute(main_files, base_url)

  # JavaScript herunterladen
  js <- httr2::request(main_files_url[1]) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string()

  # Variable ermitteln, die APP_VERSION zugewiesen wird
  # Sucht nach folgendemn Muster: provide : '"APP_VERSION" , useValue : {Variablenname}'
  # z.B.: 'provide:"APP_VERSION",useValue:hi'
  # Der Variablenname (hier "hi") kann variieren.
  match <- regexec(
    'provide\\s*:\\s*["\']APP_VERSION["\']\\s*,\\s*useValue\\s*:\\s*([[:alnum:]_$]+)',
    js,
    perl = TRUE
  )

  # Treffer aus dem JavaScript-Text extrahieren
  result <- regmatches(js, match)[[1]]

  # result enthält den gesamten Treffer und den erfassten Variablennamen.
  # Der Variablenname steht im zweiten Element.
  version_var <- result[2]

  # Definitionsmuster für die zuvor ermittelte Variable erstellen
  # Beispiel: var hi = "21.0.0"
  version_pattern <- paste0(
    'var\\s+', version_var,
    '\\s*=\\s*["\']([^"\']+)["\']'
  )

  # Versionsdefinition im JavaScript suchen und Treffer extrahieren
  version_match <- regexec(version_pattern, js, perl = TRUE)
  version_result <- regmatches(js, version_match)[[1]]

  if (length(version_result) < 2) {
    stop("Versionsnummer konnte nicht ausgelesen werden.",
         call. = FALSE)
  }

  # Versionsnummer zurückgeben
  version_result[2]
}
