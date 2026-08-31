# Log-Daten

``` r

library(eatPrepTBA)
library(dplyr)
library(tidyr)
```

Testcenter-Logs sind eine sehr nützliche Zusatzquelle, aber sie sind
keine vollständige technische Telemetrie. Sie dokumentieren vor allem
Ereignisse des Testcenters und des Players: Verbindungszustände,
Player-Zustände, Seiten- und Fortschrittszustände, Fokus-Ereignisse,
Runtime-Fehler und, wenn vorhanden, `LOADCOMPLETE`-Informationen zur
Browserumgebung.

> **Empfohlen:** Log-Daten sollten zuerst auf Verlässlichkeit geprüft
> werden, bevor Bearbeitungszeiten, Ladezeiten oder technische
> Unterschiede inhaltlich interpretiert werden.

## Was Logs leisten können

Mit den Log-Funktionen in `eatPrepTBA` lassen sich unter anderem
folgende Fragen bearbeiten:

| Frage | Wichtige Funktionen |
|----|----|
| Welche Log-Typen kommen überhaupt vor? | [`summarise_log_inventory()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_inventory.md) |
| Welche Browser, Betriebssysteme, Geräte und Auflösungen wurden genutzt? | [`summarise_log_environment()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md) |
| Sind Logdaten vollständig und plausibel genug für weitere Analysen? | [`detect_log_anomalies()`](https://iqb-research.github.io/eatPrepTBA/reference/detect_log_anomalies.md), [`summarise_log_qc()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_qc.md) |
| Gab es Verbindungsverluste oder ungewöhnliche Verbindungswechsel? | [`summarise_log_connections()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_connections.md) |
| Gab es Fokusverluste, die lange oder ungelöst waren? | [`summarise_log_focus()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_focus.md) |
| Wurden Units geladen und gestartet? | [`summarise_log_player()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_player.md), [`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md) |
| Welche Seiten und Progress-Zustände wurden beobachtet? | [`summarise_log_pages()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_pages.md), [`summarise_log_progress()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_progress.md) |
| Wie hängen beobachtete Ladezeiten mit Unitgrößen zusammen? | [`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md), [`add_unit_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/add_unit_sizes.md) |
| Gibt es Systemcheck-Hinweise auf Netzwerkbedingungen? | [`summarise_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_system_checks.md), [`add_system_check_summary()`](https://iqb-research.github.io/eatPrepTBA/reference/add_system_check_summary.md) |

Wichtig ist die Abgrenzung: `LOADCOMPLETE$loadTime` und die
Unit-Ladezeiten aus
[`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
sind beobachtete Ladezeitangaben. Sie sind **keine** direkte
Downloadgeschwindigkeit. Für echte oder browsernahe Netzwerkangaben
kommen, wenn überhaupt, eher Systemcheck-Daten in Frage.

## Daten einlesen

Logs können entweder direkt aus einem Testcenter-Workspace geladen oder
aus exportierten CSV-Dateien gelesen werden.

``` r

login <- login_testcenter(keyring = TRUE)
workspace <- access_workspace(login, ws_id = 123)

logs <- get_logs(workspace, groups = "Gruppenname")
```

``` r

logs <- read_logs("logs.csv")
```

Für große Studien ist es sinnvoll, die eingelesenen Daten einmal als
RDS-Datei zwischenzuspeichern.

``` r

readr::write_rds(logs, "logs_raw.rds")
logs <- readr::read_rds("logs_raw.rds")
```

Die meisten Log-Funktionen erwarten einen Tibble mit mindestens
`log_entry`. Für sessionbezogene Auswertungen werden, soweit vorhanden,
`group_id`, `login_name`, `login_code` und `booklet_id` verwendet.
Unitbezogene Funktionen verwenden zusätzlich `unit_key` und
`unit_alias`.

## Beispiel-Datensatz

Die folgenden künstlichen Logs sind klein genug für die Vignette,
enthalten aber typische Muster: eine plausible Session mit
`LOADCOMPLETE`, eine auffällige Session ohne `LOADCOMPLETE`, einen
Verbindungsverlust, einen ungelösten Fokusverlust, einen Runtime-Fehler
und inkonsistente Seiteninformationen.

``` r

make_loadcomplete <- function(browser_name, os_name, device,
                              width, height, load_time) {
  payload <- list(
    browserVersion = "18.3",
    browserName = browser_name,
    osName = os_name,
    device = device,
    screenSizeWidth = width,
    screenSizeHeight = height,
    loadTime = load_time
  )

  paste0("LOADCOMPLETE : ", jsonlite::toJSON(payload, auto_unbox = TRUE))
}

logs <- tibble::tribble(
  ~group_id, ~login_name, ~login_code, ~booklet_id, ~unit_key, ~unit_alias, ~ts, ~log_entry,
  "G1", "L1", "C1", "B1", "U1", "U1", 100, make_loadcomplete("Safari", "iOS 18.6.2", "Apple iPad tablet", 820, 1180, 30926),
  "G1", "L1", "C1", "B1", "U1", "U1", 150, 'CONNECTION : "POLLING"',
  "G1", "L1", "C1", "B1", "U1", "U1", 160, 'CONNECTION : "WEBSOCKET"',
  "G1", "L1", "C1", "B1", "U1", "U1", 200, "PLAYER = LOADING",
  "G1", "L1", "C1", "B1", "U1", "U1", 500, "PLAYER = RUNNING",
  "G1", "L1", "C1", "B1", "U1", "U1", 520, "PAGE_COUNT = 2",
  "G1", "L1", "C1", "B1", "U1", "U1", 530, "CURRENT_PAGE_ID = 1",
  "G1", "L1", "C1", "B1", "U1", "U1", 540, "CURRENT_PAGE_NR = 1",
  "G1", "L1", "C1", "B1", "U1", "U1", 700, "RESPONSE_PROGRESS = some",
  "G1", "L1", "C1", "B1", "U1", "U1", 2000, 'FOCUS : "HAS_NOT"',
  "G1", "L1", "C1", "B1", "U1", "U1", 2300, 'FOCUS : "HAS"',
  "G1", "L1", "C1", "B1", "U1", "U1", 2600, "CURRENT_PAGE_ID = 2",
  "G1", "L1", "C1", "B1", "U1", "U1", 2700, "CURRENT_PAGE_NR = 2",
  "G1", "L1", "C1", "B1", "U1", "U1", 3000, "RESPONSE_PROGRESS = complete",
  "G1", "L1", "C1", "B1", "U1", "U1", 3100, "PRESENTATION_PROGRESS = complete",
  "G1", "L2", "C2", "B1", "U2", "U2", 100, 'CONNECTION : "POLLING"',
  "G1", "L2", "C2", "B1", "U2", "U2", 140, 'CONNECTION : "LOST"',
  "G1", "L2", "C2", "B1", "U2", "U2", 200, "PLAYER = LOADING",
  "G1", "L2", "C2", "B1", "U2", "U2", 210, "PAGE_COUNT = 1",
  "G1", "L2", "C2", "B1", "U2", "U2", 220, "CURRENT_PAGE_NR = 2",
  "G1", "L2", "C2", "B1", "U2", "U2", 300, 'FOCUS : "HAS_NOT"',
  "G1", "L2", "C2", "B1", "U2", "U2", 400, "Runtime Error : Player crashed"
)

logs
#> # A tibble: 22 × 8
#>    group_id login_name login_code booklet_id unit_key unit_alias    ts log_entry
#>    <chr>    <chr>      <chr>      <chr>      <chr>    <chr>      <dbl> <chr>    
#>  1 G1       L1         C1         B1         U1       U1           100 "LOADCOM…
#>  2 G1       L1         C1         B1         U1       U1           150 "CONNECT…
#>  3 G1       L1         C1         B1         U1       U1           160 "CONNECT…
#>  4 G1       L1         C1         B1         U1       U1           200 "PLAYER …
#>  5 G1       L1         C1         B1         U1       U1           500 "PLAYER …
#>  6 G1       L1         C1         B1         U1       U1           520 "PAGE_CO…
#>  7 G1       L1         C1         B1         U1       U1           530 "CURRENT…
#>  8 G1       L1         C1         B1         U1       U1           540 "CURRENT…
#>  9 G1       L1         C1         B1         U1       U1           700 "RESPONS…
#> 10 G1       L1         C1         B1         U1       U1          2000 "FOCUS :…
#> # ℹ 12 more rows
```

## Erster Überblick

Auf großen Datensätzen ist
[`summarise_log_inventory()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_inventory.md)
ein guter erster Schritt, weil damit noch keine komplexen Zeit- oder
Zustandsverläufe berechnet werden. Man sieht schnell, welche
Ereignistypen überhaupt vorhanden sind und ob unbekannte oder
projektspezifische Typen auftauchen.

``` r

inventory <- summarise_log_inventory(logs)

inventory %>%
  select(log_type, log_family, n, known_log_type, supported_parser)
#> # A tibble: 10 × 5
#>    log_type              log_family        n known_log_type supported_parser
#>    <chr>                 <chr>         <int> <lgl>          <lgl>           
#>  1 CONNECTION            connection        4 TRUE           TRUE            
#>  2 CURRENT_PAGE_NR       unit_state        3 TRUE           TRUE            
#>  3 FOCUS                 focus             3 TRUE           TRUE            
#>  4 PLAYER                player            3 TRUE           TRUE            
#>  5 CURRENT_PAGE_ID       unit_state        2 TRUE           TRUE            
#>  6 PAGE_COUNT            unit_state        2 TRUE           TRUE            
#>  7 RESPONSE_PROGRESS     unit_state        2 TRUE           TRUE            
#>  8 LOADCOMPLETE          environment       1 TRUE           TRUE            
#>  9 PRESENTATION_PROGRESS unit_state        1 TRUE           TRUE            
#> 10 Runtime Error         runtime_error     1 TRUE           FALSE
```

> **Empfohlen:** Wenn viele unbekannte Log-Typen auftreten, sollten
> diese vor der eigentlichen Analyse stichprobenartig inspiziert werden.
> Unbekannte Typen können harmlos sein, aber auch auf Player- oder
> projektspezifische Ereignisse hinweisen, die in einer Studie wichtig
> sind.

## Browser, Gerät und Auflösung

[`summarise_log_environment()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md)
extrahiert Informationen aus `LOADCOMPLETE`. In Testcenter-Logs kann der
Payload etwa Browser, Betriebssystem, Gerät, Bildschirmbreite,
Bildschirmhöhe und `loadTime` enthalten.

``` r

environment <- summarise_log_environment(logs)

environment %>%
  select(
    group_id, login_name, booklet_id,
    browser_name, browser_version,
    os_family, os_version,
    device_class, screen_size_width, screen_size_height,
    screen_orientation, load_time,
    loadcomplete_parse_ok
  )
#> # A tibble: 2 × 13
#>   group_id login_name booklet_id browser_name browser_version os_family
#>   <chr>    <chr>      <chr>      <chr>        <chr>           <chr>    
#> 1 G1       L1         B1         Safari       18.3            iOS      
#> 2 G1       L2         B1         NA           NA              NA       
#> # ℹ 7 more variables: os_version <chr>, device_class <chr>,
#> #   screen_size_width <dbl>, screen_size_height <dbl>,
#> #   screen_orientation <chr>, load_time <dbl>, loadcomplete_parse_ok <lgl>
```

Die wichtigsten technischen Variablen sind:

| Variable | Bedeutung |
|----|----|
| `browser_name`, `browser_version` | Browser und Version aus `LOADCOMPLETE` |
| `os_name`, `os_family`, `os_version` | Betriebssystem, grobe Familie und Version |
| `device`, `device_class` | Gerätebezeichnung und grobe Klasse wie `tablet`, `smartphone`, `desktop` |
| `screen_size_width`, `screen_size_height` | Bildschirmgröße in CSS-Pixeln laut Browser |
| `screen_orientation` | abgeleitet aus Breite und Höhe |
| `load_time` | beobachtete initiale Ladezeit aus `LOADCOMPLETE` |
| `n_loadcomplete_events`, `loadcomplete_multiple` | Hinweise auf mehrere `LOADCOMPLETE`-Events in einer Session |
| `loadcomplete_conflicting` | mehrere `LOADCOMPLETE`-Events mit widersprüchlichen Werten |

> **Empfohlen:** Geräte, Betriebssysteme und Auflösungen sollten in
> jeder Studie mindestens tabelliert werden. Ungewöhnliche Kombinationen
> können echte technische Besonderheiten oder Datenprobleme anzeigen.

``` r

environment %>%
  count(os_family, device_class, screen_orientation, sort = TRUE)
#> # A tibble: 2 × 4
#>   os_family device_class screen_orientation     n
#>   <chr>     <chr>        <chr>              <int>
#> 1 iOS       tablet       portrait               1
#> 2 NA        NA           NA                     1
```

## Log-Verlässlichkeit und Anomalien

Vor inhaltlichen Analysen sollte geprüft werden, ob Logs strukturell
plausibel sind.
[`detect_log_anomalies()`](https://iqb-research.github.io/eatPrepTBA/reference/detect_log_anomalies.md)
gibt eine lange Tabelle auffälliger Muster zurück.
[`summarise_log_qc()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_qc.md)
verdichtet diese Tabelle zu einer Zeile pro Session.

``` r

anomalies <- detect_log_anomalies(logs)

anomalies %>%
  select(group_id, login_name, booklet_id, anomaly_code, severity, evidence)
#> # A tibble: 7 × 6
#>   group_id login_name booklet_id anomaly_code               severity evidence   
#>   <chr>    <chr>      <chr>      <chr>                      <ord>    <chr>      
#> 1 G1       L2         B1         connection_lost            warning  "CONNECTIO…
#> 2 G1       L2         B1         last_connection_lost       critical "CONNECTIO…
#> 3 G1       L2         B1         loading_without_running    warning  "PLAYER = …
#> 4 G1       L2         B1         page_nr_exceeds_page_count warning  "max_curre…
#> 5 G1       L2         B1         focus_lost_never_regained  warning  "FOCUS : \…
#> 6 G1       L2         B1         runtime_error              critical "Runtime E…
#> 7 G1       L2         B1         missing_loadcomplete       warning   NA
```

``` r

qc <- summarise_log_qc(logs, anomalies = anomalies)

qc
#> # A tibble: 2 × 13
#>   group_id login_name login_code booklet_id log_qc_flag has_critical_anomaly
#>   <chr>    <chr>      <chr>      <chr>      <chr>       <lgl>               
#> 1 G1       L1         C1         B1         ok          FALSE               
#> 2 G1       L2         C2         B1         critical    TRUE                
#> # ℹ 7 more variables: has_warning_anomaly <lgl>, has_info_anomaly <lgl>,
#> #   n_anomalies <int>, n_critical <int>, n_warning <int>, n_info <int>,
#> #   anomaly_codes <chr>
```

Die Spalte `log_qc_flag` hat die Werte `ok`, `info`, `warning` oder
`critical`. Sie ist als Screening-Flag gedacht, nicht als automatische
Ausschlussregel.

Die wichtigsten Anomaliegruppen sind:

| Bereich | Anomalie-Codes |
|----|----|
| `LOADCOMPLETE` | `missing_loadcomplete`, `malformed_loadcomplete`, `multiple_loadcomplete_in_session`, `conflicting_loadcomplete`, `loadcomplete_after_unit_start` |
| Player-Zustände | `player_running_without_loading`, `loading_without_running`, `repeated_loading`, `last_event_is_loading`, `last_event_is_running` |
| Verbindung | `connection_lost`, `many_connection_transitions`, `last_connection_lost` |
| Fokus | `focus_lost_never_regained`, `repeated_focus_lost_before_regain`, `very_long_focus_loss` |
| Zeitstempel und Seiten | `timestamp_decreases_in_input`, `zero_timestamp`, `invalid_current_page_id`, `page_count_inconsistent`, `page_nr_exceeds_page_count` |
| Technische Fehler und Sonstiges | `runtime_error`, `unknown_log_type` |

> **Empfohlen:** Sessions mit `critical` sollten einzeln geprüft werden.
> Sessions mit `warning` können oft weiterverwendet werden, sollten aber
> in Sensitivitätsanalysen oder Ausschlussentscheidungen sichtbar
> bleiben.

## Zustandsmetriken

Die State-Summary-Funktionen sind kompakter als
[`prepare_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/prepare_logs.md).
Sie behalten die Rohlogs unverändert, zählen aber wichtige Zustände pro
Session oder Session-Unit.

``` r

connections <- summarise_log_connections(logs)

connections %>%
  select(
    group_id, login_name, booklet_id,
    n_connection_events, n_connection_transitions,
    connection_states, has_connection_lost,
    last_connection_state_lost
  )
#> # A tibble: 2 × 8
#>   group_id login_name booklet_id n_connection_events n_connection_transitions
#>   <chr>    <chr>      <chr>                    <int>                    <int>
#> 1 G1       L1         B1                           2                        1
#> 2 G1       L2         B1                           2                        1
#> # ℹ 3 more variables: connection_states <chr>, has_connection_lost <lgl>,
#> #   last_connection_state_lost <lgl>
```

``` r

focus <- summarise_log_focus(logs, focus_loss_threshold_ms = 5 * 60 * 1000)

focus %>%
  select(
    group_id, login_name, booklet_id,
    n_focus_lost, n_focus_regained,
    total_focus_lost_time, max_focus_lost_time,
    has_unresolved_focus_loss
  )
#> # A tibble: 2 × 8
#>   group_id login_name booklet_id n_focus_lost n_focus_regained
#>   <chr>    <chr>      <chr>             <int>            <int>
#> 1 G1       L1         B1                    1                1
#> 2 G1       L2         B1                    1                0
#> # ℹ 3 more variables: total_focus_lost_time <dbl>, max_focus_lost_time <dbl>,
#> #   has_unresolved_focus_loss <lgl>
```

``` r

player <- summarise_log_player(logs)
pages <- summarise_log_pages(logs)
progress <- summarise_log_progress(logs)

player %>%
  select(
    group_id, login_name, booklet_id, unit_key,
    n_player_loading, n_player_running,
    first_player_state, last_player_state
  )
#> # A tibble: 2 × 8
#>   group_id login_name booklet_id unit_key n_player_loading n_player_running
#>   <chr>    <chr>      <chr>      <chr>               <int>            <int>
#> 1 G1       L1         B1         U1                      1                1
#> 2 G1       L2         B1         U2                      1                0
#> # ℹ 2 more variables: first_player_state <chr>, last_player_state <chr>

pages %>%
  select(
    group_id, login_name, booklet_id, unit_key,
    observed_page_nrs, page_count,
    n_invalid_current_page_id_events, has_invalid_current_page_id,
    reached_last_page_nr, observed_pages_complete,
    observed_page_nr_gaps, missing_page_nrs,
    page_count_consistent, page_nr_exceeds_page_count
  )
#> # A tibble: 2 × 14
#>   group_id login_name booklet_id unit_key observed_page_nrs page_count
#>   <chr>    <chr>      <chr>      <chr>    <chr>                  <dbl>
#> 1 G1       L1         B1         U1       1, 2                       2
#> 2 G1       L2         B1         U2       2                          1
#> # ℹ 8 more variables: n_invalid_current_page_id_events <int>,
#> #   has_invalid_current_page_id <lgl>, reached_last_page_nr <lgl>,
#> #   observed_pages_complete <lgl>, observed_page_nr_gaps <lgl>,
#> #   missing_page_nrs <chr>, page_count_consistent <lgl>,
#> #   page_nr_exceeds_page_count <lgl>

progress %>%
  select(
    group_id, login_name, booklet_id, unit_key,
    final_response_progress, final_presentation_progress,
    response_reached_complete, presentation_reached_complete
  )
#> # A tibble: 2 × 8
#>   group_id login_name booklet_id unit_key final_response_progress
#>   <chr>    <chr>      <chr>      <chr>    <chr>                  
#> 1 G1       L1         B1         U1       complete               
#> 2 G1       L2         B1         U2       NA                     
#> # ℹ 3 more variables: final_presentation_progress <chr>,
#> #   response_reached_complete <lgl>, presentation_reached_complete <lgl>
```

Bei Seiten ist `reached_last_page_nr` bewusst schwächer als
`observed_pages_complete`: Es zeigt nur, ob die höchste beobachtete
Seitennummer die berichtete Seitenzahl erreicht hat.
`observed_pages_complete` wird nur dann `TRUE`, wenn alle ganzzahligen
Seitennummern von 1 bis `PAGE_COUNT` tatsächlich in den Logs beobachtet
wurden; bei nicht beurteilbarer Seitenzählung bleibt der Wert `NA`.

`CURRENT_PAGE_ID = -1` ist keine echte Seite. Dieser Zustand bedeutet in
der Regel, dass der Testcenter-Host die vom Player gemeldete aktuelle
Seite gerade nicht in seinen gültigen Seiten wiederfindet. Das kann beim
Start einer Unit kurz plausibel sein, etwa bevor `validPages`
vollständig synchronisiert ist. Wiederholte oder späte Vorkommen sollten
geprüft werden;
[`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
nutzt solche unmapped page states nicht als valide Seiten und weist die
betroffenen Intervalle separat über `unmapped_page_time_ms` aus.

Für beschädigte oder abbrechende Logs sind drei Details wichtig. Eine
gültige `CURRENT_PAGE_ID`, die zugleich der letzte Log-Eintrag ist,
bleibt als Seite sichtbar; ihre Seitenzeit kann dann aber `0` sein, weil
kein späterer Endzeitpunkt mehr beobachtet wurde. Wenn eine gültige
Page-ID schon vor `PLAYER = RUNNING` geloggt wurde, wird
`delay_first_valid_page_id_ms` auf `0` gesetzt und
`valid_page_id_before_running` markiert diesen Fall. Wenn eine Unit nur
`CURRENT_PAGE_ID = -1` und keine gültige Page-ID enthält, bleiben die
Unit-Zeiten erhalten, aber `unit_has_pages = FALSE`: Seitenzeiten sind
dann nicht rekonstruierbar. `unmapped_page_time_ms` ist dabei nur ein
Rohindikator für Intervalle nach negativen Page-ID-Ereignissen, kein
vollständiger Ausgleich zur Unit-Zeit.

## Bearbeitungszeiten und Unit-Ladezeiten

Für Zeitintervalle auf Unit- und Seitenebene ist weiterhin
[`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
die zentrale Funktion. Sie berechnet unter anderem:

- `unit_time`: Zeit von `PLAYER = RUNNING` bis zum nächsten relevanten
  Ereignis
- `unit_loadtime`: Zeit von `PLAYER = LOADING` bis `PLAYER = RUNNING`
- `unit_playbacks`: einzelne Unit-Playbacks als verschachtelte Tabelle
- `focus_events`: Fokusverluste innerhalb von Unit-Verläufen
- `unit_page_logs`: Seitenzeiten, wenn `CURRENT_PAGE_ID` vorhanden ist
- `n_invalid_current_page_id_events`, `delay_first_valid_page_id_ms`,
  `valid_page_id_before_running` und `unmapped_page_time_ms`: Hinweise
  auf nicht zuordenbare Page-ID-Zustände

Standardmäßig hängt
[`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
die Session-Environment-Spalten aus
[`summarise_log_environment()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md)
an. Mit `include_environment = FALSE` bleibt die Ausgabe auf die Zeit-
und Ereignisspalten beschränkt.

``` r

unit_times <- estimate_unit_times(logs)
```

``` r

unit_times %>%
  select(
    group_id, login_name, booklet_id, unit_key,
    unit_n_play, unit_time, unit_loadtime,
    n_run_no_load, n_failed_loadings,
    unit_has_pages,
    n_invalid_current_page_id_events,
    delay_first_valid_page_id_ms,
    valid_page_id_before_running,
    unmapped_page_time_ms,
    browser_name, device_class, load_time,
    n_loadcomplete_events
  )
#> # A tibble: 1 × 18
#>   group_id login_name booklet_id unit_key unit_n_play unit_time unit_loadtime
#>   <chr>    <chr>      <chr>      <chr>          <int>     <dbl>         <dbl>
#> 1 G1       L1         B1         U1                 1      2600           300
#> # ℹ 11 more variables: n_run_no_load <int>, n_failed_loadings <int>,
#> #   unit_has_pages <lgl>, n_invalid_current_page_id_events <int>,
#> #   delay_first_valid_page_id_ms <dbl>, valid_page_id_before_running <lgl>,
#> #   unmapped_page_time_ms <dbl>, browser_name <chr>, device_class <chr>,
#> #   load_time <dbl>, n_loadcomplete_events <int>
```

`unit_loadtime` ist nicht dasselbe wie `LOADCOMPLETE$loadTime`.
`load_time` aus `LOADCOMPLETE` beschreibt die initiale
Browser-/Player-Ladeinformation einer Session. `unit_loadtime` wird aus
den Player-Zuständen einer konkreten Unit geschätzt. Wenn bereits eine
eigene Session-Tabelle mit
[`summarise_log_environment()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md)
aufgebaut wird, kann `include_environment = FALSE` gesetzt und die
Environment-Tabelle später gezielt gejoint werden.

> **Empfohlen:** Zeitvariablen sollten erst nach
> [`summarise_log_qc()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_qc.md)
> und
> [`detect_log_anomalies()`](https://iqb-research.github.io/eatPrepTBA/reference/detect_log_anomalies.md)
> interpretiert werden. Anomalien wie fehlendes `PLAYER = RUNNING`,
> Runtime-Fehler oder ein letzter Zustand `LOST` können Zeitintervalle
> unbrauchbar machen.

Wenn aus den geschätzten Zeiten Berichtstabellen mit Quantilen erzeugt
werden sollen, ist
[`compute_staytime_tables()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_staytime_tables.md)
ein nachgelagerter Schritt. Die Funktion setzt auf vorbereiteten
Zeitdaten aus
[`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
auf und kombiniert sie mit Domänen-, Antwort-, Kodier- und Metadaten für
Quarto-Reports.

> **Empfohlen:** Der Default `min_page_n_valid = 2` hält auch dünn
> besetzte, aber real beobachtete Seitenzeiten in explorativen
> Staytime-Berichten sichtbar. Für konservative finale Berichte oder
> Reproduktion älterer Ausgaben sollte die verwendete Schwelle
> dokumentiert werden; `min_page_n_valid = 11` entspricht der früheren
> Regel “mehr als zehn Beobachtungen”. Der Default
> `response_filter = "coded"` bleibt für reguläre Berichte sinnvoll.
> Verwenden Sie `response_filter = "all"` nur bewusst, wenn auch
> Beispiel-, uncodierte oder nicht verwendete Antwortzeilen in die
> Prüfung eingehen sollen.

## Unitgrößen ergänzen

Wenn Unitgrößen verfügbar sind, sollten sie als Output von
[`compute_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_sizes.md)
übergeben werden. Dadurch bleibt die Log-Analyse unabhängig davon, wie
die Größendaten ursprünglich erzeugt oder umgebaut wurden.

``` r

files <- list_files(workspace, dependencies = TRUE)
sizes <- compute_sizes(files)
```

In der Vignette verwenden wir eine kleine künstliche Dateiliste.

``` r

files <- tibble::tibble(
  type = c("Unit", "Unit", "Booklet", "Resource"),
  name = c("U1.xml", "U2.xml", "B1.xml", "asset.png"),
  size = c(800000, 1800000, 3000, 200000),
  dependencies = list(
    list(
      list(relationship_type = "isDefinedBy", object_name = "U1.xml"),
      list(relationship_type = "uses", object_name = "asset.png")
    ),
    list(
      list(relationship_type = "isDefinedBy", object_name = "U2.xml")
    ),
    list(
      list(relationship_type = "uses", object_name = "U1.xml"),
      list(relationship_type = "uses", object_name = "U2.xml")
    ),
    list()
  )
)

sizes <- compute_sizes(files)

unit_times_with_sizes <- add_unit_sizes(unit_times, sizes)

unit_times_with_sizes %>%
  select(
    group_id, login_name, unit_key,
    unit_loadtime, unit_size_mb,
    unit_size_available,
    unit_median_loadtime_per_mb
  )
#> # A tibble: 1 × 7
#>   group_id login_name unit_key unit_loadtime unit_size_mb unit_size_available
#>   <chr>    <chr>      <chr>            <dbl>        <dbl> <lgl>              
#> 1 G1       L1         U1                 300        0.763 TRUE               
#> # ℹ 1 more variable: unit_median_loadtime_per_mb <dbl>
```

Die `*_per_mb`-Variablen beschreiben beobachtete Ladezeit pro MiB. Sie
sind nützlich, um auffällige Kombinationen aus Unitgröße und Ladezeit zu
finden. Sie sind aber keine Messung der Downloadgeschwindigkeit, weil
Browser, Player, Caching, Geräteperformance und Testcenter-Zustände
gemeinsam in die Ladezeit eingehen können.

## Systemchecks ergänzen

Systemchecks sind die bessere Quelle, wenn in einer Studie tatsächlich
netzwerknahe Informationen erhoben wurden. Die Dateien sind nicht immer
gleich strukturiert; deshalb fasst
[`summarise_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_system_checks.md)
mögliche Netzwerkmetriken konservativ zusammen und behält sie zusätzlich
als List-Column.

``` r

system_checks <- get_system_checks(workspace, groups = "Gruppenname")
system_checks <- read_system_checks("system-checks.csv")
```

``` r

system_checks <- tibble::tibble(
  groupname = c("G1", "G1"),
  loginname = c("L1", "L1"),
  login_code = c("C1", "C1"),
  bookletname = c("B1", "B1"),
  downlink = c("12.5", "12.5"),
  rtt = c("50", "50"),
  effectiveType = c("4g", "4g"),
  id = c("network_check", "screen_check"),
  value = c("ok", "ok")
)

system_summary <- summarise_system_checks(system_checks)

system_summary %>%
  select(
    group_id, login_name, login_code, booklet_id,
    has_network_metrics,
    system_check_download_value,
    system_check_rtt_value,
    system_check_effective_type
  )
#> # A tibble: 1 × 8
#>   group_id login_name login_code booklet_id has_network_metrics
#>   <chr>    <chr>      <chr>      <chr>      <lgl>              
#> 1 G1       L1         C1         B1         TRUE               
#> # ℹ 3 more variables: system_check_download_value <chr>,
#> #   system_check_rtt_value <chr>, system_check_effective_type <chr>
```

Die Zusammenfassung kann anschließend an sessionbezogene Log-Summaries
gehängt werden.

``` r

session_overview <- qc %>%
  left_join(environment, by = c("group_id", "login_name", "login_code", "booklet_id")) %>%
  left_join(connections, by = c("group_id", "login_name", "login_code", "booklet_id")) %>%
  left_join(focus, by = c("group_id", "login_name", "login_code", "booklet_id")) %>%
  add_system_check_summary(system_summary)

session_overview %>%
  select(
    group_id, login_name, booklet_id,
    log_qc_flag, browser_name, device_class,
    has_connection_lost, has_unresolved_focus_loss,
    has_network_metrics,
    system_check_download_value
  )
#> # A tibble: 2 × 10
#>   group_id login_name booklet_id log_qc_flag browser_name device_class
#>   <chr>    <chr>      <chr>      <chr>       <chr>        <chr>       
#> 1 G1       L1         B1         ok          Safari       tablet      
#> 2 G1       L2         B1         critical    NA           NA          
#> # ℹ 4 more variables: has_connection_lost <lgl>,
#> #   has_unresolved_focus_loss <lgl>, has_network_metrics <lgl>,
#> #   system_check_download_value <chr>
```

## `prepare_logs()` für gezielte Detailarbeit

[`prepare_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/prepare_logs.md)
ist weiterhin nützlich, wenn einzelne Log-Ereignisse gezielt gefiltert
und in Spalten zerlegt werden sollen. Für breite Standardanalysen sind
die Summary-Funktionen meist stabiler und günstiger, weil sie genau
definierte Aggregationen liefern.

``` r

prepare_logs(logs, log_events = c("loadcomplete", "connection", "player")) %>%
  select(
    group_id, login_name, unit_key, ts,
    browser_name, os_name, device_class,
    connection, player
  )
#> # A tibble: 8 × 9
#>   group_id login_name unit_key    ts browser_name os_name    device_class
#>   <chr>    <chr>      <chr>    <dbl> <chr>        <chr>      <chr>       
#> 1 G1       L1         U1         100 Safari       iOS 18.6.2 tablet      
#> 2 G1       L1         U1         150 NA           NA         NA          
#> 3 G1       L1         U1         160 NA           NA         NA          
#> 4 G1       L1         U1         200 NA           NA         NA          
#> 5 G1       L1         U1         500 NA           NA         NA          
#> 6 G1       L2         U2         100 NA           NA         NA          
#> 7 G1       L2         U2         140 NA           NA         NA          
#> 8 G1       L2         U2         200 NA           NA         NA          
#> # ℹ 2 more variables: connection <chr>, player <chr>
```

## Empfohlener Workflow

Für reale Studien hat sich folgende Reihenfolge bewährt:

1.  Rohdaten einlesen:
    [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
    oder
    [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).
2.  Log-Typen inventarisieren:
    [`summarise_log_inventory()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_inventory.md).
3.  Geräte und Umgebung als Session-Tabelle extrahieren:
    [`summarise_log_environment()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md).
4.  Anomalien prüfen:
    [`detect_log_anomalies()`](https://iqb-research.github.io/eatPrepTBA/reference/detect_log_anomalies.md)
    und
    [`summarise_log_qc()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_qc.md).
5.  Zustände verdichten:
    [`summarise_log_connections()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_connections.md),
    [`summarise_log_focus()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_focus.md),
    [`summarise_log_player()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_player.md),
    [`summarise_log_pages()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_pages.md)
    und
    [`summarise_log_progress()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_progress.md).
6.  Zeitintervalle berechnen:
    [`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md).
    Die Environment-Spalten werden standardmäßig mitgeführt; bei
    separater Session-Tabelle kann `include_environment = FALSE` gesetzt
    werden.
7.  Optional Unitgrößen ergänzen:
    [`compute_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_sizes.md)
    und
    [`add_unit_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/add_unit_sizes.md).
8.  Optional Systemchecks ergänzen:
    [`get_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/get_system_checks.md)
    oder
    [`read_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/read_system_checks.md),
    danach
    [`summarise_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_system_checks.md)
    und
    [`add_system_check_summary()`](https://iqb-research.github.io/eatPrepTBA/reference/add_system_check_summary.md).
9.  Optional Berichtstabellen vorbereiten:
    [`compute_staytime_tables()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_staytime_tables.md)
    auf Basis der mit
    [`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
    geschätzten Zeitdaten.
10. Analyseentscheidungen dokumentieren: Welche Anomalien führen zu
    Ausschluss, Sensitivitätsanalyse oder nur zu einem Hinweis?

> **Empfohlen:** Für Berichte sollte mindestens eine Session-Tabelle mit
> `log_qc_flag`, Browser/OS/Gerät/Auflösung, Verbindungsverlusten,
> ungelösten Fokusverlusten und, falls vorhanden,
> Systemcheck-Netzwerkangaben erzeugt werden. Diese Tabelle macht
> sichtbar, ob die Logdaten als Grundlage weiterer Analysen verlässlich
> genug sind.

## Performance bei großen Logdaten

Logdaten können sehr groß sein. Einige einfache Regeln helfen, Analysen
stabil zu halten:

- Erst Inventar und QC rechnen, dann Detailtabellen.
- Rohdaten nach dem Einlesen als RDS zwischenspeichern.
- Für Detailfragen nach Log-Typen filtern, statt alle Log-Einträge
  mehrfach zu durchsuchen.
- Session- und Unit-Summaries getrennt aufbauen und erst am Ende gezielt
  joinen.
- Ladezeiten nie isoliert interpretieren, sondern zusammen mit
  Anomalien, Gerätetypen, Auflösung, Unitgröße und
  Systemcheck-Hinweisen.

``` r

inventory <- summarise_log_inventory(logs)
environment <- summarise_log_environment(logs)
anomalies <- detect_log_anomalies(logs)
qc <- summarise_log_qc(logs, anomalies = anomalies)

session_overview <- qc %>%
  left_join(environment, by = c("group_id", "login_name", "login_code", "booklet_id"))
```

Die Log-Auswertung bleibt damit modular: Jede Tabelle beantwortet eine
klar begrenzte Frage und kann bei Bedarf einzeln geprüft, gespeichert
oder erweitert werden.
