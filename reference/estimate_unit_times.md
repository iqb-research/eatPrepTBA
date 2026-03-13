# Berechnet Bearbeitungs- und Ladezeiten anhand der Logdaten

**\[experimental\]**

Berechnet geschätzte Bearbeitungs- und Ladezeiten für Units und Seiten.

- loading_time: Zeitspanne zwischen LOADING und RUNNING des Players pro
  Abspielung des Units

- unit_time: Zeitspanne zwischen RUNNING des Players und dem nächsten
  Zeitstempel (meist LOADING des nächsten Units, manchmal Sitzungsende),
  pro Abspielung des Units

- unit_n_play: Anzahl der Abspielungen des Units in dieser Session

- n_loadings: Anzahl der Ladeversuche des Units (summiert über die
  Abspielungen, und über erfolgreiche und erfolgslose Ladeversuche)

- page_time: Zeitspanne zwischen CURRENT_PAGE_ID =
  [...](https://rdrr.io/r/base/dots.html) (Ladeabschluss der Seite) und
  Ladeabschluss der nächsten Seite bzw. bis Sitzungsende

- run_no_load_i: Player wurde als RUNNING, aber vorher nicht als LOADING
  geloggt. In diesem Fall wurden Ladezeiten nicht berechnet.

Daten gruppiert nach Gruppe, Login, Booklet, Unit_key. Achtung:
unit_alias wird hier nicht beachtet und es wird nicht danach gruppiert
oder sortiert, es wird nur am Ende als Information wieder eingefügt.

## Usage

``` r
estimate_unit_times(logs)
```

## Arguments

- logs:

  Tibble. Must be a logs tibble retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

## Value

Data frame mit diversen Zeiten und Zeitstempeln pro Unit bzw. Seite
