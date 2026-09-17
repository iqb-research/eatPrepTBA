# Vom Testcenter zum Skalierungsdatensatz

Dieser Workflow führt von Testcenter-Antworten zu kodierten Daten,
psychometrischen Kennwerten und einem Personen-×-Items-Datensatz. Das
Beispiel umfasst einen Testzeitpunkt und einen Kompetenzbereich (Lesen)
mit Autocode-Kodierschemata. Für Daten ohne solche Schemata, etwa aus
StarS, siehe den Hinweis in Abschnitt 2.

| Quelle | Benötigte Daten |
|:---|:---|
| **Studio** | Units mit Kodierschemata, Item-Metadaten und Seiteninformationen |
| **Testcenter** | Antworten sowie Testdesign aus Testpersonen und Testheften |

Dateipfade, Serveradresse, Version, Workspace-IDs und Testmodus sind
Platzhalter. Die folgenden Blöcke sind für die Ausführung mit eigenen
Daten vorgesehen.

``` r

library(eatPrepTBA)
library(dplyr)
library(tidyr)
```

## 1. Studio: Units und Kodierschemata laden

Die Units müssen zur eingesetzten Testversion passen. `metadata = TRUE`
liefert die Item-Verknüpfungen, `unit_definition = TRUE` die
Seiteninformationen.

Ein **Kodierschema** legt für jede Variable fest, welche Antwort welchen
Code und wie viele Punkte erhält, etwa „richtige Auswahl → Code 1 → 1
Punkt“. Es kann auch Regeln zur Ableitung weiterer Variablen enthalten.

[`login_studio()`](https://iqb-research.github.io/eatPrepTBA/reference/login_studio.md)
meldet am Studio an,
[`access_workspace()`](https://iqb-research.github.io/eatPrepTBA/reference/access_workspace.md)
wählt den Arbeitsbereich.
[`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
lädt die Units;
[`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md)
bereitet ihre vorhandenen Kodierschemata für die Auswertung auf.

``` r

studio_login <- login_studio(app_version = "STUDIO_VERSION")
studio <- access_workspace(studio_login, ws_id = 123)

units <- get_units(studio, metadata = TRUE, unit_definition = TRUE)
units <- add_coding_scheme(units)
```

## 2. Testcenter: Antworten und Design bereitstellen

### Testcenter-Verbindung herstellen

**Auch beim CSV-Import wird hier Testcenter-Zugriff benötigt:**
[`get_design()`](https://iqb-research.github.io/eatPrepTBA/reference/get_design.md)
lädt das vollständige Testdesign. Erst damit lassen sich später fehlende
Antworten als Not reached oder Auslassung einordnen.
[`read_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md)
allein reicht dafür nicht aus.

Anmeldung mit
[`login_testcenter()`](https://iqb-research.github.io/eatPrepTBA/reference/login_testcenter.md),
Auswahl des Arbeitsbereichs mit
[`access_workspace()`](https://iqb-research.github.io/eatPrepTBA/reference/access_workspace.md).

``` r

testcenter_login <- login_testcenter(base_url = "https://TESTCENTER-ADRESSE/")
testcenter <- access_workspace(testcenter_login, ws_id = 1)
```

### Antworten: eine der beiden Alternativen wählen

**A – direkt aus dem Testcenter** mit
[`get_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md):

``` r

responses <- get_responses(testcenter)
```

**B – bereits heruntergeladenen CSV-Export einlesen** mit
[`read_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md):

``` r

responses <- read_responses("daten/Responses.csv")
```

**Ohne Autocode-Kodierschema (z. B. StarS):**
[`prepare_responses(responses)`](https://iqb-research.github.io/eatPrepTBA/reference/prepare_responses.md)
entpackt die Rohantworten zur eigenen Weiterverarbeitung; es vergibt
keine Codes oder Scores. Bereits im Export gespeicherte Kodierungen
lassen sich mit
[`prepare_coded(responses)`](https://iqb-research.github.io/eatPrepTBA/reference/prepare_coded.md)
entpacken. Die folgenden Schritte zeigen den Weg **mit** Kodierschemata;
die entpackten Daten sind dafür kein direkter Ersatz.

### Das vollständige Testdesign laden

[`get_design()`](https://iqb-research.github.io/eatPrepTBA/reference/get_design.md)
liefert die vorgesehenen Person–Testheft–Variablen-Zuordnungen.

Im gewählten **Testcenter-Workspace** müssen dafür die Materialien der
Erhebung liegen:

- **Testpersonen-XML** (z. B. `testtakers.xml`): Gruppen, Logins, Codes,
  Anmeldemodus und Zuordnung zu den Testheften.
- **Die zugehörigen Testheft-XML-Dateien** (Booklets): referenzierte
  Units und ihre Reihenfolge innerhalb der Testlets.

Die Dateien müssen dem Stand der Durchführung entsprechen. Die Variablen
und Kodierschemata kommen hier aus dem zuvor aus Studio geladenen Objekt
`units`.

``` r

design <- get_design(testcenter, units = units, mode = "run-hot-return")
```

`mode` muss zum Anmeldemodus der Erhebung passen. `units` muss alle
relevanten Aufgaben enthalten; Antworten und Design müssen dieselbe
Erhebung abdecken.

## 3. Automatisch kodieren und Ergebnis prüfen

[`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md)
nutzt das R-Paket
[eatAutoCode](https://github.com/iqb-research/eatAutoCode), das den
[IQB-Autocoder
(`@iqb/responses`)](https://github.com/iqb-berlin/responses) einbindet.
Dieser vergibt anhand des Kodierschemas Codes (`code_id`) und Punkte
(`code_score`). `prepare = TRUE` ergänzt Metadaten für die weiteren
Schritte. **Die abschließende Missing-Zuweisung folgt erst in Abschnitt
5.**

``` r

coded <- code_responses(responses, units, prepare = TRUE)
count(coded, code_status, code_type)
```

Entscheidend sind die Variablen, die später als Items ausgewertet
werden:

| `code_status` | Bewertung und nächster Schritt |
|:---|:---|
| `CODING_COMPLETE` | **In Ordnung:** Die Itemvariable ist fertig kodiert. |
| `CODING_INCOMPLETE`, `DERIVE_PENDING` | **Noch offen:** Kodierung bzw. benötigte Ausgangscodes prüfen; ggf. manuelle Codes ergänzen (Abschnitt 4). |
| `UNSET`, `CODING_ERROR`, `DERIVE_ERROR` | **Prüfen:** Kodierschema und Antworten kontrollieren; nach Korrekturen Units neu laden und erneut kodieren. |
| `DISPLAYED`, `PARTLY_DISPLAYED`, `NOT_REACHED`, `INVALID` | **In der Regel in Ordnung:** erwartbare Missing-Status, die in Abschnitt 5 behandelt werden. Unerwartete Häufungen prüfen. |

Untervariablen, die selbst keine Items werden, können andere Status
behalten, ohne dass ein Problem vorliegt. Zu klären sind offene
Kodierungen und Fehler, die die Itemvariablen oder deren Berechnung
betreffen.

## 4. Optional: manuelle Codes einfügen

**Nur nötig, wenn Antworten manuell kodiert werden. Sonst weiter mit
Abschnitt 5.** Die fertige Tabelle `codes_manual` benötigt `group_id`,
`login_name`, `login_code`, `booklet_id`, `unit_key`, `variable_id` und
`code_id`: genau einen Code je zu kodierender Antwort, passend zum
Kodierschema. `code_id` enthält den Code, nicht die Punktzahl.

``` r

# Kennungen als Text, code_id als Ganzzahl einlesen
codes_manual <- readr::read_csv("daten/codes_manual.csv",
                              col_types = readr::cols(.default = "c", code_id = "i"))

coded <- code_responses(responses, units, prepare = TRUE,
                        codes_manual = codes_manual)
count(coded, code_status, code_type)
```

Erneut die ursprünglichen `responses` übergeben: So werden die manuellen
Codes eingefügt und davon abhängige Variablen neu berechnet.
Anschließend die Status wie in Abschnitt 3 prüfen.

## 5. Design vervollständigen und Missings zuweisen

**Der Antwortexport allein enthält nicht alle vorgesehenen Antworten.**
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
ergänzt fehlende Einträge aus dem vollständigen Design. Anhand der
Unit-Reihenfolge innerhalb eines Testlets werden nicht erreichte
Aufgaben am Ende von Auslassungen vor später bearbeiteten Units
unterschieden. Bereits als Auslassung kodierte Antworten bleiben
standardmäßig Auslassungen.

Hier gelten die Standardregeln: **Auslassungen und ungültige Antworten
erhalten 0 Punkte; Not reached und Kodierfehler `NA`.** Eine eigene
Zuordnung kann über `missings` übergeben werden (siehe Funktionshilfe).

``` r

design_coded <- complete_design(coded = coded, units = units, design = design)

count(design_coded, id_used, code_type)
analysis_data <- filter(design_coded, id_used)
```

`id_used` kennzeichnet Personen mit mindestens einem gespeicherten
Kodierstatus. Mit `recode_omissions_to_not_reached = TRUE` können
zusätzlich abschließende Auslassungen als nicht erreicht eingeordnet
werden. Aufgaben, die laut Design gar nicht vorgesehen waren, sind
**Missing by Design**, nicht Not reached; ihre Zellen bleiben beim
späteren Umformen `NA`.

## 6. Psychometrische Kennwerte berechnen

Im Beispiel gehören alle Units zum Bereich Lesen; bei mehreren Bereichen
enthält `domains` die jeweilige Zuordnung (ein Bereich je Unit).
[`evaluate_psychometrics()`](https://iqb-research.github.io/eatPrepTBA/reference/evaluate_psychometrics.md)
berechnet die Kennwerte.

``` r

domains <- distinct(units, unit_key)
domains$domain <- "Lesen"

psychometrics <- evaluate_psychometrics(analysis_data, units, domains = domains)
psychometrics <- add_item_id(psychometrics, units)

psychometrics |>
  select(item_id, unit_key, variable_id, code_id, code_score,
         code_n, code_p_valid, code_pbc) |>
  distinct()
```

`code_n` zählt einen Code, `code_p_valid` ist sein Anteil unter
Antworten mit Score ungleich `NA` (einschließlich als 0 gewerteter
Missings). `code_pbc` ist die Korrelation seines Auftretens mit dem
mittleren Itemscore des Bereichs (nicht part-whole-korrigiert).

## 7. Personen × Items für eatModel

[`add_item_id()`](https://iqb-research.github.io/eatPrepTBA/reference/add_item_id.md)
ergänzt die Item-ID aus den Studio-Metadaten über `unit_key` und
`variable_id`. Variablen ohne Item-Verknüpfung erhalten `NA`; im
Beispiel werden nur verknüpfte Itemvariablen übernommen.

``` r

item_data <- add_item_id(analysis_data, units)
item_data <- filter(item_data, !is.na(item_id))

scaling_data <- item_data |>
  select(group_id, login_name, login_code, item_id, code_score) |>
  pivot_wider(names_from = item_id, values_from = code_score)
scaling_data$person_id <- seq_len(nrow(scaling_data))
```

[`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
erzeugt eine Zeile je Person und eine Scorespalte je Item; die
Testcenter-Kennungen bleiben neben `person_id` erhalten. Voraussetzung
sind eindeutige Itemnamen und genau ein Score je Person und Item.
Fehlende Werte bleiben `NA`, sie werden nicht mit 0 aufgefüllt.
`analysis_data` aufbewahren: Darin bleiben die Missing-Arten
unterscheidbar.

Die weiteren Schritte zur Skalierung beschreibt die
[eatModel-Dokumentation](https://weirichs.github.io/eatModel/).
