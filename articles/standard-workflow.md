# Vom Testcenter zum Skalierungsdatensatz

**Paketstand: eatPrepTBA 0.9.8.9037.**

Dieser Workflow führt von Testcenter-Antworten zu einem Datensatz mit
**einer Zeile je Person und einer Punktespalte je ausgewerteter Aufgabe
(Item)**. Dazu werden die Antworten kodiert, Missings eingeordnet und
psychometrische Kennwerte berechnet. Das Beispiel umfasst einen
Testzeitpunkt und einen Bereich (Lesen) mit Autocode-Kodierschemata. Für
Daten ohne solche Schemata, etwa aus StarS, siehe den Hinweis in
Abschnitt 2.

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

Eine **Unit** ist eine Aufgabeneinheit mit einer oder mehreren
Variablen. Die Units müssen zur eingesetzten Testversion passen.
`metadata = TRUE` liefert die Item-Verknüpfungen,
`unit_definition = TRUE` die Seiteninformationen.

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

Das Beispiel verwendet Studio-Version `20.0.1`. Falls auf deiner
Studio-Anmeldeseite eine andere Version steht, trage diese bei
`app_version` ein.

``` r

studio_login <- login_studio(app_version = "20.0.1")
studio <- access_workspace(studio_login, ws_id = 123)

units <- get_units(studio, metadata = TRUE, unit_definition = TRUE)
units <- add_coding_scheme(units)
```

## 2. Testcenter: Antworten und Design bereitstellen

### Testcenter-Verbindung herstellen

**Auch beim CSV-Import wird hier Testcenter-Zugriff benötigt:**
[`get_design()`](https://iqb-research.github.io/eatPrepTBA/reference/get_design.md)
lädt das vollständige Testdesign. Erst damit lassen sich später fehlende
Antworten als nicht erreicht (Not reached) oder ausgelassen einordnen.
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
liefert für jede Person die Variablen, die in ihrem Testheft vorgesehen
waren.

Im gewählten **Testcenter-Workspace** müssen dafür die Materialien der
Erhebung liegen:

- **Testpersonen-XML** (z. B. `testtakers.xml`): Gruppen, Logins, Codes,
  Anmeldemodus und Zuordnung zu den Testheften.
- **Die zugehörigen Testheft-XML-Dateien** (Booklets): referenzierte
  Units und ihre Reihenfolge innerhalb der Testlets.

Die Dateien müssen dem Stand der Durchführung entsprechen. Die
Variablenliste wird aus den Kodierschemata im zuvor aus Studio geladenen
Objekt `units` ergänzt. Seiten und Abschnitte kommen aus der
Unitdefinition; die Beziehungen zwischen Ableitungen und ihren Quellen
stehen im Kodierschema.

``` r

design <- get_design(testcenter, units = units, mode = "run-hot-return")
```

`mode` muss zum Anmeldemodus der Erhebung passen. `units` muss alle
relevanten Aufgaben enthalten; Antworten und Design müssen dieselbe
Erhebung abdecken.

## 3. Automatisch kodieren und Ergebnis prüfen

[`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md)
vergibt anhand des Kodierschemas Codes (`code_id`) und Punkte
(`code_score`). Dafür nutzt es
[eatAutoCode](https://github.com/iqb-research/eatAutoCode) mit dem
[IQB-Autocoder](https://github.com/iqb-berlin/responses).
`prepare = TRUE` bereitet das Ergebnis für die weiteren Schritte auf.
**Die abschließende Missing-Zuweisung folgt erst in Abschnitt 5.**

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

### Die voreingestellte Missing-Zuordnung

**Diese Tabelle ist in
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
bereits hinterlegt.** Du musst das Objekt `missings` für die
Beispielaufrufe nicht anlegen oder übergeben. Auslassungen und ungültige
Antworten erhalten 0 Punkte, Not reached und Kodierfehler `NA` (keine
Punktzahl).

``` r

missings <- tibble::tribble(
  ~code_id, ~code_status,   ~code_score, ~code_type,
  -96,     "NOT_REACHED",  NA_real_,   "MISSING_NOT_REACHED",
  -97,     "CODING_ERROR", NA_real_,   "MISSING_CODING_IMPOSSIBLE",
  -98,     "INVALID",      0,          "MISSING_INVALID_RESPONSE",
  -99,     "DISPLAYED",    0,          "MISSING_BY_OMISSION")
```

[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
bestimmt die Missing-Art (`code_type`) und setzt dazu Code und Punktzahl
aus der Tabelle. **`code_status` bleibt unverändert**, auch `NA` bei neu
ergänzten Zeilen. Die Statuszuordnung benötigt
[`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md)
für manuelle Missing-Codes, etwa `-99` → `DISPLAYED`. Die endgültigen
Missing-Punktzahlen vergibt erst
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
in Abschnitt 5.

**Eigene Missing-Regeln:** Den Tabellenblock oben anpassen und
ausführen. Dann `missings = missings` als Argument in Abschnitt 4 (falls
genutzt) und 5 ergänzen.

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
ergänzt fehlende Zeilen aus dem Design. Es unterscheidet nicht erreichte
Variablen am Ende von Auslassungen vor einer späteren Bearbeitung –
getrennt je Person, Testheft und Testlet (Testabschnitt).

Bereits als Auslassung kodierte Antworten bleiben mit
**`recode_omissions_to_not_reached = FALSE`** Auslassungen. Setze diese
Option auf **`TRUE`**, wenn auch abschließende Auslassungen als Not
reached eingeordnet werden sollen. Bei **Auslassung – Antwort –
Auslassung** wird dann nur die letzte Auslassung zu Not reached, sofern
danach im selben Testlet keine weitere Bearbeitung folgt.

**`preserve_not_reached = TRUE`** verhindert umgekehrt, dass Not reached
vor einer späteren Bearbeitung zur Auslassung wird. Das gilt auch für
ergänzte Designzeilen, noch fehlende Codes bei Status `NOT_REACHED` oder
`NA` und NR-Ableitungen. Auch zuvor aus Auslassungen erzeugtes NR bleibt
erhalten. Beide Optionen sind unabhängig und stehen standardmäßig auf
`FALSE`.

``` r

design_coded <- complete_design(coded = coded, units = units, design = design,
                                recode_omissions_to_not_reached = FALSE,
                                preserve_not_reached = FALSE)

count(design_coded, id_used, code_type)
analysis_data <- filter(design_coded, id_used)
```

`id_used` ist `TRUE`, wenn eine Person mindestens einen gespeicherten
Kodierstatus hat. Der Filter übernimmt diese Personen in
`analysis_data`.

Innerhalb einer Unit gilt **Seite → Abschnitt → natürlicher
Variablenname** (z. B. `V2` vor `V10`). Ableitungen mit eindeutig
zugeordneten Quellen folgen direkt ihrer letzten Quelle, auch bei
mehrstufigen Ableitungen. `variable_position` zeigt diese Reihenfolge
als Zahl ab 1. Ein Teildesign kann Positionslücken enthalten; ohne
passende Variablenmetadaten steht hier `NA`.

**Ableitungen werden nicht neu berechnet.** Ein vorhandenes gültiges
Ergebnis zählt aber als Bearbeitung: Mit `preserve_not_reached = FALSE`
wird eine davor liegende Not-reached-Quelle zur Auslassung mit 0
Punkten; mit `TRUE` bleibt sie Not reached. `code_status` bleibt dabei
unverändert; Missing-Art und Punktzahl stehen in `code_type` und
`code_score`.

Die [Missing-Szenarien im
Versionsvergleich](https://iqb-research.github.io/eatPrepTBA/articles/missing-examples.md)
zeigen die Entscheidungen an konkreten Beispielen, einschließlich der
Sortierregeln bei fehlenden Angaben oder nicht zuordenbaren Quellen.

Aufgaben, die laut Design gar nicht vorgesehen waren, sind **Missing by
Design**; ihre Zellen bleiben beim späteren Umformen `NA`.

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

`code_n` zählt, wie oft ein Code vorkommt. `code_p_valid` ist sein
Anteil unter Antworten mit Punktzahl ungleich `NA` (einschließlich als 0
gewerteter Missings). `code_pbc` ist die Korrelation seines Auftretens
mit dem mittleren Itemscore des Bereichs. Der eigene Itemscore fließt in
dieses Mittel mit ein.

## 7. Personen × Items für eatModel

[`add_item_id()`](https://iqb-research.github.io/eatPrepTBA/reference/add_item_id.md)
ergänzt die Item-ID aus den Studio-Metadaten über `unit_key` und
`variable_id`. Variablen ohne Item-Verknüpfung erhalten `NA`; im
Beispiel werden nur verknüpfte Itemvariablen übernommen.

Für die Spaltennamen wird `item` nach der Konvention `unit_key__item_id`
gebildet, z. B. `DH001__08`. `item_id` bleibt dabei eine Zeichenkette:
Führende Nullen müssen erhalten bleiben und dürfen nicht durch eine
Umwandlung in Zahlen verloren gehen.

``` r

item_data <- add_item_id(analysis_data, units)
item_data <- filter(item_data, !is.na(item_id))
```

**Optional: fehlende führende Nullen ergänzen.** Falls die
Studio-Metadaten einstellige Item-IDs wie `"8"` enthalten, diesen Chunk
vor dem nächsten Schritt ausführen. Nach der Konvention besteht
`item_id` hier aus genau zwei Ziffern: `"8"` wird zu `"08"`, `"08"` und
`"12"` bleiben unverändert. Die Prüfung bricht bei anderen Formaten ab,
damit keine ungültigen IDs entstehen.

``` r

stopifnot(all(grepl("^[0-9]{1,2}$", item_data$item_id)))
item_data <- mutate(item_data,
                    item_id = stringr::str_pad(item_id, width = 2, pad = "0"))
```

Anschließend die Itemnamen bilden und ins weite Format umformen:

``` r

item_data <- mutate(item_data, item = paste0(unit_key, "__", item_id))

scaling_data <- item_data |>
  select(any_of(c("group_id", "login_name", "login_code", "IDSTUD")),
         item, code_score) |>
  pivot_wider(names_from = item, values_from = code_score)
```

[`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
erzeugt eine Zeile je Person und eine Scorespalte je Item; die
vorhandenen Kennungen (`group_id`, `login_name`, `login_code`, `IDSTUD`)
bleiben erhalten.
[`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
überspringt fehlende Kennungsspalten. Voraussetzung sind eindeutige
Itemnamen und genau ein Score je Person und Item. Fehlende Werte bleiben
`NA`, sie werden nicht mit 0 aufgefüllt. `analysis_data` aufbewahren:
Darin bleiben die Missing-Arten unterscheidbar.

**Für Referenzstudien:** Diesen Workflow und den
[Szenarienvergleich](https://iqb-research.github.io/eatPrepTBA/articles/missing-examples.md)
als HTML zusammen mit der verwendeten Paketversion zur Auswertung
ablegen. Die Online-Dokumentation wird weiterentwickelt.

Die weiteren Schritte zur Skalierung beschreibt die
[eatModel-Dokumentation](https://weirichs.github.io/eatModel/).
