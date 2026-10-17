# Missing-Szenarien: 9001, 9036 und neue Version

**18 Beispiele, drei Versionen:** 9001, 9036 und neu 0.9.8.9040.

## Hinweise zum Vergleich

Die **gelb markierten Eingangswerte** stehen jeweils über den
Ergebnissen der drei Versionen. Eingabe und Ergebnisse zeigen diese vier
Felder:

**`code_status` / `code_id` / `code_type` / `code_score`**

Die Felder enthalten Status, Code, Ergebnistyp und Punkte; `NA` steht
für einen fehlenden Wert. **Blau und fett** kennzeichnet Änderungen
gegenüber der Eingabe. **Hellrot** markiert die Probleme der 9036 in
Fall 1 und 2; beim Überfahren mit dem Mauszeiger erscheint eine
Erläuterung. In der neuen Version bleibt `code_status` unverändert.

Für die MNR-Grenze zählen neu nur Items und die Variablen, aus denen sie
berechnet werden. `NO_CODING` zählt nie. Missings werden weiterhin für
alle Zeilen vergeben. Die Versionsbeispiele enthalten Items und ihre
Quellen. So sind `01a` und `01b` Quellen für das Item `01`, keine
eigenen Items. [Am Ende](#itemgrenze) stehen Beispiele mit unabhängigen
Hilfsvariablen.

**O** steht für Auslassung, **NR** für nicht erreicht. Die neue Version
bietet zwei unabhängige Optionen (`TRUE` = aktiviert, `FALSE` =
deaktiviert):

- **O → NR** (`recode_omissions_to_not_reached`): Mit **TRUE** werden
  Auslassungen am Ende eines Testlets zusätzlich zu NR. Mit **FALSE**
  bleiben sie O.
- **NR behalten** (`preserve_not_reached`): Mit **TRUE** bleibt NR
  erhalten, auch vor einer späteren Antwort. Mit **FALSE** kann NR dort
  zu O werden. Der Schutz gilt auch für ergänzte Zeilen, NR aus
  fehlenden Statusangaben, Ableitungen und früher aus O erzeugtes NR.
  Gültige Ergebnisse bleiben erhalten.

**Grün und fett** kennzeichnet **TRUE / TRUE**, die Einstellung für die
Referenzstudien. Die Voreinstellungen der Funktion bleiben **FALSE /
FALSE**. 9001 zeigt ein Ergebnis, 9036 zwei Einstellungen und die neue
Version alle vier Kombinationen.

`T1 · U2/V01` bedeutet: Testlet 1, Unit 2, Variable V01.
**`variable_position`** zeigt die Reihenfolge innerhalb der Unit ab 1,
einschließlich Ableitungen; sie ist bei allen Einstellungen gleich.
Teildesigns können Lücken haben, ohne passende Metadaten steht `NA`.
„Keine Antwortzeile“ bedeutet: im Design vorgesehen, aber im Eingang
nicht vorhanden.

> **Aktuell Vorsicht bei Kodierbox im Testlet-Modus:** – die NR-Grenze
> scheint fehlerhaft bestimmt zu werden, wenn die Reihenfolge der
> Itemnamen in der Itemliste nicht der vorgesehenen
> Bearbeitungsreihenfolge entspricht. Der geprüfte Item-Matrix-Export
> folgt der Unit-Reihenfolge im Testheft und der Itemliste. Seiten und
> Abschnitte der einzelnen Variablen werden dabei nicht berücksichtigt.
> Quellen:
> [Sortierung](https://github.com/iqb-berlin/coding-box/blob/39468e5a28a24dc9ec860aee46daf8d4ed5c8682/apps/backend/src/app/database/services/coding/item-dataset-cell-resolver.ts#L505-L523)
> und
> [Itemliste](https://github.com/iqb-berlin/coding-box/blob/39468e5a28a24dc9ec860aee46daf8d4ed5c8682/apps/backend/src/app/database/services/coding/item-dataset-metadata.service.ts#L116-L158),
> Quellstand vom 26.09.2026, geprüft am 12.10.2026.

## Welche Sortierregeln gelten?

Alle drei Fassungen prüfen die Units je Person, Testheft und Testlet
**vom Ende her** (`unit_booklet_no` absteigend). Die Ausgabe läuft
wieder von vorne nach hinten. Innerhalb einer Unit gilt:

| Version | Reihenfolge für die Missing-Einstufung | Sortierung der ausgegebenen Zeilen |
|:---|:---|:---|
| **9001** | Die Unit wird als Ganzes betrachtet; die Position einer Variablen innerhalb der Unit hat keinen Einfluss. | `variable_page` → `variable_section` → `variable_level`, jeweils aufsteigend. |
| **9036** | Wie 9001, auch mit `TRUE`. | Wie 9001. |
| **Neu 9040** | Seite → Abschnitt (beide numerisch) → natürlicher Variablenname. Ableitungen mit vollständig zugeordneten Quellen folgen direkt auf ihre letzte Quelle. | Dieselbe Reihenfolge wie für die Missing-Einstufung. |

- **9001/9036:** Vorhandene Datentypen bestimmen die Sortierung; `NA`
  kommt zuletzt. Bei Gleichstand bleibt die Reihenfolge nach dem
  Zusammenführen erhalten.
- **Neu:** Natürliche Namen bedeuten etwa `V2` vor `V10`. Fehlende
  Abschnitte folgen bekannten Abschnitten derselben Seite. Variablen
  ohne Seite kommen zuletzt, untereinander nach Namen.
- **Ableitungen:** Die Quellen bestimmen die Position, auch bei
  mehrstufigen Ableitungen. Gleichzeitig mögliche Ableitungen folgen
  ihren Namen; dadurch neu mögliche Ableitungen werden sofort eingefügt.
  Bei fehlenden, mehrdeutigen oder zyklischen Quellen gelten eigene
  Seite, Abschnitt und Name.
- **Alle drei:** Die Itemliste (`items_list$item_no`) ist kein
  Sortierkriterium (siehe [Fall 14](#beispiel-14)).

Die Tabellen behalten zum Vergleichen dieselbe Beispielreihenfolge bei.
Ab Fall 14 steht die tatsächliche Ausgabereihenfolge zusätzlich darüber.

**Ableitungen werden nicht neu berechnet.** Ein gültiges
Ableitungsergebnis zählt als Bearbeitung, wenn es zu einem Item oder
dessen Quellen gehört (oder die Ersatzregel für die Unit gilt). Davor
liegende NR-Quellen werden mit **NR behalten: FALSE** zu Auslassungen
mit 0 Punkten. Mit **TRUE** bleiben sie NR.

## 1. Auslassung vor und nach der letzten Antwort

[TABLE]

Fall 1: Versionsvergleich {.table .missing-comparison}

U1/V02 liegt vor einer späteren Antwort und bleibt neu auch mit O → NR:
TRUE eine Auslassung. Die 9036 wertet U1/V02 damit fälschlich als nicht
erreicht. Von den beiden Auslassungen liegt nur U1/V04 hinter der
letzten Antwort. NR behalten ändert hier nichts.

## 2. Nicht erreicht kodiert, danach Antwort in derselben Unit

[TABLE]

Fall 2: Versionsvergleich {.table .missing-comparison}

Neu wird V01 wegen der späteren Antwort auf V02 zur Auslassung, solange
NR behalten: FALSE gilt. Mit NR behalten: TRUE bleibt V01 nicht
erreicht, ebenso wie in den älteren Versionen. Die 9036 behandelt NR
damit innerhalb einer Unit anders als über Unitgrenzen hinweg (Fall 3).
V03 bleibt überall nicht erreicht.

## 3. Nicht erreicht kodiert, danach Antwort in einer anderen Unit

[TABLE]

Fall 3: Versionsvergleich {.table .missing-comparison}

Die Antwort auf U2/V01 liegt in einer späteren Unit desselben Testlets.
Deshalb wird U1/V03 zur Auslassung. Nur neu mit NR behalten: TRUE bleibt
U1/V03 nicht erreicht.

## 4. Nachfolgende Units enthalten nur Auslassungen

[TABLE]

Fall 4: Versionsvergleich {.table .missing-comparison}

Mit O → NR: FALSE wird U1/V02 wegen der späteren Auslassungen selbst zur
Auslassung. Neu verhindert NR behalten: TRUE dies. Mit O → NR: TRUE
werden alle Auslassungen als nicht erreicht gewertet; U1/V02 bleibt dann
ebenfalls nicht erreicht.

## 5. Das gesamte Testlet enthält nur Auslassungen

[TABLE]

Fall 5: Versionsvergleich {.table .missing-comparison}

O → NR: FALSE lässt alle Auslassungen bei 0 Punkten. O → NR: TRUE macht
alle zu nicht erreicht – auch ohne eine einzige beantwortete Variable.
NR behalten ändert hier nichts.

## 6. Spätere Antwort im nächsten Testlet

[TABLE]

Fall 6: Versionsvergleich {.table .missing-comparison}

Die Antwort in T2 hat keinen Einfluss auf T1. Mit O → NR: TRUE werden in
9036 und neu die abschließenden Auslassungen jedes Testlets als nicht
erreicht gewertet. NR behalten ändert hier nichts.

## 7. Einzelne Antwortzeilen fehlen

[TABLE]

Fall 7: Versionsvergleich {.table .missing-comparison}

U1/V01 wird als Auslassung ergänzt, U2/V02 und U3/V01 als nicht
erreicht. Neu mit NR behalten: TRUE bleibt auch die ergänzte Zeile
U1/V01 nicht erreicht. Nur 9036 füllt dabei `code_status`; neu bleibt
dieser bei `NA`.

## 8. Alle Antwortzeilen fehlen

[TABLE]

Fall 8: Versionsvergleich {.table .missing-comparison}

Alle Zeilen werden als nicht erreicht ergänzt; `id_used = FALSE`. Nur
9036 setzt `code_status = NOT_REACHED`, die anderen erhalten `NA`.

## 9. Ungültige Antwort und Kodierfehler

[TABLE]

Fall 9: Versionsvergleich {.table .missing-comparison}

Die ungültige Antwort auf U2/V01 und der Kodierfehler bei U2/V02 machen
U1/V03 zur Auslassung. Neu mit NR behalten: TRUE bleibt U1/V03 nicht
erreicht. Codes und Scores der ungültigen Antwort und des Kodierfehlers
bleiben überall erhalten.

## 10. NO_CODING im Versionsvergleich

[TABLE]

Fall 10: Versionsvergleich {.table .missing-comparison}

Für `NO_CODING` vergibt 9001 den Code -94, die neueren Versionen -93.
`code_status` und `code_score` bleiben gleich.

## 11. Eigenes Missing-Schema

Eigenes Schema: nicht erreicht erhält **Code -196 / Score 0.5**,
Auslassung **Code -199 / Score 0**.

[TABLE]

Fall 11: Versionsvergleich {.table .missing-comparison}

9001 ignoriert das eigene Schema; 9036 und neu übernehmen es. Nur 9036
ergänzt bei U2/V01 `code_status = CUSTOM_NOT_REACHED`; neu bleibt er
`NA`.

## 12. NOT_REACHED ohne Code, danach Antwort in derselben Unit

[TABLE]

Fall 12: Versionsvergleich {.table .missing-comparison}

Wie Fall 2, aber der Code fehlt noch. Neu wird V01 wegen der späteren
Antwort zur Auslassung. Mit NR behalten: TRUE bleibt V01 nicht erreicht,
ebenso wie früher.

## 13. NOT_REACHED ohne Code, danach Antwort in einer anderen Unit

[TABLE]

Fall 13: Versionsvergleich {.table .missing-comparison}

Wie Fall 3, aber bei den nicht erreichten Variablen fehlt der Code noch.
U1/V03 wird zur Auslassung; neu verhindert NR behalten: TRUE dies. Die
abschließenden U2/V02 und U3/V01 bleiben überall nicht erreicht.

## 14. Seitenfolge und Itemliste widersprechen sich

[TABLE]

Fall 14: Versionsvergleich {.table .missing-comparison}

V01 steht auf Seite 1, die Antwort auf V02 auf Seite 2; die Itemliste
nennt V02 zuerst. Neu wird V01 zur Auslassung, außer mit NR behalten:
TRUE. Früher bleibt es nicht erreicht. Die Itemliste spielt in keiner
Version eine Rolle.

## 15. Abgeleitete Variable mit gültigem Ergebnis

[TABLE]

Fall 15: Versionsvergleich {.table .missing-comparison}

V03 ist aus V01 abgeleitet und bereits gültig kodiert: Dieses Ergebnis
bleibt überall erhalten. Neu steht V03 direkt hinter V01 und vor V02.
V01 wird wegen der späteren Bearbeitung zur Auslassung, außer mit NR
behalten: TRUE.

## 16. Abschnitt vor Variablenname

[TABLE]

Fall 16: Versionsvergleich {.table .missing-comparison}

Auf derselben Seite liegt V02 in Abschnitt 1, die Antwort V01 in
Abschnitt 2 und V03 in Abschnitt 3. Neu wird V02 deshalb zur Auslassung,
obwohl sein Name hinter V01 steht. NR behalten: TRUE verhindert dies.
V03 bleibt nicht erreicht.

## 17. Ableitung direkt nach ihren Quellen

[TABLE]

Fall 17: Versionsvergleich {.table .missing-comparison}

01 entsteht aus 01a und 01b und steht neu direkt hinter beiden. Sein
gültiger Code bleibt erhalten. Mit NR behalten: FALSE werden die
NR-Quellen deshalb Auslassungen mit 0 Punkten; mit TRUE bleiben sie NR.
Das nachfolgende 02 bleibt NR. Es wird keine Ableitung neu berechnet.

## 18. Mehrstufige Ableitung und fehlende Quelle

[TABLE]

Fall 18: Versionsvergleich {.table .missing-comparison}

Die Quellenkette lautet 01a → 01b → 01. Neu folgen die Ableitungen
direkt ihren Quellen. Mit NR behalten: FALSE macht der gültige Code von
01 die Quelle 01a und die Ableitung 01b zu Auslassungen; mit TRUE
bleiben beide NR. Für 03 fehlt die Quelle: Seine eigene Seite 2 bleibt
maßgeblich. Wegen der unauflösbaren benötigten Quelle warnt die neue
Version und verwendet für diese Unit alle Variablen außer NO_CODING zur
Grenzbestimmung. 02 und 03 bleiben NR.

## Welche Variablen bestimmen die MNR-Grenze?

**A wurde ausgelassen, danach kommt B, dann das nicht erreichte Item
C.** A und C sind mit Items verknüpft. B hat einen gültigen Antwortcode,
sofern die Tabelle nichts anderes angibt. Es gilt **O → NR: TRUE / NR
behalten: TRUE**; danach folgt keine Bearbeitung.

| Fall                                          | Ergebnis für A      |
|:----------------------------------------------|:--------------------|
| B ist eine unabhängige Hilfsvariable          | Nicht erreicht / NA |
| B wird für die Berechnung von C benötigt      | Auslassung / 0      |
| B wird für C benötigt, hat aber NO_CODING     | Nicht erreicht / NA |
| Itemmetadaten fehlen: Ersatzregel mit Warnung | Auslassung / 0      |
| Wie Fall 1, zusätzlich unverknüpfte Einträge  | Nicht erreicht / NA |

**B hält A nur dann bei Auslassung, wenn B als Bearbeitung zählt.** Eine
unabhängige Hilfsvariable zählt nicht, eine benötigte Quelle schon. C
bleibt in allen Fällen nicht erreicht; es wird keine Ableitung neu
berechnet.

Unverknüpfte Einträge werden ohne Warnung übersprungen. Fehlen die
Metadaten oder benötigte Quellen, warnt die Funktion und prüft für diese
Unit alle Variablen außer `NO_CODING` auf Bearbeitung. Fehlende
Positionsangaben (`variable_position = NA`) schränken die Prüfung der
Reihenfolge ein.

Der Ablauf mit eigenen Daten steht im
[Standard-Workflow](https://iqb-research.github.io/eatPrepTBA/articles/standard-workflow.md).
