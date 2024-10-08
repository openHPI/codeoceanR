# CodeOcean: automatisch bewertete Übungen für R

[Berry Boessenkool](mailto:berry.boessenkool@hpi.de), 2020-2022


### **Setup** _(nur einmal)_

- Erstelle einen Ordner für alle Übungen
- Führe den folgenden Code (in R / Rstudio) aus, um das Paket `codeoceanR` zu installieren:

```r
install.packages("remotes") # ignoriere die Rtools Warnung
remotes::install_github("openHPI/codeoceanR")
```

Für mögliche Probleme (und Lösungen) bei der Installation, siehe [unten](#probleme).

### **Vorgehensweise** _(für jede Übung)_

Übung vorbereiten:

- Gehe über OpenHPI zur CodeOcean-Übung
- **Herunterladen** (Abwärtspfeil links im Dateifenster) der gesamten Aufgabe in den o.g Ordner auf dem PC, kein Entpacken erforderlich
- **Schließe** den CodeOcean-Browser **Tab**
- **Führe aus** (in R / Rstudio Konsole) `codeoceanR::rt_create()`
- **Bestätige**, die Registerkarte geschlossen zu haben
- **Wähle** die Übungsdatei aus. _Wenn entpackt (Standard unter Mac OS Safari), irgendeine Datei innerhalb des Aufgabenordners._

Verwende `rt_create` nur einmal pro Aufgabe. Starte das Aufgabenprojekt später durch öffnen der `zz_*.Rproj` Datei im Dateibrowser.

Lade mehrere Zip-Dateien runter (ohne entpacken!) und nutze `codeoceanR::rt_create_all` um alle auf einmal zu verarbeiten.


Übung bearbeiten:

- Löse Aufgabe 1
- Speichere und führe das Skript mit `CTRL + SHIFT + S` aus, dadurch wird `codeoceanR::rt_score()` ausgeführt
- Die _T1: Meldung_ (für Test 1) entspricht immer der Aufgabennummer.
- Löse Aufgabe 2
- STRG + SHIFT + S
- ...

`codeoceanR::rt_score()` überträgt deinen Code zur Bewertung an CodeOcean. Führe dies häufig aus.  


### Design

`rt_create()` sollte

- einen neuen Ordner mit einer `.Rproj`-Datei erstellen
- das Projekt in Rstudio öffnen
- mit den bereits angezeigten `Skript_n.R` Dateien
- so, dass `rt_score()` sofort funktioniert.

Verwende `rt_create(deletezip=FALSE)`, um die Zip-Datei nicht zu löschen, wenn die Aufgabenerstellung erfolgreich war.  


### codeOcean im Browser

**Nicht mehrere Tabs einer Übung öffnen**. Die automatische Speicherung könnte alles überschreiben, was du in einem zweiten (unbenutzten=leeren) Tab gemacht hast.  
Aus dem gleichen Grund  **nicht den Zurück-Button des Browsers benutzen**.  
Dank der automatischen Speicherung kannst du die Registerkarte aktualisieren oder später wieder öffnen, um die Arbeit an den Aufgaben fortzusetzen.  
Denke daran, immer von der openHPI-Plattform aus auf die Aufgaben zuzugreifen.

- Klicke auf `AUSFÜHREN`, um das Skript zu starten. Lese die Ausgabe, korrigiere deinen Code, wenn er einen Fehler auslöst. Erneut `AUSFÜHREN`.
- Klicke auf `BEWERTEN`, um zu sehen, wie viele der Tests du bestanden hast. Ändere deine Lösungen, falls nötig. Wiederhole `BEWERTEN`.  
Ignoriere Meldungen für Aufgaben, an denen du noch nicht gearbeitet hast (sie zeigen an, wie viel Arbeit noch übrig ist). Die _T1: Meldung_ (für Test 1) entspricht immer der Aufgabennummer.
- Führe `AUSFÜHREN` und `BEWERTEN` häufig aus, um Fehler leicht zu finden.

Die Aufgaben sind in separate Dateien aufgeteilt. Sie können unabhängig voneinander ausgeführt werden. Die Bewertung erfolgt für alle Dateien auf einmal.  
Ich empfehle, immer die gesamte Aufgabe zu lesen, bevor du mit der Arbeit daran beginnst ;-)

Denke daran, ein Objekt explizit aufzurufen, um seinen Wert in der `AUSFÜHREN`-Ausgabe zu sehen.  
`head(x)` / `str(x)` / `summary(x)` funktionieren wie in Rstudio.  
`plot(x)`-Ausgaben werden nur zwischen `rt_plot1()`/`rt_plot2()` angezeigt.

Wenn `BEWERTEN` dir mitteilt, dass *'Skript_x.R' nicht ausgeführt werden kann*, können nach dem Fehler keine Punkte für Aufgaben (in diesem Skript) vergeben werden.

Wenn in der obersten Zeile `ANZEIGEN` anstelle von `AUSFÜHREN` angezeigt wird, befindest du dich wahrscheinlich in einer Datentextdatei. Klicke einfach auf das gewünschte Skript.R.

Klicke auf den Pfeil oben links (oder *Verstecken* unten rechts), um die Anweisungen auszublenden, damit du mehr Platz für den Code auf dem Bildschirm hast.

Um eine komplette Übung zurückzusetzen, klicke auf die Schaltfläche mit der rückwärts laufenden Uhr im Dateifenster auf der linken Seite.  
Um nur eine einzelne Datei zurückzusetzen, klicke auf die Schaltfläche unten rechts auf dem Bildschirm.  


### Probleme

Gelegentlich gibt es Probleme bei der [Installation](#setup-nur-einmal) des `codeoceanR` Pakets aus github. 
Hier sind einige Lösungen, die dir helfen könnten.

- Auf **Windows**: Wenn Rtools installiert werden soll (das ist optional),  [installiere](https://cran.r-project.org/bin/windows/Rtools) es zB in `C:/Rtools` (Compiler Pfade dürfen keine Leerzeichen haben) und führe in R aus: `cat('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file= "~/.Renviron", append=TRUE)`
- Auf **Linux**: führe zuerst im Terminal (STRG+ALT+T) aus: `sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+` und `sudo apt-get update` und `sudo apt install libcurl4-openssl-dev libssl-dev r-cran-rjson`. Dann in R den Code oben. Wenn das nicht klappt, folge den Anweisungen beim Installieren der einzelnen Abhängigkeiten in R: `install.packages("curl")` und dann mit `"openssl"`, `"httr"` und `"rjson"`.

- Bei "Failed to install ... **Permission denied**": Schreibrechte setzen (rechtsklick auf Ordner - Eigenschaften - Sicherheit - Bearbeiten - Benutzer auswählen - Vollzugriff setzen - OK). Dazu Virenscanner ausschalten oder Rechner neustarten.
- Dauerhaft **benutzerdefinierten Ordner für Pakete** verwenden: in R `cat('R_LIBS_USER="C:/Pfad/zum/library"', file= "~/.Renviron", append=TRUE)`. Sollte nach R Neustart erste Ausgabe von `.libPaths()` sein.
- Bei "**Failed to R CMD build** package": `remotes::install_github("openHPI/codeoceanR", build=FALSE)`
- Falls nur eine **alte Version von R** möglich ist: für rjson `remotes::install_version("rjson", "0.2.20")`
- Wenn du einen **anderen Editor** als Rstudio nutzen willst: verwende unten NICHT `rt_create()`, sondern entpacke den ZIP Ordner manuell.
- Bei `rt_create()` Fehlermeldung "Could not load the **Qt platform plugin xcb**": VIELLEICHT!! EIGENES RISIKO: `sudo apt remove libxcb-xinerama0` und `sudo apt install libxcb-xinerama0`, siehe [hier](https://open.hpi.de/courses/programmieren-r2022/question/5a424cfa-3a86-4215-9337-9337e52c8277)
