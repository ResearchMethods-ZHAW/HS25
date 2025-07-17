
## Abstract

Im Kurs Research Methods verwenden wir seit einigen Jahren RMarkdown um die R Unterlagen für die Studenten bereit zu stellen. 


## Anleitung Datensätze / submodule

Damit die Datensätze nicht öffentlich einsehbar sind, befinden sie sich in einem eignen git-Verzeichnis *innerhalb dieses Ordners*. Dieses verschachteln von zwei git-Verzeichnissen funktioniert über sogenannte *submodules*: Der Ordner `datasets` ist ein *submodule* dieses Repos `HS25`.

### Submodule initialisieren

Beim erstmaligen Klonen der Übungsunterlagen müssen die Inhalte des *submodules* `datasets` mittels folgendem Befehl einmalig geholt werden:



```bash
git submodule update --init --recursive
```

### Submodule workflow


Wenn nun eine Änderung im Ordner `datasets` gemacht wurde, sind diese im HS25-repo sichtbar: Der Ordner `datasets` ist beispielsweise im *git Pane* in RStudio sichtbar, und `git status` zeigt folgende Meldung:

```{.bash}
Changes not staged for commit:
  ...
  (commit or discard the untracked or modified content in submodules)
        modified:   datasets (modified content)
```

Allerdings kann `datasets` nicht ge-staged werden: Man kann das Häckchen bei *Staged* nicht setzen, und `git add datasets` hat kein Effekt. RStudio ist mit submodules überfordert: Entweder man fürt die Operation im Terminal durch oder man benutzt einen *richtigen* Git Client (empfohlen! siehe [diesen Abschnitt in happywithgitr.com](https://happygitwithr.com/git-client.html)). Im Terminal ist die Operation relaitv einfach:

```{.bash} 
# das working directory in den Ordner "datasets" wechseln:
cd datasets

# aktuellen status ermitteln (optional)
git status

# Änderungen stagen
git add myfile.csv   # oder git add .

# Änderungen committen
git commit -m  "changed column names"

# Änderungen pushen (nach github.com/researchmethods-zhaw/datasets)
git push

# das working directory zurück in HS25 wechseln
cd ..

# die neue Version von datasets ins HS25 stagen / committen / pushen
git add datasets
git commit -m "update submodule"
git push



```


### Submodule remote

Kleine sidenote: bei der Einführung von datasets als submoule (HS23) hatte Nils der Fehler gemacht, das Einbinden mit dem `ssh` URL zu machen. Die anderen Kollegen arbeiteten arber mit `https`, was wir auch empfehlen (analog der [kurzen Einleitung in happywithgitr](https://happygitwithr.com/connect-intro). Die Wahl trifft man beim Klick auf den grünen "Code" Button auf GitHub, siehe screenshot unten:

![](images/https-ssh.png)

- `ssh` URL: git@github.com:ResearchMethods-ZHAW/datasets.git
- `https` URL: https://github.com/ResearchMethods-ZHAW/datasets.git 


Weil nils aber `ssh` verwendet, hat er mit folgender Configuration sichergestellt, dass das submodule ebenfalls via `ssh` getracked wird.

```
git config --global url.git@github.com:.insteadOf "https://github.com/"  
```



## Allgemeine Anleitungen



Seit HS2020 arbeiten wir mit Quarto, der Nachfolger von Rmarkdown. Am besten Nils und Dominik machen mit euch eine kleine Einführung dazu. Die Slides sind hier zu finden: <https://researchmethods-zhaw.github.io/Intro-for-Authors/>


- [happygitwithr: Install or upgrade R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
- [happygitwithr: Install Git](https://happygitwithr.com/install-git.html)
- RStudio Konfigurieren: Wir empfehlen folgende Konfiguration in RStudio (Tools → `Global Options`):
  - R Markdown
      - Show document outline by default: checked *(Stellt ein Inhaltsverzeichnis rechts von .Qmd files dar)*
      - Soft-wrap R Markdown files: checken *(macht autmatische Zeilenumbrüche bei .Qmd files)*
      - Show in document outline: Sections Only *(zeigt nur "Sections" im Inhaltsverzeichnis)*
      - Show output preview in: Window *(beim kopilieren von Qmd Files wird im Anschluss ein Popup mit dem Resultat dargestellt)*
      - Show equation an image previews: In a popup
      - Evaluate chunks in directory: Document
  - Code \> Tab "Saving"
      - Default Text Encoding: UTF-8
- Git konfigurieren
  - [happygitwithr: Introduce yourself to Git](https://happygitwithr.com/hello-git.html)
  - [happygitwithr: Cache credentials for HTTPS](https://happygitwithr.com/https-pat)
- Repo Klonen
  - [happygitwithr: New RStudio Project via RStudio IDE](https://happygitwithr.com/new-github-first#rstudio-ide)
  - Je nach gewählter "Clone"-Methode, muss das Git*Hub* Repo als *upstream* gesetzt werden: `git branch -u origin/main`

Inhalte Editieren und veröffentlichen


### Welches ist die Working Directory?

Alle Pfade im Dokument sind relativ zum Project zu verstehen: **Das Working directory ist der Project folder!!**.

### Wie bekomme ich eine Vorschau der Unterlagen?

Statt auf den Preview button in RStudio zu clicken empfehlen wir, quarto von der Konsole (Terminal) aus zu bedienen. `quarto render` kompiliert das jeweilige File / Projekt in html (oder pdf). Sehr praktisch ist aber `quarto preview`, welches zusätzlich zum rendern erstellt einen "Webserver" zur Verfügung stellt, wo Änderungen an den qmd Files detektiert und live ge-updated werden.

Hinweis: Auf gewissen Windows Versionen muss man den Befehl `quarto` mit `quarto.cmd` oder `quarto.exe` ersetzen. Versuche es zuerst mit quarto, wenn das nicht klappt versuche die erwähnten Varianten (siehe [hier](https://community.rstudio.com/t/bash-quarto-command-not-found/144187/2)).

### Änderungen veröffentlichen

Hier müssen wir unterscheiden zwischen den Änderungen den Source Files (Qmd) und Änderungen an den Output Files (pdf)

Um die Änderungen an den Source Files zu veröffentlichen müsst ihr diese via git auf das Repository "pushen". Vorher aber müsst ihr die Änderungen `stage`-en und `commit`-en. Ich empfehle, dass ihr zumindest zu beginn mit dem RStudio "Git" Fenster arbeitet.

-   `stage`: Setzen eines Häckchens bei "Staged" (im Terminal mit `git add .`)
-   `commit`: Klick auf den Button "commit" (im Terminal mit `git commit -m "deine message"`)
-   `pull`: Klick auf den Button "Pull" (im Terminal mit `git pull`)
-   `push`: Click auf den button "Push" (im Terminal mit `git push`)
-   

Um Änderungen an den Output Files zu Veröffentlichen muss folgender Befehl ausgeführt werden:

```sh
quarto publish gh-pages --no-prompt 
```

## FAQ

### Was tun bei folgendem Fehler `ERROR: SyntaxError: Unexpected token < in JSON at position 2`

Nach ausführen von `quarto preview` erhalte ich den obigen Fehler. Der output im Terminal sieht folgendermassen aus:

```
[54/59] fallstudie_n/1_Vorbemerkung.qmd
[55/59] fallstudie_n/2_Datenverarbeitung_Uebung.qmd
[56/59] fallstudie_n/2_Datenverarbeitung_Loesung.qmd
ERROR: SyntaxError: Unexpected token < in JSON at position 2
``` 

Ich kann den Fehler beheben, indem ich `quarto render das-letzte-qmd-file-vor-der-fehlermeldung.qmd` ausführe. In dem obigen Fall also:

```sh
quarto render fallstudie_n/2_Datenverarbeitung_Loesung.qmd
```


### Was tun bei folgendem Fehler: `error: Your local changes to the following files would be overwritten by merge:`

Bei einem `git pull` kann es zu folgender Fehlermeldung kommen.

```
error: Your local changes to the following files would be overwritten by merge:
        prepro/Prepro1_Demo.qmd
        ...
Please commit your changes or stash them before you merge.
Aborting
```

Dies bedeutet, dass das genannte File (bzw. die genannten Files) auf Github in Konfikt stehehen könnten mit dem File (bzw. den Files) bei dir auf der Festplatte. Um allfällige Konflikte zu lösen, müssen die lokalen Änderungen zuerst ge`stage`ed und dann `commit`ed werden. Anschliessend können die Änderungen von Github heruntergeladen werden.

```
git add "*.qmd"                # staged alle Files mit der Endung .qmd
git commit -m "meine message" 
git pull                       # lädt die Änderungen von Github herunter
```

### Merge Conflict in _freeze Ordner

Merge conflicts im _freeze Ordner verursachen Arbeit, obwohl diese eigentlich nicht behoben werden müssen (es handelt sich ja schliesslich ledliglich um output / target-Files). Deshalb haben wir dem Repo eine neue Datei namens .gitattributes hinzugefügt, mit folgendem Inhalt:

`_freeze/ merge=ours`

Dies soll bewirken, dass bei allen Files im Ordner _freeze bei merge conflicts die jeweils eigene (ours) gegenüber der incoming (theirs) Version gültig ist.

### Wie löse ich einen merge-conflict?

TBD
