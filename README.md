# Telegram Bot

Dieser TelegramBot liefert dir deine nächsten Lektionen aus dem Stundenplan.

## Installation
Erstelle im Root-Verzeichnis eine Datei namens `token.env`.

Nun wollen wir unseren Bot bei Telegram anlegen. Dazu schreibst du [@BotFather](https://telegram.me/BotFather) die Nachricht `/newbot` und folgst den Anweisungen.

Du erhältst einen Token, den du in der Datei `token.env` einträgst.

Danach kannst du mithilfe von [cabal](https://www.haskell.org/cabal/) den Bot installieren und starten.

```bash
cabal run
```

## Projektstruktur
Die Projektstruktur gliedert sich in verschiedene Verzeichnisse und Dateien, um die Funktionalitäten des Projekts zu organisieren und zu separieren.

### app/calendar
Der Ordner "app/calendar" beinhaltet alle Dateien, die für das Parsen der .ics-Dateien zuständig sind. Diese Dateien enthalten den Code, der benötigt wird, um Informationen aus den .ics-Dateien zu extrahieren und in einem geeigneten Format darzustellen. Das Parsen der .ics-Dateien ermöglicht es dem System, Veranstaltungen und Termine aus den eingehenden Dateien zu erfassen und zu verarbeiten.

### app/persist
Im Verzeichnis "app/persist" findet sich der Code, der für die Speicherung der Benutzerinformationen verantwortlich ist. Dies kann entweder durch das Speichern der Informationen in Dateien oder durch die Verwendung einer Datenbank realisiert werden. Das Verzeichnis enthält die entsprechenden Dateien und Module, um die Speicherung und den Zugriff auf die Benutzerdaten zu ermöglichen.

### app/BotLogic.hs
Die Datei "app/BotLogic.hs" stellt eine Schnittstelle für den Bot dar. Sie dient als zentrales Modul, das den Austausch von Bots ermöglicht. Durch die Verwendung dieser Schnittstelle ist es möglich, verschiedene Bot-Bibliotheken zu verwenden und zu ersetzen, je nach den Anforderungen des Projekts. Beispielsweise kann die Bot-Bibliothek von Telegram durch eine andere Bibliothek wie Threema ersetzt werden, ohne den restlichen Code des Projekts zu beeinflussen. Das "BotLogic" Modul bietet eine einheitliche Abstraktionsschicht für die Bot-Funktionalitäten und erleichtert so die Wartung und Erweiterung des Projekts.

### app/Main.hs
Die Datei "app/Main.hs" dient als Einstiegspunkt des Projekts. Hier wird der Telegram-Bot gestartet und die Hauptimplementierung erfolgt. Dieses Modul verwendet die Schnittstelle aus "BotLogic.hs", um die spezifischen Bot-Funktionen zu implementieren und die Interaktion mit dem Telegram-Messenger zu ermöglichen. Der Telegram-Bot kann in diesem Modul konfiguriert und gesteuert werden, um auf eingehende Nachrichten und Befehle zu reagieren und entsprechende Aktionen auszuführen.

Durch diese Strukturierung des Projekts in verschiedene Verzeichnisse und Dateien wird eine klare Trennung der Verantwortlichkeiten und eine bessere Wartbarkeit des Codes erreicht. Jedes Verzeichnis und jede Datei hat eine spezifische Aufgabe und erleichtert so die Entwicklung, Erweiterung und Wartung des Projekts.

### Entwicklung mit VS Code `.devcontainers`

#### Warum wir uns entschieden haben `.devcontainers` zu verwenden:
`.devcontainers` sind ein nützliches Werkzeug für Entwickler:innen, die konsistente und reproduzierbare Entwicklungsumgebungen sicherstellen möchten. Hier sind ein paar Gründe, warum du `.devcontainers` verwenden möchtest:

- Konsistenz: Durch die Verwendung eines `.devcontainers` kannst du sicherstellen, dass jeder in deinem Team dieselbe Entwicklungsumgebung verwendet, was helfen kann, Probleme aufgrund von Unterschieden in Abhängigkeiten, Bibliotheken und anderen umgebungsspezifischen Faktoren zu reduzieren.

- Reproduzierbarkeit: Da `.devcontainers` auf Docker-Images basieren, kannst du deine Entwicklungsumgebung einfach mit anderen teilen oder auf einer anderen Maschine reproduzieren, was die Zusammenarbeit erleichtert und beim Debuggen von Problemen hilft.

- Isolierung: `.devcontainers` bieten eine isolierte Umgebung für die Entwicklung, was helfen kann, Konflikte mit anderen Anwendungen oder Systemkonfigurationen zu vermeiden.

- Einfache Einrichtung: `.devcontainers` können mit wenigen Zeilen Konfigurationscode eingerichtet werden, was es einfach macht, mit einem neuen Projekt zu starten oder zwischen verschiedenen Umgebungen zu wechseln.

- Insgesamt sind `.devcontainers` ein leistungsstarkes Werkzeug für Entwickler:innen, die ihren Entwicklungs-Workflow optimieren und konsistente und zuverlässige Ergebnisse sicherstellen möchten.

#### Einrichtung des Entwicklungsbereichs
Zurzeit unterstützt IntelliJ keine `.devcontainers`, aber das IntelliJ-Team arbeitet daran. Andernfalls kannst du es auch lokal entwickeln, sodass du IntelliJ oder eine andere IDE verwenden kannst.

##### Voraussetzungen
Bevor du beginnst, solltest du sicherstellen, dass folgende Software installiert ist:

- Visual Studio Code
- Docker

##### Einrichtung
- Öffne Visual Studio Code.
- Verwende den Befehl "Datei" > "Ordner öffnen" im Menü, um das Projektverzeichnis zu öffnen.
-  Sobald das Projekt geöffnet ist, wird ein Dialogfenster angezeigt, das fragt, ob das Projekt in einem Container neu geöffnet werden soll. Klicke auf "Im Container neu öffnen", um fortzufahren.
- Wenn das Dialogfenster nicht angezeigt wird, kannst du das Projekt trotzdem in einem Container öffnen, indem du die Befehls-Palette verwendest. Drücke `Strg+Shift+P` unter Windows oder `Cmd+Shift+P` auf einem Mac, um die Palette zu öffnen, suche dann nach "Remote-Containers: Im Container neu öffnen" und wähle es aus.
- Sobald du das Projekt in einem Container geöffnet hast, wird Visual Studio Code das Container-Image erstellen und die Umgebung einrichten. Dieser Vorgang kann je nach Grösse des Images und der Komplexität des Projekts einige Minuten dauern.
##### Nützliche Tipps
- Wenn der Container neu erstellt werden muss, drücke `Strg+Shift+P` unter Windows oder `Cmd+Shift+P` und suche nach dem Befehl "Dev Containers: Neu erstellen und im Container öffnen".
- Beim ersten Start des Containers wird automatisch ein `cabal update` und ein ?`cabal build` durchgeführt, was einige Minuten in Anspruch nehmen kann.

## SQLite
SQLite ist eine Open-Source-Software, die eine serverlose relationale Datenbank-Engine bereitstellt. Im Gegensatz zu traditionellen Datenbank-Engines wird SQLite direkt in die Anwendung integriert und speichert alle Daten in einer einzigen Datei auf dem lokalen Dateisystem. SQLite ist aufgrund seiner Einfachheit, Portabilität, geringen Grösse und hohen Leistung sehr beliebt und wird häufig in Anwendungen verwendet, die eine lokal gespeicherte Datenbank benötigen. Es unterstützt SQL und bietet eine Vielzahl von Funktionen.
### Integration von SQLite
Im Projekt wurde SQLite als Datenbankmanagementsystem verwendet. Um SQLite in das Projekt zu integrieren, wurde die Bibliothek [sqlite-simple]('https://hackage.haskell.org/package/sqlite-simple') genutzt.

Die sqlite-simple Bibliothek bietet eine effiziente Möglichkeit, mit SQLite-Datenbanken zu arbeiten und sie in Haskell-Anwendungen zu integrieren. Mit der Bibliothek konnte die Erstellung von SQLite-Datenbanken, die Bearbeitung von Tabellen, die Ausführung von Abfragen und vieles mehr erfolgen.

Um den Inhalt der SQLite-Datenbanken zu verwalten, wird die Verwendung der [SQLite-Tools]('https://www.sqlite.org/download.html') empfohlen. Diese Tools bieten eine intuitive Benutzeroberfläche und umfassende Funktionen, um den Inhalt von SQLite-Datenbanken zu bearbeiten. Es konnte mithilfe der SQLite-Tools Abfragen ausgeführt werden, Daten exportiert und Datenbanken gesichert werden.

Insgesamt wurde in dem Projekt die Kombination aus sqlite-simple und SQLite-Tools als leistungsstarke Lösung für die Verwaltung von SQLite-Datenbanken eingesetzt.