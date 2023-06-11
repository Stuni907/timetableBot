# Telegram Bot

Dieser TelegramBot liefert dir deine nächsten Lektionen aus dem Stundenplan.

- [Telegram Bot](#telegram-bot)
  - [Installation](#installation)
  - [Projektstruktur](#projektstruktur)
    - [app/calendar](#appcalendar)
    - [src/persist](#srcpersist)
    - [test](#test)
    - [app/BotLogic.hs](#appbotlogichs)
    - [app/Main.hs](#appmainhs)
  - [Entwicklung mit VS Code `.devcontainers`](#entwicklung-mit-vs-code-devcontainers)
    - [Warum wir uns entschieden haben `.devcontainers` zu verwenden](#warum-wir-uns-entschieden-haben-devcontainers-zu-verwenden)
    - [Einrichtung des Entwicklungsbereichs](#einrichtung-des-entwicklungsbereichs)
      - [Voraussetzungen](#voraussetzungen)
      - [Einrichtung](#einrichtung)
      - [Nützliche Tipps](#nützliche-tipps)
  - [SQLite](#sqlite)
    - [Integration von SQLite](#integration-von-sqlite)
  - [Testing](#testing)
    - [Verwendung von hspec](#verwendung-von-hspec)
    - [Fokus auf Unit Tests](#fokus-auf-unit-tests)
    - [Organisation der Testdateien](#organisation-der-testdateien)
    - [Ausführung der Tests](#ausführung-der-tests)


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

### src/persist
Im Verzeichnis "src/persist" findet sich der Code, der für die Speicherung der Benutzerinformationen verantwortlich ist. Dies kann entweder durch das Speichern der Informationen in Dateien oder durch die Verwendung einer Datenbank realisiert werden. Das Verzeichnis enthält die entsprechenden Dateien und Module, um die Speicherung und den Zugriff auf die Benutzerdaten zu ermöglichen. Wir haben den Code für die Persistierung der Daten bewusst in das "src"-Verzeichnis gelegt, um ihn von der Hauptapplikation zu trennen. Dadurch wird der Persistenzcode unabhängig von der spezifischen Logik der Hauptapplikation und kann einfacher in anderen Projekten oder Anwendungen wiederverwendet werden. Durch das Auslagern des Persistenzcodes in ein separates Verzeichnis oder Modul wird die Entwicklung des Codes unabhängiger. Diese Trennung erleichtert auch die Testbarkeit des Codes.

### test
Im Verzeichnis "test" befinden sich die Unit Tests. Im Kapitel "Testing" wird genauer auf diese Tests eingegangen.

### app/BotLogic.hs
Die Datei "app/BotLogic.hs" stellt eine Schnittstelle für den Bot dar. Sie dient als zentrales Modul, das den Austausch von Bots ermöglicht. Durch die Verwendung dieser Schnittstelle ist es möglich, verschiedene Bot-Bibliotheken zu verwenden und zu ersetzen, je nach den Anforderungen des Projekts. Beispielsweise kann die Bot-Bibliothek von Telegram durch eine andere Bibliothek wie Threema ersetzt werden, ohne den restlichen Code des Projekts zu beeinflussen. Das "BotLogic" Modul bietet eine einheitliche Abstraktionsschicht für die Bot-Funktionalitäten und erleichtert so die Wartung und Erweiterung des Projekts.

### app/Main.hs
Die Datei "app/Main.hs" dient als Einstiegspunkt des Projekts. Hier wird der Telegram-Bot gestartet und die Hauptimplementierung erfolgt. Dieses Modul verwendet die Schnittstelle aus "BotLogic.hs", um die spezifischen Bot-Funktionen zu implementieren und die Interaktion mit dem Telegram-Messenger zu ermöglichen. Der Telegram-Bot kann in diesem Modul konfiguriert und gesteuert werden, um auf eingehende Nachrichten und Befehle zu reagieren und entsprechende Aktionen auszuführen.

Durch diese Strukturierung des Projekts in verschiedene Verzeichnisse und Dateien wird eine klare Trennung der Verantwortlichkeiten und eine bessere Wartbarkeit des Codes erreicht. Jedes Verzeichnis und jede Datei hat eine spezifische Aufgabe und erleichtert so die Entwicklung, Erweiterung und Wartung des Projekts.
## Entwicklung mit VS Code `.devcontainers`

### Warum wir uns entschieden haben `.devcontainers` zu verwenden
`.devcontainers` sind ein nützliches Werkzeug für Entwickler:innen, die konsistente und reproduzierbare Entwicklungsumgebungen sicherstellen möchten. Hier sind einige Gründe, warum die Verwendung von `.devcontainers` von Vorteil sein kann:

- Konsistenz: Durch die Verwendung von `.devcontainers` kann sichergestellt werden, dass jeder im Team dieselbe Entwicklungsumgebung nutzt. Dies kann dazu beitragen, Probleme aufgrund von Unterschieden in Abhängigkeiten, Bibliotheken und anderen umgebungsspezifischen Faktoren zu reduzieren.

- Reproduzierbarkeit: Da `.devcontainers` auf Docker-Images basieren, können Entwicklungsumgebungen einfach geteilt oder auf anderen Maschinen reproduziert werden. Dies erleichtert die Zusammenarbeit und unterstützt beim Debuggen von Problemen. 

- Isolierung: `.devcontainers` bieten eine isolierte Umgebung für die Entwicklung, was helfen kann, Konflikte mit anderen Anwendungen oder Systemkonfigurationen zu vermeiden.

### Einrichtung des Entwicklungsbereichs
Zurzeit unterstützt IntelliJ keine `.devcontainers`, aber das IntelliJ-Team arbeitet daran.

#### Voraussetzungen
Bevor Sie beginnen, sollten Sie sicherstellen, dass die folgende Software installiert ist:

- Visual Studio Code
- Docker

#### Einrichtung
- Öffnen Sie Visual Studio Code.
- Verwenden Sie den Befehl "Datei" > "Ordner öffnen" im Menü, um das Projektverzeichnis zu öffnen.
-  Sobald das Projekt geöffnet ist, wird ein Dialogfenster angezeigt, das fragt, ob das Projekt in einem Container neu geöffnet werden soll. Klicken Sie auf "Im Container neu öffnen", um fortzufahren.
- Wenn das Dialogfenster nicht angezeigt wird, können Sie das Projekt trotzdem in einem Container öffnen, indem Sie die Befehls-Palette verwenden. Drücken Sie unter Windows `Strg+Shift+P` oder auf einem Mac `Cmd+Shift+P`, um die Palette zu öffnen. Suchen Sie nach "Remote-Containers: Im Container neu öffnen" und wählen Sie es aus.
- Sobald Sie das Projekt in einem Container geöffnet haben, wird Visual Studio Code das Container-Image erstellen und die Umgebung einrichten. Dieser Vorgang kann je nach Grösse des Images und der Komplexität des Projekts einige Minuten dauern.
#### Nützliche Tipps
- Wenn der Container neu erstellt werden muss, drücken Sie `Strg+Shift+P` unter Windows oder `Cmd+Shift+P` und suchen Sie nach dem Befehl "Dev Containers: Neu erstellen und im Container öffnen".
- Beim ersten Start des Containers wird automatisch ein `cabal update` und ein ?`cabal build` durchgeführt, was einige Minuten in Anspruch nehmen kann.

## SQLite
SQLite ist eine Open-Source-Software, die eine serverlose relationale Datenbank-Engine bereitstellt. Im Gegensatz zu traditionellen Datenbank-Engines wird SQLite direkt in die Anwendung integriert und speichert alle Daten in einer einzigen Datei auf dem lokalen Dateisystem. SQLite ist aufgrund seiner Einfachheit, Portabilität, geringen Grösse und hohen Leistung sehr beliebt und wird häufig in Anwendungen verwendet, die eine lokal gespeicherte Datenbank benötigen. Es unterstützt SQL und bietet eine Vielzahl von Funktionen.
### Integration von SQLite
Im Projekt wurde SQLite als Datenbankmanagementsystem verwendet. Um SQLite in das Projekt zu integrieren, wurde die Bibliothek [sqlite-simple]('https://hackage.haskell.org/package/sqlite-simple') genutzt.

Die sqlite-simple Bibliothek bietet eine effiziente Möglichkeit, mit SQLite-Datenbanken zu arbeiten und sie in Haskell-Anwendungen zu integrieren. Mit der Bibliothek konnte die Erstellung von SQLite-Datenbanken, die Bearbeitung von Tabellen, die Ausführung von Abfragen und vieles mehr erfolgen.

Um den Inhalt der SQLite-Datenbanken zu verwalten, wird die Verwendung der [SQLite-Tools]('https://www.sqlite.org/download.html') empfohlen. Diese Tools bieten eine intuitive Benutzeroberfläche und umfassende Funktionen, um den Inhalt von SQLite-Datenbanken zu bearbeiten. Es konnte mithilfe der SQLite-Tools Abfragen ausgeführt werden, Daten exportiert und Datenbanken gesichert werden.

Insgesamt wurde in dem Projekt die Kombination aus sqlite-simple und SQLite-Tools als leistungsstarke Lösung für die Verwaltung von SQLite-Datenbanken eingesetzt.

## Testing
Unser Haskell-Projekt setzt auf das Test-Framework "hspec", um eine umfassende Testabdeckung sicherzustellen und die Qualität unserer Software zu gewährleisten.
Hier sind einige wichtige Punkte zum Thema Testing in unserem Projekt:

### Verwendung von hspec
- Wir setzen auf das bewährte Test-Framework [hspec](https://hspec.github.io/), um unsere Unit Tests zu schreiben und auszuführen.
- Hspec bietet uns eine intuitive und ausdrucksstarke Syntax, um unsere Tests klar zu strukturieren und verständlich zu halten.

### Fokus auf Unit Tests
- Wir konzentrieren uns auf die Erstellung von Unit Tests, um einzelne Funktionen, Module oder Klassen unserer Anwendung zu überprüfen.
- Durch die Aufteilung unserer Tests auf kleinere Einheiten können wir sicherstellen, dass jede Komponente unseres Codes isoliert getestet wird und die erwarteten Ergebnisse liefert.

### Organisation der Testdateien

- Unsere Testdateien befinden sich im Ordner "Test", wobei die Dateinamen den entsprechenden Dateien im App-Verzeichnis entsprechen.
- Wir verwenden eine einfache Namenskonvention, bei der wir das Wort "Test" am Ende des Dateinamens anfügen.
- Diese strukturierte Organisation ermöglicht uns eine klare Zuordnung zwischen den Tests und dem zugehörigen Code.

### Ausführung der Tests

- Um alle Tests auszuführen, verwenden wir den Befehl:
```bash
cabal test telegramBot-test
```
![Test status](/images/Tests_status.png)