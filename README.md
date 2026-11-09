# Plotwerk

Plotwerk ist ein schneller, nativer Mathe-Rechner für Windows: Grafikrechner, Geometrie, 3D, CAS, Wahrscheinlichkeit, Tabelle & Statistik und ein Prüfungsmodus in einer App. Geschrieben in Rust mit [egui](https://github.com/emilk/egui), ohne Internetverbindung, als einzelne `.exe`.

## Download

Installer und portable Version für **x64** und **ARM64**:

**[Neueste Version herunterladen](https://github.com/Danny6769/plotwerk/releases/latest)**

- `Plotwerk-Setup-<version>-x64.exe`: Installer für normale Windows-PCs
- `Plotwerk-Setup-<version>-arm64.exe`: Installer für Windows auf ARM (z. B. Snapdragon-Laptops)
- `Plotwerk-<version>-<arch>-portable.exe`: ohne Installation direkt starten

Windows SmartScreen kann beim ersten Start warnen, weil der Installer nicht signiert ist: "Weitere Informationen", dann "Trotzdem ausführen".

## Funktionen

**Grafikrechner**
- Funktionen, Gleichungen, implizite Kurven (`x^2 + y^2 = 9`), Ungleichungen als Flächen (`y < sin(x)`)
- Schieberegler, die automatisch für unbekannte Parameter angelegt werden
- Nullstellen, Extrema, Wendepunkte, Schnittpunkte, Tangenten, Integralflächen, Ableitungen (`f'(x)`)
- Stückweise Funktionen mit `Wenn(...)`, eingeschränkter Definitionsbereich mit `Funktion(f, a, b)`
- Wertetabelle

**Geometrie**
- Punkt (auch auf Objekten, ziehbar), Strecke, Gerade, Halbgerade, Vektor, Kreis, Kreisbogen/-sektor, Vieleck
- Schneiden, Mittelpunkt, Senkrechte, Parallele, Winkel
- Spiegeln, Drehen, Verschieben, zentrische Streckung
- Eigenschaften per Rechtsklick: Farbe, Linienstärke, Linienstil, Beschriftung, Umbenennen

**3D-Rechner**
- Flächen `z = f(x, y)`, Ebenen, Kugeln, Punkte, Geraden, Strecken, Vektoren, Raumkurven
- Ansicht mit der Maus drehen und zoomen

**CAS**
- Lösen (exakt, mit Parametern, trigonometrisch allgemein, Ungleichungen, Gleichungssysteme)
- Ableiten, Integrieren (inkl. partieller Integration und Substitution), Faktorisieren, Kürzen, Ausmultiplizieren
- Grenzwerte, Asymptoten, Taylorpolynome, Summen, Polynomdivision, ggT/kgV, Primfaktoren
- Matrizen: Determinante, Inverse, Rang, Treppennormalform, Skalar- und Kreuzprodukt

**Wahrscheinlichkeit**
- Binomial-, Normal-, Poisson-, hypergeometrische, geometrische, Exponential-, Gleich-, t- und Chi-Quadrat-Verteilung
- Diagramm mit markiertem Bereich, Wahrscheinlichkeitstabelle, Umkehrung (Quantile)

**Tabelle & Statistik**
- Tabellenkalkulation, Kennzahlen (Mittelwert, Standardabweichung, Quartile, ...)
- Regression: linear, quadratisch, kubisch, exponentiell, Potenz, logarithmisch, logistisch, Sinus

**Prüfungsmodus**
- Vollbild, Fenster bleibt im Vordergrund, Timer
- Kopfzeile grün, nach Verlassen des Programms dauerhaft rot
- Protokoll mit Uhrzeiten und Prüfcode für die Aufsicht
- Konfiguration "Standard" oder "Ohne CAS"

**Sonstiges**
- Speichern und Öffnen (`.pwk`), Export der Zeichenfläche als PNG
- Bildschirmtastatur (Strg+K), Rückgängig/Wiederholen

## Selbst bauen

```bash
cargo run --release
```

Tests des Mathe-Kerns:

```bash
cargo test -p plotwerk-math --release
```

## Hinweis

Plotwerk ist ein eigenständiges Projekt und steht in keiner Verbindung zu GeoGebra. Es enthält keinen Code, keine Grafiken und keine Marken von GeoGebra.

## Lizenz

[MIT](LICENSE)
