# Ristoranti Francia - Heatmap densità (Sud)

## Cosa hai fatto in questo progetto

1. Hai creato uno script R, `mappa_densita_ristoranti_sud_francia.R`, pronto da aprire ed eseguire in RStudio.
2. Lo script scarica automaticamente i dati necessari (e usa fallback se mancano alcuni file).
3. Genera una mappa di densità (heatmap) dei “ristoranti” nel Sud della Francia e salva gli output in `output/`.
4. Hai aggiunto un file `.gitignore` per escludere cartelle e file generati automaticamente (pacchetti locali, `data/`, `output/`, ecc.).
5. Hai creato questo file `README.md` per documentare progetto e codice.

## Come eseguire lo script in RStudio

1. Apri `mappa_densita_ristoranti_sud_francia.R`.
2. Premi **Source** (o **Run** / **Esegui**).
3. La mappa risultante sarà in:
   - `output/mappa_densita_ristoranti_sud_francia.png`

## Cosa fa il codice (mappa + fallback)

Lo script costruisce una heatmap della densità dei punti (ristoranti) nel bounding box del Sud della Francia:

### 1) Setup pacchetti
- Crea una libreria locale `./.r_libs/` e prova a installare i pacchetti necessari.
- Usa un approccio “robusto”: `sf` è considerato opzionale (spesso difficile su macOS).

### 2) Download dati
- Scarica:
  - un GeoJSON con i confini dei comuni francesi (se `sf` è disponibile)
  - un CSV “fallback” con i dati dei ristoranti
- I file vengono salvati in `data/`.

### 3) Coordinate: se mancano, simula punti
- Se il CSV non contiene colonne riconoscibili per longitudine/latitudine, lo script simula punti nel Sud della Francia mantenendo comunque una logica di densità (cluster casuali + rumore).

### 4) Heatmap (punti più densi = colori più scuri)
- Se `sf` è disponibile:
  - carica i confini dal GeoJSON
  - filtra i punti nei comuni del Sud
  - disegna la heatmap con `ggplot2::stat_density_2d_filled()`
- Se `sf` NON è disponibile:
  - usa un fallback semplificato con `ggplot2::map_data("france")`
  - mantiene comunque la heatmap sulla stessa area geografica (bbox Sud Francia)

### 5) Output
Alla fine salva:
- `output/mappa_densita_ristoranti_sud_francia.png` (PNG della heatmap)
- `output/ristoranti_sud_francia_filtrati.csv` (punti usati dalla visualizzazione)
- `output/confini_sud_francia.geojson` solo se `sf` è disponibile

