# Data guide

`data/processed/components_2019.csv` and `components_2021.csv` contain `pro_com` (municipality code) and four saved rank-normalised macro-area scores: `indice_ambientale`, `indice_socio`, `indice_economico`, `indice_servizi`. Each has 1,506 rows.

`reference_index_YEAR.csv` stores the corresponding thesis final score (`indice_finale`, larger means greater relative fragility). Files were exported without altering values from the final-index workbooks in the author's thesis archive. The saved final-index population is used explicitly; upstream macro-area workbooks contain a different row count and are not silently treated as interchangeable.

`research/INDICI YEAR/` contains the author's processed macro-area workbooks for the autoencoder experiments. `app/dati_finali_clean.rds` is the processed dashboard dataset. These are municipality-level aggregates, not individual health records.

`app/Com2021_g.*` contains the municipal boundary files used by the original app (2021 ISTAT administrative geography). The application filters Lombardy using `COD_REG == 3`. File names, CRS metadata and source attributes are retained.

Raw environmental rasters, landslide geometries, pollution archives, temporary sessions, third-party papers and draft documents are intentionally not bundled. They are not required for the final aggregation or the packaged app. Rebuilding the full upstream pipeline requires recovering the exact original input releases, their licenses and geographic concordances.

Seven final scores per year are missing in the original outputs. Their missingness is preserved and verified; no imputation is applied by the quick start.
