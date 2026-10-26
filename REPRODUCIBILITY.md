# Reproducibility

## Runnable entry points

- `python scripts/rebuild_index.py`: final aggregation from saved components, reference comparison for both years.
- `shiny::runApp("app")`: original application with its packaged processed inputs. Requires the R packages listed in README; full interactive behaviour must be checked in a configured R environment.

## Research archive

The selected R scripts preserve methodological variants rather than defining an ordered build system. Set `THESIS_DATA_DIR` to a local copy of the original TESI directory and `THESIS_EXTERNAL_DIR` to its external input parent. Referenced inputs are not all included. Different scripts may overwrite intermediate files; inspect them before running.

Open each autoencoder notebook with its own directory as the working directory. Absolute thesis paths have been replaced with relative paths. Install TensorFlow, scikit-learn, openpyxl, pandas and NumPy in an appropriate environment. Any additional imports in a notebook must also be installed. Training was not rerun during portfolio preparation, and the two notebooks contain different experimental specifications. Their latent coordinates and year-specific scaling are not automatically comparable.

No claim of full raw-data reproducibility, statistical convergence or independent validation is implied by the packaging checks. The original sources remain separate from this portfolio copy.

## Portfolio checks (26 October 2026)

Final aggregation executed successfully for both years: 1,506 municipality rows, including 7 missing final scores per year preserved exactly. Maximum absolute differences against saved outputs: 4.45e-16 (2019), 5.56e-16 (2021). All 15 included R files parse, and both notebook code sources pass Python syntax checks. The interactive Shiny app and autoencoder training were not executed in the preparation environment.
