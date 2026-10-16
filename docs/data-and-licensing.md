# Data source, attribution and rebuilding

Yeh, I. (2009). *Default of Credit Card Clients* [Dataset]. UCI Machine Learning Repository. DOI: [10.24432/C55S3H](https://doi.org/10.24432/C55S3H). UCI distributes the dataset under [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/). See the [UCI dataset page](https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients).

The original local input was **`UCI_Credit_Card.csv`**, supplied through [the uciml Kaggle dataset](https://www.kaggle.com/datasets/uciml/default-of-credit-card-clients-dataset). It has 30,000 rows, 25 columns and 6,636 defaults. The download date was not supplied; the source and provenance were checked during the original analysis on 14 October 2026. Its values were independently compared with the original UCI workbook: **750,000 cells agreed**. No raw values were corrected or deleted. Distinct customer IDs with matching profiles were retained.

Exact CSV SHA-256:

```text
a0f0ab49d6326671d6cd83be5c88dcf18007025fe9a53ecd699119c871176ca1
```

Obtain that CSV from the linked Kaggle source if rebuilding. Do not silently substitute a differently formatted export or another dataset version. The rebuild function checks the supplied source. Its workbook provenance check accesses UCI and needs internet access and `readxl`.

Raw data are not bundled and are unnecessary for the frozen app, report or verification. The RDS retains the fitted model, training-derived preprocessing, results, figures and supporting evidence, including dataset-derived outcomes, predictions and public row identifiers. It contains no account secrets or local account configuration.

## Expensive rebuild

From the repository root, after installing the runtime dependencies plus `MASS`, `ranger`, `jsonlite` and `readxl`:

```r
source("submission/CreditLens.R")
rebuild_analysis(
  data_file = "path/to/UCI_Credit_Card.csv",
  work_dir = "analysis_work"
)
```

This generates ignored intermediate analysis files and repeats nested CV, development fitting, holdout evaluation under the locked rule and the 200-replicate backward-selection bootstrap. Use a fresh work directory when changing analysis code; existing caches are checked.

To prepare a deliberately updated release, use the established verified packaging operation, which creates the RDS consumed by both app and report:

```r
package_release("analysis_work", output_dir = "submission")
creditlens("verify")
creditlens("report")
```

Packaging requires verified current outputs and preserves historical training provenance separately from release/source checksums. Updating a model release also requires checking its app/report/deployment identity. These commands document the rebuild workflow; they were not run to create the portfolio and are not routine CI work. Keep raw data and generated caches out of commits.

## Repayment descriptions and methods

Expanded repayment labels are attributed to [Chen and Ye (2022), section 4.1.1](https://arxiv.org/pdf/2209.10070) and [H2O.ai's interpretability tutorial](https://h2oai.github.io/tutorials/machine-learning-interpretability/). They supplement the abbreviated UCI description without changing the fitted encoding. Model and validation references are cited in the academic report and [README](../README.md).

See [NOTICE.md](../NOTICE.md) for the distinction between MIT-licensed original software/documentation, CC BY 4.0 data-derived content and academic-report prose. No license here claims ownership of UCI data or third-party software.
