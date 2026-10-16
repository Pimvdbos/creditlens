# Reproducibility and verification

## Canonical release

The only canonical implementation/model in this repository is the four-file [submission directory](../submission/). Its source, assets, Rmd and PDF were copied byte-for-byte from the completed individual submission. No statistical fitting, retuning, recalibration or threshold selection was performed for portfolio publication. The original training manifests remain inside the RDS separately from current release hashes.

Release: `CL-b2563abe35c5`. Selected family: complete-development LASSO. Model SHA-256 identity: `f9bad23a4b316c783968debedb0e47390b91f94f8e9480c69691379c631d0cb2`.

Git attributes preserve the submitted source/Rmd bytes across platforms because release integrity uses their original checksums. The assets supply all report figures and frozen predictions. Nothing in `docs/`, `scripts/` or `.github/` is required by the standalone submission.

## Tested software

The completed release was tested with R 4.5.3 and the following packages. These are recorded versions, not a claim that every other version is incompatible. CI uses R 4.5.3 and installs available compatible CRAN packages; its session information records the versions actually checked.

| Purpose | Packages and tested versions |
|:--|:--|
| Frozen app and verification | shiny 1.14.0; bslib 0.12.0; htmltools 0.5.9; ggplot2 4.0.2; glmnet 5.1; Matrix 1.7.4; pROC 1.19.1; digest 0.6.39; base64enc 0.1.6 |
| Report | rmarkdown 2.31; knitr 1.51; Pandoc 3.10; working LaTeX distribution |
| Analysis rebuilding | MASS 7.3.65; ranger 0.18.0; jsonlite 2.0.0; readxl 1.4.5, in addition to applicable packages above |
| Optional deployment | rsconnect 1.11.2 and an independently configured hosting account |

RStudio or a normal PATH installation can supply Pandoc. Rendering also uses standard LaTeX packages including `booktabs`, `float`, `fvextra` and `fancyhdr`; TinyTeX can manage missing TeX packages. No omitted lockfile or fixed RStudio installation path is required. Dependency installation is explicit and never happens at app startup.

```r
source("submission/CreditLens.R")
inspect_creditlens("dependencies")
inspect_creditlens("deployment")
```

## Automated checks

From the repository root:

```sh
Rscript scripts/verify.R
```

The GitHub Actions workflow runs this lightweight check on pushes and pull requests. It parses and sources the R file without starting Shiny, verifies the exact four-file boundary and checksums, and calls the existing eight-group frozen-release verification. These checks cover the selected model and 14 inputs, split/fold records, family-selection and tuning rules, recomputed saved CV/holdout metrics, 200 saved bootstrap records, three preset probabilities, non-monotone status effects, invalid inputs, zero/negative amounts, threshold extremes, undefined PPV and the readable coefficient table. They inspect saved analysis evidence; they do not rerun CV or bootstrap fitting.

Repository checks also build the Shiny app object, check packaged PNG/PDF signatures and relative documentation links, and confirm that verification does not change submission bytes. CI does **not** render the PDF, operate a browser or redeploy the app. Those checks are performed locally and on the existing public deployment; see [verification results](verification.md).

## Figure regeneration

```sh
Rscript scripts/export-figures.R
```

This creates the holdout performance image from the verified RDS, without raw data or fitting. The screenshot is captured from the public app and is not a mockup.

## Standalone commands

Inside a directory containing only the four submitted files:

```r
source("CreditLens.R")
creditlens("verify")
creditlens("app")
# In a separate R session after stopping the app:
source("CreditLens.R")
creditlens("report")
```

Deploying is optional: `creditlens("deploy")` uses external rsconnect account configuration and generates a temporary clean bundle. The repository does not contain hosting credentials. The existing [public app](https://pimvdb.shinyapps.io/creditlens/) is already deployed; routine exploration requires no hosting account.
