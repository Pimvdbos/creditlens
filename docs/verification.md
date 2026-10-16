# Verification results

Checked on **16 October 2026** for frozen release **CL-b2563abe35c5**, model **f9bad23a4b31**. These checks reuse the original fitted models and analysis records; model training was not independently repeated for portfolio publication.

| Check | Result |
|:--|:--|
| Frozen-release verification | All eight groups passed: integrity/identity, selection/tuning lock, allocations and saved metrics, saved bootstrap evidence, presets, input validation, threshold behavior and coefficients |
| Public Shiny app | 36 checks passed in fresh anonymous browser contexts, including all three pages, scenario changes, reset, stale-result clearing, invalid age, extrapolation notices, all five performance models and threshold extremes |
| Narrow screens | All three pages checked at 390 px; no document-level horizontal overflow; no browser JavaScript errors |
| Clean four-file directory | Verification and real app startup passed from a new directory outside the original project, with spaces in its path; seven additional browser checks passed against that local app |
| Clean report render | Successful with R 4.5.3, Pandoc 3.10 and LaTeX; all 45 pages had the same extracted text as the submitted PDF |
| PDF inspection | All six narrative pages visually inspected; no clipping or broken tables found; public-app hyperlink present; 39 code-appendix pages follow |
| Publication review | No credentials, email addresses or machine-specific paths found in saved asset character fields; submitted source/report reviewed; only the canonical four files and deliberate portfolio material are included |
| Statistical preservation | All four canonical file SHA-256 hashes match the completed submission; no retraining, retuning, coefficient changes or new release was needed |

The saved examples still predict **9.5%, 15.5% and 53.0%**; lower A versus higher B is **+43.5 percentage points**. The hosted app, RDS and report identify the same model/release. The public deployment was reused, with no duplicate hosted application.

The lightweight [GitHub Actions workflow](../.github/workflows/verify.yml) runs the repository checks on each push and pull request. Its actual outcome and session information are available in [Actions](https://github.com/Pimvdbos/creditlens/actions). PDF rendering, browser interaction and hosting availability are checked separately rather than represented as CI coverage. Internet-hosted services can become unavailable after a successful check.

## Preserved submission checksums

| File | SHA-256 |
|:--|:--|
| `CreditLens.R` | `42e1be343857619210198bc383ef5c6922ad5f1b66d15c9455279219e8fdb51e` |
| `creditlens_assets.rds` | `3dec1ad5c0c68d88d0646c6f11493bbc7b37c8daa1f42cb95891f10802350434` |
| `individual_assignment.Rmd` | `dc4fe562cd15ffcda9edb0d858574ce7e30ce981a7cf41c0581631f45353c53b` |
| `individual_assignment.pdf` | `661b6e2e572afcd08d3c2a8ba21245b4c428f82119ec7adbea7fe38787521a64` |
