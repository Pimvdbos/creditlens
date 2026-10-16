# Technical notes

## Development and selection

The source contains the complete analysis and app as ordinary functions. `source("submission/CreditLens.R")` only defines functions. Both the report and app load the same frozen `creditlens_assets.rds`, with model identity `f9bad23a4b316c783968debedb0e47390b91f94f8e9480c69691379c631d0cb2` and release `CL-b2563abe35c5`.

A stratified seed-5 split reserved 6,001 of 30,000 customers for holdout evaluation; 23,999 remained for development. The five procedures shared outer folds in nested 5 × 5 CV. Every training fold learned its preprocessing and any tuning independently. Final tuning used development-only inner folds. The final model was never refitted using holdout outcomes.

The 20 candidates were `AGE`, `LIMIT_BAL`, `PAY_0`, `PAY_2`–`PAY_6`, `BILL_AMT1`–`BILL_AMT6` and `PAY_AMT1`–`PAY_AMT6`. Age remained numeric. Amounts used sign(x) × log(1 + |x| / 1,000), retaining zeros and legitimate negative bill balances. LDA used training scaling and a full-rank design. LASSO used training standardization, alpha = 1 and a 100-value penalty path. The forest used 500 trees; the inner grid crossed mtry 4/8/12 with minimum node size 10/30, ultimately choosing 4 and 30.

LASSO's one-SE rule selected the largest penalty within the minimum estimated tuning loss plus its fold-based standard error (fold-loss SD divided by √5). The saved final penalty is approximately 0.008290814. This differs from selecting the model family: the family rule allowed at most 1% above the best development CV log loss and at most 0.01 below that family's CV AUC, then preferred fewer original inputs. It selected LASSO with 14 inputs and 18 nonzero contrast/numeric terms, plus the intercept.

## Repayment coding

Statuses are nominal categories: −2 = no consumption, −1 = paid in full (UCI's abbreviated “paid duly”), 0 = use of revolving credit, and positive codes indicate months delayed. Codes 3–9 are grouped as `3+`; −1 is the reference. These categories do not form a numerical delay scale. “Revolving credit” does not define an exact minimum-payment amount.

The training-only sparse-category rule pools code 1 into 2 when the training count for 1 is below 50. In the final fit, September keeps 1 distinct; August through April pool 1 and 2. This fixed fitted mapping is used at prediction time. The saved higher-probability profile gives 53.0%, 81.8% and 76.4% for September statuses 1, 2 and 3+, holding other inputs fixed. An unconstrained nominal model need not be monotone; sparse severe-delay categories and correlated histories matter. These are not causal changes or automatically evidence of an implementation error.

## Three different forms of interpretation

- **Permutation importance** measures the selected LASSO procedure's outer-fold prediction-loss increases, using three permutations per original input. Outer-fold fits may retain different variables from the deployed fit.
- **Bootstrap inclusion frequencies** concern backward AIC, with 200 successful procedure replications. They are not LASSO selection-stability estimates. The bootstrap also estimated backward AIC's apparent-AUC optimism.
- **Final coefficients** belong to the complete-development LASSO. The saved readable table includes its intercept, nonzero terms, reference categories and transformations; ordinary unpenalized p-values are not attached to these selected penalized estimates.

Inspect these distinctions directly:

```r
source("submission/CreditLens.R")
inspect_creditlens()
inspect_creditlens("coefficients")
inspect_creditlens("permutation_importance")
inspect_creditlens("bootstrap")
inspect_creditlens("diagnostics")
inspect_creditlens("tuning")
inspect_creditlens("training_provenance")
```

The RDS also contains source/provenance checks, split/fold allocations, the selection lock, CV and holdout predictions, calibration, paired comparisons, model diagnostics, sensitivity results, dependency versions and deployment records. These are supporting records of the original execution; publishing the repository did not independently repeat model training. An earlier sparse-category separation issue and its pre-holdout correction remain documented in the report and `pooling_correction` evidence.

## Performance interpretation

The forest's paired holdout AUC advantage over LASSO is 0.0088495 (95% CI 0.0003131–0.0173858), conditional on the fitted models and this test population. The forest also has lower holdout log loss and Brier score. Keeping the locked LASSO decision preserves the stated development-performance/simplicity trade-off; it is not a claim of best holdout prediction or statistical equivalence.

At 0.50, sensitivity is 31.3%, specificity 95.4% and PPV 65.9%. Moving the threshold changes the confusion counts and ROC operating point, while AUC, log loss and Brier score remain fixed. At threshold 1 nobody is flagged, so PPV is explicitly undefined. Calibration intercept/slope estimates near their ideals do not prove perfect calibration, and all validation is internal.
