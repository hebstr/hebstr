# Deferred

Findings DEFERRED lors des code reviews. À revisiter périodiquement.

| Date | Finding | Fichier | Raison du report | Échéance |
|------|---------|---------|-----------------|----------|
| 2026-03-26 | `clear_vars()` utilise `assign/rm` sur `.GlobalEnv` — remplacer par un environment dans le namespace du package | `opts.R`, `easy_out.R`, `gt_format.R`, `gtsum_format.R` | Refactoring multi-fichier, hors scope review ponctuelle | — |
| 2026-03-26 | Ajouter tests pour `ci_col` custom, `name` glue, et locale FR | `test-merge_estim_ci.R` | Couverture supplémentaire souhaitable mais pas urgente | — |
