# CLAUDE.md

## Vue d'ensemble

Package R perso regroupant des fonctions utilitaires pour l'analyse descriptive, la visualisation, le formatage de tableaux et l'export de résultats. Utilisé comme dépendance par les projets d'analyse (dont `eds_avc`).

## Architecture

```
R/
  opts.R              Options globales (langue, fonts, stats, CI, couleurs)
  themes.R            Thèmes ggplot2 standardisés (bar, tte, pca, bubble…)

  easy_descr.R        Classification auto des variables + stats descriptives
  easy_fct.R          Recodage, binning, bootstrap, sélection de variables, anonymisation
  easy_helpers.R      Helpers internes (pas d'export)
  easy_out.R          Export unifié ggplot/gt/gtsummary → PNG/SVG/HTML
  easy_view.R         Explorateur interactif de données (reactable)

  gt_format.R         Formatage GT (acronymes, footnotes, thème)
  gt_heatmap.R        Heatmap colorée depuis cross-tab
  gtsum_format.R      Formatage gtsummary (régression, by-group, univarié)
  merge_estim_ci.R    Fusion estimateur + IC en une colonne formatée

  fig_helpers.R       Graphiques de fréquence (ggcount)
  tab_helpers.R       Tests statistiques (t, ANOVA, Kruskal, chi², Fisher) + annotation tableaux
  str_helpers.R       Manipulation de strings (regex union, couleurs HTML, labels)
  qmd_helpers.R       Helpers Quarto (gt_qmd, glue_qmd, include_code_file)
  acro_helpers.R      Gestion d'acronymes (dictionnaire, extraction, formatage)
  auto_exec.R         Exécution batch de scripts R depuis un dossier

  hebstr-package.R    Namespace et imports
  hebstr-globals.R    Variables globales pour R CMD CHECK
```

## Conventions

- Bilingue EN/FR : détection auto via `getOption("OutDec")` (`.` = EN, `,` = FR)
- Toutes les fonctions publiques préfixées par famille : `easy_*`, `gt_*`, `gtsum_*`, `str_*`, `acro_*`, `theme_*`
- Options centralisées dans `set_opts()` → accessibles via `check_opts()`
- Font par défaut : Luciole (avec fallback)
- Export : PNG raster via `webshot2`, SVG/HTML direct, ratio doré `height = width / 1.618`
- Préfixe `_` dans les noms de fichiers = helper interne, pas exporté
- Tests : testthat3, 5 fichiers de tests existants (auto_exec, easy_out, merge_estim_ci, opts, qmd_helpers)

## Patterns importants

- **Tidyeval omniprésent** : `enexpr()`, `exprs()`, `inject()`, `!!`, `!!!` — ne pas simplifier en strings
- **Détection de type d'objet** : `inherits()` pour router ggplot vs gt_tbl vs gtsummary dans `easy_out()` et `gtsum_format()`
- **NSE dans acro_helpers** : utilise `env()` + `map()` avec opérateur `~` pour évaluation programmatique
- **Composition de thèmes** : `%+replace%` et `inject()` dans `themes.R`
- **gtsummary interne** : `.fmt_by()`, `.fmt_uni()`, `.fmt_reg()` dans `gtsum_format.R` — logique complexe, ne pas toucher sans comprendre le pipeline complet

## Commandes

```bash
devtools::load_all()       # Charger le package en dev
devtools::test()           # Lancer les tests
devtools::check()          # R CMD CHECK complet
devtools::document()       # Régénérer NAMESPACE et man/
```
