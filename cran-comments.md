## Submission summary

New submission of SeMiLLa (version 2.11.0). The package drafts psychometric
scale items with Large Language Models and audits them before any data are
collected: semantic separability of the items by ensemble clustering of text
embeddings, a simulation-based pre-administration gate, and content validity
rated by simulated LLM judges (Aiken's V).

## Test environments

* Local: Windows 11 Pro, R 4.4.1 (R CMD check --as-cran, with manual)
* PENDIENTE: win-builder R-devel (no enviado todavia)

## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: "New submission" - expected for a first-time submission.
* NOTE: "unable to verify current time" - local environment only.

## Notes for the reviewers

* Functions that call a language model or an embeddings service need an API
  key and a Python installation with the 'openai' module (declared in
  SystemRequirements). Their examples are wrapped in \dontrun{} for that
  reason only. Every function that does not call an external service has a
  runnable example, built on the included dataset 'semilla_demo' (synthetic
  embeddings, no API needed).
* No function writes to the user's home or working directory by default; the
  cache uses tools::R_user_dir(). No function sets a fixed random seed;
  'seed' arguments default to NULL. Simulations use at most 2 cores when
  _R_CHECK_LIMIT_CORES_ is set.
* Possibly misspelled words in DESCRIPTION, if flagged: "Javalagi" and
  "Giacobbi" (author surnames in the cited references), "LLMs" and "LLM"
  (Large Language Models).

## Downstream dependencies

There are currently no downstream dependencies.
