## Resubmission

This is a resubmission of SeMiLLa (new package; the previous submission was
version 2.12.0). In response to the review by Leonore Hochhauser:

* The Aiken reference in the Description field now has the year in
  parentheses: Aiken (1985) <doi:10.1177/0013164485451012>. All three
  references follow the form authors (year) <doi:...>.
* `options(warn = -1)` was removed from the internal CFA helper; the
  warnings of `lavaan::cfa()` are now silenced with `suppressWarnings()`
  around each call. No function changes `options(warn)` any more.

The version was raised to 2.12.6 because it also includes improvements made
after 2.12.0 (listed in NEWS.md).

## Submission summary

The package drafts psychometric scale items with Large Language Models and
audits them before any data are collected: semantic separability of the
items by ensemble clustering of text embeddings, a simulation-based
pre-administration gate, and content validity rated by simulated LLM judges
(Aiken's V).

## Test environments

* Local: Windows 11 Pro, R 4.4.1 (`R CMD check --as-cran --run-donttest`,
  with tests and PDF/HTML manual)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes for the reviewers

* The documentation is written in Spanish (declared with `Language: es`);
  the Title and Description are in English.
* The three references in the Description were checked in Crossref:
  Voss et al. (2026) <doi:10.1177/00131644261430762>, Aiken (1985)
  <doi:10.1177/0013164485451012> and Penfield and Giacobbi (2004)
  <doi:10.1207/s15327841mpee0804_3>.
* Functions that call a language model or an embeddings service need an API
  key and a Python installation with the 'openai' module (declared in
  SystemRequirements). Their examples are wrapped in `\dontrun{}` for that
  reason only. Every function that does not call an external service has a
  runnable example, built on the included dataset 'semilla_demo' (synthetic
  embeddings, no API needed).
* No function writes to disk unless the user gives a file path; the cache
  uses `tools::R_user_dir()` and is cleared with `limpiar_cache()`. No
  function sets a fixed random seed: 'seed' arguments default to NULL.
  Simulations use at most 2 cores when `_R_CHECK_LIMIT_CORES_` is set.
* Console output from `cat()` is produced only by print methods or when the
  user sets `verbose = TRUE`.
* Possibly misspelled words in DESCRIPTION, if flagged: "Javalagi" and
  "Giacobbi" (author surnames in the cited references), "LLMs" and "LLM"
  (Large Language Models).

## Downstream dependencies

There are currently no downstream dependencies.
