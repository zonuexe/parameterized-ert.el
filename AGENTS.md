# Repository Guidelines

## Project Overview
This repository is an Emacs Lisp package (`parameterized-ert.el`) that adds
parameterized test helpers for ERT. The codebase is small and intentionally
minimal, so keep changes focused and avoid introducing unnecessary
dependencies.

## Project Structure & Module Organization
- `parameterized-ert.el` is the main library source.
- `parameterized-ert-test.el` contains ERT-based unit tests.
- `tests/example.el` shows usage examples and is not part of the test suite.
- `Eask` and `Makefile` define packaging and development tasks.

## Build, Test, and Development Commands
- `make install` installs dependencies via Eask.
- `make compile` byte-compiles the package.
- `make autoloads` generates autoloads for the package file.
- `make lint` runs `checkdoc` and `check-declare`.
- `make test` runs the ERT suite via Eask.

If `make test` fails due to a missing test file, prefer running ERT directly
against `parameterized-ert-test.el` while updating the Makefile as needed.

## Coding Style & Naming Conventions
- Emacs Lisp with `lexical-binding: t` headers.
- Indentation: follow standard Emacs Lisp formatting (2 spaces).
- Internal helpers use a double dash prefix: `parameterized-ert--...`.
- Public APIs use the package prefix: `parameterized-ert-...`.
- Use Emacs Lisp naming style (e.g., `keyword-to-symbol`, not `keyword->symbol`).
- Keep file headers and `provide` forms consistent with Emacs conventions.

## Testing Guidelines
- Tests use ERT (`ert-deftest`) in `parameterized-ert-test.el`.
- Name tests with a `test-` prefix for clarity.
- Example usage lives in `tests/example.el`; do not treat it as a test.

## Commit & Pull Request Guidelines
No commit message conventions are established yet. Use short, descriptive
imperative summaries (e.g., “Add parameter label formatter”). For pull
requests, include a brief description of the change, test commands run, and
any behavior changes or compatibility notes.
