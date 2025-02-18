# Changelog

## [v0.5][v0.5] - 18 Feb 2025

* Add `###autoload` comments
* Properly toggle (enable/disable) om-dash-mode
* Update docs

[v0.5]: https://github.com/gavv/om-dash/releases/tag/v0.5

## [v0.4][v0.4] - 26 Jan 2025

**Compatibility note**: No breaking changes are expected, however some parameter and variable names are changed and using old names triggers an obsoletion warning.

* Improvements:

   * Add customization group and use `defcustom` instead of `defvar`.
   * Add `:keyword` parameter to all blocks.
   * Use `user-error` instead of `error` when appropriate.

* Renames:

   * Rework IMAP block parameters. See `om-dash-imap-default-server` variable and `:server` parameter of `om-dash-imap`.
   * Rename properties parsed by `om-dash-function` to be consistent with block parameters.
   * Rename `:table-columns` to `:columns`. Now `:columns` is used everywhere consistently.

* Bug-fixes:

   * Fix incorrect behavior of `om-dash-update-tree` and `om-dash-update-subtree` when block contents size changes.
   * Fix keyword selection for `om-dash-imap` block. Use `TODO` if there are unread emails and `DONE` otherwise.
   * Fix highlighting of 2-letter keywords (e.g. `QA`) in `om-dash` mode.
   * Properly report error on IMAP connection failure.
   * Add missing `(require 'netrc)`.

* Documentation:

   * Port README.org to `om-readme`, improve generated docs.
   * Consistent quoting style in docstrings.
   * Minor fixes in documentation.

[v0.4]: https://github.com/gavv/om-dash/releases/tag/v0.4

## [v0.3][v0.3] - 03 Dec 2024

**Compatibility note**: No breaking changes are expected, however some parameter and variable names are changed and using old names triggers an obsoletion warning.

* Implement simple plist-based query language:

    * supported for `om-dash-github` and `om-dash-orgfile`
    * simpler (and more limited) than writing github, jq, and org-ql queries manually (especially jq)
    * example: `(:review-status requested :reviewer "bob")`

* Updates in `om-dash-github` block:

  * rename `pr` to `pullreq`
  * add support for v2 github projects
  * add columns:
    * `:milestone`
    * `:assignee`
    * `:reviewer`
    * `:project`
    * `:project-status`
    * `:classic-project`
    * `:classic-project-status`
    * `:created-at`
    * `:updated-at`
    * `:closed-at`
    * `:merged-at`

* Updates in `om-dash-orgfile` block:

  * add `:query` parameter, deprecate `:todo` and `:done`
  * add `:digest` parameter

* Add `om-dash-imap` block and related settings

    The block displays unread email counters for an IMAP tree.

* Add `om-dash-function` block.

    The block displays table returned by a user-provided Elisp function.

* Templates:

  * deprecate `milestone` template (use `om-dash-github` queries instead)
  * deprecate `project-columns` template (use `om-dash-github` queries instead)

* Configuration:

  * new setting `om-dash-table-time-format`
  * face `om-dash-author` renamed to `om-dash-username`

* Bug-fixes:

  * fix error in `om-dash-table-squeeze-empty` handling

[v0.3]: https://github.com/gavv/om-dash/releases/tag/v0.3

## [v0.2][v0.2] - 28 May 2024

* Unify variable naming:
  * rename `om-dash-table-width` to `om-dash-table-fixed-width`
  * rename `om-dash-squeeze-empty-columns` to `om-dash-table-squeeze-empty`
  * rename `om-dash-link-style` to `om-dash-table-link-style`
* Documentation fixes

[v0.2]: https://github.com/gavv/om-dash/releases/tag/v0.2

## [v0.1][v0.1] - 22 May 2024

* Initial release

[v0.1]: https://github.com/gavv/om-dash/releases/tag/v0.1
