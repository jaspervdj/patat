# Changelog

- 0.4.5.0 (2016-12-05)
    * Render the date in a locale-independent manner (patch by Daniel
      Shahaf).

- 0.4.4.0 (2016-12-03)
    * Force the use of UTF-8 when generating the man page.

- 0.4.3.0 (2016-12-02)
    * Use `SOURCE_DATE_EPOCH` if it is present instead of getting the date from
      `git log`.

- 0.4.2.0 (2016-12-01)
    * Fix issues with man page generation on Travis.

- 0.4.1.0 (2016-12-01)
    * Fix compatibility with `pandoc-1.18` and `pandoc-1.19`.
    * Add a man page.

- 0.4.0.0 (2016-11-15)
    * Add configurable auto advancing.
    * Support fragmented slides.

- 0.3.3.0 (2016-10-31)
    * Add a `--version` flag.
    * Add support for `pandoc-1.18` which includes a new `LineBlock` element.

- 0.3.2.0 (2016-10-20)
    * Keep running even if errors are encountered during reload.

- 0.3.1.0 (2016-10-18)
    * Fix compilation with `lts-6.22`.

- 0.3.0.0 (2016-10-17)
    * Add syntax highlighting support.
    * Fixed slide clipping after reload.

- 0.2.0.0 (2016-10-13)
    * Add theming support.
    * Fix links display.
    * Add support for wrapping.
    * Allow org mode as input format.

- 0.1.0.0 (2016-10-02)
    * Upload first version from hotel wifi in Kalaw.
