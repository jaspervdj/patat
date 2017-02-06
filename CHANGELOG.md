# Changelog

- 0.5.0.0 (2017-02-06)
    * Add a `slideLevel` option & autodetect it.  This changes the way `patat`
      splits slides.  For more information, see the `README` or the `man` page.
      If you just want to get the old behavior back, just add:

            ---
            patat:
              slideLevel: 1
            ...

        To the top of your presentation.

    * Clear the screen when finished with the presentation.

- 0.4.7.1 (2017-01-22)
    * Bump `directory-1.3` dependency
    * Bump `time-1.7` dependency

- 0.4.7.0 (2017-01-20)
    * Bump `aeson-1.1` dependency
    * Parse YAML for settings using `yaml` instead of pandoc
    * Clarify watch & autoAdvance combination in documentation.

- 0.4.6.0 (2016-12-28)
    * Redraw the screen on unknown commands to prevent accidental typing from
      showing up.
    * Make the cursor invisible during the presentation.
    * Move the footer down one more line to gain some screen real estate.

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
