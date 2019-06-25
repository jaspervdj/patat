# Changelog

- 0.8.2.3 (2019-06-25)
    * Bump upper `pandoc` dependency to 2.8

- 0.8.2.2 (2019-02-04)
    * Bump lower `base` dependency to 4.8

- 0.8.2.1 (2019-02-03)
    * Bump `pandoc` to 2.6
    * Bump `ansi-terminal` to 0.10

- 0.8.2.0 (2019-01-24)
    * GHC 7.8 compatibility

- 0.8.1.3 (2019-01-24)
    * Bump `pandoc` to 2.4
    * Bump `yaml` to 0.11

- 0.8.1.2 (2018-10-29)
    * Work around test failure caused by slightly different syntax highlighting
      in different pandoc versions

- 0.8.1.1 (2018-10-26)
    * Tickle CircleCI cache

- 0.8.1.0 (2018-10-26)
    * Add support for italic ansi code in themes
    * Fix centered titles not being centered (contribution by Hamza Haiken)

- 0.8.0.0 (2018-08-31)
    * Themed border rendering improvements (contribution by Hamza Haiken)
    * Add support for margins (contribution by Hamza Haiken)
    * Add RGB colour support for themes (contribution by Hamza Haiken)
    * Add experimental images support
    * Add images support for iTerm2 (contribution by @2mol)

- 0.7.2.0 (2018-05-08)
    * GHC 8.4 compatibility

- 0.7.1.0 (2018-05-08)
    * GHC 8.4 compatibility

- 0.7.0.0 (2018-05-04)
    * Support HTML-style comments

- 0.6.1.2 (2018-04-30)
    * Bump `pandoc` to 2.2

- 0.6.1.1 (2018-04-27)
    * Bump `aeson` to 1.3
    * Bump `skylighting` to 0.7
    * Bump `time` to 1.9
    * Bump `ansi-terminal` to 0.8

- 0.6.1.0 (2018-01-28)
    * Bump `skylighting` to 0.6
    * Bump `pandoc` to 2.1
    * Bump `ansi-terminal` to 0.7

- 0.6.0.1 (2017-12-24)
    * Automatically upload linux binary to GitHub

- 0.6.0.0 (2017-12-19)
    * Make pandoc extensions customizable in the configuration
    * Bump `pandoc` to 2.0

- 0.5.2.2 (2017-06-14)
    * Add `network-uri` dependency to fix travis build

- 0.5.2.1 (2017-06-14)
    * Bump `optparse-applicative-0.14` dependency

- 0.5.2.0 (2017-05-16)
    * Add navigation using `PageUp` and `PageDown`.
    * Use `skylighting` instead of deprecated `highlighting-kate` for syntax
      highlighting.

- 0.5.1.2 (2017-04-26)
    * Make build reproducible even if timezone changes (patch by Félix Sipma)

- 0.5.1.1 (2017-04-23)
    * Include `README` in `Extra-source-files` so it gets displayed on Hackage

- 0.5.1.0 (2017-04-23)
    * Bump `aeson-1.2` dependency
    * Fix vertical alignment of title slides
    * Fix wrapping issue with inline code at end of line
    * Add bash-completion script generation to Makefile

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
