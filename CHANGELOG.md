# Changelog

## 0.12.0.0 (unreleased)

 *  Render tabs in code blocks by expanding them to spaces.  The amount of
    spaces a tab character aligns to is customizable using `tabStop`, e.g.
    `tabStop: 8`.  The default is 4.

 *  Rename eval.wrap to eval.container (#167)

    `wrap` is used at the top-level of settings for wrapping at a certain
    column, and inside `eval` to determine the type in which the result
    is "wrapped". Using the same name for both is confusing, so this adds
    `eval.container` as the new name for `eval.wrap`. `eval.wrap` will continue
    to be supported for the forseeable future, but its use will be discouraged.

    This also changes the values (again keeping the original ones for
    backwards-compat), so the complete changes to a configuration would be:

     -   `wrap: code` becomes `container: code`
     -   `wrap: raw` becomes `container: none`
     -   `wrap: rawInline` becomes `container: inline`

 *  Add a `type: matrix` transition effect, loosely inspired by the 1999 science
    fiction movie.

## 0.11.0.0 (2024-02-14)

 *  Support wrapping at a specific column (#164)

    Using a specific wrap column, e.g. `wrap: 60`, works well together with
    `auto` margins (see below).

 *  Support centering content with auto margins (#164)

    Configuration is done through the existing `margins` setting.

    To vertically center content, use `top: auto`. To horizontally center
    content, use both `left: auto` and `right: auto`.  For example:

    ```markdown
    ---
    title: Centered presentation
    author: John Doe
    patat:
        margins:
            left: auto
            right: auto
            top: auto
    ...

    Hello world
    ```

    Setting `wrap: true` is recommended when vertically centering content if
    there are any lines that are too wide for the terminal.

## 0.10.2.0 (2023-11-25)

 *  Add eval.wrap option

    This adds a new `wrap` section to the `eval` configuration.

    By default, the output is wrapped in a code block again with the original syntax
    highlighting.  You can customize this behaviour by setting `wrap` to:

     *  `code`: the default setting.
     *  `raw`: no formatting applied.
     *  `rawInline`: no formatting applied and no trailing newline.

    You can use `rawInline` to draw graphics.  In order to do that, for example,
    we could configure `kitten` code snippets to evaluate using [Kitty]'s
    command `icat`.  This uses the `rawInline` code setting to ensure that the
    resulting output is not wrapped in a code block, and the `fragment` and
    `replace` settings immediately replace the snippet:

        ---
        patat:
          eval:
            kitten:
              command: sed 's/^/kitten /' | bash
              replace: true
              fragment: false
              wrap: rawInline
        ...

        See, for example:

        ```kitten
        icat --align left dank-meme.jpg
        ```

[Kitty]: https://sw.kovidgoyal.net/kitty/

## 0.10.1.1 (2023-10-18)

 *  Fix issues in text wrapping when starting a transition

    This could show transitions using different wrapping or dropped characters
    when a line extends past the terminal width.

## 0.10.1.0 (2023-10-15)

 *  Add dissolve transition effect (#150)

 *  Add random transitions (#151)

    Set transition `type` to `random` to randomly sample transition effects

## 0.10.0.0 (2023-10-12)

 *  Add transition effects (#149)

    This adds a framework for setting transition effects in between slides. Only
    a single transition type is implemented at this point, `slideLeft`.

    Example configuration:

        patat:
          transition:
            type: slideLeft
            frames: 24  # Optional
            duration: 1  # Seconds, optional


 *  Allow overriding certain settings in slides (#148)

    Configuration was typically done in the metadata block of the input file,
    or in a per-user configuration.  These settings are applied to the entire
    presentation.

    We now allow selectively overriding these settings on a per-slide basis,
    by adding one or more config blocks to those slides.  Config blocks are
    comments that start with `config:`.  They can be placed anywhere in the
    slide.

        # This is a normal slide

        Normal slide content

        # This slide has a different colour header

        <!--config:
        theme:
          header: [vividYellow]
        -->

        Wow, how did that happen?

 *  Allow configuring top margin (#147)

## 0.9.2.0 (2023-09-26)

 *  Read configuration from XDG standard directory (#146)

    The per-user patat configuration file was `$HOME/.patat.yaml`,
    which does not follow the XDG standard.  We now support
    `$XDG_CONFIG_DIRECTORY/patat/config.yaml` (typically `$XDG_CONFIG_DIRECTORY`
    is set to `$HOME/.config`) which is compliant with the standard.

    Note that `$HOME/.patat.yaml` is still supported for backward-compatibility,
    but anything in `$XDG_CONFIG_DIRECTORY` takes precedence.

 *  Support filenames in bash completion (#145) (#126)

## 0.9.1.0 (2023-09-25)

 *  Fall back to forcing UTF-8 if decoding fails (#144) (#127)

    When we try to read a file that is encoded in UTF-8, and the system locale
    is not set to UTF-8, the GHC runtime system will throw an error.

    While this typically indicates that the user should update their system
    locale using e.g. the `LANG` environment variable, we want to provide a good
    initial experience for people unfamiliar with this, and in 2023 it's
    reasonable to assume files may be encoded in UTF-8.

 *  Dependency updates:
     -  Bump `skylighting` upper bound to 0.15 (#143)

## 0.9.0.0 (2023-09-13)

 *  Add proper support for speaker notes (#142)

    You can configure `patat` to write the speaker notes for the current slide
    to a file whenever the slide changes:

        patat:
          speakerNotes:
            file: /tmp/notes.txt

    Then, you can display these in a second terminal (presumably on a second
    monitor) by just displaying this file whenever it changes.  [entr] is one
    way to do that:

        echo /tmp/notes.txt | entr -s 'clear; cat /tmp/notes.txt'

    [entr]: http://eradman.com/entrproject/

    Alternatively, just use a second `patat` instance with `--watch` enabled:

        patat -w /tmp/notes.txt

 *  Add support for showing plain text files (#141)

    This isn't super useful on its own, it's meant to support speaker notes.

 *  Add syntaxDefinitions to settings (#140)

    This allows users to add custom kate highlighting XML files in the settings:

        ---
        patat:
          syntaxDefinitions:
          - 'impurescript.xml'
        ...

        Here is some *im*purescript:

        ```impurescript
        ...
        ```

## 0.8.9.0 (2023-06-27)

* Apply block quote theming to entire block (#119) (#111)
* Fix table header theming (#128)
* Dependency updates:
    - `aeson` to 2.1
    - `optparse-applicative` to 0.18
    - `pandoc` to 3.1
    - `pandoc-types` to 1.23
    - `text` to 2.0
    - `time` to 1.12

## 0.8.8.0 (2022-10-26)

* Allow hiding slide number (contribution by Paweł Dybiec)
* Support additional markdown extensions (contribution by Spreadcat)
* Dependency updates:
    - `aeson` to 2.0
    - `ansi-terminal` to 0.11
    - `base64-bytestring` to 1.2
    - `bytestring` to 0.11
    - `optparse-applicative` to 1.16
    - `pandoc` to 2.19
    - `skylighting` to 0.13

## 0.8.7.0 (2021-03-12)

* Fix alignment and display of CJK characters in presentation title, author
  and tables
* Add support for showing images in Kitty terminal
* Search in `$PATH` for `w3mimgdisplay`
* Bump `pandoc` dependency to 2.11
* Refactor `Patat.Presentation.Display` module to make it pure

## 0.8.6.1 (2020-09-18)

* Fix issue with laziness for evaluted code blocks, they should only be
  evaluated when we actually want to show them
* Bump stack resolver to `lts-16.9`

## 0.8.6.0 (2020-09-11)

* Allow evaluating code blocks (see README for more info)
* Refactor implementation of fragments
* Add breadcrumbs to title based on headers
* Error out when YAML parsing fails

## 0.8.5.0 (2020-06-29)

* Bump `pandoc` dependency to 2.9
* Switch to `goldplate` for testing

## 0.8.4.3 (2020-01-21)

* Fix Haddock syntax in some comments (contribution by Asad Saeeduddin)

## 0.8.4.2 (2020-01-18)

* Add builds for Mac OS
* Refactor CircleCI config & Makefile

## 0.8.4.1 (2019-10-29)

* Bump CircleCI configuration
* Bump release script
* Add slide seeking to `--help` output

## 0.8.4.0 (2019-10-09)

* Add slide seeking (enter slide number + `enter`)
* Fix turning tty echo off/on during presentation
* Run `w3mimgdisplay` cleanup action, fixing image issues on some terminals

## 0.8.3.0 (2019-09-07)

* Fix test failure again, and ensure that it works for multiple pandoc
  versions by slightly modifying test input
* Include pandoc version info in `patat --version`

## 0.8.2.5 (2019-08-12)

* Fix test failure caused by slightly different pandoc output for lists

## 0.8.2.4 (2019-08-12)

* Bump `optparse-applicative` upper bound to 0.16
* Bump `skylighting` upper bound to 0.9

## 0.8.2.3 (2019-06-25)

* Bump upper `pandoc` dependency to 2.8

## 0.8.2.2 (2019-02-04)

* Bump lower `base` dependency to 4.8

## 0.8.2.1 (2019-02-03)

* Bump `pandoc` to 2.6
* Bump `ansi-terminal` to 0.10

## 0.8.2.0 (2019-01-24)

* GHC 7.8 compatibility

## 0.8.1.3 (2019-01-24)

* Bump `pandoc` to 2.4
* Bump `yaml` to 0.11

## 0.8.1.2 (2018-10-29)

* Work around test failure caused by slightly different syntax highlighting
  in different pandoc versions

## 0.8.1.1 (2018-10-26)

* Tickle CircleCI cache

## 0.8.1.0 (2018-10-26)

* Add support for italic ansi code in themes
* Fix centered titles not being centered (contribution by Hamza Haiken)

## 0.8.0.0 (2018-08-31)

* Themed border rendering improvements (contribution by Hamza Haiken)
* Add support for margins (contribution by Hamza Haiken)
* Add RGB colour support for themes (contribution by Hamza Haiken)
* Add experimental images support
* Add images support for iTerm2 (contribution by @2mol)

## 0.7.2.0 (2018-05-08)

* GHC 8.4 compatibility

## 0.7.1.0 (2018-05-08)

* GHC 8.4 compatibility

## 0.7.0.0 (2018-05-04)

* Support HTML-style comments

## 0.6.1.2 (2018-04-30)

* Bump `pandoc` to 2.2

## 0.6.1.1 (2018-04-27)

* Bump `aeson` to 1.3
* Bump `skylighting` to 0.7
* Bump `time` to 1.9
* Bump `ansi-terminal` to 0.8

## 0.6.1.0 (2018-01-28)

* Bump `skylighting` to 0.6
* Bump `pandoc` to 2.1
* Bump `ansi-terminal` to 0.7

## 0.6.0.1 (2017-12-24)

* Automatically upload linux binary to GitHub

## 0.6.0.0 (2017-12-19)

* Make pandoc extensions customizable in the configuration
* Bump `pandoc` to 2.0

## 0.5.2.2 (2017-06-14)

* Add `network-uri` dependency to fix travis build

## 0.5.2.1 (2017-06-14)

* Bump `optparse-applicative-0.14` dependency

## 0.5.2.0 (2017-05-16)

* Add navigation using `PageUp` and `PageDown`.
* Use `skylighting` instead of deprecated `highlighting-kate` for syntax
  highlighting.

## 0.5.1.2 (2017-04-26)

* Make build reproducible even if timezone changes (patch by Félix Sipma)

## 0.5.1.1 (2017-04-23)

* Include `README` in `Extra-source-files` so it gets displayed on Hackage

## 0.5.1.0 (2017-04-23)

* Bump `aeson-1.2` dependency
* Fix vertical alignment of title slides
* Fix wrapping issue with inline code at end of line
* Add bash-completion script generation to Makefile

## 0.5.0.0 (2017-02-06)

* Add a `slideLevel` option & autodetect it.  This changes the way `patat`
  splits slides.  For more information, see the `README` or the `man` page.
  If you just want to get the old behavior back, just add:

        ---
        patat:
          slideLevel: 1
        ...

    To the top of your presentation.

* Clear the screen when finished with the presentation.

## 0.4.7.1 (2017-01-22)

* Bump `directory-1.3` dependency
* Bump `time-1.7` dependency

## 0.4.7.0 (2017-01-20)

* Bump `aeson-1.1` dependency
* Parse YAML for settings using `yaml` instead of pandoc
* Clarify watch & autoAdvance combination in documentation.

## 0.4.6.0 (2016-12-28)

* Redraw the screen on unknown commands to prevent accidental typing from
  showing up.
* Make the cursor invisible during the presentation.
* Move the footer down one more line to gain some screen real estate.

## 0.4.5.0 (2016-12-05)

* Render the date in a locale-independent manner (patch by Daniel
  Shahaf).

## 0.4.4.0 (2016-12-03)

* Force the use of UTF-8 when generating the man page.

## 0.4.3.0 (2016-12-02)

* Use `SOURCE_DATE_EPOCH` if it is present instead of getting the date from
  `git log`.

## 0.4.2.0 (2016-12-01)

* Fix issues with man page generation on Travis.

## 0.4.1.0 (2016-12-01)

* Fix compatibility with `pandoc-1.18` and `pandoc-1.19`.
* Add a man page.

## 0.4.0.0 (2016-11-15)

* Add configurable auto advancing.
* Support fragmented slides.

## 0.3.3.0 (2016-10-31)

* Add a `--version` flag.
* Add support for `pandoc-1.18` which includes a new `LineBlock` element.

## 0.3.2.0 (2016-10-20)

* Keep running even if errors are encountered during reload.

## 0.3.1.0 (2016-10-18)

* Fix compilation with `lts-6.22`.

## 0.3.0.0 (2016-10-17)

* Add syntax highlighting support.
* Fixed slide clipping after reload.

## 0.2.0.0 (2016-10-13)

* Add theming support.
* Fix links display.
* Add support for wrapping.
* Allow org mode as input format.

## 0.1.0.0 (2016-10-02)

* Upload first version from hotel wifi in Kalaw.
