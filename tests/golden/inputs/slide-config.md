---
patat:
  theme:
    header: [vividRed]
    strong: [vividCyan]
...

# This is a test

<!--config: margins: {top: 2} -->

This slide has **higher margin** at the top.

# This is a test

This slide has **normal margin** at the top.

<!-- config:
This is not a config, but a speaker comment.  The whitespace matters...
-->

# Different color header

<!--config:
slideNumber: false
theme:
  header: [vividYellow]
  strong: [dullGreen, onVividBlue]
-->

This slide has a **different theme** set.

<!--config:
slideNumber: false
-->

It also has no slide number.

The two config blocks should be merged.
