---
patat:
  eval:
    shImplicit:
      command: sh
      replace: true
      fragment: false
    shCode:
      command: sh
      wrap: code
      replace: true
      fragment: false
    shRaw:
      command: sh
      wrap: raw
      replace: true
      fragment: false
    shInline:
      command: sh
      wrap: rawInline
      replace: true
      fragment: false
...

# Implicit eval slide

~~~{.shImplicit}
printf '\e[1;34m%-6s\e[m' "This is text"
~~~

# Code eval slide

~~~{.shCode}
printf '\e[1;34m%-6s\e[m' "This is text"
~~~

# Raw eval slide

~~~{.shRaw}
printf '\e[1;34m%-6s\e[m' "This is text"
~~~

Newline here...

# Raw Inline eval slide

~~~{.shInline}
printf '\e[1;34m%-6s\e[m' "This is text"
~~~

No newline here...
