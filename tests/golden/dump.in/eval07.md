---
patat:
  eval:
    shImplicit:
      command: sh
      replace: true
      fragment: false
    shCode:
      command: sh
      container: code
      replace: true
      fragment: false
    shNone:
      command: sh
      container: none
      replace: true
      fragment: false
    shInline:
      command: sh
      container: inline
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

# None eval slide

~~~{.shNone}
printf '\e[1;34m%-6s\e[m' "This is text"
~~~

Newline here...

# Inline eval slide

~~~{.shInline}
printf '\e[1;34m%-6s\e[m' "This is text"
~~~

No newline here...
