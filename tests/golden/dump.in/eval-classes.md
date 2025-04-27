---
patat:
  eval:
    bash_json:
      command: bash
      syntax: json
      fragment: false
      replace: true
    bash_plain:
      command: bash
      syntax: text
      fragment: false
      replace: true
...

Output as JSON:

~~~{.bash .bash_json}
echo '{"foo": 1}'
~~~

Output as text:

~~~{.bash .bash_plain}
echo '{"foo": 1}'
~~~
