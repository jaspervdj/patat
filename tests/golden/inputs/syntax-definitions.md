---
patat:
  syntaxDefinitions:
  - 'golden/syntax-definitions/impurescript.xml'
...

Hello

```impurescript
iterate f 0 x = x
iterate f n x = iterate f (n - 1) (f x)
```
