---
patat:
  eval:
    implicitStderr:
      command: sh
      replace: true
      fragment: false
    withStderr:
      command: sh
      replace: true
      fragment: false
      stderr: true
    withoutStderr:
      command: sh
      replace: true
      fragment: false
      stderr: false
...

# Slide

~~~{.implicitStderr}
echo "Hello stdout"
sleep 0.1
echo "Hello stderr" >&2
~~~

~~~{.withStderr}
echo "Hello stdout"
sleep 0.1
echo "Hello stderr" >&2
~~~

~~~{.withoutStderr}
echo "Hello stdout"
sleep 0.1
echo "Hello stderr" >&2
~~~
