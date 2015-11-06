
Package `regez` is mostly an exercise to support the development of package [`Function`](https://github.com/piccolbo/Function). The goal  of `regez` is to support writing of readable and maintainable regular expressions. A more mature alternative is package [`rex`](https://github.com/kevinushey/rex). In particular, `rex` has a serious set of tests, whereas work on that has just started in `regez`. The main advantage of `regez` over `rex` is that `regez` checks every regular expression that it generates before use. Therefore, when assembling a complex expression from its parts, `regez` will fail early and point to the reason of a failure. With `rex`, you'll find out only when using the fully assembled regex, and it's up to you to break it down and fix it.

For instance

```r
library(rex)
```

```
## Welcome to rex, the friendly regular expression helper!
## Use 'rex_mode()' to toggle code completion for rex shortcuts and functions.
```

```r
rex(regex("abc["), "def")
```

```
## abc[def
```

```r
grep(rex(regex("abc["), "def"), "")
```

```
## Error in grep(rex(regex("abc["), "def"), ""): invalid regular expression 'abc[def', reason 'Missing ']''
```

Instead with `regez`:


```r
library(regez)
```

```
## 
## Attaching package: 'regez'
## 
## The following object is masked from 'package:rex':
## 
##     regex
```

```r
regex(~"abc[" %+% "def")
```

```
## $s
## [1] "abc\\[def"
## 
## $backrefs
## character(0)
## 
## $captured.refs
## character(0)
## 
## attr(,"class")
## [1] "RegEx"
```

Yep, I can't even write an incorrect regex in `regez`. Well, at least, that's the goal, but if you do, it should be detected immediately. Let me try harder


```r
with(
  regez.env,
  {
print(named.capture(anything, "A"))
print(named.ref("B"))

print(named.capture(anything, "A") %+% named.ref("B"))})
```

```
## $s
## [1] "(?<A>(?:.)*)"
## 
## $backrefs
## character(0)
## 
## $captured.refs
## [1] "A"
## 
## attr(,"class")
## [1] "RegEx"
## $s
## [1] "\\g{B}"
## 
## $backrefs
## [1] "B"
## 
## $captured.refs
## character(0)
## 
## attr(,"class")
## [1] "RegEx"
## $s
## [1] "(?<A>(?:.)*)\\g{B}"
## 
## $backrefs
## [1] "B"
## 
## $captured.refs
## [1] "A"
## 
## attr(,"class")
## [1] "RegEx"
```

As you can see, with backreferences bad thing can happen. In particular, two components of a larger expression are both successfully created, but, combined, result in an incorrect regex. This example looks relatively harmless, but there could be any amount of complexity between the capture and the backreference, and the error message is not wrong but not very specific either. We'll try to fix this, but outside named and positional backreferences, this should never happen.

To approximately reproduce the (brilliant, but not standards compliant) `rex` example here, guess what this does:

```
"^(?:((?:[^:])+)://)?((?:!(?::/))+)(?:(:(?:\\d)+))?(?:(/(?:.)*))?$"
```

And now guess what this does:


```r
with(
  regez.env, {
    protocol = at.least.one(none.of(":"))
    domain = at.least.one(not(":/"))
    port =  at.least.one(digit)
    path = anything
    line.begin %+%
      optional(capture(protocol) %+% "://") %+%
      capture(domain) %+%
      optional(":" %+% capture(port)) %+%
      optional("/" %+% capture(path)) %+%
      line.end})
```

```
## $s
## [1] "^(?:((?:[^:])+)://)?((?:(?!(?:.)*:/)(?:.)*)+)(?::((?:\\d)+))?(?:/((?:.)*))?$"
## 
## $backrefs
## character(0)
## 
## $captured.refs
## character(0)
## 
## attr(,"class")
## [1] "RegEx"
```

It is a very liberal regex for URLs and I don't recommend using it in practice. But a  more accurate one is only going to be more complex and is going to benefit even more from using `regez`.
