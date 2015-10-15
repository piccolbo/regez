errfun = function(e) stop(strsplit(as.character(e), ': ')[[1]][-1])

assert =
  function(x, p) {
    tryCatch(stopifnot(p(x)), error = errfun)
    x}

escape =
  function(s, is.meta)
    paste(map_if(strsplit(s, "")[[1]], is.meta, ~paste0("\\", .)), collapse = "")

add.to.package =
  function(ll)
    map(names(ll), function(n) assign(n, ll[[n]], environment(errfun)))

CharClass =
  function(char.class)
    structure(
      assert.CharClass(char.class),
      class = "CharClass")

as.CharClass = function(x) UseMethod("as.CharClass")
as.CharClass.character = function(x) CharClass(escape.CharClass(x))
as.CharClass.CharClass = identity


assert.CharClass =
  function(char.class)
    assert(char.class, function(cc) is.integer(grep(paste("[", cc, "]"), "", perl = TRUE)))

escape.CharClass = function(s) escape(s, is.meta.CharClass)

is.meta.CharClass = function(c) c %in% c("\\", "-", "[", "]", "^")

predef.CharClasses =
  map(
    c(
      alphanumeric = "[:alnum:]", alphabetic = "[:alpha:]",
      blank = "[:blank:]",    control = "[:cntrl:]",
      digit = "[:digit:]",    graphical = "[:graph:]",
      lower = "[:lower:]",    print = "[:print:]",
      punct = "[:punct:]",    space = "[:space:]",
      upper = "[:upper:]",    xdigit = "[:xdigit:]"),
    CharClass)

add.to.package(predef.CharClasses)

char.range =
  function(start.char, stop.char)
    CharClass(paste0(start.char, "-", stop.char))

RegEx =
  function(regex)
    structure(
      assert.RegEx(regex),
      class = "RegEx")

as.RegEx = function(x) UseMethod("as.RegEx")
as.RegEx.character = function(x) RegEx(escape.RegEx(x))
as.RegEx.RegEx = identity

assert.RegEx =
  function(regex)
    assert(regex, function(rx) is.integer(grep(rx, "", perl = TRUE)))

escape.RegEx = function(s) escape(s, is.meta.RegEx)

is.meta.RegEx =
  function(c) {
    c %in%
      c(".", "\\",  "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")}

concat2  = `%+%`  = function(x, y) UseMethod("concat2")

concat2.character =
  function(x, y) {
    switch(
      class(y),
      character = paste0(x, y),
      CharClass = concat(as.CharClass(x), y),
      RegEx = concat(as.RegEx(x), y))}

concat2.CharClass =
  function(x, y)
    CharClass(paste0(as.CharClass(x), as.CharClass(y)))

concat2.RegEx =
  function(x, y)
    RegEx(paste0(as.RegEx(x), as.RegEx(y)))

concat =
  function(...) {
    args = list(...)
    if(length(args) == 2)
      do.call(concat2, args)
    else
      concat2(args[[1]], do.call(concat, args[-1]))}

escape.seq =
  map(
    c(
      BEL = "a", ESC = "e", FF = "f", CR = "r", TAB = "t",
      word.char = "w",  non.word.char = "W", digit = "d",
      space.char = "s", non.digit = "D",     non.space.char = "S",
      word.edge = "b", not.word.edge = "B"),
    function(x) RegEx(paste0("\\", x)))

add.to.package(escape.seq)

anychar = RegEx(".")
line.begin = RegEx("^")
line.end = RegEx("$")

build.RegEx = function(...) RegEx(paste0(list(...), collapse = ""))


any.of = function(cc) build.RegEx("[", as.CharClass(cc), "]")
none.of = function(cc) build.RegEx("[^", as.CharClass(cc), "]")

enclose = function(rx) build.RegEx("(", as.RegEx(rx), ")")

wildcard = function(rx, ...) build.RegEx(enclose(as.RegEx(rx)), paste0(...))

optional =  function(x)  wildcard(x, "?")
any.number.of = function(x) wildcard(x, "*")
at.least.one = function(x) wildcard(x, "+")
exactly.n = function(x, n) wildcard(x, "{", n, "}")
at.least.n = function(x, n) wildcard(x, "{", n, ",}")
range.of = function(x, n, m) wildcard(x, "{", n, ",", m, "}")

or = `%|%` =
  function(rxl, rxr) build.RegEx(enclose(as.RegEx(rxl)), "|", enclose(as.RegEx(rxr)))
