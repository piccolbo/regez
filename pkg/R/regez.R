
errfun = function(e) stop(strsplit(as.character(e), ': ')[[1]][-1])

x = Argument(help = "Any R object")
p = Argument(validate = is.function)

assert =
  Function(x, p, ~{
    tryCatch(stopifnot(p(x)), error = errfun)
    TRUE})

s =
  Argument(
    process = function(x) as.character(x)[1],
    help = "A length-one character vector, or any object that can be coerced to a
  character vector by `as.character`, of which all elments but one will be discarded")

is.meta = Argument(validate = is.function)
escape =
  Function(s, is.meta,
    ~paste(map_if(strsplit(s, "")[[1]], is.meta, ~paste0("\\", .)), collapse = ""))

ll = Argument(validate = is.list)

CharClass =
  Function(s,
    ~structure(
      s,
      class = "CharClass"),
    postcondition = assert.CharClass)

as.CharClass = Function(x, ~as.CharClass_(x))
as.CharClass_ = function(x) UseMethod("as.CharClass_")
as.CharClass_.character = Function(x, ~CharClass(escape.CharClass(x)))
as.CharClass_.CharClass = identity

char.class =
  Argument(
    process = as.CharClass,
    help = "A `CharClass` object, representing a valid character class according
    to the PCRE definition, or an R object coerceable to it")

assert.CharClass =
  Function(
    s,
    ~assert(s, function(cc) is.integer(grep(paste("[", cc, "]"), "", perl = TRUE))))

escape.CharClass = Function(s, ~escape(s, is.meta.CharClass))

one.char =
  Argument(
    validate = function(x) is.character(x) && nchar(x) == 1)

is.meta.CharClass = Function(one.char, ~one.char %in% c("\\", "-", "[", "]", "^"))

predef.CharClasses =
  map(
    c(
      alphanumeric = "[:alnum:]", alphabetic = "[:alpha:]",
      blank = "[:blank:]",    control = "[:cntrl:]",
      digit = "[:digit:]",    graphical = "[:graph:]",
      lower = "[:lower:]",    printable = "[:print:]",
      punct = "[:punct:]",    space = "[:space:]",
      upper = "[:upper:]",    xdigit = "[:xdigit:]"),
    CharClass)

attach(predef.CharClasses)
for(class.name in names(predef.CharClasses))
  export(class.name)

start.char = stop.char = one.char

char.range =
  Function(
    start.char,
    stop.char
    ~CharClass(paste0(start.char, "-", stop.char)))

RegEx =
  Function(
    s,
    ~structure(
      s,
      class = "RegEx"),
    postcondition = assert.RegEx)

as.RegEx = Function(x, ~as.RegEx_(x))
as.RegEx_ = function(x) UseMethod("as.RegEx_")
as.RegEx_.character = Function(x, ~RegEx(escape.RegEx(x)))
as.RegEx_.RegEx = identity

regex =
  Argument(
    process = as.RegEx,
    help = "A `RegEx` object, representing a valid regex according to PCRE or
    an R object coerceable to it")

assert.RegEx =
  Function(
    s,
    ~assert(s, function(rx) is.integer(grep(rx, "", perl = TRUE))))

escape.RegEx = Function(s, ~escape(s, is.meta.RegEx))

is.meta.RegEx =
  Function(one.char,  ~{
    one.char %in%
      c(".", "\\",  "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")})

y = x

conF = partial(Function, x, y)
concat2  = `%+%`  = conF(~concat2_(x, y))
concat2_ = function(x, y) UseMethod("concat2_")

concat2_.character =
  conF( ~{
    switch(
      class(y),
      character = paste0(x, y),
      CharClass = concat(as.CharClass(x), y),
      RegEx = concat(as.RegEx(x), y))})

concat2_.CharClass =
  conF( ~CharClass(paste0(as.CharClass(x), as.CharClass(y))))

concat2_.RegEx =
  conF( ~RegEx(paste0(as.RegEx(x), as.RegEx(y))))

concat =
  Function(dots.., ~{
    args = list(...)
    if(length(args) == 2)
      do.call(concat2, args)
    else
      concat2(args[[1]], do.call(concat, args[-1]))})

escape.seq =
  map(
    c(
      BEL = "a", ESC = "e", FF = "f", CR = "r", TAB = "t",
      word.char = "w",  non.word.char = "W", digit = "d",
      space.char = "s", non.digit = "D",     non.space.char = "S",
      word.edge = "b", not.word.edge = "B"),
    function(x) RegEx(paste0("\\", x)))


anychar = RegEx(".")
line.begin = RegEx("^")
line.end = RegEx("$")
attach(escape.seq)
for(esc.name in names(escape.seq))
  export(esc.name)

build.RegEx = Function(dots.., ~RegEx(paste0(list(...), collapse = "")))

any.of = Function(char.class,  ~build.RegEx("[", char.class, "]"))
none.of = Function(char.class, ~build.RegEx("[^", char.class, "]"))

wildcard = function(rx, ...) build.RegEx(enclose(as.RegEx(rx)), paste0(...))
enclose = Function(regex, ~build.RegEx("(", regex, ")"))


n = m = Argument(process = as.integer)

optional =  function(x)  wildcard(x, "?")
any.number.of = function(x) wildcard(x, "*")
at.least.one = function(x) wildcard(x, "+")
exactly.n = function(x, n) wildcard(x, "{", n, "}")
at.least.n = function(x, n) wildcard(x, "{", n, ",}")
range.of = function(x, n, m) wildcard(x, "{", n, ",", m, "}")

rxl =  rxr = regex
or = `%|%` =
  Function(rxl, rxr, ~build.RegEx(enclose(rxl), "|", enclose(rxr)))
