
errfun = function(e) stop(strsplit(as.character(e), ': ')[[1]][-1])

x = Argument(help = "Any R object")
p = Argument(validate = is.function, help = "A function returning a length-one logical")

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
    validate = function(x) is.character(x) && nchar(x) == 1,
    help = "A single character")

is.meta.CharClass = Function(one.char, ~one.char %in% c("\\", "-", "[", "]", "^"))

predef.CharClasses =
  list(
    alphanumeric = c("[:alnum:]", "alphanumeric characters"),
    alphabetic = c("[:alpha:]",   "alphabetic characters"),
    blank = c("[:blank:]",        "blank characters"),
    control = c("[:cntrl:]",      "control characters"),
    digit = c("[:digit:]",        "digits"),
    graphical = c("[:graph:]",    "graphical characters"),
    lower = c("[:lower:]",        "lowercase letters"),
    printable = c("[:print:]",    "printable characters"),
    punctuation = c("[:punct:]",  "punctuation"),
    space = c("[:space:]",        "white space, including newlines"),
    upper = c("[:upper:]",        "uppercase letters"),
    hexadecimal = c("[:xdigit:]", "hexadecimal digits"))

predef.CharClasses =
  map2(
    predef.CharClasses,
    names(predef.CharClasses),
    function(x, n) {
      y = CharClass(x[[1]])
      attr(y, "help") =
        Help(
          title = n,
          description = paste("Character class containing all", x[[2]]),
          usage = n,
          examples = paste0("any.of(", n, ")"))
      y})

attach(predef.CharClasses)
for(class.name in names(predef.CharClasses))
  export(class.name)

start.char = stop.char = one.char

char.range =
  Function(
    start.char,
    stop.char
    ~CharClass(paste0(start.char, "-", stop.char)),
    export = TRUE)

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

rx =
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

rxl  = rx

rxr = Argument(
  process = function(x) if(is.CaptureRef(x)) x else as.RegEx(x),
  help = "A `RegEx` object, representing a valid regex according to PCRE or
    an R object coerceable to it, or a capture reference referring to an existing capture group")

conF =
  partial(
    Function, rxl, rxr,
    help =
      Help(
        title = "Concatenate regular expressions",
        description = "Concatenate two regular expression into a valid regular expression"))
concat2  = `%+%`  = conF(~concat2_(rxl, rxr))
export("%+%")
concat2_ = function(rxl, rxr) UseMethod("concat2_")

concat2_.character =
  conF( ~{
    switch(
      class(rxr),
      character = paste0(rxl, rxr),
      CharClass = concat2(as.CharClass(rxl), rxr),
      RegEx = concat2(as.RegEx(rxl), rxr),
      CaptureRef = concat2(as.RegEx(rxl), rxr))})

concat2_.CharClass =
  conF( ~CharClass(paste0(as.CharClass(rxl), as.CharClass(rxr))))

concat2_.RegEx =
  conF( ~RegEx(paste0(as.RegEx(rxl), if(is.CaptureRef(rxr)) rxr else as.RegEx(rxr))))

concat =
  Function(dots.., ~{
    args = list(...)
    if(length(args) == 2)
      do.call(concat2, args)
    else
      concat2(args[[1]], do.call(concat, args[-1]))},
    export = TRUE,
    help =
      Help(
        title = "Concatenate multiple regular expressions",
        description = "Concatenate multiple regular expressions into a valid regular expression",
        arguments = list("..." = "one or more regular expressions or R expressions that can be cast to one")))

escape.seq =
  map(
    c(
      bell = "a", esc = "e", FF = "f", carriage.return = "r", tab = "t",
      word.char = "w",  non.word.char = "W", digit = "d",
      space.char = "s", non.digit = "D",     non.space.char = "S",
      word.edge = "b", not.word.edge = "B"),
    function(x) RegEx(paste0("\\", x)))


attach(escape.seq)
for(esc.name in names(escape.seq))
  export(esc.name)

other.regex =
  map(c(anychar = ".", line.begin = "^", line.end = "$"), RegEx)

attach(other.regex)
for(export.name in names(other.regex))
  export(export.name)

build.RegEx = Function(dots.., ~RegEx(paste0(list(...), collapse = "")))

any.of =
  Function(
    char.class,
    ~build.RegEx("[", char.class, "]"),
    export = TRUE)

none.of =
  Function(
    char.class,
    ~build.RegEx("[^", char.class, "]"),
    export = TRUE)

wtfun = function(x) switch(x, greedy = "", lazy = "?", possessive = "+")
wildcard =
  function(rx, ..., type) build.RegEx(group(as.RegEx(rx)), paste0(..., wtfun(type)))

n = m = Argument(process = as.integer)
wildchoices = c("greedy", "lazy", "possessive")
wildtype =
  Argument(default = wildchoices, process = function(x) match.arg(x, wildchoices))

wildF = partial(Function, rx, wildtype, export = TRUE)
optional      = wildF(      ~wildcard(rx, "?",                 type = wildtype))
any.number.of = wildF(      ~wildcard(rx, "*",                 type = wildtype))
at.least.one  = wildF(      ~wildcard(rx, "+",                 type = wildtype))
exactly.n     = wildF(n,    ~wildcard(rx, "{", n, "}",         type = wildtype))
at.least.n    = wildF(n,    ~wildcard(rx, "{", n, ",}",        type = wildtype))
range.of      = wildF(n, m, ~wildcard(rx, "{", n, ",", m, "}", type = wildtype))

or = `%|%` =
  Function(
    rxl, rxr,
    ~build.RegEx(group(rxl), "|", group(rxr)),
    export = TRUE)

encloseFun =
  function(prefix)
    partial(Function, rx, eval(bquote(~build.RegEx("(", prefix, rx, ")"), list(prefix = prefix))), export = TRUE)

capture = encloseFun("")()

name = s
name$validate = function(name) grep("^\\w+$", name) == 1

named.capture =
  Function(rx, name, ~build.RegEx("(?<", name, ">", rx, ")"), export = TRUE)

CaptureRef =
  Function(
    name,
    ~structure(
      name,
      class = "CaptureRef"))

is.CaptureRef = function(cr) "CaptureRef" %in% class(cr)

capture.ref =
  Function(name, ~CaptureRef(paste0("\\g{", name, "}")), export = TRUE)

group = encloseFun("?:")()

if.followed.by = encloseFun("?=")()
if.not.followed.by = encloseFun("?!")()
if.following = encloseFun("?<=")()
if.not.following = encloseFun("?<!")()

anything = any.number.of(anychar)
something = at.least.one(anychar)
export("anything")
export("something")

## this goes last

sf = sys.frame(sys.nframe())
regez.env = exported(sf)
rx_ = Argument(validate = function(x) "formula" %in% class(x))
regex = Function(rx_,  ~as.RegEx(eval(as.list(rx_)[[2]], regez.env, environment(rx_))))
#load.exports()
