
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
as.CharClass_.character =
  Function(
    x,
    ~CharClass(escape.CharClass(paste0(unique(unlist(strsplit(x, ""))), collapse = ""))))
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

charclass.test =
  function(alphabet, charclass)
    function()
      quickcheck::test(
        forall(
          x = rcharacter(elements = list(alphabet = alphabet, nchar.min = 1, nchar.max = 1)),
          y = rcharacter(size = ~length(x)),
          z = rcharacter(size = ~length(x)),
          length(x) == length(grep(pattern = regex(~any.of(charclass)), paste0(y, x, z)))))

str2cc = function(x) strsplit(x = rawToChar(as.raw(x)), split = "")[[1]]

alphac = c(letters, LETTERS)
digitc = 0:9
alnumc = c(alphac, digitc)
blankc = c(" ", "\t")
cntrlc =  c(str2cc(c(1:31)), "\xb1")
punctc = strsplit(x = "!\"#$%'()*+,-./:;<=>?@[\\]^_`{|}~", split = "")[[1]]
graphc = c(alnumc,  punctc)
spacec = c(blankc, "\n", "\f", "\r", "\v") # \v is vertical tab?
printc = c(graphc, spacec)
xLETTERS = str2cc(65:70)
xletters = tolower(xLETTERS)
xdigitc = c(digitc, xletters, xLETTERS)

predef.CharClasses =
  list(
    alphanumeric = c("[:alnum:]", "alphanumeric characters" , alnumc),
    alphabetic = c("[:alpha:]",   "alphabetic characters", alphac),
    blank = c("[:blank:]",        "blank characters", blankc),
    control = c("[:cntrl:]",      "control characters", cntrlc),
    digit = c("[:digit:]",        "digits", digitc),
    graphical = c("[:graph:]",    "graphical characters", graphc),
    lower = c("[:lower:]",        "lowercase letters", letters),
    printable = c("[:print:]",    "printable characters", printc),
    punctuation = c("[:punct:]",  "punctuation", punctc),
    space = c("[:space:]",        "white space, including newlines", spacec),
    upper = c("[:upper:]",        "uppercase letters", LETTERS),
    hexadecimal = c("[:xdigit:]", "hexadecimal digits", xdigitc))

predef.CharClasses[] =
  map2(
    predef.CharClasses,
    names(predef.CharClasses),
    function(x, n) {
      y = CharClass(x[[1]])
      y =
        set.help(
          y,
          Help(
            title = n,
            description = paste("Character class containing all", x[[2]]),
            usage = n,
            examples = paste0("any.of(", n, ")")))
      y = set.tests(y, list(charclass.test(x[[3]], y)))
      export(y)})

attach = partial(base::attach, name = "regez")
attach(predef.CharClasses)

start.char = stop.char = one.char

char.range =
  Function(
    start.char,
    stop.char
    ~CharClass(paste0(start.char, "-", stop.char)),
    export = TRUE)

backrefs =
  captured.refs =
  Argument(process = as.character, default = character(0))

RegEx =
  Function(
    s,
    backrefs,
    captured.refs,
    ~structure(
      list(
        s = s,
        backrefs = backrefs,
        captured.refs = captured.refs),
      class = "RegEx"),
    postcondition = assert.RegEx)

as.RegEx = Function(x, ~as.RegEx_(x))
as.RegEx_ = function(x) UseMethod("as.RegEx_")
as.RegEx_.character = Function(x, ~RegEx(escape.RegEx(x)))
as.RegEx_.default = function(x) as.RegEx_(as.character(x))
as.RegEx_.CharClass = function(x) stop("Can't convert a CharClass to a RegEx. Use any.of or none.of")
as.RegEx_.RegEx = identity

is.RegEx = function(x) "RegEx" %in% class(x)
print.RegEx = function(x) cat(x$s)

rx =
  Argument(
    process = as.RegEx,
    help = "A `RegEx` object, representing a valid regex according to PCRE or
    an R object coerceable to it")

empty.capture =
  function(refs)
    paste0(map(refs, function(x) paste0("(?<", x, ">)")), collapse = "")

assert.RegEx =
  Function(
    x,
    ~assert(
      x,
      function(z)
        is.list(z) &&
        all(names(z) == c("s", "backrefs", "captured.refs")) &&
        is.integer(
          grep(
            paste0(empty.capture(setdiff(z$backrefs, z$captured.refs)), z$s),
            "",
            perl = TRUE))))

escape.RegEx = Function(s, ~escape(s, is.meta.RegEx))

is.meta.RegEx =
  Function(one.char,  ~{
    one.char %in%
      c(".", "\\",  "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")})

rxl  = rxr = rx

#
conF =
  partial(
    Function, rxl, rxr,
    help =
      Help(
        title = "Concatenate regular expressions",
        description = "Concatenate two regular expression into a valid regular expression"))
concat2  = conF(~concat2_(rxl, rxr))
`%+%`= export(concat2)
concat2_ = function(rxl, rxr) UseMethod("concat2_")

concat2_.character =
  conF( ~{
    switch(
      class(rxr),
      character = paste0(rxl, rxr),
      CharClass = concat2(as.CharClass(rxl), rxr),
      RegEx = concat2(as.RegEx(rxl), rxr))})

concat2_.CharClass =
  conF( ~CharClass(paste0(as.CharClass(rxl), as.CharClass(rxr))))

concat2_.RegEx =
  conF( ~{
         RegEx(
           s = paste0(rxl$s, rxr$s),
           backrefs = union(rxl$backrefs, rxr$backrefs),
           captured.refs = union(rxl$captured.refs, rxr$captured.refs))})

concat =
  Function(
    dots.., ~{
    args = list(...)
    if(length(args) == 0) RegEx("")
    else {
      if(length(args) == 1) arg[[1]]
      else {
        if(length(args) == 2)
          do.call(concat2, args)
        else
          concat2(args[[1]], do.call(concat, args[-1]))}}},
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
    function(x) export(RegEx(paste0("\\", x))))

attach(escape.seq)

other.regex =
  map(
    c(anychar = ".", line.begin = "^", line.end = "$"),
    function(x) export(RegEx(x)))

attach(other.regex)

build.RegEx =
  Function(
    dots..,
    ~RegEx(
      s = paste0(map(list(...), ~if(is.RegEx(.)) .$s else .), collapse = ""),
      backrefs = unique(unlist(map(keep(list(...), is.RegEx), "backrefs"))),
      captured.refs = unique(unlist(map(keep(list(...), is.RegEx), "captured.refs")))))

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
  Function(
    rx,
    name, ~{
      ret = build.RegEx("(?<", name, ">", rx, ")")
      ret$captured.refs = union(name, ret$captured.refs)
      ret},
    export = TRUE)

named.ref =
  Function(name, ~RegEx(paste0("\\g{", name, "}"), backrefs = name), export = TRUE)

pos = Argument(validate = function(x) is.numeric(x) && x == as.integer(x))

pos.ref =
  Function(pos, ~named.reg(as.character(pos)), export = TRUE)

group = encloseFun("?:")()

if.followed.by = encloseFun("?=")()
if.not.followed.by = encloseFun("?!")()
if.following = encloseFun("?<=")()
if.not.following = encloseFun("?<!")()

anything = export(any.number.of(anychar))

not =
  Function(
    rx,
    ~ if.not.followed.by(anything %+% rx) %+% anything,
    export = TRUE)

something = export(at.least.one(anychar))

## this goes last

regez.env =
  list2env(
    keep.exported(
      lmap(
        unique(
          c(ls(),
            unlist(
              map(
                which(search() == "regez"), ~ls(pos = .))))),
        ~setNames(list(get(.)), .))))



regex =
  Function(x, ~regex_(x))

regex_ = function(x) UseMethod("regex_")

regex_.formula=
  Function(
    x,  ~{
      y = as.RegEx(eval(as.list(x)[[2]], regez.env, environment(x)))
      unresolved = setdiff(y$backrefs, y$captured.refs)
      if(length(unresolved) > 0)
        stop("Unresolved backrefs: ", unresolved, "\n")
      y$s})

regex_.RegEx =
  Function(
    x,
    ~x$s)


help =
  function() {
    topic.name = as.character(substitute(topic))
    if(topic.name %in% ls(regez.env))
      Function::help(eval(substitute(topic), regez.env))
    else
      utils::help(topic.name)}

formals(help) = formals(utils::help)
#load.exports()
