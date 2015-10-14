errfun =
  function(e)
    stop(
      strsplit(
        as.character(e),
        'Error in grep\\(x, \\"\\"\\): ')[[1]][-1])

assert.valid.regex =
  function(x)
    tryCatch(
      is.rx(x) || is.integer(grep(x, "")),
      error = errfun)

is.rx = function(x) "RX" %in% class(x)

metacharacters =
  c(".", "\\",  "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
is.metacharacter = function(x) x%in%metacharacters

escape = function(x) rxq(paste0("\\", x))

rx =
  function(...) {
    rxq(paste0(map(list(...), rx.one), collapse = ""))}

rx.one = function(x) {UseMethod("rx.one")}

rx.one.RX = function(x) {x}
rx.one.character =
  function(x){
    x =
      paste(
        map_if(strsplit(x, "")[[1]], is.metacharacter, escape),
        collapse = "")
    rxq(x)}

rxq =
  function(x) {
    assert.valid.regex(x)
    structure(
      x,
      class = "RX")}

rangex =
  function(x, y = NULL) {
    stopifnot(is.null(y) || (nchar(x) == 1 && nchar(y) == 1))
    rxq(
      paste0(
        "[",
        rx(paste0(x, if(!is.null(y)) "-", y)),
        "]"))}

BEL = rx(escape("a"))
ESC = rx(escape("e"))
FF = rx(escape("f"))
CR = rx(escape("r"))
TAB = rx(escape("t"))

char.class =
  alphanumeric = rx("[:alnum:]")
alphabetic = rx("[:alpha:]")
blank = rx("[:blank:]")
control = rx("[:cntrl:]")
digit = rx("[:digit:]")
graphical = rx("[:graph:]")
lower = rx("[:lower:]")
print = rx("[:print:]")
punct = rx("[:punct:]")
space = rx("[:space:]")
upper = rx("[:upper:]")
xdigit = rx("[:xdigit:]")

any = rx(".")
word.char = rx(escape("w"))
non.word.char = rx(escape("W"))
digit = rx(escape("d"))
space.char = rx(escape("s"))
non.digit = rx(escape("D"))
non.space.char = rx(escape("S"))
line.begin = rx("^")
line.end = rx("$")
word.begin = rx(escape("<"))
word.end = rx(escape(">"))
word.edge = rx(escape("b"))
not.word.edge = rx(escape("B"))

enclose = function(x) rxq(paste0("(", rx(x), ")"))
optional = function(x) rx(enclose(x), "?")
any.number.of = function(x) rx(enclose(x), "*")
at.least.one = function(x) rx(enclose(x), "+")
exactly.n = function(x, n) rx(enclose(x), "{", n, "}")
at.least.n = function(x, n) rx(enclose(x), "{", n, ",}")
a.range = function(x, n, m) rx(enclose(x), "{", n, ",", m, "}")
greedy = function(x) rx(x, "?")
or = `%|%` =
  function(x, y) rx(enclose(x), "|", enclose(y))

word = rx(word.begin, at.least.one(word.char), word.end)