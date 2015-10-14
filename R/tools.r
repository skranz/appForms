copy.into.missing.fields = function(dest, source) {
  restore.point("copy.into.empty.fields")

  new.fields = setdiff(names(source), names(dest))
  dest[new.fields] = source[new.fields]
  dest
}

examples.first.none.null = function() {
  get.none.null(NULL, 5,4)
}

first.none.null = function(...) {
  args = list(...)
  for (val in args) {
    if (!is.null(val)) return(val)
  }
  return(NULL)

}


colored.html = function(txt, color="blue") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}

stop.without.error <-function(...){
  opt <- options(show.error.messages=FALSE)
  on.exit(options(opt))
  display(...)
  stop()
}