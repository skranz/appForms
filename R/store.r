file.mem.store = function(name, file.name=paste0(name,".yaml"), is.df=TRUE, load=TRUE) {
  restore.point("file.mem.store")

  st = new.env()

  st$is.df = is.df
  if (load) {
    if (file.exists(file.name)) {
      text = paste0(readLines(file.name,warn = FALSE), collapse="\n")
    } else {
      text = NULL
    }
    if (length(text)>0) {
      restore.point("ksdkfdknfdhoif")

      st$li = read.yaml(text = text, keep.quotes=FALSE)
    } else {
      st$li = NULL
    }
    if (is.df & length(st$li)>0) {
      st$df = as.data.frame(rbindlist(st$li,fill = TRUE))
    } else {
      st$df = NULL
    }
  }

  #st$append.con = file(file.name, open="at", blocking=FALSE)

  st$add = function(vals) {
    restore.point("file.mem.store.add")

    vals$DATE_TIME = as.character(Sys.time())
    vals$DATE = as.character(as.Date(Sys.time()))
    st$li[[length(st$li)+1]] = vals

    json = paste0(toJSON(vals), collapse="")

    con = file(file.name, open="at", blocking=FALSE)
    write(paste0("- ",json), file=con)
    close(con)
    #writeLines(paste0("- ",json), con = st$append.con)
    if (st$is.df) {
      if (!is.null(st$df)) {
        st$df = rbindlist(c(list(st$df), list(as.data.table(vals))))
      } else {
        st$df = as.data.table(vals)
      }
    }
  }

  st$get.data = function() {
    st$df
  }

  st

}
