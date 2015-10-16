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
        st$df = rbind(st$df, as.data.frame(vals))
      } else {
        st$df = as.data.frame(vals)
      }
    }
  }

  st$get.data = function() {
    st$df
  }

  st

}

store.rank =function(store, field, value=store$li[[ind]][[field]], ind=length(store$li)) {
  restore.point("store.rank")

  #n = length(store$li)

  vals = store$df[[field]]
  n_total = sum(!is.na(vals))
  rank_min = sum(vals>value,na.rm = TRUE)+1
  rank_max = n_total-sum(vals<value,na.rm = TRUE)

  nlist(n_total, rank_min, rank_max)

}

allResultsUI = function(stores, tab.titles=NULL, titles=NULL,names=paste0("resultsTable__",seq_along(stores)),user_col=NULL, value_col=NULL,...) {
  restore.point("allResultsUI")


  uis = lapply(seq_along(stores), function(i) {
    allResultsTableUI(store=stores[[i]], title=titles[[i]], name=names[i],user_col=user_col, value_col = value_col,...)
  })

  if (length(uis)>1) {
    tabs = lapply(seq_along(uis), function(i) {
      tabPanel(title = tab.titles[i],uis[[i]])
    })
    ui =do.call(tabsetPanel,tabs)
  } else {
    ui = uis[[1]]
  }
  ui
}


allResultsTableUI = function(store, data=store$get.data(), name="resultsTable", user_col=NULL, value_col=NULL, greater_better=TRUE, title=NULL,   opts = list(paging=FALSE, lengthMenu=FALSE, pageLength=NROW(data),searching=FALSE, info=FALSE)) {

  restore.point("allResultsTableUI")

  dtid = paste0(name,"__Table")

  if (!is.null(user_col)) {
    if (!is.null(value_col)) {
      modes = c("all","first","last","best")
    } else {
      modes = c("all","first","last")
    }
  } else {
    modes = "all"
  }
  if (length(modes)>1) {
    btns = lapply(modes, function(mode) {
      btnid = paste0(name,"__",mode,"__Btn")
      bsButton(btnid,mode,size = "small")
    })
  } else {
    btns = NULL
  }

  ui = list(
    h2(title),
    btns,
    uiOutput(dtid)
    #dataTableOutput(dtid)
  )
  update.table = function(mode,...) {
    if (mode == "all") {
      df = data
    } else if (mode=="first") {
      df = filter(group_by_(data,.dots=user_col),DATE_TIME == min(DATE_TIME))
      df = filter(df, row_number()==1)
    } else if (mode=="last") {
      df = filter(group_by_(data,.dots=user_col),DATE_TIME == max(DATE_TIME))
      df = filter(df, row_number()==1)
    } else if (mode=="best") {
      if (greater_better) {
        call = substitute(col == max(col), list(col=as.name(value_col)))
      } else {
        call = substitute(col == min(col), list(col=as.name(value_col)))
      }
      df = filter(group_by_(data,.dots=user_col), call)
      df = filter(df, row_number()==1)
    }

    if (!is.null(value_col)) {
      sign = if (greater_better) -1 else 1
      df = df[order(sign*df[[value_col]]),]
    }
    setUI(dtid, HTML(html.table(df)))
    #setDataTable(dtid, df, options=opts)
  }

  # Add button handlers
  if (length(modes)>1) {
    for (mode in modes) {
      btnid = paste0(name,"__",mode,"__Btn")
      buttonHandler(btnid, fun=update.table, mode=mode)
    }
  }


  update.table(modes[1])
  ui
}