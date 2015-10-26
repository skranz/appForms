
store.rank =function(store, field, value=store$li[[ind]][[field]], ind=length(store$li)) {
  restore.point("store.rank")

  #n = length(store$li)

  vals = store$df[[field]]
  n_total = sum(!is.na(vals))
  rank_min = sum(vals>value,na.rm = TRUE)+1
  rank_max = n_total-sum(vals<value,na.rm = TRUE)

  nlist(n_total, rank_min, rank_max)

}

showAllResults = function(stores, container, tab.titles=NULL, titles=NULL,names=paste0("resultsTable__",seq_along(stores)),user_col=NULL, value_col=NULL, password=NULL, entered.password = isTRUE(app$allResultsEnteredPassword), app=getApp(),prefix="allResultsForm__",password.text="The results of all users can only be shown with the lecturer password",...) {
  restore.point("showAllResults")


  if (!is.null(password) & !entered.password) {
    login.fun = function(..., app=getApp()) {
      app$allResultsEnteredPassword = TRUE
      showAllResults(stores=stores, container=container, tab.titles=tab.titles,  titles=titles,names=names,user_col=user_col, value_col=value_col, password=password, entered.password=TRUE, prefix=prefix)
    }
    ui = passwordLogin(id = prefix,login.fun = login.fun,text=password.text,password=password)
    setUI(container, ui)
    return()
  }

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
  setUI(container,ui)
}


allResultsTableUI = function(store, data=store$get.data(), name="resultsTable", user_col=NULL, value_col=NULL, greater_better=TRUE, title=NULL,   opts = list(paging=FALSE, lengthMenu=FALSE, pageLength=NROW(data),searching=FALSE, info=FALSE)) {

  restore.point("allResultsTableUI")

  data = as_data_frame(data)
  dtid = paste0(name,"__Table")

  if (!is.null(user_col)) {
    if (!is.null(value_col)) {
      modes = c("last","first","best","all")
    } else {
      modes = c("last","first","all")
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
    #uiOutput(dtid)
    dataTableOutput(dtid)
  )

  update.table = function(mode,...) {
    restore.point("update.table")
    cat("\nupdate.table.....................................\n")
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

    df = dplyr::select(df, -userid)
    if (NROW(df)>0) {


      #df = cbind(data.frame(pos=1:NROW(df)),df)
    }
    #setUI(dtid, HTML(html.table(df)))
    setDataTable(dtid, df, options=opts)
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