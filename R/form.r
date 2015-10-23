.APP.FORMS.GLOB = new.env()

examples.appForm = function() {
  yaml="
fields:
  user:
    type: character
    failure_msg: 'Bitte wählen Sie einen Nutzernamen.'
  alpha_1:
    type: numeric
    min: 0
    max: 1
    na_value: '-'
    failure_msg: 'Bitte wählen Sie einen Anteil zwischen 0 und 1. Setzen Sie einen Dezimalpunkt . statt Dezimalkomma. Tippen Sie - um keinen Vertrag anzubieten.'
  alpha_2:
    type: numeric
    min: 0
    max: 1
    na_value: '-'
    failure_msg: 'Bitte wählen Sie einen Anteil zwischen 0 und 1. Setzen Sie einen Dezimalpunkt . statt Dezimalkomma.  Tippen Sie - um keinen Vertrag anzubieten.'
opts:
  name_as_label: TRUE
"
  yaml = enc2utf8(yaml)

  restore.point.options(display.restore.point = TRUE)
  app = eventsApp()


  form = read.yaml(text=yaml,utf8 = TRUE)
  form$success.handler = function(...) {
    cat("\nGreat you inserted valid numbers!")
  }
  setForm(form)
  ui = simpleFormUI(form)

  file = "D:/libraries/investgame/investgame/game1_input.rmd"
  ui = markdownFormUI(file=file, knit=TRUE)

  addFormHandlers(form)
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser = rstudio::viewer)

}

setForm = function(form, app=getApp()) {
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]] = form
  } else {
    app$.ACTIVE.FORM = form
  }
}

getForm = function(app=getApp()) {
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]]
  } else {
    app$.ACTIVE.FORM
  }
}

formSubmitButton = function(label="Ok", form=getForm()) {
  restore.point("formSubmitButton")

  id = paste0(form$prefix,"submitBtn",form$postfix)
  HTML(as.character(actionButton(id, label)))
}

addFormHandlers = function(form, success.handler=form$success.handler) {
  id = paste0(form$prefix,"submitBtn",form$postfix)
  buttonHandler(id,formSubmitClick, form=form, success.handler=success.handler)
}

formUI = function(form, add_handlers=FALSE,  success_fun=form$success_fun,...) {
  restore.point("formUI")

  ui = NULL
  if (!is.null(form[["ui_fun"]])) {
    ui = do.call(form[["ui_fun"]],list(form=form,...))
  } else if (!is.null(form$md_source)) {
    ui = markdownFormUI(form=form,...)
  } else if (!is.null(form$file)) {
    ext = tolower(tools::file_ext(x = form$file))
    if (ext=="rmd" | ext=="md") {
      ui = markdownFormUI(form=form,...)
    } else {
      stop(paste0("Form generation for file type .",ext," not yet implemented."))
    }
  } else {
    ui = simpleFormUI(form=form,...)
  }
  if (add_handlers) {
    addFormHandlers(form=form, success_fun=success_fun,...)
  }
  ui
}

simpleFormUI = function(form, fields=form$fields, submitBtn=NULL, submitLabel="Submit",...) {
  li = lapply(names(fields), function(name) {
    HTML(fieldInput(name=name,form=form))
  })

  if (is.null(submitBtn)) {
    id = paste0(form$prefix,"submitBtn",form$postfix)
    submitBtn = actionButton(id,submitLabel)
  }
  c(li, list(submitBtn))
}

markdownFormUI = function(form=NULL,file=form[["file"]], text=form$md_source, parse.form=TRUE, params = form[["params"]], set.UTF8=TRUE, whiskers=TRUE, knit=isTRUE(form$knit), parent.env = parent.frame(), fragment.only=TRUE, start.token = "# <--START-->", ret.val="HTML", select.blocks=TRUE,...) {
  restore.point("markdownFormUI")

  if (!is.null(file) & is.null(text)) {
    text = readLines(file,warn = FALSE)
  } else {
    if (length(text)==1) text = sep.lines(text)
  }

  if (set.UTF8)
    Encoding(text)<-"UTF-8"

  if (parse.form & is.null(form)) {
    form = get.front.matter.form(text=text)
  }


  if (!is.null(start.token)) {
    rows = which(text==start.token)
    if (length(rows)>0) {
      text = text[(rows[1]+1):length(text)]
    }
  }

  if (select.blocks & !is.null(params)) {
    text = sep.lines(text)
    text = select.markdown.blocks(text, params)
  }
  if (whiskers) {
    params$form = form
    setForm(form)
    text = paste0(text, collapse="\n")
    text = replace.whiskers(text,params)
  }
  if (knit) {
    if (!is.null(form))
      setForm(form)
    if (!is.null(params)) {
      env = as.environment(params)
      parent.env(env)<-parent.env
    } else {
      env = parent.env
    }
    if (ret.val == "md") {
      return(knit(text=text,quiet=TRUE,envir=env))
    }

    html = knitr::knit2html(text=text, quiet=TRUE,envir=env, fragment.only=fragment.only)
    html = gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",html, fixed=TRUE)
    html = gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",html, fixed=TRUE)
    #html =gsub("\\\\","\\\\\\\\",html, fixed=TRUE)

  } else {
    if (ret.val == "md") {
      return(text)
    }
    # Neccessary to make mathjax work
    #text =gsub("\\\\","\\\\\\\\",text, fixed=TRUE)
    html = markdownToHTML(text=text, fragment.only=fragment.only)
  }
  #rmarkdown::render(text=text)
  HTML(html)

}


viewMarkdownForm = function(file=NULL,form=NULL, params=NULL, knit=TRUE, launch.browser = rstudioapi::viewer, ui = NULL,...) {
  app = eventsApp()
  if (is.null(ui)) {
    ui = markdownFormUI(file=file, form=form, params=params, knit=knit,...)
    form = getForm()
  }
  if (!is.null(form)) {
    addFormHandlers(form,function(...) cat("\nGreat, all values are ok!"))
  }
  app$ui = fluidPage(with_mathjax(ui))
  runEventsApp(app, launch.browser=launch.browser)
}


md2html = function(text,mode="rmarkdown",...) {
  restore.point("md2html")

  if (mode=="rmarkdown") {
    dir = tempdir()
    md.file = tempfile("md",tmpdir = dir,fileext = ".md")
    writeLines(text,md.file)
    html.file = tempfile("html",tmpdir = dir,fileext = ".html")
    ret = rmarkdown::render(input=md.file, output_file=html.file,quiet = TRUE)
    html = readLines(html.file,warn = FALSE)
  }
}

formSubmitClick = function(form, success.handler = NULL,...) {
  restore.point("formSubmitClick")

  res = getFormValues(form=form)
  restore.point("formSubmitClick_2")
  if (res$ok & (!is.null(success.handler))) {
    success.handler(values=res$values, form=form)
  }
}

getFormValues = function(form=getForm(),fields=form$fields,field.names=names(fields), prefix=form$prefix, postfix=form$postfix, show.alerts=TRUE) {
  restore.point("getFormValues")

  values = lapply(field.names, function(name) {
    id = paste0(prefix,name,postfix)
    getInputValue(name)
  })
  names(values) = field.names
  check = checkFormValues(values, form=form, show.alerts=TRUE)

  return(check)
}

checkFormValues = function(values, form, fields=form$fields[field.names], field.names=names(values), show.alerts = TRUE, get.failure.msg = FALSE) {
  restore.point("checkFormValues")

  li = lapply(field.names, function(name) {
    ret = checkFieldValue(values[[name]], fields[[name]])
    if (!ret$ok & show.alerts) {
      showFieldAlert(name=name, msg=ret$msg, form=form)
    } else {
      clearFieldAlert(name=name, form=form)
    }
    ret
  })
  names(li)= field.names
  values = lapply(li, function(el) el$value)
  failed.fields = field.names[sapply(li, function(el) !el$ok)]
  ok = length(failed.fields) == 0
  if (get.failure.msg) {
    failure.msg = sapply(li[failed.fields], function(el) el$msg)
    return(list(ok=ok,values=values,failed.fields=failed.fields, failure.msg=failure.msg))
  }
  return(list(ok=ok,values=values,failed.fields=failed.fields))
}

clearFieldAlert = function(name=field$name,field, form) {
  showFieldAlert(name=name,msg="", form=form, color=NULL)
}

showFieldAlert = function(name=field$name, msg="",field, prefix=form$prefix, postfix=form$postfix, form=NULL, color="red") {
  id = paste0(prefix,name,postfix,"__Alert")

  if (!is.null(color))
    msg = colored.html(msg, color)

  setUI(id,HTML(msg))
}

checkFieldValue = function(value, field) {
  restore.point("checkFieldValue")


  if (isTRUE(field$type=="numeric")) {
    num = as.numeric(value)
    if (is.na(num)) {
      if (value %in% field$na_value | isTRUE(field$optional)) {
        return(list(ok=TRUE,msg="", value=num))
      }
      msg = fieldFailureMsg(field, value)
      return(list(ok=FALSE,msg=msg, value=num))
    }

    if (!is.null(field$max)) {
      if (num>field$max) {
        msg = fieldFailureMsg(field, value)
        return(list(ok=FALSE,msg=msg, value=num))
      }
    }
    if (!is.null(field$min)) {
      if (num<field$min) {
        msg = fieldFailureMsg(field, value)
        return(list(ok=FALSE,msg=msg, value=num))
      }
    }
    return(list(ok=TRUE,msg="", value=num))
  }
  if (nchar(value)==0 & !isTRUE(field$optional)) {
    msg = fieldFailureMsg(field, value)
    return(list(ok=FALSE,msg=msg, value=value))
  }
  return(list(ok=TRUE,msg="", value=value))
}

fieldFailureMsg = function(field,value, use.custom = TRUE) {
  restore.point("fieldFailureMsg")

  if (use.custom & !is.null(field$failure_msg))
    return(field$failure_msg)

  if (isTRUE(field$type=="numeric")) {
    msg = "Please enter a number "
    if (!is.null(field[["min"]]) & !is.null(field[["max"]])) {
      msg = paste0(msg, " between ", field[["min"]], " and ", field[["max"]])
    } else if (!is.null(field[["min"]])) {
      msg = paste0(msg, " above or equal to ", field[["min"]])
    } else if (!is.null(field[["max"]])) {
      msg = paste0(msg, " below or equal to ", field[["min"]])
    }
    if (!is.null(field$na_value)) {
      msg = paste0(msg,". For no number enter ", paste0('"',field$na_value,'"', collapse=" or "))
    }
    msg = paste0(msg,".")
    return(msg)
  }

  msg = "Please enter a valid input."
  return(msg)
}


fieldInput = function(name=field$name, label=field$label, value=first.none.null(form$params[[name]],field$value), type=field$type, min=field$min, max=field$max, step=field$step, maxchar=field$maxchar, choices=field$choices, prefix=form$prefix, postfix=form$postfix, field=fields[[name]], fields=form$fields, field_alert = !is.false(opts$field_alert), opts=form$opts, form=getForm()) {

  restore.point("fieldInput")

  if (isTRUE(opts$name_as_label) & is.null(label)) {
    label=name
  }
  id = paste0(prefix,name,postfix)
  if (is.null(field[["choices"]])) {
    if (is.null(value)) value = ""
    ret = textInputVector(id, label=label, value=value)
  } else {
    li = as.list(choices)
    ret = as.character(selectizeInput(id, label,choices=choices, selected=value))
  }
  if (field_alert) {
    alert_id = paste0(id,"__Alert")
    alert = as.character(uiOutput(alert_id))
    ret = paste0(ret,"\n", alert)
  }


  ret = paste0(ret,"\n\n")
  return(ret)
  return(HTML(ret))
}
