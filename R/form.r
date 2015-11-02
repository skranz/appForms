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
  set.form(form)
  ui = form.ui.simple(form)

  file = "D:/libraries/investgame/investgame/game1_input.rmd"
  ui = form.ui.markdown(file=file, knit=TRUE)

  add.form.handlers(form)
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser = rstudio::viewer)

}

init.form = function(form) {
  restore.point("init.form")

  if (is.null(form$type))
    form$type = infer.form.type(form)

  if (!identical(form$type,"simple")) {
    init.form.fun = paste0("init.form.",form$type)
    form = do.call(init.form.fun, list(form=form))
  }
  form
}

infer.form.type = function(form) {
  restore.point("infer.form.type")

  if (!is.null(form[["type"]])) form$type

  if (!is.null(form[["forms"]])) "side_by_side"

  if (!is.null(form[["file"]])) {
    ext = tolower(tools::file_ext(form$file))
    if (ext == "md" | ext == "rmd") return("markdown")
  }

  type = "simple"
  return(type)

}

set.form = function(form, app=getApp()) {
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]] = form
  } else {
    app$.ACTIVE.FORM = form
  }
}

get.form = function(app=getApp()) {
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]]
  } else {
    app$.ACTIVE.FORM
  }
}

formSubmitButton = function(label="Ok", form=get.form()) {
  restore.point("formSubmitButton")

  id = paste0(form$prefix,"submitBtn",form$postfix)
  HTML(as.character(actionButton(id, label)))
}

add.form.handlers = function(form, success.handler=form$success.handler,...) {
  restore.point("add.form.handlers")


  id = paste0(form$prefix,"submitBtn",form$postfix)
  buttonHandler(id,formSubmitClick, form=form, success.handler=success.handler,...)
}

form.ui = function(form, params=form$params, add_handlers=FALSE,  success_fun=form$success_fun,...) {
  restore.point("form.ui")

  ui = NULL

  form.fun = paste0("form.ui.",form$type)
  ui = do.call(form.fun, list(form=form, params=params,...))
  if (add_handlers) {
    add.form.handlers(form=form, success_fun=success_fun,...)
  }
  ui
}

form.ui.simple = function(form, fields=form$fields, submitBtn=NULL, submitLabel="Submit",...) {
  li = lapply(names(fields), function(name) {
    HTML(fieldInput(name=name,form=form))
  })

  if (is.null(submitBtn)) {
    id = paste0(form$prefix,"submitBtn",form$postfix)
    submitBtn = actionButton(id,submitLabel)
  }
  c(li, list(submitBtn))
}


view.form = function(form, params, launch.browser = rstudioapi::viewer, ...) {
  app = eventsApp()
  if (is.null(ui)) {
    ui = form.ui(form=form, params=params,...)
    form = get.form()
  }
  if (!is.null(form)) {
    add.form.handlers(form,function(...) cat("\nGreat, all values are ok!"))
  }
  app$ui = fluidPage(with_mathjax(ui))
  runEventsApp(app, launch.browser=launch.browser)
}

formSubmitClick = function(form, success.handler = NULL,app=getApp(),id=NULL,session=NULL,...) {
  restore.point("formSubmitClick")

  res = get.form.values(form=form)
  restore.point("formSubmitClick_2")
  if (res$ok & (!is.null(success.handler))) {
    success.handler(values=res$values, form=form,...)
  }
}

get.form.values = function(form=get.form(),fields=form$fields,field.names=names(fields), prefix=form$prefix, postfix=form$postfix, show.alerts=TRUE) {
  restore.point("get.form.values")

  values = lapply(field.names, function(name) {
    id = paste0(prefix,name,postfix)
    getInputValue(name)
  })
  names(values) = field.names
  check = check.form.values(values, form=form, show.alerts=TRUE)

  return(check)
}

check.form.values = function(values, form, fields=form$fields[field.names], field.names=names(values), show.alerts = TRUE, get.failure.msg = FALSE) {
  restore.point("check.form.values")

  li = lapply(field.names, function(name) {
    ret = check.field.value(values[[name]], fields[[name]])
    if (!ret$ok & show.alerts) {
      show.field.alert(name=name, msg=ret$msg, form=form)
    } else {
      clear.field.alert(name=name, form=form)
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

clear.field.alert = function(name=field$name,field, form) {
  show.field.alert(name=name,msg="", form=form, color=NULL)
}

show.field.alert = function(name=field$name, msg="",field, prefix=form$prefix, postfix=form$postfix, form=NULL, color="red") {
  id = paste0(prefix,name,postfix,"__Alert")

  if (!is.null(color))
    msg = colored.html(msg, color)

  setUI(id,HTML(msg))
}

check.field.value = function(value, field) {
  restore.point("check.field.value")


  if (isTRUE(field$type=="numeric")) {
    num = as.numeric(value)
    if (is.na(num)) {
      if (value %in% field$na_value | isTRUE(field$optional)) {
        return(list(ok=TRUE,msg="", value=num))
      }
      msg = field.failure.msg(field, value)
      return(list(ok=FALSE,msg=msg, value=num))
    }

    if (!is.null(field$max)) {
      if (num>field$max) {
        msg = field.failure.msg(field, value)
        return(list(ok=FALSE,msg=msg, value=num))
      }
    }
    if (!is.null(field$min)) {
      if (num<field$min) {
        msg = field.failure.msg(field, value)
        return(list(ok=FALSE,msg=msg, value=num))
      }
    }
    return(list(ok=TRUE,msg="", value=num))
  }
  if (nchar(value)==0 & !isTRUE(field$optional)) {
    msg = field.failure.msg(field, value)
    return(list(ok=FALSE,msg=msg, value=value))
  }
  return(list(ok=TRUE,msg="", value=value))
}

field.failure.msg = function(field,value, use.custom = TRUE) {
  restore.point("field.failure.msg")

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


fieldInput = function(name=field$name, label=field$label, value=first.none.null(form$params[[name]],field$value), type=field$type, min=field$min, max=field$max, step=field$step, maxchar=field$maxchar, choices=field$choices, prefix=form$prefix, postfix=form$postfix, field=fields[[name]], fields=form$fields, field_alert = !is.false(opts$field_alert), opts=form$opts, form=get.form()) {

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
