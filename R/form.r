.APP.FORMS.GLOB = new.env()

examples.appForm = function() {

  yaml="
fields:
  user:
    type: character
    required: TRUE
  alpha_1:
    type: numeric
    min: 0
    max: 1
    na_value: '-'
    required: TRUE
  alpha_2:
    type: numeric
    min: 0
    max: 1
    na_value: '-'
    required: TRUE
opts:
  name_as_label: TRUE
"
  restore.point.options(display.restore.point = TRUE)

  app = eventsApp()

  form = read.yaml(text=yaml)
  setForm(form)
  ui = simpleFormUI(form)
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

addFormHandlers = function(form) {
  id = paste0(form$prefix,"submitBtn",form$postfix)
  buttonHandler(id,formSubmitClick, form=form)
}

simpleFormUI = function(form, fields=form$fields, submitBtn=NULL, submitLabel="Submit") {
  li = lapply(names(fields), function(name) {
    fieldInput(name=name,form=form)
  })

  if (is.null(submitBtn)) {
    id = paste0(form$prefix,"submitBtn",form$postfix)
    submitBtn = actionButton(id,submitLabel)
  }
  c(li, list(submitBtn))
}

formSubmitClick = function(form, success.fun = NULL,...) {
  restore.point("formSubmitClick")

  values = getFormValues(form=form)
  restore.point("formSubmitClick_2")
  check = checkFormValues(values, form)
}

getFormValues = function(form=getForm(),fields=form$fields,field.names=names(fields), prefix=form$prefix, postfix=form$postfix) {
  restore.point("getFormValues")

  values = lapply(field.names, function(name) {
    id = paste0(prefix,name,postfix)
    getInputValue(name)
  })
  names(values) = field.names
  values
}

checkFormValues = function(values, form, fields=form$fields[field.names], field.names=names(values), show.alerts = TRUE) {
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
  do.call("rbind",li)
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
      if (value %in% field$na_value) {
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


fieldInput = function(name=field$name, label=field$label, value=field$value, type=field$type, min=field$min, max=field$max, step=field$step, maxchar=field$maxchar, choices=field$choices, prefix=form$prefix, postfix=form$postfix, field=fields[[name]], fields=form$fields, field_alert = !is.false(opts$field_alert), opts=form$opts, form=getForm()) {

  restore.point("fieldInput")

  if (isTRUE(opts$name_as_label) & is.null(label)) {
    label=name
  }
  id = paste0(prefix,name,postfix)
  if (is.null(field[["choices"]])) {
    if (is.null(value)) value = ""
    ret = textInputVector(id, label=label, value="")
  } else {
    li = as.list(choices)
    ret = as.character(selectizeInput(id, label,choices=choices))
  }
  if (field_alert) {
    alert_id = paste0(id,"__Alert")
    alert = as.character(uiOutput(alert_id))
    ret = paste0(ret,"\n", alert)
  }

  return(HTML(ret))
}
