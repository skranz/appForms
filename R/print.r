printFormsUI = function(forms,forms.params, only.active=TRUE, add.form.title=TRUE,main=NULL, back.fun = NULL, prefix="", postfix="", btn.size="small", current.form=NULL, just.current=TRUE, container.id=NULL,...) {
  restore.point("printFormsUI")


  if (!is.null(current.form) & just.current) {
    if (is.character(current.form)) {
      forms.inds = which(names(forms)==current.form)
    } else {
      forms.inds = current.form
    }
  } else if (only.active) {
    forms.inds = which(sapply(forms, function(form) form$active))
  } else {
    form.inds = seq_along(forms)
  }

  ui.li = lapply(forms.inds, function(i) {
    formUI(forms[[i]], params=forms.params[[i]])
  })


  txt = sapply(seq_along(ui.li), function(i) {
    paste0(
      if (add.form.title) h2(forms[[forms.inds[i] ]]$title) else "",
      paste0(as.character(ui.li[[i]]), collapse="\n")
    )
  })
  txt = paste0(txt, collapse="\n")
  if (!is.null(main)) txt = paste0(h1(main),txt)

  back.btn.id = paste0(prefix,"backFromPrintFormBtn",postfix)
  current.btn.id = paste0(prefix,"printCurrentFormBtn",postfix)
  all.btn.id = paste0(prefix,"printAllFormsBtn",postfix)

  btns = list(
    bsButton(back.btn.id, label="", icon=icon("arrow-left"), size=btn.size)
  )
  bsTooltip(back.btn.id, "Back", placement = "bottom", trigger = "hover", options = NULL)
  buttonHandler(back.btn.id, back.fun)

  if (!is.null(current.form) & !is.null(container.id)) {
    btns = c(btns,list(
      bsButton(current.btn.id, label="", icon=icon("file-o"), size=btn.size, style = if (just.current) "priority" else "default"),
      bsButton(all.btn.id, label="", icon=icon("files-o"), size=btn.size, style = if (!just.current) "priority" else "default")
    ))

    bsTooltip(current.btn.id, paste0("Print just ", current.form), placement = "bottom", trigger = "hover", options = NULL)

    bsTooltip(all.btn.id, paste0("Print all (", paste0(names(forms), collapse="\n"),")"), placement = "bottom", trigger = "hover", options = NULL)

    show.print = function(just.current=FALSE,...) {
      restore.point("show.print")

      ui = printFormsUI(forms,forms.params=forms.params, only.active, add.form.title,main, back.fun, prefix, postfix, btn.size, current.form, just.current, container.id,...)
      setUI(container.id, ui)
    }

    buttonHandler(current.btn.id, show.print, just.current=TRUE)
    buttonHandler(all.btn.id, show.print, just.current=FALSE)

  }

  btns = paste0(unlist(lapply(btns, as.character)), collapse="\n")



  txt = paste0(btns,txt)
  with_mathjax(HTML(txt))

}