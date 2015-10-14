
chooseFormButtons = function(forms,show.fun, current.form = NULL, prefix="", postfix="", add.handler=TRUE, size="small", block=FALSE)
{
  restore.point("formsButtonMenu")

  fnames = names(forms)
  btnIds = paste0(prefix,fnames,"__showFormBtn",postfix)

  if (is.character(current.form)) {
    current.form = which(fnames==current.form)
  }


  btns = lapply(seq_along(forms), function(i) {
    restore.point("formsButtonMenu.inner")

    form = forms[[i]]
    label = form$menu_title
    if (is.null(label)) label = form$title
    disabled = is.false(form$active)

    style = if (identical(i,current.form)) "primary" else "default"

    if (add.handler)
      buttonHandler(btnIds[i], fun = show.fun, name = fnames[i],form=forms[[i]], forms=forms, if.handler.exists = "skip")
    list(bsButton(btnIds[i],label, size=size, style=style,  disabled=disabled))
  })

  #wellPanel(btns)
  btns
}
