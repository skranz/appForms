findNextActiveForm = function(forms, current.form) {
  restore.point("findNextActiveForm")


  if (is.character(current.form)) {
    current.form = which(names(forms)==current.form)
  }
  if (current.form >= length(forms)) return(NULL)

  for (ind in (current.form+1:length(forms))) {
    if (!is.false(forms[[ind]]$active))
      return(names(forms)[ind])
  }
  return(NULL)
}


nextFormButton = function(forms,show.fun, current.form, next.form=NULL, prefix="", postfix="", add.handler=TRUE, size="small", style="default", label="Next",id=NULL,...) {
  restore.point("nextFormButton")


  names = names(forms)
  if (is.null(next.form)) {
    if (is.character(current.form)) {
      current.form = which(names==current.form)
    }
    if (current.form >= length(forms)) return(NULL)
    next.form = current.form+1
  }
  form = forms[[next.form]]
  if (is.false(form$fromNextButton)) return(NULL)

  name = names(forms)[next.form]
  if (is.null(id)) {
    id = paste0(name,"__showNextBtn")
  }
  id = paste0(prefix,id,postfix)

  disabled = is.false(form$active)

  if (add.handler)
    buttonHandler(id, fun = show.fun, name = name,form=form, forms=forms, if.handler.exists = "skip")

  bsButton(id,label, size=size, style=style,  disabled=disabled,...)
}


chooseFormButtons = function(forms,show.fun, current.form = NULL, prefix="", postfix="", add.handler=TRUE, size="small", block=FALSE, used=names(forms), labels=NULL)
{
  restore.point("formsButtonMenu")

  btnIds = paste0(prefix,used,"__showFormBtn",postfix)

  if (is.character(current.form)) {
    current.form = which(used==current.form)
  }

  used.ind = which(names(forms)==used)

  btns = lapply(seq_along(used.ind), function(row) {
    restore.point("formsButtonMenu.inner")
    i = used.ind[row]

    form = forms[[i]]
    label = first.none.null(labels[row],form$menu_title,form$title)
    disabled = is.false(form$active)

    style = if (identical(i,current.form)) "primary" else "default"

    if (add.handler)
      buttonHandler(btnIds[i], fun = show.fun, name = used[i],form=forms[[i]], forms=forms, if.handler.exists = "skip")
    list(bsButton(btnIds[i],label, size=size, style=style,  disabled=disabled))
  })

  #wellPanel(btns)
  btns
}
