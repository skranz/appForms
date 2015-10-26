simpleUserNameUI = function(title="", label=NULL, submitLabel="Ok",submit.handler = NULL, prefix="", postfix="", userid="", lang="en",...) {
  restore.point("simpleUserNameUI")

  if (is.null(lang)) lang = "en"

  if (is.null(label)) {
    if (lang == "de" ) {
      label =   "Bitte geben Sie einen Nutzernamen ein:"
    } else {
      label = "Please enter a user name:"
    }
  }


  txt.id = paste0(prefix,"userIdInput",postfix)
  btn.id = paste0(prefix,"userNameSubmitBtn",postfix)
  info.id = paste0(prefix,"userNameInfoUI",postfix)
  ui = list(
    h3(title),
    textInput(txt.id, label=label,value=userid),
    uiOutput(info.id),
    actionButton(btn.id, submitLabel)
  )
  setUI(info.id,"")

  buttonHandler(btn.id, txt.id=txt.id, ..., submit.handler=submit.handler, function(txt.id,...) {
    userid = getInputValue(txt.id)
    if (nchar(userid)==0) {
      txt = colored.html("Bitte suchen Sie sich einen Nutzernamen aus und geben ihn ein.",color = "red")
      setUI(info.id,HTML(txt))
      return()
    }
    nickname = userid
    submit.handler(userid=userid, nickname=nickname,...)
  })
  ui
}