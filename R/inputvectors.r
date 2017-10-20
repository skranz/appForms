textInputVector = function(inputId, label="", value="", readonly = rep(FALSE, length(inputid)) ){
  code = paste0('
<div class="form-group shiny-input-container">
  <label for="',inputId,'">',label,'</label>
  <input id="',inputId,'" type="text" class="form-control" value="',value,'"', ifelse(readonly,' readonly',''),'/>
</div>
  ')
  code
}

numericInputVector = function (inputId, label=NULL, value=0)  {
  code = paste0('
<div class="form-group shiny-input-container">
  <label for="',inputId,'">',label,'</label>
  <input id="',inputId,'" type="number" class="form-control" value="',value,'"/>
</div>
  ')
  code = paste0('
  <input id="',inputId,'" type="number" class="form-control" value="',value,'"/>
  ')

  code
}

extraSmallButtonVector = function(id, label="",icon=NULL) {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="btn btn-default action-button btn-xs">',icon,label,'</button>')
}


smallButtonVector = function(id, label="",icon=NULL) {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="btn btn-default action-button btn-sm">',icon,label,'</button>')
}

actionButtonVector = function(id, label="",icon=NULL) {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="btn btn-default action-button">',icon,label,'</button>')
}

checkBoxInputVector = function (inputId, label, value = FALSE)  {
  code = rep("", length(inputId))
  checked.str = ifelse(value,' checked="checked"',"")
  value = rep(value, length.out=length(code))

  code = paste0('
<div class="form-group shiny-input-container">
  <div class="checkbox">
    <label>
      <input id="',inputId,'" type="checkbox" ', checked.str,'/>
      <span>',label,'</span>
    </label>
  </div>
</div>
  ')
  code
}

uiOutputVector = function(id) {
  paste0('<div id="',id,'" class="shiny-html-output"></div>')
}

selectizeInputVector = function (inputId, label, choices, selected=1)  {
  code = rep("", length(inputId))
  checked.str = ifelse(value,' checked="checked"',"")
  value = rep(value, length.out=length(code))

  choices.lab = names(choices)
  if (is.null(choices.lab)) choices.lab = choices

  if (!is.list(selected)) {
    selected.str = rep("", length(inputId))
    selected.str[selected] = "selected"

    options.str = paste0(collapse="\n",
      '<option value="',choices,'" ',  selected.str,'>',
      choices.lab,'</option>'
    )
  } else {
    stop()
  }

  code = paste0('
<div class="form-group shiny-input-container">
  <label class="control-label" for="',inputId,'">"',inputId,'</label>
  <div>
    <select id="',inputId,'" class="form-control">
    ',options.str,'
    <script type="application/json" data-for="', inputId,'">{}</script>
  </div>
</div>
  ')
  code
}
