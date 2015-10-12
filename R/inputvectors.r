textInputVector = function(inputId, label=NULL, value="") {


  code = paste0('
<div class="form-group shiny-input-container">
  <label for="',inputId,'">',label,'</label>
  <input id="',inputId,'" type="text" class="form-control" value="',value,'"/>
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
