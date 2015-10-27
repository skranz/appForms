examples.scenapp = function() {

  library(shinyEvents)
  library(appForms)


  setwd("D:/libraries/investgame/investgame2")
  source("invest_game_2.r")
  file = "game2.yaml"
  app  = scenApp(file, values=list(user="Jane Doe", i_2=4))
  runEventsApp(app, launch.browser=rstudio::viewer)

}

scenApp = function(file=file,dir=getwd(),sca=NULL, values=list(), user_choice="simple", userid=NULL, nickname=userid, results.password=NULL,...) {
  restore.point("scenApp")

  if (is.null(sca))
    sca = init.sca(file=file, dir=dir, container.id = "mainUI", values=values, user_choice=user_choice, userid=userid, nickname=nickname,results.password=results.password,...)


  app = eventsApp()
  app$ui = fluidPage(
    with_mathjax(uiOutput(sca$container.id))
  )
  app$title = sca$title
  app$sca = sca

  app$initHandler = function(app,...) {
    app$sca = as.environment(as.list(app$sca))

    if (!is.null(userid)) {
      sca.show.form(1)
    } else {
      sca.show.user.form(sca=app$sca)
    }
  }

  app
}



init.sca = function(file, dir=getwd(), container.id = "mainUI", next.btn.label="next", values=list(), user_choice="simple", userid=NULL, nickname=userid, results.password=NULL) {
  restore.point("init.sca")

  sca = read.yaml(file=file)
  sca = as.environment(sca)
  sca.init.params(sca=sca)

  sca$user_choice = user_choice
  sca$userid   = userid
  sca$nickname = nickname
  sca$values = values
  sca$values = copy.into.missing.fields(sca$values,sca$params)
  sca$container.id = container.id
  sca$forms = lapply(sca$forms, init.form)

  sca$results.password = results.password

  sca$next.btn.label = next.btn.label

  # make data stores
  sca$stores = lapply(names(sca$scens), function(scen.name) {
    file.mem.store(name=paste0("data_",scen.name))
  })
  names(sca$stores) = names(sca$scens)

  sca$scens = lapply(sca$scens, function(scen) {
    if (!is.null(scen$run)) scen$run_ = parse(text=scen$run)
    scen
  })

  sca$scenvals = vector("list",length(sca$scens))
  names(sca$scenvals) = names(sca$scens)

  sca$inpvals = list()

  sca.set.forms.activeness(sca)

  sca
}


sca.show.form = function(name, sca=app$sca, app=getApp(),form=NULL,...) {
  restore.point("sca.show.form")

  sca$current.form = name

  if (is.numeric(name)) {
    name = names(sca$forms)[[name]]
  }

  if (is.null(form))
    form = sca$forms[[name]]

  if (isTRUE(form$has_input)) {
    sca.show.input.form(form=form, name=name, sca=sca, app=app, ...)
    return()
  }

  forms = sca$forms
  menu = chooseFormButtons(forms=forms, show.fun = sca.show.form, current.form=name)
  lower.menu = chooseFormButtons(forms=forms, show.fun = sca.show.form, current.form=name, postfix="_lower")
  next.btn = nextFormButton(forms=forms, show.fun = sca.show.form, current.form=name, label=sca$next.btn.label)

  params = sca.form.params(form, sca)

  ui = formUI(form=form, params=params, scen.params=sca$scenvals)

  extra.btns = list(
    sca.print.button(sca=sca),
    sca.all.results.btn(sca=sca)
  )
  ui = with_mathjax(list(h2(sca$title), menu,extra.btns,hr(),ui,next.btn,hr(),lower.menu))
  ui = sca.layout.ui(ui, form=form, sca=sca)

  setUI(sca$container.id, ui)

}

sca.show.input.form = function(name, sca=app$sca, app=getApp(),form=NULL,...) {
  restore.point("sca.show.input.form")
  forms=sca$forms

  menu = chooseFormButtons(forms=forms, show.fun = sca.show.form, current.form=name)

  params = sca.form.params(form, sca)
  ui = formUI(form=form, params=params, scen.params=sca$scenvals)

  addFormHandlers(form=form,success.handler = sca.input.submit,form.name=name, sca=sca)

  extra.btns = list(
    sca.print.button(sca=sca),
    sca.all.results.btn(sca=sca)
  )
  ui = with_mathjax(list(h2(sca$title), menu, extra.btns, hr(),ui))
  ui = sca.layout.ui(ui, form=form, sca=sca)
  setUI(sca$container.id, ui)

}

sca.input.submit = function(values,form,sca=app$sca,form.name, app=getApp(),...) {
  restore.point("sca.input.submit")

  values = as.list(values)

  sca$inpvals[[form.name]] = values
  sca$values[names(values)] = values

  if (length(form$run_scens)>0) {
    for (scen.name in form$run_scens)
      sca.run.scen(scen.name=scen.name, sca=sca)
  }

  sca.set.forms.activeness(sca)


  next.form = findNextActiveForm(forms=sca$forms, current.form = form.name)



  if (!is.null(next.form))
    sca.show.form(name = next.form)

}

sca.run.scen = function(scen.name, sca, scen=sca$scens[[scen.name]], global.params=NULL) {
  restore.point("sca.run.scen")

  store = sca$stores[[scen.name]]
  ret = eval(scen$run_, sca$values)
  ret = copy.into.missing.fields(ret,global.params)

  stats = store.rank(store = store, field=scen$value_col)

  vals = ret[!sapply(ret, is.function)]
  vals = c(list(userid=sca$userid, nickname=sca$nickname, scen_name=scen.name, scen_title=scen$scen_title),vals, stats)

  store$add(vals)
  sca$scenvals[[scen.name]] = vals

}



sca.init.params = function(params=sca$params, sca) {
  restore.point("sca.init.params")

  for (i in seq_along(params)) {
    if (is.character(params[[i]])) {
      params[i] = try(eval(parse(text=params[[i]]), params))
    }
  }
  sca$params = params
  invisible(params)
}

sca.layout.ui = function(ui, layout=list(), form=NULL, sca=NULL) {
  restore.point("sca.layout.ui")

  layout = copy.into.missing.fields(layout, form$layout)
  layout = copy.into.missing.fields(layout, sca$layout)
  layout = copy.into.missing.fields(layout, list(left_margin=2, right_margin=2))

  column(width = 12-layout$left_margin-layout$right_margin, offset=layout$left_margin,ui)
}

sca.print.button = function(sca,id = "scenariosPrintBtn", label="", btn.icon=icon(name = "print",lib = "font-awesome"),size="small",...) {
  restore.point("sca.print.button")


  back.fun=function(...) {
    sca.show.form(name = sca$current.form,sca = sca)
  }
  btn = bsButton(id,label, size=size, icon=btn.icon,...)

  buttonHandler(id, function(...) {
    restore.point("sca.print.click")
    forms.params = lapply(sca$forms, sca.form.params, sca=sca)

    ui = printFormsUI(forms=sca$forms, forms.params=forms.params, main=sca$title, scen.params=sca$scenvals, current.form = sca$current.form, just.current=TRUE, back.fun=back.fun, container.id=sca$container.id)
    setUI(sca$container.id, ui)
  })
  btn
}

sca.all.results.btn = function(sca,id = "scenariosAllResultsBtn", label="", btn.icon=icon(name = "table"),size="small",...) {
  restore.point("sca.all.results.btn")
  btn = bsButton(id,label, size=size, icon=btn.icon,...)
  buttonHandler(id, sca.all.results.click,sca=sca)
  btn
}

sca.all.results.click = function(sca,...) {
  restore.point("sca.all.results.click")

  scen.titles = sapply(sca$scens, function(scen) scen$scen_title)

  back.btn.id = "scaAllResultsBackBtn"
  back.btn = bsButton(back.btn.id, label="", icon=icon("arrow-left"), size="small")
  current.form = sca$current.form
  buttonHandler(back.btn.id, function(...) {
    sca.show.form(name = current.form,sca = sca)
  })

  setUI(sca$container.id, with_mathjax(list(h2(paste0("Submissions ",sca$title)), back.btn,uiOutput("scenariosAllResultsUI"))))

  showAllResults(sca$stores, tab.titles=scen.titles, user_col="nickname", value_col=sca$scens[[1]]$value_col, password=sca$results.password,container="scenariosAllResultsUI", ignore.cols=c("userid","scen_name","scen_title"))

}


sca.show.user.form = function(sca, current.form=1) {

  submit.fun = function(sca, userid, nickname, ...) {
    sca$userid = userid
    sca$nickname = nickname
    sca.show.form(current.form, sca=sca)
  }

  ui = simpleUserNameUI(title=NULL,lang=sca$lang,submit.handler = submit.fun, sca=sca)

  setUI(sca$container.id, list(h2(sca$title),ui))
}

sca.form.params = function(form,sca) {
  restore.point("sca.form.params")

  params = form$params
  if (is.null(params)) params = list()

  if (!is.null(form[["scen"]])) {
    params = copy.into.missing.fields(sca$scenvals[[form$scen]], form$params)
  } else {
    params = form$params
  }
  params = copy.into.missing.fields(params,sca$values)
  params
}

sca.set.forms.activeness = function(sca) {
  restore.point("sca.set.forms.activeness")

  for (ind in seq_along(sca$forms)) {
    form = sca$forms[[ind]]
    if (length(form[["scen"]])==1) {
      sca$forms[[ind]]$active = length(sca$scenvals[[form$scen]])>0
    } else if (length(form[["scen"]])>1){
      lens = sapply(sca$scenvals[form$scen], length)
      sca$forms[[ind]]$active = !any(lens==0)
    } else {
      sca$forms[[ind]]$active = TRUE
    }
  }

}