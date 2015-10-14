
# Use local version of MathJax so that problem sets really run offline
with_mathjax <- function(html) {
  restore.point("with_mathjax")

  if (!can.connect.to.MathJax()) return(html)

  #path =  paste0(system.file('www', package='RTutor'),"/MathJax")
  #if (!file.exists(path))
  path <- '//cdn.mathjax.org/mathjax/latest'

  command = paste0(path, '/MathJax.js?config=TeX-AMS-MML_HTMLorMML')
  #path <- 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'

  tagList(
  tags$head(
  singleton(tags$script(src = command, type = 'text/javascript'))
  ),
  html,
  tags$script(HTML('MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'))
  )
}

can.connect.to.MathJax = function() {
  library(RCurl)
  url.exists("http://cdn.mathjax.org/mathjax/latest/MathJax.js")
 # url.exists("http://www.mathjax.org/")
}
