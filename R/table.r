
html.table = function(df, sel.row=NULL, col.names=TRUE, row.names=FALSE, border=TRUE, bg.color =c("#ededfe","#fcfcff")) {
  restore.point("hwrite.selTable")
  n = NROW(df)

  row.bgcolor = rep(bg.color,length=n)

  if (!is.null(sel.row)) {
    row.bgcolor[sel.row]='#ffdc98'
    row.bgcolor[sel.row]='#00ff00'
  }

  if (col.names) {
    colnames = colnames(df)
    head = paste0('<th>',colnames,'</th>', collapse="")
    head = paste0('<tr>', head, '</tr>')
  } else {
    head = ""
  }

  cols = 1:NCOL(df)
  code = paste0('"<td nowrap bgcolor=\\"",row.bgcolor,"\\">", df[[',cols,']],"</td>"', collapse=",")
  code = paste0('paste0("<tr>",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  tab = paste0('<table>\n', head, main, "\n</table>")

  th.style='font-weight: bold; margin: 5px; padding: 5px; border: solid 1px black; text-align: center;'
  td.style='font-weight: normal; margin: 5px; padding: 5px; border: solid 1px black; font-family: monospace ; text-align: left;'

  tab = paste0("<style>",
    " table {	border-collapse: collapse;}\n",
    " td {", td.style,"}\n",
    " th {", th.style,"}\n",
    "</style>",tab
  )

  #writeLines(tab, "test.html")
  tab

  return(tab)
  border = 0
  tab = hwrite(df, row.bgcolor=row.bgcolor, border=border, col.names=col.names, row.names=row.names, th.style=style, td.style=style, row.style=list('font-weight:bold'))

  #cn = gsub("_"," ",colnames(df), fixed=TRUE)

  #lapply(df,class)
  #dat = as.data.frame(df[,c(2,3,4,5)])
  #hwrite(dat)
  #hwriter::hwrite(as.data.frame(df[,1:5]))
}


