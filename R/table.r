
html.table = function(df, sel.row=NULL, col.names=TRUE, row.names=FALSE, border=0, bg.color =c("#ededfe","#fcfcff")) {
  restore.point("hwrite.selTable")
  n = NROW(df)

  row.bgcolor = rep(bg.color,length=n)
  #row.bgcolor = rep(c("#dddddd","#ffffff"),length=n)
  #row.bgcolor = rep(c("#fefeff","#ededfe"),length=n)
  #row.bgcolor = rep(c("#eeffee","#aaffaa"),length=n)

  if (!is.null(sel.row)) {
    row.bgcolor[sel.row]='#ffdc98'
    row.bgcolor[sel.row]='#00ff00'
  }
  style='margin: 5px; padding: 5px; border: solid 1px black; font-family: monospace ; text-align: left'
  hwrite(df, row.bgcolor=row.bgcolor, border=border, col.names=col.names, row.names=row.names, style=style, col.style=style, table.style=style)

  #cn = gsub("_"," ",colnames(df), fixed=TRUE)

  #lapply(df,class)
  #dat = as.data.frame(df[,c(2,3,4,5)])
  #hwrite(dat)
  #hwriter::hwrite(as.data.frame(df[,1:5]))
}
