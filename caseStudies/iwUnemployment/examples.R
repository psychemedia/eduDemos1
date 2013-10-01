list.of.packages <- c("XML", "RCurl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(XML)
library(RCurl)

commaReplace = function(node) {
  val = xmlValue(node)
  ans = gsub(",", "", val)
  if(is.na(ans))
    val
  else
    ans
}

#total1, male1, female1
tt=postForm("https://www.nomisweb.co.uk/reports/lmp/la/2038431803/subreports/jsa_time_series/report.aspx", .encoding='utf-8', pivot="total1")
tables <- readHTMLTable(tt,stringsAsFactors = FALSE,elFun=commaReplace ) #encoding = "UTF-8")
nn=gsub('([\n\t\r])','',names(tables$`NULL`))
names(tables$`NULL`)=nn
ud=tables$`NULL`
ud['Isle of Wight']=lapply(ud['Isle of Wight'], function(x) as.integer(x))
ud['South East(%)']=lapply(ud['South East(%)'], function(x) as.numeric(x))
ud['Great Britain(%)']=lapply(ud['Great Britain(%)'], function(x) as.numeric(x))

ageBreakdown=function(age){
  tt=postForm("https://www.nomisweb.co.uk/reports/lmp/la/2038431803/subreports/ccadr_time_series/report.aspx", .encoding='utf-8', pivot=age)
  tables <- readHTMLTable(tt,stringsAsFactors = FALSE,elFun=commaReplace ) #encoding = "UTF-8")
  nn=gsub('([\n\t\r])','',names(tables$`NULL`))
  names(tables$`NULL`)=nn
  ud=tables$`NULL`
  ud['Isle of Wight']=lapply(ud['Isle of Wight'], function(x) as.integer(x))
  ud['South East(%)']=lapply(ud['South East(%)'], function(x) as.numeric(x))
  ud['Great Britain(%)']=lapply(ud['Great Britain(%)'], function(x) as.numeric(x))
  ud['tDate']=lapply(ud['Date'], function(x) as.Date(paste(28,x),"%d %b %Y"))
  ud
}

#ages:
#total: aget, adt1 (<6 months), adt2 (6-12 months), adt3 (>12 months)
#18-24: age1, ad1, ad2, ad3
#25-49: age2, ad4, ad5, ad6
#50+: age3, ad7, ad8, ad9

aget=ageBreakdown('aget')

myts <- ts(aget["Isle of Wight"], start=c(2006, 1), end=c(2013, 8), frequency=12)
plot(myts)
monthplot(myts)


aget$dd <- unlist(format(aget["tDate"],'%b'))


aget$dy <- unlist(format(aget["tDate"],'%Y'))
aget$dm <- unlist(format(aget["tDate"],'%b'))

ggplot(aget)+geom_line(aes_string(x='dy',y='`Isle of Wight`',group='dm'))+facet_grid(~dm)