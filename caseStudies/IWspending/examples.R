iw <- read.csv("~/code/Rcode/eduDemos1/caseStudies/IWspending/IWccl2012_13_TransparencyTest4.csv")

#Cast to date type to support plots
iw$Date=as.Date(iw$Date)

##EXAMPLE SUBSETS

#Search for suppliers
sw=subset(iw,grepl('SOUTHERN', iw$SupplierNameClustered))

#More specific supplier search
se=subset(iw, SupplierNameClustered=='SOUTHERN ELECTRIC')


#Crude example plot to highlight matched +/1 amounts
se1a=subset(se,ServiceArea=='Ryde Town Hall' & Amount>0)
se1b=subset(se,ServiceArea=='Ryde Town Hall' & Amount<=0)

require(ggplot2)
ggplot()+geom_point(data=se1a, aes(x=Date,y=Amount),pch=1,size=1)+geom_point(data=se1b, aes(x=Date,y=-Amount),size=3,col='red',pch=2)+ggtitle('Ryde Town Hall - Energy Payments to Southern Electric (red=-Amount)')+xlab(NULL)+ylab('Amount (£)')

ggplot(se1a)+geom_point( aes(x=Date,y=Amount),pch=1,size=1)+geom_point(data=se1b, aes(x=Date,y=-Amount),size=3,col='red',pch=2)+facet_wrap(~ExpensesType)+ggtitle('Ryde Town Hall - Energy Payments to Southern Electric (red=-Amount)')+xlab(NULL)+ylab('Amount (£)')

#Example  - supplier with most transactions in a set
library(plyr)
tt=data.frame(table(subset(iw,Amount<0,select="SupplierNameClustered")))
head(arrange(subset(tt,Freq>0),desc(Freq)))

#Find the unique negative amounts
nse1b=-unique(se1b$Amount)
#Find the unique positive amounts
pse1a=unique(se1a$Amount)
#Find matching positive and negative amounts
balitems=intersect(nse1b,pse1a)
#Subset the data based on balanced positive and negative amounts
bals=subset(se,abs(Amount) %in% balitems)

#Group the data by sorting
arrange(bals,abs(Amount))


##Find which suppliers were involved in most number of negative payments
#Identify negative payments
nn=subset(iw,Amount<=0)
#Count the number of each unique supplier
nnd=data.frame(table(nn$SupplierNameClustered))
#Display the suppliers with the largest count of negative payments
head(arrange(nnd,-Freq))

#Display most common payment values
commonAmounts=function(df,bcol='Amount',num=5){
  head(arrange(data.frame(table(df[[bcol]])),-Freq),num)
}

ca=commonAmounts(iw)

#We can then get the rows corresponding to these common payments
commonPayments=function(df,bcol,commonAmounts){
  df[abs(df[[bcol]]) %in% commonAmounts,]
}
xx=commonPayments(iw,"Amount",ca$Var1)

#Let's combine those
commonReport=function(df,bcol="Amount"){
  ca.list=commonAmounts(df)
  commonPayments(df,bcol,ca.list$Var1)
}

cx=commonReport(iw)

#R does pivot tables...
#We can also run summary statistics over those common payment rows.
zz1=aggregate(index ~ ServiceArea + Amount, data =xx, FUN="length")
head(arrange(zz1,-index))

zz2=aggregate(index ~ ServiceArea +ExpensesType+ Amount, data =xx, FUN="length")
head(arrange(zz2,-index))

zz3=aggregate(index ~ SupplierName+ Amount, data =xx, FUN="length")
head(arrange(zz3,-index))

zz4=aggregate(index ~ SupplierNameClustered+ Amount, data =xx, FUN="length")
head(arrange(zz4,-index))


overpayments=function(df,fcol,bcol='Amount',num=5){
  df=subset(df, grepl('((overpay)|(refund))', df[[fcol]],ignore.case=T))
  #head(arrange(data.frame(table(df[[bcol]])),-Freq),num)
  df[ order(df[,bcol]), ]
}

rf=overpayments(iw,'ExpensesType')

balanced.items=function(df,bcol){
  #Find the positive amounts
  positems=df[ df[[bcol]]>0, ]
  #Find the negative amounts
  negitems=df[ df[[bcol]]<=0, ]
  
  #Find the absolute unique negative amounts
  uniqabsnegitems=-unique(negitems[[bcol]])
  #Find the unique positive amounts
  uniqpositems=unique(positems[[bcol]])
  
  #Find matching positive and negative amounts
  balitems=intersect(uniqabsnegitems,uniqpositems)
  #Subset the data based on balanced positive and negative amounts
  #bals=subset(se,abs(Amount) %in% balitems)
  bals=df[abs(df[[bcol]]) %in% balitems,]
  #Group the data by sorting, largest absolute amounts first
  bals[ order(-abs(bals[,bcol])), ]
}

dse=balanced.items(se,'Amount')


for (i in ll) {
  g=ggplot(subset(iw,Directorate==i))+geom_line(aes(x=Date,y=cumAmount))+ggtitle(i)
  print(g)
}

iw=ddply(iw,.(Directorate),mutate,dirCumAmount=cumsum(Amount))

ggplot(iw)+geom_line(aes(x=Date,y=dirCumAmount,group=Directorate,col=Directorate))

