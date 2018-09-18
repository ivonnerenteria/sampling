library(sampling)
library(survey)
library(grid)
data(api)
attach(apipop)

table(apipop$stype)

esc_str_E=apipop[apipop[, "stype"]=="E", ]
esc_str_H=apipop[apipop[, "stype"]=="H", ]
esc_str_M=apipop[apipop[, "stype"]=="M", ]

dim(table(esc_str_E$dnum))
dim(table(esc_str_H$dnum))
dim(table(esc_str_M$dnum))

numEsc_E=cbind(table(esc_str_E$dnum)[1:669])
probE=cbind(numEsc_E,prob=numEsc_E[1:669,1]/4421)
nprobE=probE[1:669,2]

numEsc_H=cbind(table(esc_str_H$dnum)[1:355])
probH=cbind(numEsc_H,prob=numEsc_H[1:355,1]/755)
nprobH=probH[1:355,2]

numEsc_M=cbind(table(esc_str_M$dnum)[1:445])
probM=cbind(numEsc_M,prob=numEsc_M[1:445,1]/1018)
nprobM=probM[1:445,2]

#Sacando la muestra de los 10 distritos del estrato de colegios tipo E
auxE=cluster(esc_str_E,clustername=c("dnum"), 10,description=T,pik=nprobE)
auxH=cluster(esc_str_H,clustername=c("dnum"), 5,description=T,pik=nprobH)
auxM=cluster(esc_str_M,clustername=c("dnum"), 5,description=T,pik=nprobM)     

#Pesos de muestreo y factor de correccion
muestraCE=getdata(apipop, auxE)
mmE=dim(auxE)[1]
CE=data.frame(fpc=rep(669,mmE),pw=rep(4421/mmE,mmE))
muestraCE=cbind(numc=1:mmE,muestraCE,CE)

muestraCH=getdata(apipop, auxH)
mmH=dim(auxH)[1]
CH=data.frame(fpc=rep(355,mmH),pw=rep(755/mmH,mmH))
muestraCH=cbind(numc=1:mmH,muestraCH,CH)

muestraCM=getdata(apipop, auxM)
mmM=dim(auxM)[1]
CM=data.frame(fpc=rep(445,mmM),pw=rep(1018/mmM,mmM))
muestraCM=cbind(numc=1:mmM,muestraCM,CM)

unidades=rbind(muestraCE,muestraCH,muestraCM)
svydesign(id=~dnum, strata=~stype, fpc=~fpc, weights=~pw, data= unidades,nest=TRUE)

congloT=svydesign(id=~dnum, strata=~stype, fpc=~fpc,weights=~pw,data=unidades,nest=TRUE)
svymean(~api00,congloT)

conglo1=svydesign(id=~dnum, fpc=~fpc, weights=~pw, data=muestraCE)
svymean(~api00,conglo1)

conglo2=svydesign(id=~dnum, fpc=~fpc, weights=~pw, data=muestraCH)
svymean(~api00,conglo2)

conglo3=svydesign(id=~dnum, fpc=~fpc, weights=~pw, data=muestraCM)
svymean(~api00,conglo3)

svyquantile(~api00,congloT, 0.67)
