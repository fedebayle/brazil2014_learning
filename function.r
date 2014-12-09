#a<-cv.glm(arg,glm(gf~0+elo+elo_dif,data=arg, family=poisson),K=10)
#a<-glm(gf~0+elo+elo_dif,data=arg, family=poisson)
#bos<-subset(data, data$pais=='Bosnia and Herzegovina')
#bos_xy<-subset(bos, select=c('elo', 'elo_dif', 'gf'))
#arg_xy<-subset(arg, select=c('elo', 'elo_dif', 'gf'))
#b<-bestglm(arg_xy, IC="CV", family=poisson, t=10)
library(glmnet)
set.seed(511)
setwd('C:\\Users\\fedebayle\\Desktop\\wc_final')
#setwd('C:\\Users\\BAYLEFE\\Desktop\\wc\\wc_final')
pais_wc<-c('Algeria',	'Argentina',	'Australia',	'Belgium',	'Bosnia and Herzegovina',	'Brazil',	'Cameroon',	'Chile',	'Colombia',	'Costa Rica',	'Croatia',	'Ecuador',	'England',	'France',	'Germany',	'Ghana',	'Greece',	'Honduras',	'Iran',	'Italy',	'Ivory Coast',	'Japan',	'Mexico',	'Netherlands',	'Nigeria',	'Portugal',	'Russia',	'South Korea',	'Spain',	'Switzerland',	'United States',	'Uruguay')
grupo_wc<-c('H','F','B','H','F','A','A','B','C','D','A','E','D','E','G','G','C','E','F','D','C','C','A','B','F','G','H','H','B','E','G','D') 
temp<-as.data.frame(cbind(pais_wc,grupo_wc))

datos<-read.table('input3.txt', sep='\t', header=TRUE, stringsAsFactors=FALSE)
for (i in seq(1,length(datos[,1]))){
	datos$avg_gf[i]<-mean(tail(datos$gf[datos$pais==datos$pais[i] & datos$n<i],10))
	if (datos$n[i]%%2==0){datos$avg_gc[i]<-mean(tail(datos$gc[datos$pais==datos$pais[datos$id==datos$id[i] & datos$pais!=datos$pais[i]] & datos$n<(i-1)],5))}else{datos$avg_gc[i]<-mean(tail(datos$gc[datos$pais==datos$pais[datos$id==datos$id[i] & datos$pais!=datos$pais[i]] & datos$n<(i-1)],5))
    }
	datos$avg_gc_p[i]<-mean(tail(datos$gc[datos$pais==datos$pais[i] & datos$n<i],5))
}
test<-NULL
for (i in seq(1,32)){
 test$pais[i]<-pais_wc[i]
 test$avg_gf[i]<-tail(datos$avg_gf[datos$pais==pais_wc[i]],1)
 test$avg_gc[i]<-tail(datos$avg_gc[datos$pais==pais_wc[i]],1)
 test$avg_gc_p[i]<-tail(datos$avg_gc_p[datos$pais==pais_wc[i]],1)
 }
 test<-as.data.frame(test)
first_round<-read.table('1era_ronda3.txt', sep='\t', header=TRUE, stringsAsFactors=FALSE)
for (i in seq(1,48)){
 first_round$avg_gf1[i]<-test$avg_gf[test$pais==first_round$Equipo1[i]]
 first_round$avg_gc1[i]<-test$avg_gc_p[test$pais==first_round$Equipo1[i]]
 first_round$avg_gf2[i]<-test$avg_gf[test$pais==first_round$Equipo2[i]]
 first_round$avg_gc2[i]<-test$avg_gc_p[test$pais==first_round$Equipo2[i]]
 }
first_round<-as.data.frame(first_round)
#write.table(first_round2,'prueba.txt', sep='\t')
data<-subset(datos, datos$año>1974)
data<-data[complete.cases(data),]
##########################################GOLES
goles<-function(data, npartido){
#equipo1
d<-subset(data, data$pais==first_round$Equipo1[npartido])
d_xy<-subset(d, select=c('elo', 'elo_dif','avg_gf','avg_gc' ,'gf'))
x<-as.matrix(d_xy[,1:4])
y<-as.matrix(d_xy[,5])
#d_xy<-subset(d, select=c('elo', 'elo_dif','avg_gc' ,'gf'))
#x<-as.matrix(d_xy[,1:3])
#y<-as.matrix(d_xy[,4])
cv<-cv.glmnet(x,y,family="poisson", nfolds=10,alpha=1)
l<-cv$lambda.min
alpha=1
#fit the model
p<-matrix(0, nrow=1, ncol=4)
#p<-matrix(0, nrow=1, ncol=3)
p[1,1]<-first_round$elo1[npartido]
p[1,2]<-first_round$elo_dif1[npartido]
p[1,3]<-first_round$avg_gf1[npartido]
p[1,4]<-first_round$avg_gc1[npartido]
#p[1,3]<-first_round$avg_gc1[npartido]
fits<- glmnet( x, y, family="poisson", alpha=alpha, nlambda=100)
res1 <- predict(fits, newx=p,s=l, type="response")
if (res1[1,1]-as.numeric(first_round$Equipo1_bet[npartido])>0){res1_bet<-res1[1,1]-as.numeric(first_round$Equipo1_bet[npartido])}else{res1_bet<-0}
pais1<-first_round$Equipo1[npartido]
if (res1[1,1]-as.numeric(first_round$Equipo1_bet[npartido])>0){goles_pais1<-round(mean(rpois(100000,(res1[1,1]-as.numeric(first_round$Equipo1_bet[npartido])))))}else{goles_pais1<-round(mean(rpois(100000,0)))}
#goles_pais1<-trunc(mean(rpois(100000,res1)))
#prob_pais1<-res1[1,1]
#equipo2
d<-subset(data, data$pais==first_round$Equipo2[npartido])
d_xy<-subset(d, select=c('elo', 'elo_dif','avg_gf','avg_gc' ,'gf'))
x<-as.matrix(d_xy[,1:4])
y<-as.matrix(d_xy[,5])
#d_xy<-subset(d, select=c('elo', 'elo_dif','avg_gc' ,'gf'))
#x<-as.matrix(d_xy[,1:3])
#y<-as.matrix(d_xy[,4])
cv<-cv.glmnet(x,y,family="poisson", nfolds=10,alpha=1)
l<-cv$lambda.min
alpha=1
#fit the model
p<-matrix(0, nrow=1, ncol=4)
#p<-matrix(0, nrow=1, ncol=3)
p[1,1]<-first_round$elo2[npartido]
p[1,2]<-first_round$elo_dif2[npartido]
p[1,3]<-first_round$avg_gf2[npartido]
p[1,4]<-first_round$avg_gc1[npartido]
#p[1,3]<-first_round$avg_gc1[npartido]
fits<- glmnet( x, y, family="poisson", alpha=alpha, nlambda=100)
res2 <- predict(fits, newx=p,s=l, type="response")
if (res2[1,1]-as.numeric(first_round$Equipo2_bet[npartido])>0){res2_bet<-res2[1,1]-as.numeric(first_round$Equipo2_bet[npartido])}else{res2_bet<-0}
pais2<-first_round$Equipo2[npartido]
if (res2[1,1]-as.numeric(first_round$Equipo2_bet[npartido])>0){goles_pais2<-round(mean(rpois(100000,(res2[1,1]-as.numeric(first_round$Equipo2_bet[npartido])))))}else{goles_pais2<-round(mean(rpois(100000,0)))}
#goles_pais2<-trunc(mean(rpois(100000,res2)))
#probas
i<-0
a<-NULL
b<-NULL
e<-0
eq1<-0
eq2<-0
prob<-NULL
while (i<10000){
  eq1<-rpois(1,res1_bet)
  eq2<-rpois(1,res2_bet)
  if (eq1>eq2){a[i]<-1}
  else
  if (eq1<eq2){b[i]<-1}
  i=i+1
}
a_per<-sum(a, na.rm=TRUE)/10000
b_per<-sum(b,na.rm=TRUE)/10000
#return
resultado<-NULL
resultado$pais[1]<-pais1
resultado$pais[2]<-pais2
resultado$lambda[1]<-res1_bet
resultado$prob[1]<-a_per
resultado$goles[1]<-goles_pais1
resultado$goles[2]<-goles_pais2
resultado$lambda[2]<-res2_bet
resultado$prob[2]<-b_per
resultado<-as.data.frame(resultado)
return(resultado)
}
############################################################
for (i in seq(1,48)){
 t<-goles(data,i)
 first_round$Equipo1_glmnet[i]<-t$goles[1]
 first_round$Equipo2_glmnet[i]<-t$goles[2]
 first_round$prob1_glmnet[i]<-t$prob[1]
 first_round$prob2_glmnet[i]<-t$prob[2]
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[1]){
	first_round$Equipo1_p[i]<-3
	first_round$Equipo2_p[i]<-0
	if (first_round$Equipo2_glmnet[i]==first_round$Equipo1_glmnet[i]){first_round$Equipo1_glmnet[i]<-as.numeric(first_round$Equipo2_glmnet[i])+1}
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[2]){
	first_round$Equipo2_p[i]<-3
	first_round$Equipo1_p[i]<-0
	if (first_round$Equipo2_glmnet[i]==first_round$Equipo1_glmnet[i]){first_round$Equipo2_glmnet[i]<-as.numeric(first_round$Equipo1_glmnet[i])+1}
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==(1-t$prob[1]-t$prob[2])){
	first_round$Equipo1_p[i]<-1
	first_round$Equipo2_p[i]<-1
	first_round$Equipo2_glmnet[i]<-first_round$Equipo1_glmnet[i]
	}
 }

##############################################
#partidos ya jugados
first_round$Equipo1_glmnet[1]<-3
first_round$Equipo2_glmnet[1]<-1
first_round$Equipo1_glmnet[2]<-1
first_round$Equipo2_glmnet[2]<-0
first_round$Equipo1_glmnet[3]<-0
first_round$Equipo2_glmnet[3]<-0
first_round$Equipo1_glmnet[7]<-1
first_round$Equipo2_glmnet[7]<-5
first_round$Equipo1_glmnet[8]<-3
first_round$Equipo2_glmnet[8]<-1
first_round$Equipo1_glmnet[13]<-3
first_round$Equipo2_glmnet[13]<-0
first_round$Equipo1_glmnet[14]<-2
first_round$Equipo2_glmnet[14]<-1
first_round$Equipo1_glmnet[19]<-1
first_round$Equipo2_glmnet[19]<-3
first_round$Equipo1_glmnet[20]<-1
first_round$Equipo2_glmnet[20]<-2
first_round$Equipo1_glmnet[25]<-2
first_round$Equipo2_glmnet[25]<-1
first_round$Equipo1_glmnet[26]<-3
first_round$Equipo2_glmnet[26]<-0
first_round$Equipo1_glmnet[31]<-2
first_round$Equipo2_glmnet[31]<-1
first_round$Equipo1_glmnet[32]<-0
first_round$Equipo2_glmnet[32]<-0
first_round$Equipo1_glmnet[37]<-4
first_round$Equipo2_glmnet[37]<-0
first_round$Equipo1_glmnet[38]<-1
first_round$Equipo2_glmnet[38]<-2
first_round$Equipo1_glmnet[43]<-2
first_round$Equipo2_glmnet[43]<-1
first_round$Equipo1_glmnet[44]<-1
first_round$Equipo2_glmnet[44]<-1
first_round$Equipo1_p[1]<-3
first_round$Equipo2_p[1]<-0
first_round$Equipo1_p[2]<-3
first_round$Equipo2_p[2]<-0
first_round$Equipo1_p[7]<-0
first_round$Equipo2_p[7]<-3
first_round$Equipo1_p[8]<-3
first_round$Equipo2_p[8]<-0
first_round$Equipo1_p[13]<-3
first_round$Equipo2_p[13]<-0
first_round$Equipo1_p[14]<-3
first_round$Equipo2_p[14]<-0
first_round$Equipo1_p[19]<-0
first_round$Equipo2_p[19]<-3
first_round$Equipo1_p[20]<-0
first_round$Equipo2_p[20]<-3
first_round$Equipo1_p[25]<-3
first_round$Equipo2_p[25]<-0
first_round$Equipo1_p[26]<-3
first_round$Equipo2_p[26]<-0
first_round$Equipo1_p[31]<-3
first_round$Equipo2_p[31]<-0
first_round$Equipo1_p[32]<-1
first_round$Equipo2_p[32]<-1
first_round$Equipo1_p[37]<-3
first_round$Equipo1_p[38]<-0
first_round$Equipo2_p[38]<-3
first_round$Equipo1_p[3]<-1
first_round$Equipo2_p[3]<-1
first_round$Equipo1_p[43]<-3
first_round$Equipo2_p[43]<-0
first_round$Equipo1_p[44]<-1
first_round$Equipo2_p[44]<-1
#######################################################
tabla<-NULL
 for (i in seq(1,32)){
	tabla$Equipo[i]<-pais_wc[i]
	tabla$puntos[i]<-sum(as.numeric(first_round$Equipo1_p[first_round$Equipo1==pais_wc[i]]),as.numeric(first_round$Equipo2_p[first_round$Equipo2==pais_wc[i]]))
	tabla$pj[i]<-sum(length(first_round$Equipo1[first_round$Equipo1==pais_wc[i]]),length(first_round$Equipo2[first_round$Equipo2==pais_wc[i]]))
	tabla$gf[i]<-sum(as.numeric(first_round$Equipo1_glmnet[first_round$Equipo1==pais_wc[i]]),as.numeric(first_round$Equipo2_glmnet[first_round$Equipo2==pais_wc[i]]))
	tabla$gc[i]<-sum(as.numeric(first_round$Equipo2_glmnet[first_round$Equipo1==pais_wc[i]]),as.numeric(first_round$Equipo1_glmnet[first_round$Equipo2==pais_wc[i]]))
	tabla$gd[i]<-tabla$gf[i]-tabla$gc[i]
	tabla$grupo[i]<-temp$grupo_wc[temp$pais_wc==pais_wc[i]]

 }
tabla<-as.data.frame(tabla,stringsAsFactors=FALSE)
tabla<-tabla[with(tabla,order(pj,puntos, gd, gf,gc,decreasing=TRUE)),]
#######################################################################################
#octavos
first_round$Equipo1[49]<-as.character(head(tabla$Equipo[tabla$grupo=='1'],2)[1])
first_round$Equipo2[49]<-as.character(head(tabla$Equipo[tabla$grupo=='2'],2)[2])
first_round$Equipo1[51]<-as.character(head(tabla$Equipo[tabla$grupo=='1'],2)[2])
first_round$Equipo2[51]<-as.character(head(tabla$Equipo[tabla$grupo=='2'],2)[1])
first_round$Equipo1[50]<-as.character(head(tabla$Equipo[tabla$grupo=='3'],2)[1])
first_round$Equipo2[50]<-as.character(head(tabla$Equipo[tabla$grupo=='4'],2)[2])
first_round$Equipo1[52]<-as.character(head(tabla$Equipo[tabla$grupo=='3'],2)[2])
first_round$Equipo2[52]<-as.character(head(tabla$Equipo[tabla$grupo=='4'],2)[1])
first_round$Equipo1[53]<-as.character(head(tabla$Equipo[tabla$grupo=='5'],2)[1])
first_round$Equipo2[53]<-as.character(head(tabla$Equipo[tabla$grupo=='6'],2)[2])
first_round$Equipo1[55]<-as.character(head(tabla$Equipo[tabla$grupo=='5'],2)[2])
first_round$Equipo2[55]<-as.character(head(tabla$Equipo[tabla$grupo=='6'],2)[1]) 
first_round$Equipo1[54]<-as.character(head(tabla$Equipo[tabla$grupo=='7'],2)[1])
first_round$Equipo2[54]<-as.character(head(tabla$Equipo[tabla$grupo=='8'],2)[2])
first_round$Equipo1[56]<-as.character(head(tabla$Equipo[tabla$grupo=='7'],2)[2])
first_round$Equipo2[56]<-as.character(head(tabla$Equipo[tabla$grupo=='8'],2)[1]) 
####ELO
for (i in seq(49,56)){
	first_round$elo1[i]<-as.numeric(head(first_round$elo1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$elo2[i]<-as.numeric(head(first_round$elo2[first_round$Equipo2==first_round$Equipo2[i]],1))
	first_round$elo_dif1[i]<-first_round$elo1[i]-first_round$elo2[i]
	first_round$elo_dif2[i]<--first_round$elo_dif1[i]
	first_round$avg_gc1[i]<-as.numeric(head(first_round$avg_gc1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$avg_gf1[i]<-as.numeric(head(first_round$avg_gf1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$avg_gc2[i]<-as.numeric(head(first_round$avg_gc2[first_round$Equipo1==first_round$Equipo2[i]],1))
	first_round$avg_gf2[i]<-as.numeric(head(first_round$avg_gf2[first_round$Equipo1==first_round$Equipo2[i]],1))
}

for (i in seq(49,56)){
 t<-goles(data,i)
 first_round$Equipo1_glmnet[i]<-t$goles[1]
 first_round$Equipo2_glmnet[i]<-t$goles[2]
 first_round$prob1_glmnet[i]<-t$prob[1]
 first_round$prob2_glmnet[i]<-t$prob[2]
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[1]){
	first_round$Equipo1_p[i]<-3
	first_round$Equipo2_p[i]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[2]){
	first_round$Equipo2_p[i]<-3
	first_round$Equipo1_p[i]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==(1-t$prob[1]-t$prob[2])){
	first_round$Equipo1_p[i]<-1
	first_round$Equipo2_p[i]<-1
	}	
 }
#######################################################################################
#cuartos
####57
if (first_round$prob1_glmnet[49]>first_round$prob2_glmnet[49]){first_round$Equipo1[57]<-first_round$Equipo1[49]}else {first_round$Equipo1[57]<-first_round$Equipo2[49]}
if (first_round$prob1_glmnet[50]>first_round$prob2_glmnet[50]){first_round$Equipo2[57]<-first_round$Equipo1[50]}else{first_round$Equipo2[57]<-first_round$Equipo2[50]}
####58
if (first_round$prob1_glmnet[53]>first_round$prob2_glmnet[53]){first_round$Equipo1[58]<-first_round$Equipo1[53]}else {first_round$Equipo1[58]<-first_round$Equipo2[53]}
if (first_round$prob1_glmnet[54]>first_round$prob2_glmnet[54]){first_round$Equipo2[58]<-first_round$Equipo1[54]}else {first_round$Equipo2[58]<-first_round$Equipo2[54]}
####59
if (first_round$prob1_glmnet[51]>first_round$prob2_glmnet[51]){first_round$Equipo1[59]<-first_round$Equipo1[51]}else {first_round$Equipo1[59]<-first_round$Equipo2[51]}
if (first_round$prob1_glmnet[52]>first_round$prob2_glmnet[52]){first_round$Equipo2[59]<-first_round$Equipo1[52]}else{first_round$Equipo2[59]<-first_round$Equipo2[52]}
####60
if (first_round$prob1_glmnet[55]>first_round$prob2_glmnet[55]){first_round$Equipo1[60]<-first_round$Equipo1[55]}else {first_round$Equipo1[60]<-first_round$Equipo2[55]}
if (first_round$prob1_glmnet[56]>first_round$prob2_glmnet[56]){first_round$Equipo2[60]<-first_round$Equipo1[56]}else {first_round$Equipo2[60]<-first_round$Equipo2[56]}
####ELO
for (i in seq(57,60)){
	first_round$elo1[i]<-as.numeric(head(first_round$elo1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$elo2[i]<-as.numeric(head(first_round$elo1[first_round$Equipo1==first_round$Equipo2[i]],1))
	first_round$elo_dif1[i]<-first_round$elo1[i]-first_round$elo2[i]
	first_round$elo_dif2[i]<--first_round$elo_dif1[i]
	first_round$avg_gc1[i]<-as.numeric(head(first_round$avg_gc1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$avg_gf1[i]<-as.numeric(head(first_round$avg_gf1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$avg_gc2[i]<-as.numeric(head(first_round$avg_gc2[first_round$Equipo1==first_round$Equipo2[i]],1))
	first_round$avg_gf2[i]<-as.numeric(head(first_round$avg_gf2[first_round$Equipo1==first_round$Equipo2[i]],1))
}

for (i in seq(57,60)){
 t<-goles(data,i)
 first_round$Equipo1_glmnet[i]<-t$goles[1]
 first_round$Equipo2_glmnet[i]<-t$goles[2]
 first_round$prob1_glmnet[i]<-t$prob[1]
 first_round$prob2_glmnet[i]<-t$prob[2]
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[1]){
	first_round$Equipo1_p[i]<-3
	first_round$Equipo2_p[i]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[2]){
	first_round$Equipo2_p[i]<-3
	first_round$Equipo1_p[i]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==(1-t$prob[1]-t$prob[2])){
	first_round$Equipo1_p[i]<-1
	first_round$Equipo2_p[i]<-1
	}	
 }
############
##semi
#61
if (first_round$prob1_glmnet[57]>first_round$prob2_glmnet[57]){first_round$Equipo1[61]<-first_round$Equipo1[57]}else {first_round$Equipo1[61]<-first_round$Equipo2[57]}
if (first_round$prob1_glmnet[58]>first_round$prob2_glmnet[58]){first_round$Equipo2[61]<-first_round$Equipo1[58]}else{first_round$Equipo2[61]<-first_round$Equipo2[58]}
####62
if (first_round$prob1_glmnet[59]>first_round$prob2_glmnet[59]){first_round$Equipo1[62]<-first_round$Equipo1[59]}else {first_round$Equipo1[62]<-first_round$Equipo2[59]}
if (first_round$prob1_glmnet[60]>first_round$prob2_glmnet[60]){first_round$Equipo2[62]<-first_round$Equipo1[60]}else {first_round$Equipo2[62]<-first_round$Equipo2[60]}

####ELO
for (i in seq(61,62)){
	first_round$elo1[i]<-as.numeric(head(first_round$elo1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$elo2[i]<-as.numeric(head(first_round$elo2[first_round$Equipo2==first_round$Equipo2[i]],1))
	first_round$elo_dif1[i]<-first_round$elo1[i]-first_round$elo2[i]
	first_round$elo_dif2[i]<--first_round$elo_dif1[i]
	first_round$avg_gc1[i]<-as.numeric(head(first_round$avg_gc1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$avg_gf1[i]<-as.numeric(head(first_round$avg_gf1[first_round$Equipo1==first_round$Equipo1[i]],1))
	first_round$avg_gc2[i]<-as.numeric(head(first_round$avg_gc2[first_round$Equipo2==first_round$Equipo2[i]],1))
	first_round$avg_gf2[i]<-as.numeric(head(first_round$avg_gf2[first_round$Equipo2==first_round$Equipo2[i]],1))
}
for (i in seq(61,62)){
 t<-goles(data,i)
 first_round$Equipo1_glmnet[i]<-t$goles[1]
 first_round$Equipo2_glmnet[i]<-t$goles[2]
 first_round$prob1_glmnet[i]<-t$prob[1]
 first_round$prob2_glmnet[i]<-t$prob[2]
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[1]){
	first_round$Equipo1_p[i]<-3
	first_round$Equipo2_p[i]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[2]){
	first_round$Equipo2_p[i]<-3
	first_round$Equipo1_p[i]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==(1-t$prob[1]-t$prob[2])){
	first_round$Equipo1_p[i]<-1
	first_round$Equipo2_p[i]<-1
	}	
 }

############
############
##63 3er
if (first_round$prob1_glmnet[61]<first_round$prob2_glmnet[61]){first_round$Equipo1[63]<-first_round$Equipo1[61]}else {first_round$Equipo1[63]<-first_round$Equipo2[61]}
if (first_round$prob1_glmnet[62]<first_round$prob2_glmnet[62]){first_round$Equipo2[63]<-first_round$Equipo1[62]}else{first_round$Equipo2[63]<-first_round$Equipo2[62]}

####ELO
first_round$elo1[63]<-as.numeric(head(first_round$elo1[first_round$Equipo1==first_round$Equipo1[63]],1))
first_round$elo2[63]<-as.numeric(head(first_round$elo2[first_round$Equipo2==first_round$Equipo2[63]],1))
first_round$elo_dif1[63]<-first_round$elo1[63]-first_round$elo2[63]
first_round$elo_dif2[63]<--first_round$elo_dif1[63]
first_round$avg_gc1[63]<-as.numeric(head(first_round$avg_gc1[first_round$Equipo1==first_round$Equipo1[63]],1))
first_round$avg_gf1[63]<-as.numeric(head(first_round$avg_gf1[first_round$Equipo1==first_round$Equipo1[63]],1))
first_round$avg_gc2[63]<-as.numeric(head(first_round$avg_gc2[first_round$Equipo2==first_round$Equipo2[63]],1))
first_round$avg_gf2[63]<-as.numeric(head(first_round$avg_gf2[first_round$Equipo2==first_round$Equipo2[63]],1))

t<-goles(data,63)
first_round$Equipo1_glmnet[63]<-t$goles[1]
first_round$Equipo2_glmnet[63]<-t$goles[2]
first_round$prob1_glmnet[63]<-t$prob[1]
first_round$prob2_glmnet[63]<-t$prob[2]
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[1]){
	first_round$Equipo1_p[63]<-3
	first_round$Equipo2_p[63]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[2]){
	first_round$Equipo2_p[63]<-3
	first_round$Equipo1_p[63]<-0
	} 
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==(1-t$prob[1]-t$prob[2])){
	first_round$Equipo1_p[63]<-1
	first_round$Equipo2_p[63]<-1
	}
############
##64
if (first_round$prob1_glmnet[61]>first_round$prob2_glmnet[61]){first_round$Equipo1[64]<-first_round$Equipo1[61]}else {first_round$Equipo1[64]<-first_round$Equipo2[61]}
if (first_round$prob1_glmnet[62]>first_round$prob2_glmnet[62]){first_round$Equipo2[64]<-first_round$Equipo1[62]}else
{first_round$Equipo2[64]<-first_round$Equipo2[62]}

####ELO
first_round$elo1[64]<-as.numeric(head(first_round$elo1[first_round$Equipo1==first_round$Equipo1[64]],1))
first_round$elo2[64]<-as.numeric(head(first_round$elo2[first_round$Equipo2==first_round$Equipo2[64]],1))
first_round$elo_dif1[64]<-first_round$elo1[64]-first_round$elo2[64]
first_round$elo_dif2[64]<--first_round$elo_dif1[64]
first_round$avg_gc1[64]<-as.numeric(head(first_round$avg_gc1[first_round$Equipo1==first_round$Equipo1[64]],1))
first_round$avg_gf1[64]<-as.numeric(head(first_round$avg_gf1[first_round$Equipo1==first_round$Equipo1[64]],1))
first_round$avg_gc2[64]<-as.numeric(head(first_round$avg_gc2[first_round$Equipo2==first_round$Equipo2[64]],1))
first_round$avg_gf2[64]<-as.numeric(head(first_round$avg_gf2[first_round$Equipo2==first_round$Equipo2[64]],1))

 t<-goles(data,64)
 first_round$Equipo1_glmnet[64]<-t$goles[1]
 first_round$Equipo2_glmnet[64]<-t$goles[2]
 first_round$prob1_glmnet[64]<-t$prob[1]
 first_round$prob2_glmnet[64]<-t$prob[2]
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[1]){
	first_round$Equipo1_p[64]<-3
	first_round$Equipo2_p[64]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==t$prob[2]){
	first_round$Equipo2_p[64]<-3
	first_round$Equipo1_p[64]<-0
	}
 if (max(t$prob[1],t$prob[2],(1-t$prob[1]-t$prob[2]))==(1-t$prob[1]-t$prob[2])){
	first_round$Equipo1_p[64]<-1
	first_round$Equipo2_p[64]<-1
	}	 

#tabla final
tabla<-NULL
 for (i in seq(1,32)){
	tabla$Equipo[i]<-pais_wc[i]
	tabla$puntos[i]<-sum(as.numeric(first_round$Equipo1_p[first_round$Equipo1==pais_wc[i]]),as.numeric(first_round$Equipo2_p[first_round$Equipo2==pais_wc[i]]))
	tabla$pj[i]<-sum(length(first_round$Equipo1[first_round$Equipo1==pais_wc[i]]),length(first_round$Equipo2[first_round$Equipo2==pais_wc[i]]))
	tabla$gf[i]<-sum(as.numeric(first_round$Equipo1_glmnet[first_round$Equipo1==pais_wc[i]]),as.numeric(first_round$Equipo2_glmnet[first_round$Equipo2==pais_wc[i]]))
	tabla$gc[i]<-sum(as.numeric(first_round$Equipo2_glmnet[first_round$Equipo1==pais_wc[i]]),as.numeric(first_round$Equipo1_glmnet[first_round$Equipo2==pais_wc[i]]))
	tabla$gd[i]<-tabla$gf[i]-tabla$gc[i]
	tabla$grupo[i]<-temp$grupo_wc[temp$pais_wc==pais_wc[i]]

 }
tabla<-as.data.frame(tabla,stringsAsFactors=FALSE)
tabla<-tabla[with(tabla,order(pj,puntos, gd, gf,gc,decreasing=TRUE)),]
#write.table(tabla,'2006_glmnet.txt' ,sep='\t')
