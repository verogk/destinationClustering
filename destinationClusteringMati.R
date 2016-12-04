setwd("/home/despegar/rstudio_data/vseminario/Destino Caro o Barato/")

library(rjson)
library(data.table)
library(reshape2)
library(FactoMineR)
library(ggplot2)
library(geosphere)
library(flexclust)
library(gridExtra)
library(fossil)

### Sherman data ####
# Aca se generan tres tablas.
# - averageables.dt tiene formato largo (melt) y en cada fila el id del destino y un tag. 
#   hospitality, gastronomy, nightlife, naturalwealth, culturalattractions, experience
#   El destino recibe un rating por cada tag entonces puede promediarse.
#   Un solo review genera varias de estas filas, ya que es una fila por tag.
# - additives.dt tiene formato largo (melt) y en cada fila el id del destino y un tag. 
#   Un solo review genera varias de estas filas, ya que es una fila por tag.
#   la gente elige los tags que corresponden al destino entonces hay que hacer un count.
# - dest.dt tiene un overall rating para el destino.

dest.json = fromJSON(file="http://10.2.7.6/v3/reviews/summaries?type=city&snapshot")

dest.dt = data.frame(item_id=character(),review_count=numeric(),overall_rating=numeric(),stringsAsFactors=FALSE)
averageables.dt = data.frame(item_id=character(),code=character(),count=numeric(),rating=numeric(),stringsAsFactors=FALSE)
additives.dt = data.frame(item_id=character(),code=character(),count=numeric(),stringsAsFactors=FALSE)

count.averageables = 1
count.additives = 1

n = length(dest.json)

for (i in 1:n){
  
  dest.dt[i,]$item_id = dest.json[[i]]$item_id
  dest.dt[i,]$review_count = dest.json[[i]]$review_count
  dest.dt[i,]$overall_rating = dest.json[[i]]$qualifications$overall_rating
  
  if (length(dest.json[[i]]$qualifications$averageables)>0) {
    print(i)
    for (j in 1:length(dest.json[[i]]$qualifications$averageables)){
      averageables.dt[count.averageables,]$item_id = dest.json[[i]]$item_id
      averageables.dt[count.averageables,]$code = dest.json[[i]]$qualifications$averageables[[j]]$code
      averageables.dt[count.averageables,]$count = dest.json[[i]]$qualifications$averageables[[j]]$count
      averageables.dt[count.averageables,]$rating = dest.json[[i]]$qualifications$averageables[[j]]$rating
      count.averageables = count.averageables + 1
      
    }
  }
  
  if (length(dest.json[[i]]$qualifications$additives)>0) {
    print(i)
    for (j in 1:length(dest.json[[i]]$qualifications$additives)){
      additives.dt[count.additives,]$item_id = dest.json[[i]]$item_id
      additives.dt[count.additives,]$code = dest.json[[i]]$qualifications$additives[[j]]$code
      additives.dt[count.additives,]$count = dest.json[[i]]$qualifications$additives[[j]]$count
      count.additives = count.additives + 1
      
    }
  }
}

### GEO data ####
# tablita con id de destino, IATA, latitud, longitud
geo.json = fromJSON(file="http://10.2.7.6/geo-services-web/service/webcontext/city/simplified/allCities")$data
geo.unlist = unlist(geo.json)

geo.colnames = c("oid","iataCode","description.ES","code","countryOid","latitude","longitude")
geo.columns = c("oid","iataCode","description.ES","code","countryOid","^latitude$","^longitude$")
geo.dt = data.frame(tabulateJSON(geo.unlist,"oid", geo.columns, geo.colnames))
geo.dt[,c(1,5:7)] = apply(geo.dt[,c(1,5:7)], 2, function(x) as.numeric(as.character(x)))

### Long to wide ####
dest.dt = data.table(dest.dt)
additives.dt = data.table(additives.dt)
averageables.dt = data.table(averageables.dt)
geo.dt = data.table(geo.dt)

setnames(geo.dt,c("oid"),c("item_id"))
geo.dt$item_id = as.character(geo.dt$item_id)

# dcast para los atributos que se cuentan. Por default agrega por length
additives.count = data.table(dcast(additives.dt,item_id ~ code,value.var="count",fill=0 ))

# dcast para los atributos que se promedian por ser ratings. Por default agrega por length
# empiezo contandolos y luego se divide por el total de ratings para ese destino. 
averageables.count = data.table(dcast(averageables.dt,item_id ~ code,value.var="count",fill=0 ))
averageables.rating = data.table(dcast(averageables.dt,item_id ~ code,value.var="rating",fill=NA ))

### Limpio additives ####

# Costo del viaje
# como porcentaje de gente que hizo review del costo para ese destino
# primero cuento el total de gente que hizo review para ese destino. 
# luego divido la cantidad de ocurrencia de un tag particular por ese total
setnames(additives.count, c("less20usd","20or50usd","50or100usd","100or200usd","200or400usd","400or600usd","more600usd"),
         c("costless20usd","cost20or50usd","cost50or100usd","cost100or200usd","cost200or400usd","cost400or600usd","costmore600usd"))
additives.count[,cost.reviews := costless20usd + cost20or50usd + cost50or100usd + cost100or200usd + cost200or400usd + cost400or600usd + costmore600usd]
additives.cost = additives.count[,list(item_id,
                                       costless20usd=costless20usd/cost.reviews,
                                       cost20or50usd=cost20or50usd/cost.reviews,
                                       cost50or100usd=cost50or100usd/cost.reviews,
                                       cost100or200usd=cost100or200usd/cost.reviews,
                                       cost200or400usd=cost200or400usd/cost.reviews,
                                       cost400or600usd=cost400or600usd/cost.reviews,
                                       costmore600usd=costmore600usd/cost.reviews)]

# columna adicional con el promedio ponderado 
# (valor medio del rango * cantidad de gente que declaro ese costo para ese destino)
additives.cost[,weighted.cost := costless20usd*10 + cost20or50usd*35 + 
                 cost50or100usd*75 + cost100or200usd*150 + cost200or400usd*300 + 
                 cost400or600usd*500+ costmore600usd*1000]

# Tiempo de estadia
setnames(additives.count,c("2-3day","4-6day","1Week","10day","2weeks","1month","morethanamonth"),c("stay2to3day","stay4to6day","stay1Week","stay10day","stay2weeks","stay1month","staymorethanamonth"))
additives.count[,stay.reviews := stay2to3day + stay4to6day + stay1Week + stay10day + stay2weeks + stay1month + staymorethanamonth]
additives.stay = additives.count[,list(item_id,
                                       stay2to3day =  stay2to3day/stay.reviews,
                                       stay4to6day = stay4to6day/stay.reviews,
                                       stay1Week = stay1Week/stay.reviews,
                                       stay10day = stay10day/stay.reviews,
                                       stay2weeks = stay2weeks/stay.reviews,
                                       stay1month = stay1month/stay.reviews,
                                       staymorethanamonth = staymorethanamonth/stay.reviews)]

# columna adicional con el promedio ponderado 
# (valor medio del rango * cantidad de gente que declaro ese costo para ese destino)
additives.stay[,weighted.stay := stay2to3day*2 + stay4to6day*4.5 + stay1Week*7 + stay10day*10 + stay2weeks*15 + stay1month*30 + staymorethanamonth*90]

# Composicion de pasajeros
additives.count[,travelertype.reviews := businessTrip + couple + family + singles + friends]
additives.travelertype = additives.count[,list(item_id,
                                               businessTrip =  businessTrip/travelertype.reviews,
                                               couple = couple/travelertype.reviews,
                                               family = family/travelertype.reviews,
                                               singles = singles/travelertype.reviews,
                                               friends = friends/travelertype.reviews)]

### Limpio averageables ####
# Veo si hay correlacion entre total de reviews en averageable y additives#
# averageables.max es una tabla que me dice, para cada destino, el maximo de reviews. 
# En general la gente da un rating para todas las categorias pero hay unos pocos casos en que solo ratearon algunas.
# el "1" del apply es para que se aplique el maximo para cada fila. 
averageables.max=data.table(item_id=as.character(averageables.count$item_id),max.count=apply(averageables.count[,c(-1),with=FALSE],1,max))

total.reviews = merge(averageables.max,additives.count[,list(item_id,stay.reviews)],by="item_id",all=T)
ggplot(data=total.reviews,aes(max.count,stay.reviews))+geom_jitter()
lm(stay.reviews~max.count,data=total.reviews)

setnames(averageables.rating,"nightlife","nightlife.av")

### Filtro de destinos con pocos reviews ####

#Grafico cantidad de reviews por destino
reviews.acumulados = data.table(destinations.cum=cumsum(table(additives.count$cost.reviews)),
                                destinations.cum.prop=cumsum(table(additives.count$cost.reviews))/sum(table(additives.count$cost.reviews)),
                                destinations=table(additives.count$cost.reviews),
                                reviews=as.numeric(names(table(additives.count$cost.reviews))))

qplot(log(additives.count$cost.reviews+1), binwidth = 0.5) +
  xlab('log(Reviews+1)') +
  ylab('Cantidad de destinos')

ggplot(data=reviews.acumulados, aes(x=reviews, y=destinations.cum.prop)) +
  geom_line() +
  xlab('Reviews por destino') +
  ylab('Acumulado') 

quantile(additives.count$cost.reviews,probs=seq(0,1,by=0.1))

#Copio la cantidad de reviews para compararlas en un excel. No hay grandes diferencias.
write.csv(additives.count[,list(item_id,cost.reviews,stay.reviews,travelertype.reviews)],file="count_reviews.csv",row.names=F)

# me quedo con los destinos para los que tengo al menos 100 reviews de costo y cuyo IATA esta en la tabla de geo
item_id.keep = additives.count[cost.reviews>=100 & item_id %in% geo.dt$item_id]$item_id

### Others - Aplico componentes principales ####
# Otras caracteristicas - aplicar princomp

additives.others = additives.count[,setdiff(names(additives.count),c("stay2to3day","stay4to6day","stay1Week","stay10day","stay2weeks","stay1month","staymorethanamonth","stay.reviews",
                                                                     "businessTrip","couple","family","singles","friends",
                                                                     "costless20usd","cost20or50usd","cost50or100usd","cost100or200usd","cost200or400usd","cost400or600usd","costmore600usd",
                                                                     "cost.reviews","stay.reviews","travelertype.reviews")),
                                   with=FALSE]

# Me quedo con las columnas que se preguntan hoy en la encuesta
additives.others=additives.others[,list(item_id,
                                        adventure,
                                        archeology,
                                        fishing,
                                        hiking,
                                        lake,
                                        mountain,
                                        nature,
                                        openair,
                                        relax,
                                        ski,
                                        diving,
                                        extremesports,
                                        rio,
                                        snorkel,
                                        sunandsea,
                                        surf,
                                        watersports,
                                        amusement,
                                        architecture,
                                        art,
                                        carnival,
                                        casino,
                                        city,
                                        culturaltours,
                                        ement,
                                        exhibitions,
                                        gastronomic,
                                        gateways,
                                        historiccentre,
                                        livemusic,
                                        museum,
                                        nightlife,
                                        religion,
                                        Shop,
                                        theatre,
                                        tourism,
                                        wines,
                                        honeymoon,
                                        hotsprings,
                                        romantic)]


# Convierto los count de los Others en porcentajes
# queda la cantidad que aparece cada tag como porcentaje del total de tags (max).
additives.others = cbind(additives.others[,list(item_id)],additives.others[,c(-1),with=FALSE]/apply(additives.others[,c(-1),with=FALSE],1,max))
# agrego el IATA
additives.others = merge(additives.others,geo.dt[,list(item_id,iataCode=as.character(iataCode))],by="item_id",all.x=T)

#pone NA 1, NA 2, numerando los IATAS que no estan 
additives.others$iataCode[is.na(additives.others$iataCode)] = paste0(rep("NA",sum(is.na(additives.others$iataCode))),1:sum(is.na(additives.others$iataCode)),sep="")

rownames(additives.others) = additives.others$iataCode
additives.others[,iataCode := NULL] # AAAAH POR QUE BORRA EL IATA??

# Me quedo solo con los tags que elegi quedarme por tener n grande.
additives.others = additives.others[item_id %in% item_id.keep,]

# PCA with function princomp (principal components analysis) 
# PCA es un analisis que reduce la cantidad de variables cuando hay alta correlacion entre las variables
# y me deja una combinacion lineal de un subset de variables que no estan correlacionasas
# el subset de variables es tal qe minimiza la suma de residuos al cuadrado (la distancia al cuadrado entre los puntos y la proyeccion)
pca.additives.others = princomp(additives.others[,c(-1),with=FALSE], cor = T)

# sqrt of eigenvalues (armo un vector de eigenvalores que son los coeficientes que dictan la combinacion lineal de 
# las variables que maximiza la varianza total)
# PCA genera una matriz donde ordena las variables de acuerdo a la fraccion total de la varianza que explican
pca.additives.others.eigen = data.frame(eigen.prop=pca.additives.others$sdev^2/sum(pca.additives.others$sdev^2),names=names(pca.additives.others$sdev))

barplot(pca.additives.others.eigen$eigen.prop[1:10],names.arg=pca.additives.others.eigen$names[1:10],col="dodgerblue2")
pca.additives.others.eigen$eigen[1:15] 
cumsum(pca.additives.others.eigen$eigen[1:15])
# esto me muestra que las primeras 15 explican el 84% de la varianza

# loadings
# elresultado del PCA es una lista de vectores (los principal components)
# formados por los coefficientes que definen cada componente, llamados "loadings"
unclass(pca.additives.others$loadings)
write.csv(unclass(pca.additives.others$loadings),"pca_additivies_others_loadings.csv")

# PCs (aka scores)
head(pca.additives.others$scores)
scores = data.table(pca.additives.others$scores)

# plot of observations
ggplot(data = scores, aes(x = Comp.1, y = Comp.2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(colour = "tomato") +
  ggtitle("Principal component 1 vs. principal component 2")


#Grafico de varianza por fila

stay.volatility = apply(additives.stay[,2:(ncol(additives.stay)-1),with=FALSE],1,sd)
cost.volatility = apply(additives.cost[,2:(ncol(additives.cost)-1),with=FALSE],1,sd)
others.volatility = apply(additives.others[,2:ncol(additives.others),with=FALSE],1,sd)
travelertype.volatility = apply(additives.travelertype[,2:ncol(additives.travelertype),with=FALSE],1,sd)
volatility.dt = data.table(stay.volatility=stay.volatility,
                           cost.volatility=cost.volatility,
                           others.volatility=others.volatility,
                           travelertype.volatility=travelertype.volatility,
                           sample.size=additives.count$stay.reviews)
volatility.melt = melt(data = volatility.dt[sample.size>0],id="sample.size",measure=c("stay.volatility",
                                                                                      "cost.volatility",
                                                                                      "others.volatility",
                                                                                      "travelertype.volatility"),
                       variable.name="Dimension",value.name="SD")

ggplot(data = volatility.melt, aes(x=sample.size, y=SD, color = Dimension)) + geom_point()+ geom_smooth(se=FALSE) #Esto no me dice mucho.
ggplot(data = volatility.melt, aes(x=log(sample.size), y=SD, color = Dimension)) + geom_point()+ geom_smooth(se=FALSE) #Esto no me dice mucho.

### Merging ####
#Hago un merge de todo#

#dest.dt con geo.dt
all.dt = merge(dest.dt,geo.dt,by="item_id",all.x=T)
#all con additives.cost
all.dt = merge(all.dt,additives.cost,by="item_id",all.x=T)
#all con additives.stay
all.dt = merge(all.dt,additives.stay,by="item_id",all.x=T)
#all con additives.travelertype
all.dt = merge(all.dt,additives.travelertype,by="item_id",all.x=T)
#all con averageables.rating
all.dt = merge(all.dt,averageables.rating,by="item_id",all.x=T)
#all con others
all.dt = merge(all.dt,additives.others,by="item_id",all.x=T)

all.dt.filt = all.dt[item_id %in% item_id.keep]
rownames(all.dt.filt) <- all.dt$iataCode[row.keep]

#Separo las filas con las que me voy a quedar
row.keep = (1:nrow(all.dt))[all.dt$item_id %in% item_id.keep]

### Distancia geodesica ####

distVincentyEllipsoid(p1=, p2, a=6378137, b=6356752.3142, f=1/298.257223563)

#Creo la matriz de distancia geod??sica
p1 = all.dt[item_id==6738,list(longitude,latitude)]
p2 = all.dt[item_id==1431,list(longitude,latitude)]
distVincentySphere(p1,p2)/1000

geo.distances = distm(all.dt.filt[,list(longitude,latitude)], fun=distVincentySphere)/1000
rownames(geo.distances) = all.dt.filt$item_id
colnames(geo.distances) = all.dt.filt$item_id

geo.cmd = cmdscale(geo.distances,eig=TRUE,k=2)
geo.cmd.points = data.table(geo.cmd$points)

ggplot(data = geo.cmd.points, aes(x = V1, y = V2, label = all.dt.filt$iataCode)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  #geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  geom_text(colour = all.dt.filt$countryOid, alpha = 0.8, size = 4) +
  ggtitle("Geo Multidimensional Scaling")

### Actividad del usuario ####

path="/home/despegar/rstudio_data/mroqueta/recommenders/"
files <- list.files(path=path,pattern=".txt")

pre_proc.raw <- read.table(paste0(path,files[1]),header=T,nrows=1000,sep="\t",blank.lines.skip=T,fill=T)

sapply(pre_proc.raw,class)

classes <- rep("NULL", ncol(pre_proc.raw))

classes[c(1:3,5,6,11)] <- "factor"

rm(pre_proc.raw)

clus <- makeCluster(10)
clusterExport(clus,c("classes","fread"))
raws <- parLapply(clus,files, function(x) fread(paste0("/home/despegar/rstudio_data/mroqueta/recommenders/",x),T,sep="\t",colClasses=classes))
stopCluster(clus)

#raw.full <- fread(files[1],T,sep="\t",colClasses=classes)

raw.full <- rbindlist(raws)
#rm(raws)
#raw.full$ref = NULL
gc()

setnames(raw.full,names(raw.full),c("FECHA","HORA","userid","flow","country","ref","destino_IATA","ip"))

#Elimino todos los destinos que son unicamente numeros o "," y me agregar ruido
raw.full <- raw.full[-grep("^[^A-Z]+$",destino_IATA),]
#raw.full <- raw.full[-grep(",",destino_IATA),]

#Elimino las IPs de bots y trafico interno
filtros.ip <- "^200.32.121.(6[4-9])|7[0-9])$|^200.0.230.(23[2-9])$|^200.55.16.(24[0-7])$|^200.43.6.(24[0-7])$|^201.234.94.(22[4-9]|23[0-9])$|^200.55.59.([1-9]|1[0-9]|2[0-9]|3[0-1])$|^200.49.12.(19[2-9]|20[0-7])$|^200.123.184.(13[2-5])$^201.234.29.(4[8-9]|5[0-9]|6[0-3])$|^200.127.144.(17[6-9]|18[0-9]|19[0-1])$|^190.221.176.(20[8-9]|21[0-5])$^181.30.6.(3[2-9])$|^200.80.130.(3[2-9]|4[0-9]|5[0-9]|6[0-3])$|^200.155.143.(1[6-9]|2[0-3])$|^189.57.188.(1[6-9]|2[0-3])$^186.231.66.(4[0-7])$|^201.46.247.([0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]|6[0-9]|7[0-9]|8[0-9]|9[0-9]|10[0-9]|11[0-9]|12[0-9]|13[0-9]|14[0-9]|15[0-9]|16[0-9]|17[0-9]|18[0-9]|19[0-9]|20[0-9]|21[0-9]|22[0-9]|23[0-9]|24[0-9]|25[0-5])$^200.155.143.([0-9]|1[0-5])$|^201.85.18.(14[4-9]|15[0-9])$|^200.183.52.(12[8-9]|13[0-5])$|^177.124.244.(9[6-9]|10[0-3])$|^200.124.198.(24[4-7])$|^200.124.201.(8[8-9]|9[0-5])$|^200.124.205.(20[8-9]|21[0-5])$^200.71.5.(17[6-9]|18[0-3])$|^190.126.205.(19[2-9])$|^190.145.29.(14[4-9]|15[0-1])$|^190.60.92.([8-9]|1[0-5])$|^181.48.63.(6[4-9]|7[0-9]|8[0-9]9[0-5])$|^189.204.79.(12[8-9]|13[0-5])$^201.161.8.(8[0-9]|9[0-5])$^190.196.5.(5[6-9]|6[0-3])$|^190.82.118.(5[6-9]|6[0-3])$|^200.26.165.(14[4-9]|15[0-1])$|^200.11.213.(23[2-9])$|^75.149.245.(9[6-9]|10[0-3])$|^181.65.156.(17[6-9]|18[0-9]|19[0-1])$|^190.12.9.(8[0-7])$|^186.66.135.(19[2-9])$^186.176.203.(15[2-9])$|^64.76.243.([0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]|6[0-9]|7[0-9]|8[0-9]|9[0-9]|10[0-9]|11[0-9]|12[0-9]|13[0-9]|14[0-9]|15[0-9]|16[0-9]|17[0-9]|18[0-9]|19[0-9]|20[0-9]|21[0-9]|22[0-9]|23[0-9]|24[0-9]|25[0-5])$|^200.127.158.(4[8-9]|5[0-5])$^216.22.100.([0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]|6[0-9]|7[0-9]|8[0-9]|9[0-9]|10[0-9]|11[0-9]|12[0-9]|13[0-9]|14[0-9]|15[0-9]|16[0-9]|17[0-9]|18[0-9]|19[0-9]|20[0-9]|21[0-9]|22[0-9]|23[0-9]|24[0-9]|25[0-5])$|^4.71.210.(14[4-9]|15[0-1])$^200.127.158.(4[8-9]|5[0-5])$"

#Elimino a los que tienen mal trackeado el pais
paises <- c("AR", "BO", "BR", "CL", "CO", "CR", "DE", "DO", "EC", "ES", "GT", "HN", "IT", "MX", "NI", "PA", "PE", "PR", "PY", "SV", "US", "UY", "VE")
raw.full <- raw.full[country %in% paises,]

if(length(grep(filtros.ip,raw.full$ip))>0) {
  raw <- raw.full[-grep(filtros.ip,ip),][flow %in% c("SEARCH","LANDING:RESULTS","LANDING:DETAIL","DETAIL","CHECKOUT","THANKS") & (destino_IATA != "") & (country != ""),]
} else {
  raw <- raw.full[flow %in% c("SEARCH","LANDING:RESULTS","LANDING:DETAIL","DETAIL","CHECKOUT","THANKS") & (destino_IATA != "") & (country != ""),]
}

#rm(raw.full)

raw <- droplevels(raw)

raw$userid <- as.factor(raw$userid)

levels(raw$userid) <- 1:length(levels(raw$userid))

raw$userid <- as.numeric(as.character(raw$userid))

#muestra <- sample(unique(raw$userid),100000) #me quedo con un sample para trabajar
#raw <- raw[userid %in% muestra,]

raw[,time := paste(FECHA,HORA,sep="-")]
raw[,country:=toupper(country)]

table(raw$flow)

#Ordeno todos los destinos para armar la matriz
raw[,orden:=rank(time),by=list(userid,destino_IATA,flow)]
raw <- raw[order(userid,flow,orden),]

#Me quedo con la primera accion de cada destino y flujo
user.destination <- raw[orden==1,][,orden:=NULL] #me quedo con la primera accion de flujo-destino
user.destination[,orden.by.destination:=rank(time),by=list(userid,destino_IATA)] #ordeno para un mismo usuario el orden con el que fue los destinos, quedandome con el primero de cada flujo
user.destination[,orden.by.flow:=rank(time),by=list(userid,flow)]
user.destination[,flow.destinations:= max(orden.by.flow),by=list(userid,destino_IATA)] #cuento todos los destinos distintos por flujo
user.destination[,total.destinations:= length(unique(destino_IATA)),by=list(userid)] #cuento todos los destinos dinstintos totales del usuario
user.destination[,total.flow.destinations:= length(unique(destino_IATA)),by=list(userid,flow)]
user.destination[,total.thanks:= sum(flow=="THANKS"),by=list(userid)] #cuento todos los destinos dinstintos totales del usuario

#Me quedo con usuarios que tienen mas de un destino y menos de 20
keep <- unique(user.destination[between(total.destinations,1,20),]$userid) #as.numeric(names(which(total.destination >= 2)))
user.destination <- user.destination[userid %in% keep,]

# Jaccard en busquedas
user.destination.search = user.destination.merge(user.destination.table = user.destination,flows.input = "SEARCH",flows.output = "SEARCH")
jaccard.search = it.it.to.long(user.destination.merge = user.destination.search,probs = T,sparse = F)
dis.jaccard.search = 1-jaccard.search #va a seguir siendo una metrica
dis.jaccard.search[rownames(dis.jaccard.search)%in%c("ORL","MIA","FLL"),colnames(dis.jaccard.search)%in%c("ORL","MIA","FLL")]

# Jaccard en compras
user.destination.thanks = user.destination.merge(user.destination.table = user.destination,flows.input = "THANKS",flows.output = "THANKS")
jaccard.thanks = it.it.to.long(user.destination.merge = user.destination.thanks,probs = T,sparse = F)
dis.jaccard.thanks = 1-jaccard.thanks # Va a seguir siendo una metrica
dis.jaccard.thanks[rownames(dis.jaccard.thanks)%in%c("ORL","MIA","FLL"),colnames(dis.jaccard.thanks)%in%c("ORL","MIA","FLL")]

dis.jaccard.search.filt = dis.jaccard.search[rownames(dis.jaccard.search) %in% all.dt.filt$iataCode,
                                             colnames(dis.jaccard.search) %in% all.dt.filt$iataCode]

dis.jaccard.thanks.filt = dis.jaccard.thanks[rownames(dis.jaccard.thanks) %in% all.dt.filt$iataCode,
                                             colnames(dis.jaccard.thanks) %in% all.dt.filt$iataCode]

dis.jaccard.search.filt = dis.jaccard.search.filt[match(all.dt.filt$iataCode,rownames(dis.jaccard.search.filt)),]
dis.jaccard.search.filt = dis.jaccard.search.filt[,match(all.dt.filt$iataCode,colnames(dis.jaccard.search.filt))]
dis.jaccard.search.filt = as.dist(dis.jaccard.search.filt)

dis.jaccard.thanks.filt = dis.jaccard.thanks.filt[match(all.dt.filt$iataCode,rownames(dis.jaccard.thanks.filt)),]
dis.jaccard.thanks.filt = dis.jaccard.thanks.filt[,match(all.dt.filt$iataCode,colnames(dis.jaccard.thanks.filt))]
dis.jaccard.thanks.filt = as.dist(dis.jaccard.thanks.filt)

#tri.ineq(dis.jaccard.thanks)
#tri.ineq(dis.jaccard.search)


