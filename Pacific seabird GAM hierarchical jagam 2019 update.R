### Updating PAcific Seabird Counts
# 
# #setwd("M:/My Documents/State of Birds/SOCB/seabird data 2017")
 library(dplyr)
 library(tibble)
 library(tidyr)
 library(mgcv)
 library(rjags)
 library(mgcv)
 
 colraw = read.csv("Copy of All West Coast Monitoring Plot Summary_2019_10_WilsonL.csv",
                   stringsAsFactors = F)
 ### figure out how to use data from all species plots for each species
 ### while still calculating a colony-level trend/trajectory
plotcls = paste0("p",1:15)

 coldf = expand.grid(colony = unique(colraw$Colony),
                     species = unique(colraw$Species),
                     year = unique(colraw$Year),
                     plot = plotcls,
                     plotgroup = unique(colraw$spplots))
 
 for(ss in unique(coldf$species)){
   tmp1 = colraw[which(colraw$Species == ss),]
    for(cc in unique(tmp1$Colony)){
     tmp2 = tmp1[which(tmp1$Colony == cc),]
      for(yy in unique(tmp2$Year)){
        print(paste(ss,yy,cc))
        tmp3 = tmp2[which(tmp2$Year == yy),]
          for(pp in unique(coldf$plot)){
            if(nrow(tmp3) > 1){
            for(i in 1:nrow(tmp3)){
              tmp4 = tmp3[i,]
              spp = as.character(tmp4["spplots"])
            coldf[which(coldf$colony == cc &
                          coldf$species == ss &
                          coldf$year == yy &
                          coldf$plot == pp &
                          coldf$plotgroup == spp),"count"] <- as.integer(tmp4[pp])
            }#i
            }else{
              spp = as.character(tmp3["spplots"])
              coldf[which(coldf$colony == cc &
                            coldf$species == ss &
                            coldf$year == yy &
                            coldf$plot == pp &
                            coldf$plotgroup == spp),"count"] <- as.integer(tmp3[pp])
              
            }
            }#pp
      }#yy
    }#cc
 }#ss
 
coldf = coldf[which(!is.na(coldf$count)),] 


coldfout = coldf[order(coldf$species,
                       coldf$colony,
                       coldf$plotgroup,
                       coldf$plot,
                       coldf$year),] 

nrow(coldfout) == nrow(unique(coldfout[,c(1:5)]))

write.csv(coldfout,"complete count database western canada 2019.csv")
 
coldf = coldfout 

coldf[,"plot.u"] <- paste(coldf$plotgroup,coldf$plot,sep = "-")
coldf[,"col.plot.u"] <- paste(coldf$colony,coldf$plotgroup,coldf$plot,sep = "-")

meansbyspplot = tapply(coldf$count,
                       coldf[,c("col.plot.u","species")],
                       mean,
                       na.rm = T)

sdsbyspplot = tapply(coldf$count,
                       coldf[,c("col.plot.u","species")],
                       sd,
                       na.rm = T)

minsbyspplot = tapply(coldf$count,
                       coldf[,c("col.plot.u","species")],
                       min,
                       na.rm = T)
maxsbyspplot = tapply(coldf$count,
                       coldf[,c("col.plot.u","species")],
                       max,
                       na.rm = T)




species = c("ANMU","CAAU","RHAU","TUPU")
#### read in species-level colony size estimates
poplist = list()
length(poplist) = 4
names(poplist) = species 

pop.anmu = read.csv("StatusOfBirdsUpdate_ANMU_20180328_WilsonL.csv",
                    stringsAsFactors = F)
pop.anmu = pop.anmu[which(pop.anmu$Most.current.survey == "yes"),]
poplist[["ANMU"]] = pop.anmu

pop.caau = read.csv("StatusOfBirdsUpdate_CAAU_20180327_WilsonL.csv",
                    stringsAsFactors = F)
pop.caau = pop.caau[which(pop.caau$Most.current.survey == "yes"),]
poplist[["CAAU"]] = pop.caau


pop.rhau = read.csv("StatusOfBirdsUpdate_RHAU_20180327_WilsonL.csv",
                    stringsAsFactors = F)
pop.rhau = pop.rhau[which(pop.rhau$Most.current.survey == "yes"),]
poplist[["RHAU"]] = pop.rhau


pop.tupu = read.csv("StatusOfBirdsUpdate_TUPU_20180328_WilsonL.csv",
                    stringsAsFactors = F)
pop.tupu = pop.tupu[which(pop.tupu$Most.current.survey == "yes"),]
pop.tupu[which(is.na(pop.tupu$NestPairs)),"NestPairs"] = pop.tupu[which(is.na(pop.tupu$NestPairs)),"Individual"]/2
poplist[["TUPU"]] = pop.tupu


colext = c("SpeciesID","Region","SubRegion","Year","SiteName","NestPairs")



for(sp in species){
  if(sp == species[1]){
  popsbysp = poplist[[sp]][,colext]
  }else{
    tmp = poplist[[sp]][,colext]
    popsbysp = rbind(popsbysp,tmp)
  }
  
  
  
  
}#sp



#list of colonies by species, with data
colonies = unique(coldf[,c("species","colony")])
col1 = merge(colonies,popsbysp[,c("SpeciesID","SiteName","NestPairs","Year")],
             by.x = c("species","colony"),
             by.y = c("SpeciesID","SiteName"),
             all.x = T)
for(sp in species){
  
ww = which(col1$species == sp)
col1[ww,"sp.prop"] = col1[ww,"NestPairs"]/sum(col1[ww,"NestPairs"])

}

col1$yearcolest = col1$Year




source("c:/Functions/transparency function.r")

j = 0

species = c("ANMU","CAAU","RHAU","TUPU")

for(ss in species){
#  for(ss in re.run){
    
  j = j+1

  if(ss %in% c("CAAU","RHAU")){
    if(ss == "CAAU"){
    spcolplots = names(meansbyspplot[which(meansbyspplot[,ss] > 18.5),ss])
    }else{
      spcolplots = names(meansbyspplot[which(meansbyspplot[,ss] > 15),ss])
      
    }
  }else{
  spcolplots = names(meansbyspplot[which(meansbyspplot[,ss] > 5),ss])
  }
 spdat = coldf[which(coldf$col.plot.u %in% spcolplots & coldf$species == ss),] 
 
 ncnt.plot = table(spdat$col.plot.u) # this and next line remove plots with only 1 count
 
 spdat = spdat[which(spdat$col.plot.u %in% names(ncnt.plot)[which(ncnt.plot > 1)]),]
 
 for(k in 1:ncol(spdat)){
   if(is.factor(spdat[,k])){
spdat[,k] <- as.character(spdat[,k])
   }
 }
 
 
 spdat$col.f = as.integer(factor(spdat$colony))
 nplot = NA
 for(cc in 1:max(spdat$col.f)){
   ww = which(spdat$col.f == cc)
   ppp = 0
   for(pp in unique(spdat[ww,"col.plot.u"])){
     ppp = ppp+1
     www = which(spdat$col.f == cc & spdat$col.plot.u == pp)
     spdat[www,"col.plot.f"] <- ppp
   }
   nplot[cc] = ppp
   
   
 }

 spdat = merge(spdat,
               col1[which(col1$species == ss),c("colony","sp.prop","NestPairs","yearcolest")],
               by = "colony")
 
 spdat$yr = spdat$year-(min(spdat$year)-1) 
 nyrs = length(unique(spdat$yr))
 yrs = sort(unique(spdat$yr))
 #spdat$colony = factor(spdat$colony)
colony <- spdat$col.f

wtlst1 <- unique(spdat[,c("col.f","sp.prop","NestPairs","yearcolest")])
wtlst = wtlst1[order(wtlst1$col.f),"NestPairs"]
wtlst2 = wtlst/sum(wtlst)
wts <- spdat$sp.prop
baseyearY = wtlst1$yearcolest

yrs = unique(spdat[,c("year","yr")])

baseyear = baseyearY-(min(yrs$year)-1)
if(any(baseyear < 1)){
  baseyear[which(baseyear < 1)] <- 1
} 


count <- spdat$count
plot = spdat$col.plot.f

  ncounts = length(count)
  ncolony <- max(colony)
  
  
  
  if(ncolony == 1) {
    print(paste("only one colony for",ss))
  }
  year <- spdat$yr
  ymax <- max(year)
  ymin <- min(year)

nknots = 6 


ymaxpred = 2019-min(spdat$year-1)
nyearspred = length(ymin:ymaxpred)
years = ymin:ymaxpred

nyears = length(years)
preddat = data.frame(yrs = years,
                     count = 1)

form = as.formula(paste("count","~",
                        "s(yrs,k =",nknots,")"))


gamprep = jagam(formula = form,
                data = preddat,
                file = "tempgam.txt",
                centred = T)



dat = list(X = gamprep$jags.data$X,
           S1 = gamprep$jags.data$S1,
           zero = gamprep$jags.data$zero,
           colony = colony,
           plot = plot,
           ncounts = ncounts,
           ncolony = ncolony,
           count = count,
           nplot = nplot,
           nknots = nknots,
           nyearspred = nyearspred,
           year = year,
 wts = wts,
 wtlst = wtlst,
 wtlst2 = wtlst2,
 baseyear = baseyear)

gamfitqp <- jags.model(data = dat,
                       file = paste0("GAM model Westcoast quasialt interaction W jagam2.txt"),
                       n.chains = 3)


adaptest <- adapt(object = gamfitqp,
                  n.iter = 10)

while(adaptest == F){
  adaptest <- adapt(object = gamfitqp,
                    n.iter = 1000)
  
}
nburn = 100000
update(gamfitqp,n.iter = nburn)


poster <- coda.samples(gamfitqp,
                                c("sdnoise",
                                  "etapred",
                                  "etapredr",
                                  "B.X",
                                  "beta.X",
                                  "sdbeta",
                                  "x.gampred",
                                  "pop",
                                  "popr",
                                  #"sdX",
                                  #"C",
                                  "plt",
                                  "sd.plt"),
                                n.iter = 240000,
                                #inits = test$jags.ini
                                thin = 40)


out <- summary(poster)

# pdf(paste0("results/",ss,"Western quasi alt hierarchical jagam"," mcmc diagnostic.pdf"))
# plot(poster)
# dev.off()

colonylist = unique(spdat[,c("colony","col.f")])
colonylistord = colonylist[order(colonylist$col.f),"colony"]



qout <- out$quantiles


netapred = paste0("etapred[",rep(1:ncolony,each = nyearspred),",",rep(1:nyearspred,times = ncolony),"]")


indicescol = data.frame(qout[netapred,])
indicescol[,"node"] <- netapred
indicescol[,"year"] <- rep(ymin:ymaxpred,times = ncolony)+min(spdat$year-1)
indicescol[,"colony"] <-  rep(1:ncolony,each = nyearspred)
indicescol[,"species"] <- ss
colonylist = unique(spdat[,c("colony","col.f")])
colonylistord = colonylist[order(colonylist$col.f),"colony"]
indicescol[,"colonyname"] <- rep(colonylistord,each = nyearspred)
indicescol[,"nknots"] <- nknots


netapredr = paste0("etapredr[",rep(1:ncolony,each = nyearspred),",",rep(1:nyearspred,times = ncolony),"]")


indicescolr = data.frame(qout[netapredr,])
indicescolr[,"node"] <- netapredr
indicescolr[,"year"] <- rep(ymin:ymaxpred,times = ncolony)+min(spdat$year-1) 
indicescolr[,"colony"] <-  rep(1:ncolony,each = nyearspred)
indicescolr[,"species"] <- ss 
colonylist = unique(spdat[,c("colony","col.f")]) 
colonylistord = colonylist[order(colonylist$col.f),"colony"]
indicescolr[,"colonyname"] <- rep(colonylistord,each = nyearspred) 
indicescolr[,"nknots"] <- nknots 

popi <- paste0("pop[",1:nyearspred,"]")
indicesreg <- data.frame(qout[popi,])
indicesreg[,"node"] <- popi
indicesreg[,"year"] <- (ymin:ymaxpred)+min(spdat$year-1)
indicesreg[,"species"] <- ss
indicesreg[,"nknots"] <- nknots

popir <- paste0("popr[",1:nyearspred,"]")
indicesregr <- data.frame(qout[popir,])
indicesregr[,"node"] <- popir
indicesregr[,"year"] <- (ymin:ymaxpred)+min(spdat$year-1) 
indicesregr[,"species"] <- ss
indicesregr[,"nknots"] <- nknots

names(indicescol)[which(names(indicescol) == "X50.")] <- "index"
names(indicescol)[which(names(indicescol) == "X2.5.")] <- "index.lci"
names(indicescol)[which(names(indicescol) == "X97.5.")] <- "index.uci"


names(indicescolr)[which(names(indicescolr) == "X50.")] <- "index"
names(indicescolr)[which(names(indicescolr) == "X2.5.")] <- "index.lci"
names(indicescolr)[which(names(indicescolr) == "X97.5.")] <- "index.uci"

names(indicesreg)[which(names(indicesreg) == "X50.")] <- "index"
names(indicesreg)[which(names(indicesreg) == "X2.5.")] <- "index.lci"
names(indicesreg)[which(names(indicesreg) == "X97.5.")] <- "index.uci"

#
names(indicesregr)[which(names(indicesregr) == "X50.")] <- "index"
names(indicesregr)[which(names(indicesregr) == "X2.5.")] <- "index.lci"
names(indicesregr)[which(names(indicesregr) == "X97.5.")] <- "index.uci"



trendcol = rbind(colonylist,colonylist)
trendcol$time = rep(c("long-term","short-term"),each = ncolony)
trendcol[2*ncolony+1,"col.f"] <- NA
trendcol[2*ncolony+1,"colony"] <- "regional"
trendcol[2*ncolony+1,"time"] <- "long-term"
trendcol[2*ncolony+2,"col.f"] <- NA
trendcol[2*ncolony+2,"colony"] <- "regional"
trendcol[2*ncolony+2,"time"] <- "short-term"

trendcol[2*ncolony+3,"col.f"] <- NA
trendcol[2*ncolony+3,"colony"] <- "regional.sum"
trendcol[2*ncolony+3,"time"] <- "long-term"
trendcol[2*ncolony+4,"col.f"] <- NA
trendcol[2*ncolony+4,"colony"] <- "regional.sum"
trendcol[2*ncolony+4,"time"] <- "short-term"

for (cc in 1:ncolony){

netapredi = paste0("etapred[",cc,",",ymin,"]")

base = unlist(poster[,netapredi, drop=FALSE]) 

netapredi = paste0("etapred[",cc,",",ymaxpred-10,"]")

base10 = unlist(poster[,netapredi, drop=FALSE]) 


netapredi = paste0("etapred[",cc,",",ymaxpred,"]")

post = unlist(poster[,netapredi, drop=FALSE]) 

for(tt in c("long-term","short-term")){
  if(tt == "long-term"){
    baset = base
  ymint = ymin
  }else{
    baset = base10
  ymint = ymaxpred-10}
  trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"start.year"] <- ymint+min(spdat$year-1)
  trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"end.year"] <- ymaxpred+min(spdat$year-1)
  
  
        trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"trend"] <- round(median(((post/baset)^(1/(ymaxpred-ymint))-1)*100),3)
      trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"trend.lci"] <- round(quantile(((post/baset)^(1/(ymaxpred-ymint))-1)*100,0.025),3)
      trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"trend.uci"] <- round(quantile(((post/baset)^(1/(ymaxpred-ymint))-1)*100,0.975),3)

      trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"pop.change"] <- (round(quantile(post/baset,0.5),3)-1)*100
      trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"pop.change.lci"] <- (round(quantile(post/baset,0.025),3)-1)*100
      trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"pop.change.uci"] <- (round(quantile(post/baset,0.975),3)-1)*100
      
      trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"weight.p"] <- round(wtlst1[which(wtlst1$col.f == cc),"sp.prop"],2)
      trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"NestPairs"] <- wtlst1[which(wtlst1$col.f == cc),"NestPairs"]
      
      
           if(tt == "long-term"){
        tmpind = indicescol[which(indicescol$colony == cc),]
        minyy = tmpind[which.min(tmpind$index),"year"]
        miny = minyy-(min(tmpind$year)-1)
        maxyy = tmpind[which.max(tmpind$index),"year"]
        maxy = maxyy-(min(tmpind$year)-1)
        
        netapredi = paste0("etapred[",cc,",",miny,"]")
        
        basetmin = unlist(poster[,netapredi, drop=FALSE]) 
        netapredi = paste0("etapred[",cc,",",maxy,"]")
        
        basetmax = unlist(poster[,netapredi, drop=FALSE]) 
        
        
        trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"change.from.min"] <- (round(quantile(post/basetmin,0.5),3)-1)*100
        trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"change.from.min.lci"] <- (round(quantile(post/basetmin,0.025),3)-1)*100
        trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"change.from.min.uci"] <- (round(quantile(post/basetmin,0.975),3)-1)*100
        
        trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"change.from.max"] <- (round(quantile(post/basetmax,0.5),3)-1)*100
        trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"change.from.max.lci"] <- (round(quantile(post/basetmax,0.025),3)-1)*100
        trendcol[which(trendcol$col.f == cc & trendcol$time == tt),"change.from.max.uci"] <- (round(quantile(post/basetmax,0.975),3)-1)*100
        
      }
      
}#tt

      
      
   }#cc


for (cc in c("regional","regional.sum")){
  
 if(cc == "regional"){
   
   popi = paste0("pop[",ymin,"]")
  
  base = unlist(poster[,popi, drop=FALSE]) 
  
  popi = paste0("pop[",ymaxpred-10,"]")
  
  base10 = unlist(poster[,popi, drop=FALSE]) 
  
  popi = paste0("pop[",ymaxpred,"]")
  
  post = unlist(poster[,popi, drop=FALSE]) 
  
 }else{
   
   popi = paste0("popr[",ymin,"]")
   
   base = unlist(poster[,popi, drop=FALSE]) 
   
   popi = paste0("popr[",ymaxpred-10,"]")
   
   base10 = unlist(poster[,popi, drop=FALSE]) 
   
   popi = paste0("popr[",ymaxpred,"]")
   
   post = unlist(poster[,popi, drop=FALSE]) 
   
 }

  for(tt in c("long-term","short-term")){
    if(tt == "long-term"){
      baset = base
      ymint = ymin
    }else{
      baset = base10
      ymint = ymaxpred-10}
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"start.year"] <- ymint+min(spdat$year-1)
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"end.year"] <- ymaxpred+min(spdat$year-1)
    
    
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"trend"] <- round(median(((post/baset)^(1/(ymaxpred-ymint))-1)*100),3)
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"trend.lci"] <- round(quantile(((post/baset)^(1/(ymaxpred-ymint))-1)*100,0.025),3)
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"trend.uci"] <- round(quantile(((post/baset)^(1/(ymaxpred-ymint))-1)*100,0.975),3)
    
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"pop.change"] <- (round(quantile(post/baset,0.5),3)-1)*100
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"pop.change.lci"] <- (round(quantile(post/baset,0.025),3)-1)*100
    trendcol[which(trendcol$colony == cc & trendcol$time == tt),"pop.change.uci"] <- (round(quantile(post/baset,0.975),3)-1)*100
    
    if(tt == "long-term"){
      if(cc == "regional.sum"){
      tmpind = indicesregr
      minyy = tmpind[which.min(tmpind$index),"year"]
      miny = minyy-(min(tmpind$year)-1)
      maxyy = tmpind[which.max(tmpind$index),"year"]
      maxy = maxyy-(min(tmpind$year)-1)
      
      popir <- paste0("popr[",miny,"]")
     
      basetmin = unlist(poster[,popir, drop=FALSE]) 
      popir <- paste0("popr[",maxy,"]")
      
      basetmax = unlist(poster[,popir, drop=FALSE]) 
      }else{
        tmpind = indicesreg
        minyy = tmpind[which.min(tmpind$index),"year"]
        miny = minyy-(min(tmpind$year)-1)
        maxyy = tmpind[which.max(tmpind$index),"year"]
        maxy = maxyy-(min(tmpind$year)-1)
        
        popi <- paste0("pop[",miny,"]")
        
        basetmin = unlist(poster[,popi, drop=FALSE]) 
        popi <- paste0("pop[",maxy,"]")
        
        basetmax = unlist(poster[,popi, drop=FALSE]) 
      }
      
      trendcol[which(trendcol$colony == cc & trendcol$time == tt),"change.from.min"] <- (round(quantile(post/basetmin,0.5),3)-1)*100
      trendcol[which(trendcol$colony == cc & trendcol$time == tt),"change.from.min.lci"] <- (round(quantile(post/basetmin,0.025),3)-1)*100
      trendcol[which(trendcol$colony == cc & trendcol$time == tt),"change.from.min.uci"] <- (round(quantile(post/basetmin,0.975),3)-1)*100
      
      trendcol[which(trendcol$colony == cc & trendcol$time == tt),"change.from.max"] <- (round(quantile(post/basetmax,0.5),3)-1)*100
      trendcol[which(trendcol$colony == cc & trendcol$time == tt),"change.from.max.lci"] <- (round(quantile(post/basetmax,0.025),3)-1)*100
      trendcol[which(trendcol$colony == cc & trendcol$time == tt),"change.from.max.uci"] <- (round(quantile(post/basetmax,0.975),3)-1)*100
      
    }
    
  }#tt
  
}#cc


trendcol$species = ss


########explore a;lternative plots, panel plots, centered colony-level pattern
##### smooths of plot-level patterns.
xplot = sort(unique(indicescol$year))
png(filename = paste0("results/",ss,"western jaggam trajectory overplot quasi poisson hierarchical ",".png"),
    res = 300,
    height = 8,
    width = 12,
    units = "in") 
par(mar = c(3,3,3,12))

plot(1,1,
     xlim = c(min(spdat$year),2030),
     ylim = c(0,max(c(indicesreg$index*1.6,indicescol$index*1.5))),
     type = "l",
     main = ss,
     bty = "l",
     ylab = "population by colony and total",
     xlab = "")

polygon(x = c(xplot,rev(xplot)),
        y = c(indicesreg$index.uci,rev(indicesreg$index.lci)),
        col = transp.func(grey(0.8),0.6),
        border = NA)
lines(y = indicesreg$index,
      x = xplot,
      col = "black")

tr = 0.2
for(cc in 1:ncolony){
  tmp = indicescol[which(indicescol$colony == cc),"index"]
  tmplci = indicescol[which(indicescol$colony == cc),"index.lci"]
  tmpuci = indicescol[which(indicescol$colony == cc),"index.uci"]
  #coli = unique(indicescol[which(indicescol$colony == j),"colonyname"])
  polygon(x = c(xplot,rev(xplot)),
          y = c(tmplci,rev(tmpuci)),
          col = transp.func(rainbow(length(unique(colony)))[cc],0.1),
          border = NA)
  lines(y = tmp,
        x = xplot,
        col = rainbow(length(unique(colony)))[cc])
  for(pp in 1:nplot[cc]){
    centr = c((xplot[floor(length(xplot)/2)]),tmp[floor(length(xplot)/2)])
    rawplot = spdat[which(spdat$col.f == cc & spdat$col.plot.f == pp),]
    rawplot = rawplot[order(rawplot$year),]
    
    rawcentr = floor(nrow(rawplot)/2)
    #rawplot$count = (rawplot$count/rawplot$count[rawcentr])*centr[2]
  points(col = transp.func(rainbow(length(unique(colony)))[cc],tr),
         x = rawplot$year,
         y = rawplot$count,
         cex = 0.5)
  lines(col = transp.func(rainbow(length(unique(colony)))[cc],tr),
         x = rawplot$year,
         y = rawplot$count,
        lwd = 2)
  }
  text(colonylist[which(colonylist$col.f == cc),"colony"],
       x = 2018,
       y = tmp[length(tmp)] * (rnorm(1,0,0.05)+1),
       col = rainbow(length(unique(colony)))[cc],
       pos = 4)
}


dev.off()



###### panels by colony

xplot = sort(unique(indicescol$year))
pdf(file = paste0("results/",ss,"western jaggam trajectory colony panels quasi poisson hierarchical ",".pdf"),
 #   res = 300,
    height = 8,
    width = 12) 
par(mar = c(3,3,3,1))


tr = 0.2
for(cc in 1:ncolony){
  tmp = indicescol[which(indicescol$colony == cc),"index"]
  tmplci = indicescol[which(indicescol$colony == cc),"index.lci"]
  tmpuci = indicescol[which(indicescol$colony == cc),"index.uci"]
  #coli = unique(indicescol[which(indicescol$colony == j),"colonyname"])
  rawploto = spdat[which(spdat$col.f == cc),]
  plot(1,1,
       xlim = c(min(indicescol$year),2020),
       ylim = c(0,max(c(tmp*1.5,rawploto$count))),
       type = "l",
       main = paste(ss,colonylist[which(colonylist$col.f == cc),"colony"]),
       bty = "l",
       ylab = "population by colony and total",
       xlab = "")
  
  polygon(x = c(xplot,rev(xplot)),
          y = c(tmplci,rev(tmpuci)),
          col = transp.func(rainbow(length(unique(colony)))[cc],0.1),
          border = NA)
  lines(y = tmp,
        x = xplot,
        col = rainbow(length(unique(colony)))[cc])
  for(pp in 1:nplot[cc]){
    centr = c((xplot[floor(length(xplot)/2)]),tmp[floor(length(xplot)/2)])
    rawplot = spdat[which(spdat$col.f == cc & spdat$col.plot.f == pp),]
    rawplot = rawplot[order(rawplot$year),]
    
    rawcentr = floor(nrow(rawplot)/2)
    #rawplot$count = (rawplot$count/rawplot$count[rawcentr])*centr[2]
    points(col = transp.func(rainbow(length(unique(colony)))[cc],tr),
           x = rawplot$year,
           y = rawplot$count,
           cex = 0.5)
    lines(col = transp.func(rainbow(length(unique(colony)))[cc],tr),
          x = rawplot$year,
          y = rawplot$count,
          lwd = 2)
  }

}


dev.off()




xplot = sort(unique(indicescolr$year))
png(filename = paste0("results/",ss,"western jaggam trajectory raw overplot quasi poisson hierarchical ",".png"),
    res = 300,
    height = 8,
    width = 12,
    units = "in") 
par(mar = c(3,3,3,12))

plot(1,1,
     xlim = c(min(spdat$year),2030),
     ylim = c(0,max(indicesregr$index*1.6)),
     type = "l",
     main = ss,
     bty = "l",
     ylab = "population by colony and total",
     xlab = "")

polygon(x = c(xplot,rev(xplot)),
        y = c(indicesregr$index.uci,rev(indicesregr$index.lci)),
        col = transp.func(grey(0.8),0.6),
        border = NA)
lines(y = indicesregr$index,
      x = xplot,
      col = "black")

tr = 0.2
for(cc in 1:ncolony){
  tmp = indicescolr[which(indicescolr$colony == cc),"index"]
  tmplci = indicescolr[which(indicescolr$colony == cc),"index.lci"]
  tmpuci = indicescolr[which(indicescolr$colony == cc),"index.uci"]
  #coli = unique(indicescolr[which(indicescolr$colony == j),"colonyname"])
  polygon(x = c(xplot,rev(xplot)),
          y = c(tmplci,rev(tmpuci)),
          col = transp.func(rainbow(length(unique(colony)))[cc],0.1),
          border = NA)
  lines(y = tmp,
        x = xplot,
        col = rainbow(length(unique(colony)))[cc])

  text(colonylist[which(colonylist$col.f == cc),"colony"],
       x = 2020,
       y = tmp[length(tmp)] * (rnorm(1,0,0.05)+1),
       col = rainbow(length(unique(colony)))[cc],
       pos = 4)
}


dev.off()

if(j == 1){
  indicespop <- indicesreg
  
  indicescolony <- indicescol
  
  indicespopr <- indicesregr
  
  indicescolonyr <- indicescolr
  
  trendcolout = trendcol
  
}else{
  indicespop = rbind(indicespop,indicesreg)
  indicescolony = rbind(indicescolony,indicescol)

  indicespopr = rbind(indicespopr,indicesregr)
  indicescolonyr = rbind(indicescolonyr,indicescolr)
  
  trendcolout = rbind(trendcolout,trendcol)
  }                      


write.csv(indicescolony,paste0("results/colony level indices hierarchical"," qpoisson.csv"))

write.csv(indicespop,paste0("results/population level indices hierarchical"," qpoisson.csv"))

write.csv(indicescolonyr,paste0("results/colony level indices hierarchical scaled"," qpoisson.csv"))

write.csv(indicespopr,paste0("results/population level indices hierarchical scaled"," qpoisson.csv"))


write.csv(trendcolout,paste0("results/population level trends hierarchical scaled"," qpoisson.csv"))


}#ss















