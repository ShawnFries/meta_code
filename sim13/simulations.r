##don't forget to setwd()

#scenario 1
ptm<-proc.time()
res<-sim.fun(mu=0.001, k=5, nlwr=5, nuppr=50, h=0, dist="NA", sce=1)
proc.time()-ptm

#scenario 2
ptm<-proc.time()
res<-sim.fun(mu=0.001, k=5, nlwr=51, nuppr=200, h=0, dist="NA", sce=2)
proc.time()-ptm

ptm<-proc.time()
for(i in 1:nrow(scenarios)){
  mui<-scenarios[i,]$true.pi
  ki<-scenarios[i,]$num.studies
  hi<-scenarios[i,]$heterogeneity
  disti<-scenarios[i,]$dist.of.pi
  scei<-scenarios[i,]$scenario
  
  if(scenarios[i,]$sample.sizes=="small"){
      sim.fun(mu=mui, k=ki, nlwr=5, nuppr=50, h=hi, dist=disti, sce=scei)  
  }else if(scenarios[i,]$sample.sizes=="medium"){
      sim.fun(mu=mui, k=ki, nlwr=51, nuppr=200, h=hi, dist=disti, sce=scei) 
  }else if(scenarios[i,]$sample.sizes=="large"){
      sim.fun(mu=mui, k=ki, nlwr=201, nuppr=1000, h=hi, dist=disti, sce=scei) 
  }else if(scenarios[i,]$sample.sizes=="mix"){
      sim.fun(mu=mui, k=ki, nmix=TRUE, h=hi, dist=disti, sce=scei) 
  }
}
proc.time()-ptm
#~50 mins