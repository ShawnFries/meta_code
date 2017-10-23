##to replicate simulation of data in the Trikalinos et al. (2013) report

sim.fun<-function(mu, k, nlwr=0, nuppr=0, nmix=FALSE, h, dist, sce){
  #mu=true pi
  #k=number of studies
  #nlwr=lower bound of sample size 
  #nuppr=upper bound of sample size 
  #nmix=mixed sample size (logical TRUE or FALSE)
  #h=heterogeneity factor (0, 0.1, 0.5)
  #dist=distribution to draw sample pi from ("beta", "unif", "NA" for when h=0)
  #sce=scenario no.
     res<-NULL

     for(i in 1:1000){
         #sample sizes
         ni<-if(nmix==FALSE){
               #sample() includes starting and ending values 
               sample(x=nlwr:nuppr, size=k, replace=TRUE)
             }else if(nmix==TRUE){
               #table 2
               k.s<-round(k*0.5)
               k.m<-round(k*0.4)
               k.l<-k-k.s-k.m
               c(sample(5:50, size=k.s, replace=TRUE), 
                 sample(51:200, size=k.m, replace=TRUE),
                 sample(201:1000, size=k.l, replace=TRUE))
             }#ends else if(nmix==TRUE) 
         
         #number of events in each study
         xi<-c()
         #heterogeneity
         if(h==0){
            for(j in 1:length(ni)){
               pii<-mu
               xi[j]<-rbinom(1, ni[j], pii)
            }#ends for(j in 1:length(ni))
         }else if(h!=0){
            #table 2
            tau<-mu*h
            #table A-3
            sigma<-tau*mu
            #for beta
            alpha<-(1-mu)*(mu/sigma)^2-mu
            beta<-(1-mu)*alpha/mu
            #for uniform
            a<-mu-sqrt(3)*sigma
            b<-mu+sqrt(3)*sigma
           
            #sample pi
            pii<-c()
            for(j in 1:length(ni)){
                pii[j]<-if(dist=="beta"){
                          rbeta(1, alpha, beta, ncp = 0)
                        }else if(dist=="unif"){
                          runif(1, a, b)
                        }#ends if(dist==)
                xi[j]<-rbinom(1, ni[j], pii[j])
            }#ends for(j in 1:length(ni))
         }#ends else if(h!=0)

        resi<-data.frame(scenario=sce,
                         run=i,
                         dist.of.pi=dist,
                         num.of.studies=k,
                         sample.sizes=ni,
                         true.pi=mu,
                         heterogeneity=h,
                         num.of.events=xi,
                         sample.pi=pii)
        res<-rbind(res, resi)
     }#ends for(i in 1:1000) 
     
  file.path<-paste0("scenario_", sce, ".csv")
  write.csv(res, file=file.path)
  
  res
}#ends function


#run time
ptm<-proc.time()
res<-sim.fun(mu=0.1, k=5, nlwr=5, nuppr=50, h=0.1, dist="beta", sce=1)
proc.time()-ptm

#mixed sample sizes
ptm<-proc.time()
res<-sim.fun(mu=0.1, k=5, nmix=TRUE, h=0.1, dist="beta", sce=1)
proc.time()-ptm

#check results
#sample sizes should be uniformly distributed
hist(res$sample.sizes)
table(res$scenario)
table(res$dist.of.pi)
table(res$num.of.studies)
table(res$true.pi)
table(res$heterogeneity)
table(res$num.of.events)
hist(res$num.of.events)
#sample.pi should be uniformly/beta distributed
hist(res$sample.pi)


# ncp in rbeta
x<-c()
for(i in 1:1000){
  x[i]<-rbeta(1, 1, 2)
}

y<-c()
for(i in 1:1000){
  y[i]<-rbeta(1, 1, 2, ncp=0)
}

par(mfrow=c(1,2))
hist(x, ylim=c(1, 200), breaks=30)
hist(y, ylim=c(1, 200), breaks=30)




# set.seed(123)
# x<-rbinom(1, 12, 0.5)
# set.seed(123)
# y<-rbinom(1, 12, 0.5)
# set.seed(123)
# z<-rbinom(1, 10, 0.5)
# x
# y
# z

