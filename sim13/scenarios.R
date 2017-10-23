#dist.of.pi: "unif", "beta" ("NA" aka h=0)
#true pi (mu): 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5
#number of studies (k): 5, 15, 30
#sample sizes: small (5-50), medium (51-200), large(201-1000), mixed
#heterogeneity: 0, 0.1, 0.5
#2*11*3*4*3=792

scenarios<-data.frame(
  scenario=1:792,
  true.pi=rep(c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5), each=72),
  dist.of.pi=rep(rep(c("NA", "unif", "beta"), each=24), 11),
  num.studies=rep(rep(c(5, 15, 30), each=8), 33),
  sample.sizes=rep(c(rep(c("small", "medium", "large", "mix"), 6), 
                     rep(c("small", "medium", "large", "mix"), 6, each=2)), 11),
  heterogeneity=rep(c(rep(0, 24), rep(c(0.1, 0.5), 24)), 11)
)

ptm<-proc.time()
identical.sce<-identical.row(scenarios)
proc.time()-ptm

identical.res<-lapply(identical.sce, function(vec)any(vec, na.rm=TRUE))
#not any row is identical to any other row
any(unlist(identical.res))




##function to check if any rows in a dataframe are identical
identical.row<-function(dat){
  #Lst<-matrix(ncol=ncol(dat), nrow=nrow(dat))
  Lst<-list()
  for(i in 1:nrow(dat)){
    lst<-c()
    for(j in 1:nrow(dat)){
      lst[j]<-if(i<j){
        identical(c(dat[i,]), c(dat[j,]))
      }
    }
    Lst[[i]]<-lst  
  }
  #Lst has as many elements as nrow-1 (last row is not compared to anythong), 
  #each containing nrow logical values
  Lst
}

test<-data.frame(x=c(1, 1, 3, 3, 3),
                 y=c(1, 1, 3, 5, 3),
                 z=c("a", "a", "b", "b", "b"))
test.res<-identical.row(test)
#in here, Lst[[1]] suggests that the 1st row is identical to the 2nd
#Lst[[3]] suggests that the 3rd row is identical to the 5th
test.res

#check if any row is identical to any other row in the dataset
lapply(test.res, function(vec)any(vec))