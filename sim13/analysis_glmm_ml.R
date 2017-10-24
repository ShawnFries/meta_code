library(dplyr)
library(readr)
library(metafor)

###glmm.ml
ptm<-proc.time()

bias=c()
bias.prop=c()
RMSE=c()
RMSE.prop=c()
coverage=c()

##dobby1
dobby1<-function(vec)as.numeric(as.character(vec))

for(i in 1:792){
  data.path<-paste0("sim_data/scenario_", i, ".csv")
  dat<-suppressWarnings(read_csv(data.path))
  
  ##use dplyr to obtain an rma object for each run
  ##back-transform with predict()
  ##store estimate and CI
  ##deselect the rma object column
  res<-dat %>% 
    group_by(run) %>% 
    summarise(obj=list(unlist(predict(rma.glmm(measure="PLO", method="FE", 
                                               xi=num.of.events, ni=sample.sizes),
                                      transf=transf.ilogit))[c(1,3,4)]),
              est=dobby1(unlist(obj)[1]),
              ci.lb=dobby1(unlist(obj)[2]),
              ci.ub=dobby1(unlist(obj)[3])) %>%
    select(-obj)
  
  ##scenario information
  scenario_info<-dat%>%
    filter(!duplicated(dat$run)) %>%
    select(-X1, -sample.sizes, -num.of.events)
  ##scenario information joined with estimates of each run by "run"
  res<-left_join(scenario_info, res, by="run")
  
  file.path<-paste0("sim_res/glmm_fe/scenario_", res$scenario[1], "_glmm_fe_res.csv")
  write.csv(res, file=file.path)
  
  true.pii<-res$true.pi[1]
  bias[i]<-mean(true.pii-res$est)
  bias.prop[i]<-mean((true.pii-res$est)/true.pii)
  RMSE[i]<-mean((res$est-true.pii)^2)
  RMSE.prop[i]<-mean((res$est-true.pii)^2/true.pii)
  coverage[i]<-sum(res$ci.lb<=true.pii & res$ci.ub>=true.pii)/1000
  
}#ends for(i in 792)

metrics<-data.frame(bias=bias,
                    bias.prop=bias.prop,
                    RMSE=RMSE,
                    RMSE.prop=RMSE.prop,
                    coverage=coverage)

metrics<-cbind(scenarios, metrics)

write.csv(metrics, file="sim_res/glmm_fe/metrics_glmm_fe.csv")


proc.time()-ptm
