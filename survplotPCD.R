survplotPCD<-function(data,status,time,negative,positive,group,labels)
{
library('survival')
library('survminer')
library('RColorBrewer')
groups3<-data
groups3$survival2<-gsub(negative,1,groups3[,status])
groups3$survival2<-gsub(positive,2,groups3$survival2)
groups3$survival2<-as.numeric(groups3$survival2)
groups3<-groups3[which(groups3$survival2>0),]
groups3$time2<-as.numeric(groups3[,time])
groups3$group2<-groups3[,group]
groups3<-groups3[,c('time2','survival2','group2')]
attach(groups3)
fit <- survfit(Surv(time2, survival2) ~ group2)
options(repr.plot.height=6,repr.plot.width=5)
ggsurvplot(fit, data = groups3,
           pval = T,
      #     conf.int = T,
           risk.table = T,
           legend.labs=labels,
           legend.title="Subgroups",
           ylab=time,xlab = " Time (Months)",
           risk.table.col = "strata",
       #    linetype = "strata",
       #    surv.median.line = "hv",
       #    ggtheme = theme_bw(),
           palette = gsub('#FFFF33','grey',brewer.pal(9,"Set1")))
}
