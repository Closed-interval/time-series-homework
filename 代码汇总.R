rm(list = ls())
library("ggplot2")
library("svglite")
library("tseries")
library("ggforce")
library("forecast")
library("FinTS")
##数据读取以及时序图
datas = read.csv("datas_time_series.csv")
colnames(datas) = c("date","index","index")
set = rbind(datas[,c(1:2)],datas[,c(1,3)])
colnames(datas) = c("date","上证指数","深证指数")
set = cbind(set,
            data.frame(
              "groups" = c(rep("上证指数",length(datas[,1])),
                           rep("深证指数",length(datas[,1]))))
            )
set[,1] = paste0(set[,1],"-01")
set[,1] = as.Date(set[,1],format = "%m-%Y-%d")
ggplot(set,aes(x = date,y = index))+
  geom_line(aes(group = groups,color = groups))+theme_classic()+ylab("指数")+xlab("日期")
ggsave("time_seriess.svg")
##一阶差分及作图
dsh = datas[c(2:182),2] - datas[c(1:181),2]
dsz = datas[c(2:182),3] - datas[c(1:181),3]
set2 = data.frame("date" = set$date[-c(1,183)],
                  "index" = c(dsh,dsz),
                  "groups" = c(rep("上证指数",181),rep("深证指数",181)))
ggplot(set2,aes(x = date,y = index,color = groups))+
  geom_line()+ylab("指数")+xlab("日期")+theme_classic()
ggsave("dsh_dsz.svg")
##零均值检验
u1 = mean(dsh)
u2 = mean(dsz)
s1 = sqrt(var(dsh))
s2 = sqrt(var(dsz))
##自相关函数和偏自相关函数
acf_dsh = acf(dsh)
acf_dsh = data.frame("lag" = acf_dsh$lag,"acf" = acf_dsh$acf)
var_acf_dsh = rep(0,length(acf_dsh[,1]))
var_acf_dsh[1] = (1+2*acf_dsh$acf[1]^2)/length(dsh)
for(i in c(2:length(var_acf_dsh))){
  var_acf_dsh[i] = var_acf_dsh[i-1]+2/length(dsh)*acf_dsh$acf[i]^2
}
acf_dsh = cbind(acf_dsh,data.frame("标准差" = sqrt(var_acf_dsh)))
ggplot(acf_dsh)+geom_point(aes(x = lag,y = acf),color = "#ff6768")+
  geom_segment(aes(x = lag,xend=lag,y =0,yend= acf),color = "#ff6768")+
  geom_point(aes(x = lag,y = 标准差),color = "#8ac6d1")+
  geom_point(aes(x = lag,y = -标准差),color = "#8ac6d1")+
  theme_classic()+
  geom_segment(x = -1.07,y = 0,xend = 23,yend = 0)

ggsave("acf_dsh.svg")

acf_dsz = acf(dsz)
acf_dsz = data.frame("lag" = acf_dsz$lag,"acf" = acf_dsz$acf)
var_acf_dsz = rep(0,length(acf_dsz[,1]))
var_acf_dsz[1] = (1+2*acf_dsz$acf[1]^2)/length(dsz)
for(i in c(2:length(var_acf_dsz))){
  var_acf_dsz[i] = var_acf_dsz[i-1]+2/length(dsz)*acf_dsz$acf[i]^2
}
acf_dsz = cbind(acf_dsz,data.frame("标准差" = sqrt(var_acf_dsz)))

ggplot(acf_dsz)+geom_point(aes(x = lag,y = acf),color = "#ff6768")+
  geom_segment(aes(x = lag,xend=lag,y =0,yend= acf),color = "#ff6768")+
  geom_point(aes(x = lag,y = 标准差),color = "#8ac6d1")+
  geom_point(aes(x = lag,y = -标准差),color = "#8ac6d1")+
  theme_classic()+
  geom_segment(x = -1.07,y = 0,xend = 23,yend = 0)

ggsave("acf_dsz.svg")

pacf_dsh = pacf(dsh)
pacf_dsh = data.frame("lag" = pacf_dsh$lag,
                      "pacf" = pacf_dsh$acf,
                      "s" = rep(sqrt(1/length(pacf_dsh$lag)),length(pacf_dsh$lag))
                      )

ggplot(pacf_dsh)+geom_point(aes(x = lag,y = pacf),color = "#ff6768")+
  geom_segment(aes(x = lag,xend=lag,y =0,yend= pacf),color = "#ff6768")+
  geom_point(aes(x = lag,y = s),color = "#8ac6d1")+
  geom_point(aes(x = lag,y = -s),color = "#8ac6d1")+
  theme_classic()+
  geom_segment(x = -1.07,y = 0,xend = 23,yend = 0)
ggsave("pacf_dsh.svg")

pacf_dsz = pacf(dsz)
pacf_dsz = data.frame("lag" = pacf_dsz$lag,
                      "pacf" = pacf_dsz$acf,
                      "s" = rep(sqrt(1/length(pacf_dsz$lag)),length(pacf_dsz$lag))
                      )

ggplot(pacf_dsz)+geom_point(aes(x = lag,y = pacf),color = "#ff6768")+
  geom_segment(aes(x = lag,xend=lag,y =0,yend= pacf),color = "#ff6768")+
  geom_point(aes(x = lag,y = s),color = "#8ac6d1")+
  geom_point(aes(x = lag,y = -s),color = "#8ac6d1")+
  theme_classic()+
  geom_segment(x = -1.07,y = 0,xend = 23,yend = 0)
ggsave("pacf_dsz.svg")
##AIC和BIC准则模型定阶

#该函数返回AIC/BIC的值
ABIC_get <- function(data,order,method = "AIC"){
  #order=c(p,d,q)
  x = arima(data,order = order)
  if(method == "AIC"){
    return(AIC(x))
  }else{
    if(method == "BIC"){
      return(BIC(x))
    }
  }
}

##计算指标
n=10
AIC_SH = rep(0,n)
BIC_SH = rep(0,n)
AIC_SZ = rep(0,n)
BIC_SZ = rep(0,n)
for(i in c(1:n)){
  AIC_SH[i] = ABIC_get(dsh,c(0,0,i),"AIC")
  BIC_SH[i] = ABIC_get(dsh,c(0,0,i),"BIC")
  AIC_SZ[i] = ABIC_get(dsz,c(i,0,0),"AIC")
  BIC_SZ[i] = ABIC_get(dsz,c(i,0,0),"BIC")
}
ABIC_SH = data.frame("q" = rep(c(1:n),2),
                     "index" = c(AIC_SH,BIC_SH),
                     "groups" = c(rep("AIC",n),rep("BIC",n))
                     )
ABIC_SZ = data.frame("p" = rep(c(1:n),2),
                     "index" = c(AIC_SZ,BIC_SZ),
                     "groups" = c(rep("AIC",n),rep("BIC",n))
                     )

##绘图
ggplot(ABIC_SH)+
  geom_line(aes(x = q,y = index,color = groups),linetype = "dashed")+
  geom_point(aes(x = q,y = index,color = groups),size = 3)+
  theme_bw()+scale_x_continuous(breaks = c(1:10))
ggsave("ABIC_SH.svg")
ggplot(ABIC_SZ)+
  geom_line(aes(x = p,y = index,color = groups),linetype = "dashed")+
  geom_point(aes(x = p,y = index,color = groups),size = 3)+
  theme_bw()+scale_x_continuous(breaks = c(1:10))
ggsave("ABIC_SZ.svg")
##上证模型定阶的F检验
sh1 = arima(dsh,order = c(0,0,1))
sh5 = arima(dsh,order = c(0,0,5))
f = (sum(sh1$residuals^2)-sum(sh5$residuals^2))/4/(sum(sh1$residuals^2))*(181-1)
cat("F统计量的值为",f,",而F(4,180)的0.95分位点为",
    qf(0.95,4,180),",0.99分位点为",qf(0.99,4,180),sep = "")
cat("\n","上证的MA(5)模型的参数为:",sh5$coef,sep = "")

##深证模型定阶的F检验
sz1 = arima(dsz,c(1,0,0))
sz3 = arima(dsz,c(3,0,0))
f = (sum(sz1$residuals^2)-sum(sz3$residuals^2))/2/(sum(sz1$residuals^2))*(181-2)
cat("\n","F统计量的值为",f,",而F(2,179)的0.95分位点为",
    qf(0.95,2,179),",0.99分位点为",qf(0.99,2,179),sep = "")
cat("\n","深证的AR(3)模型的参数为:",sz3$coef,sep = "")
##模型的适应性检验
#绘制残差图
model_sh = sh5
model_sz = sz3
r = cbind(set[-c(1,183),c(1,3)],
          data.frame(
            "residuals" = c(model_sh$residuals,model_sz$residuals)
            )
          )
ggplot(r[c(1:181),],aes(x = date,y = residuals,color = groups))+
  geom_line(size = 0.3)+geom_point(size = 1)+theme_bw()+
  facet_zoom(ylim= c(-1300,1200),zoom.size = 0.75)
ggsave("re_sh.svg")
ggplot(r[c(182:362),],aes(x = date,y = residuals,color = groups))+
  geom_line(size = 0.3)+geom_point(size = 1)+theme_bw()
ggsave("re_sz.svg")

#计算自相关函数
re_acf_sh = acf(model_sh$residuals)$acf[c(2:19)]
re_acf_sz = acf(model_sz$residuals)$acf[c(2:19)]
q_sh = 0
q_sz = 0
for(i in c(1:length(re_acf_sh))){
  q_sh = q_sh+re_acf_sh[i]^2/(181-i)
  q_sz = q_sz+re_acf_sz[i]^2/(181-i)
}
q_sh = q_sh*181*(181+2)
q_sz = q_sz*181*(181+2)
pchisq(q_sh,18-5)
pchisq(q_sz,18-3)
##模型的预测
#为了直接调用的方便，用原序列重新拟合一次模型,计算结果与之前略有差异，但基本可以忽略
model_sh = arima(datas$上证指数,c(0,1,5))
model_sz = arima(datas$深证指数,c(3,1,0))
forecast_sh = forecast(model_sh,h = 10)
forecast_sz = forecast(model_sz,h = 10)
fore_sh = data.frame(
  "date" = rep(c(set[c(1:182),1],
                 as.Date(paste0(c(3:12),rep("-2018",10),rep("-1",10)),format = "%m-%Y-%d")),2),
  "index" = c(datas$上证指数,rep(NA,10),forecast_sh$fitted,forecast_sh$mean),
  "up8" = c(rep(NA,(182*2+10)),forecast_sh$upper[,1]),
  "up95" = c(rep(NA,(182*2+10)),forecast_sh$upper[,2]),
  "low8" = c(rep(NA,(182*2+10)),forecast_sh$lower[,1]),
  "low95" = c(rep(NA,(182*2+10)),forecast_sh$lower[,2]),
  "groups" = c(rep("上证指数",182+10),rep("拟合/预测",182+10))
)
fore_sh[c(1:374),c(3:6)] = fore_sh[c(1:374),2]
ggplot()+geom_line(data = fore_sh,aes(x = date,y = index,color = groups),size = 0.4)+
  theme_bw()+
  geom_ribbon(data = fore_sh[c(193:384),],aes(x = date,ymax = up8,ymin = low8),fill = "#08ffc8",alpha = 0.5)+
  geom_ribbon(data = fore_sh[c(193:384),],aes(x = date,ymax = up95,ymin = low95),fill = "#ffb6b9",alpha = 0.3)
ggsave("fore_sh.svg")
fore_sz = data.frame(
  "date" = rep(c(set[c(1:182),1],
                 as.Date(paste0(c(3:12),rep("-2018",10),rep("-1",10)),format = "%m-%Y-%d")),2),
  "index" = c(datas$深证指数,rep(NA,10),forecast_sz$fitted,forecast_sz$mean),
  "up8" = c(rep(NA,(182*2+10)),forecast_sz$upper[,1]),
  "up95" = c(rep(NA,(182*2+10)),forecast_sz$upper[,2]),
  "low8" = c(rep(NA,(182*2+10)),forecast_sz$lower[,1]),
  "low95" = c(rep(NA,(182*2+10)),forecast_sz$lower[,2]),
  "groups" = c(rep("深证指数",182+10),rep("拟合/预测",182+10))
)
fore_sz[c(1:374),c(3:6)] = fore_sz[c(1:374),2]
ggplot()+geom_line(data = fore_sz,aes(x = date,y = index,color = groups),size = 0.4)+theme_bw()+
  geom_ribbon(data = fore_sz[c(193:384),],aes(x = date,ymax = up8,ymin = low8),fill = "#08ffc8",alpha = 0.5)+
  geom_ribbon(data = fore_sz[c(193:384),],aes(x = date,ymax = up95,ymin = low95),fill = "#ffb6b9",alpha = 0.3)
ggsave("fore_sz.svg")
##pandit-wu方法
q_sh = rep(0,4)
q_sz = rep(0,4)
for(i in c(1:4)){
  q_sh[i] = sum(arima(dsh,c(2*i,0,2*i-1))$residuals^2)
  q_sz[i] = sum(arima(dsz,c(2*i,0,2*i-1))$residuals^2)
}
f_sh = rep(0,3);f_sz = rep(0,3)
p_sh = rep(0,3);p_sz = rep(0,3)

for(i in c(2:4)){
  f_sh[i-1] = (q_sh[i-1]-q_sh[i])/6/q_sh[i]*(181-6*i+1)
  f_sz[i-1] = (q_sz[i-1]-q_sz[i])/6/q_sz[i]*(181-6*i+1)
  p_sh[i-1] = 1-pf(f_sh[i-1],6,181-6*i+1)
  p_sz[i-1] = 1-pf(f_sz[i-1],6,181-6*i+1)
}
pw_model_sh = arima(dsh,c(4,0,3))
pw_model_sh_star = arima(dsh,c(4,0,3),fixed = c(0,NA,NA,NA,NA,NA,NA,NA))
pw_model_sz = arima(dsz,c(2,0,1))
#pandit-wu方法适应性检验
r_sh_pw = data.frame("date" = set[c(2:182),1],
                     "residuals" = pw_model_sh_star$residuals,
                     "groups" = rep("上证指数",181)
                     )
r_sz_pw = data.frame("date" = set[c(2:182),1],
                     "residuals" = pw_model_sz$residuals,
                     "groups" = rep("深证指数",181)
                     )
ggplot(r_sh_pw,aes(x = date,y = residuals,color = groups))+geom_line(size = 0.3)+
  geom_point(size = 1)+theme_bw()+
  facet_zoom(ylim= c(-1300,1200),zoom.size = 0.75)
ggsave("r_sh_pw.svg")
ggplot(r_sz_pw,aes(x = date,y = residuals,color = groups))+geom_line(size = 0.3)+
  geom_point(size = 1)+theme_bw()
ggsave("r_sz_pw.svg")

re_acf_sh_pw = acf(r_sh_pw$residuals)$acf[c(2:19)]
re_acf_sz_pw = acf(r_sz_pw$residuals)$acf[c(2:19)]
q_sh_pw = 0
q_sz_pw = 0
for(i in c(1:length(re_acf_sh_pw))){
  q_sh_pw = q_sh_pw + re_acf_sh_pw[i]^2/(181-i)
  q_sz_pw = q_sz_pw + re_acf_sz_pw[i]^2/(181-i)
}
q_sh_pw = q_sh_pw*181*(181+2)
q_sz_pw = q_sz_pw*181*(181+2)
1-pchisq(q_sh_pw,18-(4+3))
1-pchisq(q_sz_pw,18-(2+1))
##GARCH效应检验
ARCH_p_sh = rep(0,7);ARCH_p_sz = rep(0,7)
ARCH_chi2_sh = rep(0,7);ARCH_chi2_sz = rep(0,7)
ARCH_pp_sh = rep(0,7);ARCH_pp_sz = rep(0,7)
ARCH_Q_sh = rep(0,7);ARCH_Q_sz = rep(0,7)
for(i in c(1:7)){
  a = ArchTest(log(datas$上证指数),lag = i)
  b = Box.test(log(datas$上证指数),lag = i,type = "Ljung-Box")
  ARCH_chi2_sh[i] = a$statistic
  ARCH_p_sh[i] = a$p.value
  ARCH_Q_sh[i] = b$statistic
  ARCH_pp_sh[i] = b$p.value
  c = ArchTest(log(datas$深证指数),lag = i)
  d = Box.test(log(datas$深证指数),lag = i,type = "Ljung-Box")
  ARCH_chi2_sz[i] = c$statistic
  ARCH_p_sz[i] = c$p.value
  ARCH_Q_sz[i] = d$statistic
  ARCH_pp_sz[i] = d$p.value
}
##GARCH模型拟合
AIC_garch_sh = matrix(0,ncol = 4,nrow = 4)
AIC_garch_sz = matrix(0,ncol = 4,nrow = 4)
for(i in c(1:4)){
  for(j in c(1:4)){
    AIC_garch_sh[i,j] = AIC(garch(log(datas$上证指数),
                                  order = c(i,j),trace = F))
    AIC_garch_sz[i,j] = AIC(garch(log(datas$深证指数),
                                  order = c(i,j),trace = F))
  }
}

garch_model_sh = garch(log(datas$上证指数),order = c(4,1),trace = F)
garch_model_sz = garch(log(datas$深证指数),order = c(4,1),trace = F)
summary(garch_model_sh)
summary(garch_model_sz)
##GARCH模型的条件方差预测
sig_sh = data.frame("date" = set[c(1:182),1],"cv" = garch_model_sh$fitted.values[,1]^2,"groups" = "上证指数")
sig_sz = data.frame("date" = set[c(1:182),1],"cv" = garch_model_sz$fitted.values[,1]^2,"groups" = "深证指数")
sig = rbind(sig_sh,sig_sz)
ggplot(sig,aes(x = date,y = cv,groups = groups,color = groups,fill = groups))+
  geom_area(position = "identity")+theme_bw()+
  scale_fill_manual(values = c("#FF4B68","#a5dff9"))+
  scale_color_manual(values = c("#FF4B68","#a5dff9"))
ggsave("cvcurve.svg")
##删去代码运行中的一些中间变量
rm(
  list = c("a","b","c","d","i","j","n","ABIC_get",
            "forecast_sh","forecast_sz","r","sh1","sh5","sig",
            "sz1","sz3","set","set2","pw_model_sh","AIC_SH",
            "AIC_SZ","BIC_SH","BIC_SZ","f","q_sh",
            "p_sh","f_sh","q_sz","p_sz","f_sz")
   )