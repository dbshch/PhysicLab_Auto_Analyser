s_args<-commandArgs()
library(xtable)
library(ggplot2)
args<-c(1:5)
for(i in 6:20){
  args<-c(args,as.numeric(s_args[i]))
}
rct=expression(R*C)
urct=expression(C^2*uR^2+R^2*uC^2)
rlt=expression(L/R)
urlt=expression(L*uR/R/R)
rlct=expression((L*C)^0.5)
urlct=expression(L^0.5*uC/2/(C)^0.5)
re=expression(T.5/log(2))
ure=expression(uT.5/log(2))
rlce=expression(T.5/bt)
urlce=expression(uT.5/bt)
sf1<-function(model,goal){
  now<-0.8
  d<-0.1
  while(d>0.0001){
    pref<-abs(predict(model,data.frame(ff0=seq(now-d,now+d,d/10)))-goal)
    for(i in 1:21){
      if(pref[i]==min(pref)){
        now<-now+(i-11)*d/10
        d<-d/10
        break
      }
    }
  }
  return(now[1])
}

sf2<-function(model,goal){
  now<-1.2
  d<-0.1
  while(d>0.0001){
    pref<-abs(predict(model,data.frame(ff0=seq(now-d,now+d,d/10)))-goal)
    for(i in 1:21){
      if(pref[i]==min(pref)){
        now<-now+(i-11)*d/10
        d<-d/10
        break
      }
    }
  }
  return(now[1])
}

d<-""
R<-args[6]
C<-args[9]*10^(-9)
f<-args[7]*1000
E<-args[8]
T.5<-args[10]*10^(-6)
uR<-0.01
uf<-1
uE<-0.01
uC<-0.01*10^(-9)
uT.5<-0.001*10^(-6)
taut<-eval(rct)
taue<-eval(re)
utaut<-eval(urct)
utaue<-eval(ure)
T1<-data.frame(R,f,E,C,T.5,uR,uf,uE,uC,uT.5,taue,taut,utaut,utaue)
tmp<-paste(T1$taut,T1$utaut,T1$utaut/T1$taut*100,T1$taue,T1$utaue,T1$utaue/T1$taue*100,(T1$taue-T1$taut)/T1$taut*100,sep=";")
d<-paste(d,tmp,"\r\n",sep="")


f<-args[11]*1000
E<-args[12]
T.5<-args[13]*10^(-6)
L<-0.01
uR<-0.01
uf<-1
uE<-0.01
uC<-0.01*10^(-9)
uT.5<-0.01*10^(-6)
taut<-eval(rlt)
taue<-eval(re)
utaut<-eval(urlt)
utaue<-eval(ure)
T2<-data.frame(R,f,E,C,T.5,uR,uf,uE,uC,uT.5,taue,taut,utaut,utaue)
tmp<-paste(T2$taut,T2$utaut,T2$utaut/T2$taut*100,T2$taue,T2$utaue,T2$utaue/T2$taue*100,(T2$taue-T2$taut)/T2$taut*100,sep=";")
d<-paste(d,tmp,"\r\n",sep="")

f<-args[15]*1000
E<-args[14]
T.5<-args[16]*10^(-6)
L<-0.01
uR<-0.01
uf<-1
uE<-0.01
uC<-0.01*10^(-9)
uT.5<-0.01*10^(-6)
bt<-1.68
taut<-eval(rlct)
taue<-eval(rlce)
utaut<-eval(urlct)
utaue<-eval(urlce)
T3<-data.frame(L,f,E,C,T.5,uf,uE,uC,uT.5,taue,taut,utaut,utaue)
tmp<-paste(T3$taut,T3$utaut,T3$utaut/T3$taut*100,T3$taue,T3$utaue,T3$utaue/T3$taue*100,(T3$taue-T3$taut)/T3$taut*100,sep=";")
d<-paste(d,tmp,"\r\n",sep="")



f0<-1/(2*pi*(L*C)^0.5)
uf0<-uC/4/pi/(L*C^3)^0.5
E<-args[17]
if (s_args[22]=="0"){
  f<-seq(args[18],args[19],args[20])*1000
} else {
  f<-c()
  for (i in strsplit(s_args[22],",")){
    f<-c(f, as.numeric(i))
  }
  f<-f*1000
}
UR<-c()
for (i in strsplit(s_args[21],",")){
  UR<-c(UR, as.numeric(i))
}
# UR<-c(2.48,2.84,3.2,3.64,4.12,4.76,5.4,6.16,6.92,7.48,7.68,7.52,7.08,6.48,5.9,5.36,4.92,4.5,4.16,3.84,3.6)
uUR<-0.01
phi<-atan((2*pi*f*L-1/2/pi/f/C)/R)*180
ff0<-f/f0
f<-f/1000
T4<-data.frame(R,uR,L,C,uC,f0,E,UR,f,ff0,phi,uUR)
f<-f*1000
Q<-1/(2*pi*f0*R*C)
uff0<-((uf/f0)^2+(f*uf0/f0^2)^2)^0.5
uQ<-((uf/2/pi/R/C/f0^2)^2+(uR/2/pi/R^2/C/f0^2)^2+(uC/2/pi/R/C^2/f0)^2)^0.5
tmp<-paste(Q,uQ,sep=";")
p1<-ggplot(data=T4,mapping=aes(x=ff0,y=UR))+geom_point()+geom_smooth(span=0.5)+xlab(expression(f/f[0]))+ylab(expression(paste(U[R],"[V]")))+geom_errorbar(aes(ymin=UR-uUR, ymax=UR+uUR), width=.005)
p1
ggsave("static\\p1.jpeg", dpi=300, width=4, height=3)
p2<-ggplot(data=T4,mapping=aes(x=ff0,y=phi))+geom_point()+geom_smooth(span=0.5)+xlab(expression(f/f[0]))+ylab(expression(phi))
p2
ggsave("static\\p2.jpeg", dpi=300, width=4, height=3)
pre=data.frame(ff0,UR)
model<-loess(UR~ff0,data=pre,span=0.4)
UM<-predict(model,data.frame(ff0=1))
goal<-UM/sqrt(2)
f1<-sf1(model,goal)
f2<-sf2(model,goal)
Qe<-1/(f2-f1)
urQ<-abs((Qe-Q)/Q*100)

tmp<-paste(tmp,f0,uf0,f1,f2,Qe,urQ,sep=";")
write(print(xtable(T4[8:11],digits=c(1,2,3,4,2))), "table.txt")
pre<-data.frame(ff0,UR)

d<-paste(d,tmp,sep="")

write(d, "foo.txt")