
‎A<-read.csv("C:\\Users\\Admin\\Desktop\\sample.ver 3.csv")‎
‎z<-vector(mode="numeric")‎
‎for (i in 1:70){‎
‎W<-function(a){a*(log(‎
‎((a-1)*A$hl[i]+A$m[i])/(1-((a-1)*A$hl[i]+A$m[i]))‎
‎))+a*(log(((1-a)*A$hr[i]+A$m[i])/(1-((1-a)*A$hr[i]+A$m[i]))))}‎
‎Q<-integrate(W,0,1)$value‎
‎z[i]<-Q}‎
‎B<-matrix(c(rep(1,70),age,pain,type,cholestoral,blood),nrow=70,ncol=6)‎
‎B1=z*B‎
‎C<-apply(B1,2,sum),C<-as.matrix(C),D<-D%*%B‎
‎B=2*D%*%C‎
‎\end{verbatim}‎
‎\end{latin}‎
کدهای برنامه آزمون الگوریتم به صورت زیر می‌باشد:
‎\begin{latin}‎
‎\begin{verbatim}‎
‎library(foreign)‎
 ‎T<-read.spss("C:\\Users\\Admin\\Desktop\\testing.sav")‎
 ‎d1<-T$age‎, ‎d2<-T$pain‎
 ‎d3<-T$type,d4<-T$cholestoral,d5<-T$blood‎
‎N<-matrix(c(rep(1,233),d1,d2,d3,d4,d5),nrow=233,ncol=6)‎
‎R<-vector(mode="numeric")‎
‎X1<-vector(mode="numeric")‎
‎Z<-vector(mode="numeric")‎
‎for (i in 1:233){‎
‎X<-function(x){‎
‎K<-(B[1]+B[2]*N[i,2]+B[3]*N[i,3]+B[4]*N[i,4]+B[5]*N[i,5]+B[6]*N[i,6])‎
‎(1-2^{-x})*1/(1+exp(-((1/2)*(x-1)+log(9/2)+K)))‎+
‎(1-2^{-x})*1/(1+exp(-((1/2)*(1-x)+log(9/2)+K)))}‎
‎Q<-integrate(X,0,1)$value‎
‎Z[i]<-Q}‎
‎for (i in 1:233){‎
‎Q1<-ifelse(Z[i]<0.5‎, ‎0‎, ‎1)‎
‎X1[i]<-Q1}‎
‎\end{verbatim}‎
‎\end{latin}‎
کدهای مربوط به شبیه سازی عبارتند از:
‎\begin{latin}‎
‎\begin{verbatim}‎
‎a1<-runif(70,33,80),a1<-round(a1)‎
‎a2<-runif(70,100,200‎, ‎a2<-round(a2)‎
‎a3<-runif(70,140,570),a3<-round(a3)‎
‎a4<-runif(70,110,200),a4<-round(a4)‎
‎a5<-runif(70,0,6.5)‎
 ‎c=rep(c(0.3,0.8),c(60,10))‎, ‎cl<-runif(60,0,0.3),cll<-runif(10,0.6,0.8)‎
 ‎c1<-c(cl,cll),cu<-runif(60,0.3,0.6)‎, ‎cuu<-runif(10,0.8,1)‎
 ‎c2<-c(cu,cuu)‎, ‎hl=c-c1‎, ‎hu=c2-c‎
‎z<-vector(mode="numeric")‎
‎for (i in 1:70){‎
‎W<-function(a){a*(log(‎
‎((a-1)*hl[i]+c[i])/(1-((a-1)*hl[i]+c[i]))‎
‎))+a*(log(((1-a)*hu[i]+c[i])/(1-((1-a)*hu[i]+c[i]))‎
))}
‎Q<-integrate(W,0,1)$value‎
‎z[i]<-Q}‎
‎z[1]+z[2]‎
‎B<-matrix(c(rep(1,70),a1,a2,a3,a4,a5),nrow=70,ncol=6)‎
‎B1=z[1]*B[1,],B1=z*B‎
‎C<-apply(B1,2,sum)‎
‎C<-as.matrix(C),D<-t(B),str(D)‎
‎D<-D%*%B,D<-solve(D),bi=2*D%*%C‎
‎MS=(B-bi)^2,k=apply(MS,2,sum)‎
‎
