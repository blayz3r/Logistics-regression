data=read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
sigmoid=function(p){
exp(p)/(1+exp(p))
}
y=data$admit
n=nrow(data)
x=cbind(1,data$gre,data$gpa)
Ba=solve(t(x)%*%x)%*%t(x)%*%(y)
ua=x%*%Ba
itr=function(Ba){
u=r=as.vector(array(NA,n))
for(i in 1:n){
u[i]=sigmoid(t(Ba)%*%x[i,])
r[i]=u[i]*(1-u[i])
}
S=diag(r)
list(u=u,Sa=S)
}
ic=0
eps=1
while(ic<1000 && eps>0.000001){
BOLD=(solve(t(x)%*%itr(Ba)$Sa%*%x))%*%t(x)%*%((itr(Ba)$Sa%*%x%*%Ba)+y-itr(Ba)$u)
BNEW=(solve(t(x)%*%itr(BOLD)$Sa%*%x))%*%t(x)%*%((((itr(BOLD)$Sa))%*%x%*%BOLD)+y-itr(BOLD)$u)
eps=(t(BNEW)%*%BNEW)-(t(BOLD)%*%BOLD)
BOLD=BNEW
ic=ic+1
}





