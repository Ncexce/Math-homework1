#Created on 2020/2/24,9:17
#Data Initialize
source=data.frame(x=1:10,y=c(15152,5093,2644,2009,2051,1891,1751,825,892,399))
print(source)
sect1=data.frame(x=1:3,y=c(15152,5093,2644))
sect2=data.frame(x=3:7,y=c(2644,2009,2051,1891,1751))
sect3=data.frame(x=7:10,y=c(1751,825,892,399))
#Regression Calc
fs1=lm(y~x,sect1)
fs2=lm(y~x,sect2)
fs3=lm(y~x,sect3)
#Export graph plots.svg
svg("plots.svg")
plot(source,xlim=c(1,12))
abline(a=fs1$coefficients[1],b=fs1$coefficients[2],col="green")
abline(a=fs2$coefficients[1],b=fs2$coefficients[2],col="blue")
abline(a=fs3$coefficients[1],b=fs3$coefficients[2],col="red")
dev.off()
#Print the graph on the screen
plot(source,xlim=c(1,12))
abline(a=fs1$coefficients[1],b=fs1$coefficients[2],col="green")
abline(a=fs2$coefficients[1],b=fs2$coefficients[2],col="blue")
abline(a=fs3$coefficients[1],b=fs3$coefficients[2],col="red")
#Solve the function(y=0) && Export
f=function(x, a, b){return(a*x+b)}
root=uniroot(f,c(10,12),a=fs3$coefficients[2],b=fs3$coefficients[1],tol=0.1)
root$root
# Will vanish after 11 days.