#GOVARDOVSKII (2000) VISUAL TEMPLATE
#Fix to include other members of photoreceptors
#Beta-band calculation incorporated

sens3<-function(umax,smax,mmax,lmax,scut=0,mcut=0,lcut=0)

{
	
#Sensitivites w/o oil droplets
S.u<-1/(exp(69.7*(.8795+.0459*exp(-(umax-300)^2/11940)-(umax/(300:700))))+exp(28*(.922-umax/(300:700)))+exp(-14.9*(1.104-(umax/(300:700))))+.674)
S.s<-1/(exp(69.7*(.8795+.0459*exp(-(smax-300)^2/11940)-(smax/(300:700))))+exp(28*(.922-smax/(300:700)))+exp(-14.9*(1.104-(smax/(300:700))))+.674)
S.m<-1/(exp(69.7*(.8795+.0459*exp(-(mmax-300)^2/11940)-(mmax/(300:700))))+exp(28*(.922-mmax/(300:700)))+exp(-14.9*(1.104-(mmax/(300:700))))+.674)
S.l<-1/(exp(69.7*(.8795+.0459*exp(-(lmax-300)^2/11940)-(lmax/(300:700))))+exp(28*(.922-lmax/(300:700)))+exp(-14.9*(1.104-(lmax/(300:700))))+.674)

#OIL DROPLET TRANS - INPUT CUT AND MID (AFTER HART ET AL. 2005)
#T.x<-function(l.cut,l.mid) {
#exp(-exp(-2.89*(0.5/(l.mid-l.cut))*(300:700-l.cut)+1.08))
#}

#Calculating beta band

A.B = 0.26

b.u = -40.5+0.195*umax
b.s = -40.5+0.195*smax
b.m = -40.5+0.195*mmax
b.l = -40.5+0.195*lmax

umB.u = 189+0.315*umax
umB.s = 189+0.315*smax
umB.m = 189+0.315*mmax
umB.l = 189+0.315*lmax

SB.u = A.B*exp(-(((300:700)-umB.u)/b.u)^2)
SB.s = A.B*exp(-(((300:700)-umB.s)/b.s)^2)
SB.m = A.B*exp(-(((300:700)-umB.m)/b.m)^2)
SB.l = A.B*exp(-(((300:700)-umB.l)/b.l)^2)


#Adding beta band to alpha band

F.u = S.u+SB.u
F.s = S.s+SB.s
F.m = S.m+SB.m
F.l = S.l+SB.l

F.u = F.u/max(F.u)
F.s = F.s/max(F.s)
F.m = F.m/max(F.m)
F.l = F.l/max(F.l)


if(scut&mcut&lcut==0)

{

final<-data.frame(u=F.u,s=F.s,m=F.m,l=F.l)


}

else

{

#OIL DROPLET TRANSMISSION FROM λ CUT (HART ET AL. 2005)
T.u<-exp(-exp(-2.89*(.5/((.96*300+33.57)-300))*(300:700-300)+1.08))
T.s<-exp(-exp(-2.89*(.5/((.99*scut+24.38)-scut))*(300:700-scut)+1.08))
T.m<-exp(-exp(-2.89*(.5/((.9*mcut+70.03)-mcut))*(300:700-mcut)+1.08))
T.l<-exp(-exp(-2.89*(.5/((.99*lcut+28.65)-lcut))*(300:700-lcut)+1.08))

#OCULAR MEDIA SPECTRUM MODEL (HART ET AL. 2005)
#T.e<-log(8.928*10^-13*(300:700)^5-2.595*10^-9*(300:700)^4+3.006*10^-6*(300:700)^3-.001736*(300:700)^2+.5013*(300:700)-55.56)

final<-data.frame(u=F.u*T.u,s=F.s*T.s,m=F.m*T.m,l=F.l*T.l)


}

final

}