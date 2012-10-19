
tetraplot<- function(tcsres, size=0.025, col='black', new=T, hspin=T, vspin=F, floor=T) {

if(new)
   open3d()

# can't figure out how to change the character type

ttv=pavo::ttvertex

cu=t(col2rgb('#984EA3'))/255
cs=t(col2rgb('#377EB8'))/255
cm=t(col2rgb('#4DAF4A'))/255
cl=t(col2rgb('#E41A1C'))/255

plot3d(unlist(ttv[c('xu','xs','xm','xl')]),
		unlist(ttv[c('yu','ys','ym','yl')]),
		unlist(ttv[c('zu','zs','zm','zl')]), type='s', lit=F,
		radius=0.02, box=F, axes=F, xlab='',ylab='',zlab='',
		col=c(rgb(cu[1],cu[2],cu[3]), rgb(cs[1],cs[2],cs[3]), 
		rgb(cm[1],cm[2],cm[3]), rgb(cl[1],cl[2],cl[3])))


segments3d(ttv[c('xu','xs')], ttv[c('yu','ys')], ttv[c('zu','zs')], color='lightgrey')
segments3d(ttv[c('xu','xm')], ttv[c('yu','ym')], ttv[c('zu','zm')], color='lightgrey')
segments3d(ttv[c('xu','xl')], ttv[c('yu','yl')], ttv[c('zu','zl')], color='lightgrey')
segments3d(ttv[c('xs','xm')], ttv[c('ys','ym')], ttv[c('zs','zm')], color='lightgrey')
segments3d(ttv[c('xs','xl')], ttv[c('ys','yl')], ttv[c('zs','zl')], color='lightgrey')
segments3d(ttv[c('xl','xm')], ttv[c('yl','ym')], ttv[c('zl','zm')], color='lightgrey')

spheres3d(0,0,0,col='grey', radius=0.01, lit=F)

spheres3d(tcsres$tcs[,c('x','y','z')], radius=size, color=col, lit=F)

if(floor){
  vertices <- c( 
      -0.7, -0.5, -0.3, 1.0,
       0.7, -0.5, -0.3, 1.0,
       0.7,  1, -0.3, 1.0,
      -0.7,  1, -0.3, 1.0
  				)
  indices <- c( 1, 2, 3, 4 )
  
 wire3d( qmesh3d(vertices,indices), lit=F )
	}
	
if(hspin)
   play3d(spin3d(axis=c(0,0,1), rpm=20), duration=3)

if(vspin)
   play3d(spin3d(axis=c(1,0,0), rpm=20), duration=3)

}



require(pavo)
require(rgl)

 a=getspec('~/github/pavo/examplespec')
 b=getspec('~/github/pavo/examplespec','ttt')
 a=data.frame(cbind(a,b))
 rm(b)
 vis.a=vismodel(a)
 tcs.a=tcs(vis.a)



tetraplot(tcs.a, new=T,hspin=T)

#voltest<-t(convhulln(tcs.a$tcs[,c('x','y','z')]))

#coord <- tcs.a$tcs[,c('x','y','z')]

#rgl.triangles(coord[voltest,1],coord[voltest,2],coord[voltest,3],alpha=.2)

#saving options seem very limited
rgl.postscript('testplot.pdf',fmt='pdf')
rgl.snapshot('testplot.png')