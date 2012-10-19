
tetraplot<- function(tcsres, size=5, col='black', new=T, hspin=T, vspin=F) {

if(new)
   open3d()

# can't figure out how to change the character type

ttv=pavo::ttvertex

plot3d(unlist(ttv[c('xu','xs','xm','xl')]),
		unlist(ttv[c('yu','ys','ym','yl')]),
		unlist(ttv[c('zu','zs','zm','zl')]),
		size=5, box=F, axes=F, xlab='',ylab='',zlab='',
		col=c('purple','blue','green','red'))

segments3d(ttv[c('xu','xs')], ttv[c('yu','ys')], ttv[c('zu','zs')], alpha=0.5)
segments3d(ttv[c('xu','xm')], ttv[c('yu','ym')], ttv[c('zu','zm')], alpha=0.5)
segments3d(ttv[c('xu','xl')], ttv[c('yu','yl')], ttv[c('zu','zl')], alpha=0.5)
segments3d(ttv[c('xs','xm')], ttv[c('ys','ym')], ttv[c('zs','zm')], alpha=0.5)
segments3d(ttv[c('xs','xl')], ttv[c('ys','yl')], ttv[c('zs','zl')], alpha=0.5)
segments3d(ttv[c('xl','xm')], ttv[c('yl','ym')], ttv[c('zl','zm')], alpha=0.5)

points3d(0,0,0,col='grey', pch=22,size=2)

points3d(tcsres$tcs[,c('x','y','z')], size=size, color=col)

# #   vertices <- c( 
     # -0.7, -0.5, -0.3, 1.0,
      # 0.7, -0.5, -0.3, 1.0,
      # 0.7,  0.8, -0.3, 1.0,
     # -0.7,  0.8, -0.3, 1.0
  # )
  # indices <- c( 1, 2, 3, 4 )
  
# wire3d( qmesh3d(vertices,indices) )

if(hspin)
   play3d(spin3d(axis=c(0,0,1), rpm=10), duration=5)

if(vspin)
   play3d(spin3d(axis=c(1,0,0), rpm=10), duration=5)

}



require(pavo)

a=getspec('~/github/pavo/examplespec')
b=getspec('~/github/pavo/examplespec','ttt')
a=data.frame(cbind(a,b))
rm(b)
vis.a=vismodel(a)
tcs.a=tcs(vis.a)



tetraplot(tcs.a, new=F)


#saving options seem very limited
rgl.snapshot('testplot.png')