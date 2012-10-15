huedisp <- function(res.p){
ind=t(combn(nrow(res.p),2))
apply(ind,1, function(x)	
	 acos((cos(res.p[x[1],'h.phi'])*cos(res.p[x[2],'h.phi'])*cos(res.p[x[1],'h.theta'] -
	 res.p[x[2],'h.theta'])) + (sin(res.p[x[1],'h.phi'])*sin(res.p[x[2],'h.phi'])))
     )
}


