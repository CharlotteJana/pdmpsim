 OÖK <- pdmpBorder(descr = "3Gebiete, Übergangsraten sind nicht zeitkonstant",
                  parms = list(β = 1, a_11 = -1, a_21 = -1.5, a_31 = -1.2, a_22 = 2, a_12 = 2.5, a_32 = 3, a_23= -2.5, a_13 = -3, a_33 = -2, 
                                s_12 = 1, s_21 = 2, s_31 = 1, s_13 = 2, s_23 = 3, s_32 = 4, 
                                κ112 = 5, κ113 = 3, κ131 = 3, κ132 = 0, κ121 = 0, κ123 = 1, κ212 = 5, κ213 = 3, κ231 = 3, κ232 = 0, κ221 = 0, 
                                κ223 = 1 , κ312 = 4.5, κ313 = 3, κ331 = 3, κ332 = 0, κ321 = 0, κ323 = 1),
                  init = c(ξ = 6, θ = c(1, 2)),
                  borroot = function(t, x, parms){return(c(x[1]-2))}, 
                  terroot = function(t, x, parms){return(c(x[1]))},
                  discStates = list(area = (c(1, 2, 3)), modi = c(1, 2, 3)),
                  dynfunc = function(t, x, parms) {
                    dξ <- with(as.list(c(x, parms)),switch(θ1, switch(θ2, a_11*ξ - β, a_12 - β*ξ, a_13*ξ - β), 
                                                            switch(θ2, a_21*ξ - β, a_22 - β*ξ, a_23*ξ - β), 
                                                            switch(θ2, a_31*ξ - β, a_32 - β*ξ, a_33*ξ - β))) 
                    return(c(dξ, 0, 0))
                  },
                  ratefunc = function(t, x, parms) {
                    return(with(as.list(c(x, parms)), c(switch(θ1, switch(θ2, c(κ112, κ113, s_12*ξ, s_13*ξ), c(κ123, κ121, 0, 0), c(κ131, κ132, 0, 0)),
                                                                  switch(θ2, c(κ212, κ213, s_23*ξ, s_21*ξ), c(κ223, κ221, 0, 0), c(κ231, κ232, 0, 0)),
                                                                  switch(θ2, c(κ312, κ313, s_31*ξ, s_32*ξ), c(κ323, κ321, 0, 0), c(κ331, κ332, 0, 0))))))
                  },
                  jumpfunc = function(t, x, parms, jtype) {
                    return(with(as.list(c(x,parms)), c(x[1], switch(jtype, switch(x[2], 1, 2, 3), switch(x[2], 1, 2, 3), 
                                                                    switch(x[2], 2 ,3 ,1), switch(x[2], 3, 2, 1)),
                      switch(jtype, switch(x[3], 2, 3, 1), switch(x[3], 3, 1, 2), switch(x[3], 1, 2, 3), switch(x[3], 1, 2, 3)))))
                  },  
                  
                  borderfunc = function(t, x, parms){ return(with(as.list(c(x,parms)), c(x[1:2], 1)))
                  },
                  times = c(from = 0, to = 100, by = 0.1))

# load it and plot a simulation:
data("OÖK")
plot(sim(OÖK))