 OÖK <- pdmpBorder(descr = "3Gebiete, Übergangsraten sind nicht zeitkonstant",
                  parms = list(β = 1, a_11 = -1, a_21 = -1.5, a_31 = -1.2, a_22 = 2, a_12 = 2.5, a_32 = 3, a_23= -2.5, a_13 = -3, a_33 = -2, 
                                s_12 = 1, s_21 = 2, s_31 = 1, s_13 = 2, s_23 = 3, s_32 = 4, 
                                κ112 = 5, κ113 = 3, κ131 = 3, κ132 = 0, κ121 = 0, κ123 = 1, κ212 = 5, κ213 = 3, κ231 = 3, κ232 = 0, κ221 = 0, 
                                κ223 = 1 , κ312 = 4.5, κ313 = 3, κ331 = 3, κ332 = 0, κ321 = 0, κ323 = 1),
                  init = c(energy = 6, area = 1, modi = 2),
                  borroot = function(t, x, parms){return(c(x[1]-9))}, 
                  terroot = function(t, x, parms){return(c(x[1]))},
                  discStates = list(area = (c(1, 2, 3)), modi = c(1, 2, 3)),
                  dynfunc = function(t, x, parms) {
                    denergy <- with(as.list(c(x, parms)),switch(area, switch(modi, a_11*energy - β, a_12 - β*energy, a_13*energy - β), 
                                                            switch(modi, a_21*energy - β, a_22 - β*energy, a_23*energy - β), 
                                                            switch(modi, a_31*energy - β, a_32 - β*energy, a_33*energy - β))) 
                    return(c(denergy, 0, 0))
                  },
                  ratefunc = function(t, x, parms) {
                    return(with(as.list(c(x, parms)), c(switch(area, switch(modi, c(κ112, κ113, s_12*energy, s_13*energy), c(κ123, κ121, 0, 0), c(κ131, κ132, 0, 0)),
                                                                  switch(modi, c(κ212, κ213, s_23*energy, s_21*energy), c(κ223, κ221, 0, 0), c(κ231, κ232, 0, 0)),
                                                                  switch(modi, c(κ312, κ313, s_31*energy, s_32*energy), c(κ323, κ321, 0, 0), c(κ331, κ332, 0, 0))))))
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