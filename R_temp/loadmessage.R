.onAttach <- function(lib, pkg)  {
    packageStartupMessage('When citing results from pavo, refer to original sources:
tristimulus summary variables: Montgomerie 2006
photon catch model: Vorobyev & Osorio 1998, Vorobyev et al. 1998
tetracolorspace model and volume overlap: Stoddard & Prum 2008, Stoddard & Stevens 2011
package implementation: Maia et al. Submitted (see citation())

see vignette for details and full referneces'

,
                          appendLF = TRUE)
}
