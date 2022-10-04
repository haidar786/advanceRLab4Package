printA <- function(x, ...) {

}


data(iris)
mod_object <- linreg(Petal.Length~Species, data = iris)

printA(mod_object)
