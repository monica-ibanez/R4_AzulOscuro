##### LIBRERÍAS ################################################################
#PLUMBER
if (!require(plumber)){
  install.packages("plumber")
  library(plumber)
} else {
  library(plumber)
}



#* @apiTitle Recomendador de productos de Eroski
#* @apiDescription Esta Interfaz de Programación de Aplicaciones tiene como objetivo recomendar productos a clientes de Eroski

#* Echo back the input AAAAAAAAAAAAA
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
