# Lösung zu functions


library(checkmate)

################################### a ##########################################
set.seed(20141012)
x <- sample(replace = TRUE, 20, x = c(1:10, NA))
y <- runif(min = 0, max = 1, 20)
cor(x = x, y = y, use = "pairwise.complete.obs", method = "kendall")



################################### b ##########################################
# Rueckgabewert ist 3.
# Dies entsteht aus dem "lazy evaluation" Prinzip. Die Werte werden erst
# ausgewertet wenn sie benoetigt werden. Da zuerst x ausgewertet wird, 
# wird y auf den Wert 1 festgesetzt und x auf 2.
# Der Eingabewert y = 0 wird nicht mehr ausgewertet.

# Falls man y + x betrachtet wird erst y ausgewertet und auf 0 gesetzt und im 
# Anschluss erst x.



################################### c ##########################################
# Rueckgabewert ist 100.
# Entspricht ebenfalls dem "lazy evaluation" Prinzip. Die Wert x wird erst 
# ausgewertet sobald er benoetigt wird. Vorher ist z schon mit 100 gespeichert.




################################### d ##########################################
'%xor%' <- function(expr_1, expr_2) {
  
  # Input check
  assert_logical(expr_1)
  assert_logical(expr_2)
  assert_set_equal(length(expr_1), length(expr_2))
  
  # xor implementation
  result <- c()
  for (i in 1:length(expr_1)) {
    if (identical(expr_1[i], expr_2[i])) {
      result <- c(result, FALSE)
    }
    if (!identical(expr_1[i], expr_2[i])) {
      result <- c(result, TRUE)
    }
  }
  
  result
} 



################################### e ##########################################
## options ()
# Sichere aktuelle Einstellungen
options_save <- options()
# Verändern
options(digits = 15)
# wiederherstellen
options(options_save)


## par()
# Sichere aktulle Einstellung
par_save <- par(no.readonly = TRUE)
# Verändern
par(mfrow = c(2,2))
# wiederherstellen
par(par_save)


################################### f ########################################## 
plot_new <- function(x, y, ...) {
  # Ensure that graphic will be closed at the end of the funcition
  on.exit(dev.off())
  
  # Constructe function and plots.
  dev.new()
  plot(x, y, ...)
}




################################### g ##########################################
# Rueckgabewert: Vektor mit (c = 10) also:
# Das erste c bedeudet: ein Vekor wird gebildet
# Das zweite c: String fuer den der Wert abgeseichert werden soll
# Das dritte c: Wert der dem String zugeordnet werden soll 
# (vorher auf 10 gesetzt)
