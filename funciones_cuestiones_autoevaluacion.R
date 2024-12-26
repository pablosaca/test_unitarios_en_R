

library(testthat)

# CUESTIÓN 2

count_word_details <- function(word_vec){
  
  word_count <- table(word_vec) # con una tabla de frecuencias podemos contar las veces que un elemento se repite en un vector
  
  letter_reporting_vec <- c() # se genera un vector para ir incorporando el número de letras que tiene la palabra
  concurrence_reporting_vec <- c() # se genera un vector para ir incorporando el número de concurrencias de la palabra en el vector
  for (word in names(word_count)) {
    word_length <- nchar(word) # cuenta letra de cada palabra
    letter_reporting_vec[word] <- word_length # se añade al vector el número de letras que tiene la palabra (similar a diccionario de python incluyendo la palabra como key)
    concurrence_reporting_vec[word] = word_count[word] # se añade al vector el número de veces que aparece la palabra en la secuencia a analizar (similar a diccionario de python incluyendo la palabra como key)
  }
  # finalmente, se crea un dataframe como formato de salida
  reporting_df <- data.frame(n_cocurrencia_palabra = concurrence_reporting_vec, n_caracteres_palabra = letter_reporting_vec)
  return(reporting_df)
}

test_that("Tests Unitarios de la función count_word_details - Salida", {
  
  word_vec <- c("master", "formacion", "datos", "analitica", "aprendizaje", "estadistica")
  output <- count_word_details(word_vec)
  
  expect_is(output, "data.frame") # chequeo la salida es un dataframe
  expect_equal(ncol(output), 2)  # chequeamos el número de columnas
  expect_equal(nrow(output), 6)  # chequeamos el número de filas
  expect_named(output, c("n_cocurrencia_palabra", "n_caracteres_palabra")) # chequeamos el nombre del dataframe
}
)

# CUESTIÓN 3

impar_number <- function(n) {
  
  # chequea si el valor no es numérico
  if (!is.numeric(n)) {
    stop(paste0("El valor introducido no es un número. Prueba otra vez."))
  }
  
  # chequea si el valor no es un número entero
  if (n != floor(n)) {
    stop(paste0("El valor introducido no es un número entero: ", n, ". Prueba otra vez."))
  }
  
  if (n %% 2 != 0) {
    return(n)  # devuelve el número que se le pasa a la función
  } else {
    return(0)  # devuelve siempre 0
  }
}

cummulative_impar_number <- function(input_){
  
  if (!is.list(input_)){
    stop(paste0("La entrada de la función debe ser una lista"))
  }

  cum_number_vec <- c() # vector donde se va a obtener la suma acumulada
  # iteramos sobre las posiciones de la lista (por eos )
  for (i in 1:length(input_)){
    value <- impar_number(input_[[i]]) # chequea si el número es par o impar
    cum_number_vec[i] <- value # incluimos los valores en el vector acumulado
  }
  return(sum(cum_number_vec)) # suma de los vectores
}


test_that("Tests Unitarios de la función impar-par", {
  
  expect_error(impar_number("hola"), "El valor introducido no es un número. Prueba otra vez.")
  expect_error(impar_number(5.5), "El valor introducido no es un número entero: 5.5. Prueba otra vez.") 
  expect_equal(impar_number(5), 5)  # chequeamos que el número sea impar
  expect_equal(impar_number(6), 0)  # chequeamos que el número sea par
}
)


test_that("Tests Unitarios de la función cummulative_impar_number", {
  
  expect_error(cummulative_impar_number(c(15, 3, 10)), "La entrada de la función debe ser una lista")
  expect_error(cummulative_impar_number(10), "La entrada de la función debe ser una lista")
  expect_error(
    cummulative_impar_number(
      list("formacion", "uned", "datos")), "El valor introducido no es un número. Prueba otra vez."
  ) # realmente está chequeando la función impar_number
  expect_error(
    cummulative_impar_number(
      list(10, 5, 2, "uned")), "El valor introducido no es un número. Prueba otra vez."
    ) # realmente está chequeando la función impar_number
  
  
  expect_equal(cummulative_impar_number(list(6, 4, 2)), 0)  # chequeamos resultado de la suma (igualdad)
  expect_equal(cummulative_impar_number(list(3, 2, 5, 10)), 8)  # chequeamos resultado de la suma (igualdad)
  expect_gt(cummulative_impar_number(list(15)), 0)  # chequeamos resultado de la suma (mayor que cero)
  expect_lte(cummulative_impar_number(list(15, 2, 5, 10, 5)), 30)  # chequeamos resultado de la suma (menor o igual)
  
  expect_is(cummulative_impar_number(list(15, 2, 5, 10, 5)), "numeric") # chequeo la salida es un valor numérico
}
)

# CUESTIÓN 4

number_to_string <- function(n) {
  
  # chequea si el valor no es numérico
  if (!is.numeric(n)) {
    stop(paste0("El valor introducido no es un número. Prueba otra vez."))
  }
  
  # chequea si el valor no es un número entero
  if (n != floor(n)) {
    stop(paste0("El valor introducido no es un número entero: ", n, ". Prueba otra vez."))
  }
  
  # la función interna letters proporciona un vector de caracteres con las letras del abecedario
  string <- paste(sample(letters, n), collapse = "") # se genera aleatoriamente una muestra de caracteres de tamaño n
  return(toupper(string))
}

cummulative_string <- function(input_){
  
  if (!is.list(input_)){
    stop(paste0("La entrada de la función debe ser una lista"))
  }
  
  number_vec <- unlist(input_)   # conversión de la lista a vector
  cum_vec <- c() # vector donde se va a obtener la suma acumulada
  for (i in 1:length(number_vec)){
    n <- number_vec[i]
    string <- number_to_string(n)
    cum_vec[i] <- string
  }
  return(data.frame(numero_entrada = number_vec, string = cum_vec))
}


test_that("Tests Unitarios de la función number_to_string", {

  expect_error(number_to_string("hola"), "El valor introducido no es un número. Prueba otra vez.")
  expect_error(number_to_string("P"), "El valor introducido no es un número. Prueba otra vez.")
  expect_error(number_to_string(5.5), "El valor introducido no es un número entero: 5.5. Prueba otra vez.") 
  
  expect_is(number_to_string(5), "character")  # chequeamos la salida es un string
  expect_equal(nchar(number_to_string(5)), 5)  # chequeamos la longitud de la cadena de caracteres
}
)

test_that("Tests Unitarios de la función cummulative_string", {
  
  expect_error(cummulative_string(c(10, 3)), "La entrada de la función debe ser una lista")

  output <- cummulative_string(list(5, 2))
  expect_is(output, "data.frame")  # chequeamos la salida es un dataframe
  expect_equal(ncol(output), 2)  # chequeamos el número de columnas
  expect_equal(nrow(output), 2)  # chequeamos el número de filas
  expect_named(output, c("numero_entrada", "string")) # chequeamos el nombre del dataframe
  
}
)


# CUESTION 5

character_to_lower_letter <- function(character) {
  
  if (!is.character(character)) {
    stop("El input determinado para analizar la palabra debe un string")
  }
  
  # comprueba si se ha indicado un string vacío
  if (nchar(character) == 0) {
    stop("El string debe tener una caracter. El input es un string vacío")
  } 
  
  # Comprueba si el string contiene más de un carácter
  if (nchar(character) > 1) {
    stop(paste0("Debes devolver una sola letra. Has devuelto: ", character))
  }
  
  return(character)
}

word_starts_with_character <- function(word, character){
  
  if (!is.character(word)) {
    stop("La entrada debe ser un string")
  }
  
  if (!is.character(character)) {
    stop("El input determinado para analizar la palabra debe un string")
  }
  
  word <- trimws(word) # elimina espacios en blanco al principio y al final
  word <- tolower(word) # convierte la palabra a minúsculas

  character <- trimws(character)
  character <- tolower(character)
  
  # se extrae la primera letra de la palabra y se compara con la letra indicada
  if (substr(word, 1, 1) == character) {
    print(word)
    return(word)
  } else return("")
}

filter_words_by_starting_character <- function(input_, character) {
  
  character <- character_to_lower_letter(character)
  
  word_vec <- vector()   # inicialización un vector vacío para almacenar las palabras
  # se recorre la lista de palabras y se incluyen aquellas que cumplan la condición
  for (i in input_) {
    output <- word_starts_with_character(i, character)
    if (nchar(output) > 0){
      word_vec <- c(word_vec, i)
    }
  }
  
  # si no hay palabras, se indica
  if (length(word_vec) == 0) {
    return(paste0("No hay ninguna palabra que comience por ", character))
  } else {
    return(paste0("Hay ", length(word_vec), " palabras que comienzan por ", character, ": ", paste(word_vec, collapse = ", ")))
  }
}


test_that("Tests Unitarios de la función character_to_lower_letter", {
  
  expect_error(character_to_lower_letter(5), "El input determinado para analizar la palabra debe un string")
  expect_error(character_to_lower_letter(""), "El string debe tener una caracter. El input es un string vacío")
  expect_error(character_to_lower_letter("pablo"), "Debes devolver una sola letra. Has devuelto: pablo")

  expect_is(character_to_lower_letter("a"), "character") # chequeo la salida es un string
  expect_true(character_to_lower_letter("p") == "p")
  expect_false(character_to_lower_letter("p") == "P") # esto solo es igual si le pasamos el tolower
  }
)

test_that("Tests Unitarios de la función word_starts_with_character", {
  
  expect_error(word_starts_with_character(50, 50), "La entrada debe ser un string") # la primera comparación es la de la palabra
  expect_error(word_starts_with_character(list("palabra"), 50), "La entrada debe ser un string") # la primera comparación es la de la palabra
  expect_error(word_starts_with_character("palabra", 50), "El input determinado para analizar la palabra debe un string") # la primera comparación es la de la palabra

  expect_is(word_starts_with_character("palabra", "M"), "character") # chequeo la salida es un string
  expect_true(word_starts_with_character("palabra", "P") == "palabra") # coincide inicio de palabra con letra (output -> true)
  expect_true(word_starts_with_character("datos", "P") == "") # no coincide inicio de palabra con letra -> string vacío (output -> true)
  expect_false(word_starts_with_character("datos", "P") == "datos") # no coincide inicio de palabra con letra -> string vacío (output -> false)
    }
)

test_that("Tests Unitarios de la función filter_words_by_starting_character", {
  
  words_list <- list("master", "formacion", "uned", "analisis", "datos", "analitica")

  expect_is(filter_words_by_starting_character(words_list, "a"), "character") # chequeo la salida es un string
  expect_true(filter_words_by_starting_character(words_list, "p") == "No hay ninguna palabra que comience por p") # no hay palabras que empiecen por
  expect_true(filter_words_by_starting_character(words_list, "A") == "Hay 2 palabras que comienzan por A: analisis, analitica")
  }
)

