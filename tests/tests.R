
#**************************************************************
#************** APLICACIÓN DE LOS TEST UNITARIOS **************
#**************************************************************


library(testthat)

# path <- getwd()
# final_path <- paste0(path, "/functions/functions.R")
# source(final_path) # para ejecutar los tests como si fuese un script (se necesita disponer de la ruta)

source("../functions/functions.R") # para  ejecutar los tests desde la funcionalidad propia de Rstudio (Run Tests)

test_that("Tests Unitarios de la función count_word_details - Salida", {
  
  word_vec <- c("master", "formacion", "datos", "analitica", "aprendizaje", "estadistica")
  expect_error(count_word_details(c()), "La entrada de la función debe ser una lista")
  
  word_list <- list("master", "formacion", "datos", "analitica", "aprendizaje", "estadistica")
  output <- count_word_details(word_list)
  
  expect_is(output, "data.frame") # chequeo la salida es un dataframe
  expect_equal(ncol(output), 2)  # chequeamos el número de columnas
  expect_equal(nrow(output), 6)  # chequeamos el número de filas
  expect_named(output, c("n_cocurrencia_palabra", "n_caracteres_palabra")) # chequeamos el nombre del dataframe
}
)

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

