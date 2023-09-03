package introduccion

import javax.naming.OperationNotSupportedException

class EjercicioListas {
  /*
  * Punto 2 repetir lista
  * @param lista Lista a repetir
  * @param n Número de veces a repetir la lista
  * @return Lista repetida n veces
  * @example repetirLista(List(1, 2, 3), 2) = List(List(1,1),List(2,2),List(3,3))
  * @throws IllegalArgumentException si n es negativo
  */
  def repetirListas(lista: List[Int], n: Int): List[List[Int]] = {
    var listaRepetida: List[List[Int]] = List()
    var numero_elementos: Int = lista.size
    for (i <- 1 to numero_elementos) {
      var listaInterna: List[Int] = List()
      if (n == 0) {
        listaRepetida = List() +: listaRepetida
      }
      else if (n > 0) {
        for (j <- 1 to n) {
          listaInterna = lista(i - 1) +: listaInterna
        }
        listaRepetida = listaInterna +: listaRepetida
      }
      else if (n < 0) {
        throw new java.lang.IllegalArgumentException("No valores menores de 0")
      }
    }
    return listaRepetida.reverse
  }

  /*
  * Punto 3: Filtrar listas
  * @param criterioIn Criterio de filtrado que puede ser "mayor", "menor", "mayoroigual", "igual", "diferente" o "menoroigual"
  * @param n Número a comparar
  * @param lista Lista a filtrar
  * @return Lista filtrada de acuerdo al criterio y n
  * @throws IllegalArgumentException si el criterio no es uno de los valores válidos
  */
  def filtrarListas(criterioIn: String, n: Int, lista: List[Int]): List[Int] = {
    val criterio = criterioIn.toLowerCase()
    criterio match {
      case "mayor" =>
        lista.filter(_ > n)
      case "menor" =>
        lista.filter(_ < n)
      case "mayoroigual" =>
        lista.filter(_ >= n)
      case "igual" =>
        lista.filter(_ == n)
      case "diferente" =>
        lista.filter(_ != n)
      case "menoroigual" =>
        lista.filter(_ <= n)
      case _ =>
        throw new IllegalArgumentException("Criterio no válido")
    }
  }
}