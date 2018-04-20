/*
  Number n should be a group of (n + 1)
  Total rabbits: (n + 1) * groups
*/
object Solution {
    def numRabbits(answers: Array[Int]): Int = {
        answers.foldLeft(Map.empty[Int, Int]){case (s, i) =>
            s.get(i) match {
              case None    => s ++ Map(i -> 1)
              case Some(c) => s ++ Map(i -> (c + 1))
            }
        }.foldLeft(0){ case (s, i) =>
            val n = i._1 + 1
            val group = i._2 / n
            if ( n * group < i._2) {
              s + n * (group + 1)
            } else {
              s + n * group
            }
        }
    }
}
