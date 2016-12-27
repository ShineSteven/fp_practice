package shine.st.practice.tlp

/**
  * Created by shinest on 2016/11/23.
  */

trait Status

trait Open extends Status

trait Close extends Status

trait Door[S <: Status]

object Door {
  def apply[S <: Status] = new Door[S] {}

  def close[S <: Open](door: Door[S]) = Door[Close]

  def open[S <: Close](door: Door[S]) = Door[Open]
}

object PhantomTypes {
  //they are used as type constraints but never instantiated.
  def main(args: Array[String]): Unit = {
    val openDoor = Door[Open]
    val closedDoor = Door.close(openDoor)
    val openAgainDoor = Door.open(closedDoor)

//    compile error
//    val openOpenDoor = Door.open(openDoor)
//    val closeClosedDoor = Door.close(closedDoor)
  }
}
