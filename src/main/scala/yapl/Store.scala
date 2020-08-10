package yapl

import scala.annotation.tailrec

trait Store[A] {
    type Location = Int

    def size: Int

    def malloc(value: A): (Location, Store[A])
    def lookup(loc: Location): A
    def update(loc: Location, value: A): (Location, Store[A])
}

case class BasicStore[A](memory: Map[Int, A]) extends Store[A] {
    override def size: Location = getFreeAddress

    override def malloc(value: A): (Location, BasicStore[A]) = {
        val newLoc = getFreeAddress
        (newLoc, copy(memory = memory + (newLoc -> value)))
    }

    override def lookup(loc: Location): A = memory.apply(loc)

    override def update(loc: Location, value: A): (Location, BasicStore[A]) =
        (loc, copy(memory = memory + (loc -> value)))

    private def getFreeAddress : Location = {
        @tailrec
        def getNextFreeAddress(l : Location) : Location = {
            if (!memory.contains(l)) {
                l
            } else {
                getNextFreeAddress(l + 1)
            }
        }
        getNextFreeAddress(0)
    }
}

case class RefCountStore[A](maxSize: Int, memory: Map[Int, (A, Int)]) extends Store[A] {

    override def size: Location = memory.count(x => x != null)

    override def malloc(value: A): (Location, Store[A]) = {
        val newLocation = getFreeAddress
        (newLocation, copy(memory = memory + (newLocation -> (value, 1))))
    }

    override def lookup(loc: Location): A = memory.apply(loc)._1

    override def update(loc: Location, value: A): (Location, Store[A]) = {
        (loc, copy(memory = memory + (loc -> (value, refCount(loc)))))
    }

    private def getFreeAddress : Location = {
        @tailrec
        def getNextFreeAddress(l : Location) : Location = {
            if (l >= maxSize)
                sys.error("out of memory")
            else if (!memory.contains(l))
                l
            else
                getNextFreeAddress(l + 1)
        }
        getNextFreeAddress(0)
    }

    private def refCount(index : Location) : Int = memory.get(index) match {
        case Some((_, n)) => n
        case None => 0
    }
}