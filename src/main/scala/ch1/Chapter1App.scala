package ch1

import scala.annotation.tailrec

object Chapter1App {

  sealed trait MyList[+A] {


  }


  case object MyNil extends MyList[Nothing]

  case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
    def empty[A]: MyList[A] = apply[A]()

    def sum(ints: MyList[Int]): Int = ints match {
      case MyNil => 0
      case MyCons(x, xs) => x + sum(xs)
    }

    def tailFirst[A](l: MyList[A]): A = l match {
      case MyNil => sys.error("tail of empty list")
      case MyCons(head, _) => head
    }

    def setHead[A](l: MyList[A], a: A): MyList[Any] = l match {
      case MyNil => sys.error("setHead on empty list")
      case MyCons(_, t) => MyCons(a, t)
    }

    def drop[A](l: MyList[A], n: Int): MyList[A] =
      if (n <= 0) l
      else l match {
        case MyNil => MyNil
        case MyCons(_, t) => drop(t, n - 1)
      }

    /* def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
       case MyCons(h, t) if f(h) => dropWhile(t, f)
       case _ => l
     }*/

    def dropWhile[A](as: MyList[A])(f: A => Boolean): MyList[A] =
      as match {
        case MyCons(h, t) if f(h) => dropWhile(t)(f)
        case _ => as
      }

    def init[A](l: MyList[A]): MyList[A] = l match {
      case MyNil => sys.error("init on empty list")
      case MyCons(_, MyNil) => MyNil
      case MyCons(h, t) => MyCons(h, init(t))
    }

    def init2[A](l: MyList[A]): MyList[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      @annotation.tailrec
      def go(cur: MyList[A]): MyList[A] = cur match {
        case MyNil => sys.error("init of empty list")
        case MyCons(_, MyNil) => MyList(buf.toList: _*)
        case MyCons(h, t) => buf += h; go(t)
      }

      go(l)
    }

    @tailrec
    def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
      case MyNil => z
      case MyCons(h, t) => foldLeft(t, f(z, h))(f)
    }

    //@tailrec
    //pushing frames onto the call stack
    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
      case MyNil => z
      case MyCons(h, t) => f(h, foldRight(t, z)(f))
    }

    def apply[A](as: A*): MyList[A] = if (as.isEmpty) MyNil else MyCons(as.head, apply(as.tail: _*))

    def sum2(ns: MyList[Int]): Int = foldLeft(ns, 0)(_ + _)

    def product2(ns: MyList[Int]): Int = foldLeft(ns, 1)((x, y) => x * y)

    def length[A](l: MyList[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

    def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, MyList[A]())((acc, h) => MyCons(h, acc))

    def foldRightUsingFoldLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = foldLeft(l, z)((left, right) => f(right, left))

    def foldRightUsingFoldLeft_1[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def foldLeftUsingFoldRight[A, B](l: MyList[A], z: B)(f: (B, A) => B): B =
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def appendViaFoldRight[A](l: MyList[A], r: MyList[A]): MyList[A] = foldRight(l, r)(MyCons(_, _))

    def lengthUsingFoldRight[A](as: MyList[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

    def add1(l: MyList[Int]): MyList[Int] = foldRight(l, MyNil: MyList[Int])((h, t) => MyCons(h + 1, t))
  }

  def main(args: Array[String]): Unit = {
    import MyList._
    //Improving type inference for higher-order functions
    val l = add1(MyList(1, 2, 3, 4, 3, 5))
    println(l)

  }
}
