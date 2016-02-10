package GA

import scala._
import scalaz._
import Scalaz._

object Durp {

    trait Pet {
        def name: String
        def renamed(newName: String): Pet
    }

    case class Fish(name: String, age: Int) extends Pet {
        def renamed(newName: String): Fish = copy(name = newName)
    }


    // since renamed must return a Pet nothing stops us from changing the type of Kitty to Fish
    case class Kitty(name: String, color: String) extends Pet {
        def renamed(newName: String): Fish = Fish(newName, 42)
    }


    // return type is not specific enough. Now we are left with the same problem of not being
    // able to specify the return type since renamed is a trait method. we get:

    // found    : Pet
    // required : A

    // def esquire[A <: Pet](a: A): A = a.renamed(a.name + ", Esq.")
    def esquire[A <: Pet](a: A): Pet = a.renamed(a.name + ", Esq.")

    trait FPet[A <: FPet[A]] {
        def name: String
        def renamed(newName: String): A
    }
}

object Hurp {

    // an F-bounded type is parametrized over its own subtypes allowing us
    // to reference the implementing type in the superclass
    trait Petf[A <: Petf[A]] {
        def name: String 
        def renamed(newName: String): A
    }

    // here Fishf passes itself as a type argument
    case class Fishf(name: String, age: Int) extends Petf[Fishf] {
        def renamed(newName: String) = copy(name = newName)
    }

    case class Kittyf(name: String, color: String) extends Petf[Kittyf] {
        // Will not compile
        // def renamed(newName: String): Fishf = Fish(newName, 42)
        def renamed(newName: String): Kittyf = copy(name = newName)
    }

    def esquire[A <: Petf[A]](a: A): A = a.renamed(a.name + ", Esq. ")


    // Uh oh...
    case class Liarf(name: String, loot: String) extends Petf[Fishf] {
        def renamed(newName: String): Fishf = new Fishf(newName, 42)
    }
}

object Derp {

    // We try "Self type annotation"
    trait Car[A <: Car[A]] { this: A =>
        def name: String
        def renamed(newName: String): A
    }

    case class Mustang(name: String, hp: Int) extends Car[Mustang] {
        def renamed(newName: String): Mustang = copy(name = newName)
    }



    // Does not compile. Sadly :(

    // case class Skoda(name: Sting, color: String) extends Car[Mustang] {
    //     def renamed(newName: String): Mustang = copy(name = newName)
    // }


}
