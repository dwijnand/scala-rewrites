package fix

import scalafix.v1._
import scala.annotation.tailrec
import scala.meta._

import MyPatch.syntax._

object Scala_2_13 {
  val EOL         = SymbolMatcher.exact("scala/compat/Platform.EOL.")
  val currentTime = SymbolMatcher.exact("scala/compat/Platform.currentTime().")
  val arraycopy   = SymbolMatcher.exact("scala/compat/Platform.arraycopy().")

  val deprecatedConsoleReadBoolean = SymbolMatcher.exact("scala/DeprecatedConsole#readBoolean().")
  val deprecatedConsoleReadByte    = SymbolMatcher.exact("scala/DeprecatedConsole#readByte().")
  val deprecatedConsoleReadChar    = SymbolMatcher.exact("scala/DeprecatedConsole#readChar().")
  val deprecatedConsoleReadDouble  = SymbolMatcher.exact("scala/DeprecatedConsole#readDouble().")
  val deprecatedConsoleReadFloat   = SymbolMatcher.exact("scala/DeprecatedConsole#readFloat().")
  val deprecatedConsoleReadInt     = SymbolMatcher.exact("scala/DeprecatedConsole#readInt().")
  val deprecatedConsoleReadLine    = SymbolMatcher.exact("scala/DeprecatedConsole#readLine().")
  val deprecatedConsoleReadLine1   = SymbolMatcher.exact("scala/DeprecatedConsole#readLine(+1).")
  val deprecatedConsoleReadLong    = SymbolMatcher.exact("scala/DeprecatedConsole#readLong().")
  val deprecatedConsoleReadShort   = SymbolMatcher.exact("scala/DeprecatedConsole#readShort().")
  val deprecatedConsoleReadf       = SymbolMatcher.exact("scala/DeprecatedConsole#readf().")
  val deprecatedConsoleReadf1      = SymbolMatcher.exact("scala/DeprecatedConsole#readf1().")
  val deprecatedConsoleReadf2      = SymbolMatcher.exact("scala/DeprecatedConsole#readf2().")
  val deprecatedConsoleReadf3      = SymbolMatcher.exact("scala/DeprecatedConsole#readf3().")

  val arrowAssoc = SymbolMatcher.exact("scala/Predef.ArrowAssoc#`→`().")
}

final class Scala_2_13 extends SemanticRule("Scala_2_13") {
  import Scala_2_13._

  override def fix(implicit doc: SemanticDocument): Patch = {
    val handled = scala.collection.mutable.Set.empty[Tree]
    @tailrec def recordHandled(t: Tree): Unit = {
      handled += t
      t match {
        case Term.Apply(fun, _)           => recordHandled(fun)
        case Term.ApplyInfix(_, op, _, _) => recordHandled(op)
        case Term.Select(_, name)         => recordHandled(name)
        case _                            => ()
      }
    }

    val globalImports = scala.collection.mutable.Set.empty[String]
    def addGlobalImport(importer: Importer) = {
      if (globalImports.add(importer.structure))
        MyPatch.addGlobalImport(importer)
      else
        MyPatch.empty
    }

    def replaceToken(t: Tree, orig: String, repl: String) = {
      recordHandled(t)
      t.tokens.collect {
        case t if t.text == orig => MyPatch.replaceToken(t, repl)
      }.asMyPatch
    }

    def replaceTree(t: Tree, s: String) = {
      recordHandled(t)
      MyPatch.replaceTree(t, s)
    }

    def stdInReplace(tree: Tree, name: String) = {
      replaceTree(tree, s"StdIn.$name") + addGlobalImport(importer"scala.io.StdIn")
    }

    lazy val fixI: PartialFunction[Tree, MyPatch] = {
      case Term.Interpolate(_, _, args) =>
        args.collect(fixI).map {
          case MyPatch.replaceTree(from: Term.Name, to)
            if !to.startsWith("{") && !to.endsWith("}") && to.contains(".")
          =>
            MyPatch.replaceTree(from, s"{$to}")
          case p => p
        }.asMyPatch

      case EOL(i: Importee)  => MyPatch.removeImportee(i)
      case EOL(t: Term.Name) =>
        replaceTree(t, "EOL") + addGlobalImport(importer"java.lang.System.{lineSeparator => EOL}")
      case EOL(t: Term)      => replaceTree(t, "System.lineSeparator")

      case currentTime(i: Importee) => MyPatch.removeImportee(i)
      case currentTime(t: Term)     => replaceTree(t, "System.currentTimeMillis")

      case arraycopy(i: Importee)      => MyPatch.removeImportee(i)
      case arraycopy(Term.Apply(t, _)) => replaceTree(t, "System.arraycopy")

      case deprecatedConsoleReadBoolean(Term.Apply(t, _)) => stdInReplace(t, "readBoolean")
      case deprecatedConsoleReadByte(   Term.Apply(t, _)) => stdInReplace(t, "readByte")
      case deprecatedConsoleReadChar(   Term.Apply(t, _)) => stdInReplace(t, "readChar")
      case deprecatedConsoleReadDouble( Term.Apply(t, _)) => stdInReplace(t, "readDouble")
      case deprecatedConsoleReadFloat(  Term.Apply(t, _)) => stdInReplace(t, "readFloat")
      case deprecatedConsoleReadInt(    Term.Apply(t, _)) => stdInReplace(t, "readInt")
      case deprecatedConsoleReadLine(   Term.Apply(t, _)) => stdInReplace(t, "readLine")
      case deprecatedConsoleReadLine1(  Term.Apply(t, _)) => stdInReplace(t, "readLine")
      case deprecatedConsoleReadLong(   Term.Apply(t, _)) => stdInReplace(t, "readLong")
      case deprecatedConsoleReadShort(  Term.Apply(t, _)) => stdInReplace(t, "readShort")
      case deprecatedConsoleReadf(      Term.Apply(t, _)) => stdInReplace(t, "readf")
      case deprecatedConsoleReadf1(     Term.Apply(t, _)) => stdInReplace(t, "readf1")
      case deprecatedConsoleReadf2(     Term.Apply(t, _)) => stdInReplace(t, "readf2")
      case deprecatedConsoleReadf3(     Term.Apply(t, _)) => stdInReplace(t, "readf3")

      case t: Case          => replaceToken(t, "⇒", "=>")
      case t: Type.Function => replaceToken(t, "⇒", "=>")
      case t: Importee      => replaceToken(t, "⇒", "=>")
      case arrowAssoc(t)    => replaceToken(t, "→", "->")

      case t @ Lit.Symbol(sym) => MyPatch.replaceTree(t, s"""Symbol("${sym.name}")""")
    }
    doc.tree.collect(new Combined({ case t if !handled(t) => t }, fixI)).asMyPatch.toPatch
  }
}

private sealed abstract class MyPatch {
  import MyPatch._

  def +(other: MyPatch): MyPatch =
    if (this eq other) this
    else if (this == empty) other
    else if (other == empty) this
    else Concat(this, other)

  def toPatch: Patch = this match {
    case MyPatch.empty             => Patch.empty
    case removeImportee(importee)  => Patch.removeImportee(importee)
    case addGlobalImport(importee) => Patch.addGlobalImport(importee)
    case replaceToken(from, to)    => Patch.replaceToken(from, to)
    case replaceTree(from, to)     => Patch.replaceTree(from, to)
    case Concat(a, b)              => a.toPatch + b.toPatch
  }
}

private object MyPatch {
  case object empty extends MyPatch
  final case class removeImportee(importee: Importee) extends MyPatch
  final case class addGlobalImport(importer: Importer) extends MyPatch
  final case class replaceToken(token: Token, toReplace: String) extends MyPatch
  final case class replaceTree(from: Tree, to: String) extends MyPatch
  final case class Concat(a: MyPatch, b: MyPatch) extends MyPatch

  def fromIterable(seq: Iterable[MyPatch]): MyPatch = seq.foldLeft(empty: MyPatch)(_ + _)

  object syntax {
    implicit class IterableMyPatchSyntax(patches: Iterable[MyPatch]) {
      def asMyPatch: MyPatch = MyPatch.fromIterable(patches)
    }
  }
}

private object Combined {
  private[this] val mockDefaultFunc: Any => Any = _ => mockDefaultFunc
  private def mockDefaultFunction[B] = mockDefaultFunc.asInstanceOf[Any => B]
  private def fallbackOccurred[B](x: B) = mockDefaultFunc eq x.asInstanceOf[AnyRef]
}

private final class Combined[-A, B, +C] (pf: PartialFunction[A, B], k: PartialFunction[B, C]) extends PartialFunction[A, C] {
  import Combined._

  def isDefinedAt(x: A): Boolean = {
    val b = pf.applyOrElse(x, mockDefaultFunction)
    !fallbackOccurred(b) || k.isDefinedAt(b)
  }

  def apply(x: A): C = k(pf(x))

  override def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 = {
    val pfv = pf.applyOrElse(x, mockDefaultFunction)
    if (fallbackOccurred(pfv)) default(x) else k.applyOrElse(pfv, (_: B) => default(x))
  }
}
