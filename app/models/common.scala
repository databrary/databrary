package models

import play.api.mvc.{PathBindable,QueryStringBindable}
import anorm._
import dbrary._
import util._

/* An enhanced "lazy val" that takes arguments */
class CachedVal[T, S](init : S => T) {
  private var x : Option[T] = None
  def apply(s : S) : T = synchronized(x.getOrElse(update(init(s))))
  def update(v : T) : T = {
    x = Some(v)
    v
  }
}
object CachedVal {
  import scala.language.implicitConversions
  def apply[T, S](init : S => T) = new CachedVal[T,S](init)
  implicit def implicitGetCached[T, S](x : CachedVal[T, S])(implicit s : S) : T = x(s)
}

/* We wrap id/pk values in a class tagged with a type representing the source table */
class GenericId[I,+T](val unId : I) {
  // I don't understand why this is necessary (and it's also not quite right with inheritance):
  def equals(i : GenericId[I,_]) = i.unId equals unId
  def ==(i : GenericId[I,_]) = i.unId == unId
  def !=(i : GenericId[I,_]) = !(this == i)
  override def toString = "Id(" + unId.toString + ")"
}
final class IntId[+T](unId : Int) extends GenericId[Int,T](unId)
object IntId {
  def apply[T](i : Int) = new IntId[T](i)
  implicit def pathBindableId[T] : PathBindable[IntId[T]] = PathBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def queryStringBindableId[T] : QueryStringBindable[IntId[T]] = QueryStringBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def toStatementId[T] : ToStatement[IntId[T]] = dbrary.Anorm.toStatementMap[IntId[T],Int](_.unId)
  implicit def columnId[T] : Column[IntId[T]] = dbrary.Anorm.columnMap[IntId[T],Int](apply[T] _)
}
private[models] trait HasId[+T] {
  type Id = IntId[T]
  def asId(i : Int) : Id = new IntId[T](i)
}

private[models] trait TableRow
private[models] trait TableRowId[+T] extends TableRow {
  val id : IntId[T]
  override def hashCode = id.unId
  def equals(a : this.type) = a.id == id
}

private[models] abstract trait TableView {
  private[models] val table : String /* table name */
  private[this] val _tableOID = CachedVal[Long,Site.DB](SQL("SELECT oid FROM pg_class WHERE relname = {name}").on('name -> table).single(SqlParser.scalar[Long])(_))
  private[models] def tableOID(implicit db : Site.DB) : Long = _tableOID

  private[models] type Row
  private[models] val row : SelectParser[Row]
  private[models] def * : String = row.select
  private[models] val src : String = table /* the source for selects */

  protected def SELECT(q : String = "") : SimpleSql[Row] = 
    SQL("SELECT " + * + " FROM " + src + (if (q.isEmpty) "" else " " + q)).using(row)
  protected def JOIN(t : TableView, q : String = "") : SimpleSql[Row ~ t.Row] = {
    val j = row ~ t.row
    SQL("SELECT " + j.select + " FROM " + src + " JOIN " + t.src + (if (q.isEmpty) "" else " " + q)).using(j)
  }

  import scala.language.implicitConversions
  protected implicit def tableColumn(col : Symbol) = SelectColumn(table, col.name)
}

private[models] abstract class Table[R <: TableRow](private[models] val table : String) extends TableView {
  type Row = R
}
private[models] abstract class TableId[R <: TableRowId[R]](table : String) extends Table[R](table) with HasId[R]

private[models] object Anorm {
  type Args = Seq[(Symbol, ParameterValue[_])]
  def Args(args : (Symbol, ParameterValue[_])*) : Args = List(args : _*)

  def insertArgs(args : Args) = {
    val names = args.map(_._1.name)
    names.mkString("(", ", ", ")") + " VALUES " + names.mkString("({", "}, {", "})")
  }

  def setArgs(args : Args, sep : String = ", ") =
    args.map(_._1.name).map(n => n + " = {" + n + "}").mkString(sep)
}

trait SitePage {
  def pageName(implicit site : Site) : String
  def pageParent(implicit site : Site) : Option[SitePage]
  def pageURL : String
}
