package scala.collection

import sun.misc.Unsafe
import scala.language.experimental.macros
import scala.reflect.macros.Context

package workstealing {

  object Utils {
    val unsafe = getUnsafe()

    def getUnsafe(): Unsafe = {
      if (this.getClass.getClassLoader == null) Unsafe.getUnsafe()
      try {
        val fld = classOf[Unsafe].getDeclaredField("theUnsafe")
        fld.setAccessible(true)
        return fld.get(this.getClass).asInstanceOf[Unsafe]
      } catch {
        case e: Throwable => throw new RuntimeException("Could not obtain access to sun.misc.Unsafe", e)
      }
    }

    def readVolatile[T](l: T): T = macro readVolatile_impl[T]
    def getBaseAndOffset(f:java.lang.reflect.Field, c:java.lang.Class[Any]):(java.lang.Class[Any],Long) = ???
    
    
    def readVolatile_impl[T: c.WeakTypeTag](c: Context)(l: c.Expr[T]): c.Expr[T] = {
      import c.universe._

      l.tree match {
        case Select(obj, sel) =>
          val name = c.Expr[String](Literal(Constant(sel.encoded)))
          val objRef = c.Expr[Any](obj)
          l.actualType match {
            case x if (x =:= typeOf[Int]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getIntVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Long]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getLongVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Byte]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getByteVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Boolean]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getBooleanVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Double]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getDoubleVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Float]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getFloatVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Short]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getShortVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x <:< typeOf[java.lang.Object]) => reify {
              {
                val ob = objRef.splice
                val obField = ob.getClass.getDeclaredField(name.splice)
                val offset = unsafe.fieldOffset(obField)
                unsafe.getObjectVolatile(ob, offset).asInstanceOf[T]
              }
            }

            case _ => c.abort(l.tree.pos, "field type " + l.actualType + " not supported")

          }
        case _ => c.abort(l.tree.pos, "Expected <object>.<field>")
      }
    }

  }

}

