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
    def getFieldBaseAndOffset(c: Any, name:String):(Any,Long) = {
      val field =  c.getClass.getDeclaredField(name)
      val isStatic = java.lang.reflect.Modifier.isStatic(field.getModifiers())
      isStatic match {
        case true =>
          (unsafe.staticFieldBase(field),unsafe.staticFieldOffset(field))
        case false =>
          (c, unsafe.objectFieldOffset(field))

      }
    }
    
    
    def readVolatile_impl[T: c.WeakTypeTag](c: Context)(l: c.Expr[T]): c.Expr[T] = {
      import c.universe._

      l.tree match {
        case Select(obj, sel) =>
          val name = c.Expr[String](Literal(Constant(sel.encoded)))
          val objRef = c.Expr[Any](obj)
          l.actualType match {
            case x if (x =:= typeOf[Int]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
                unsafe.getIntVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Long]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
                unsafe.getLongVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Byte]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
                unsafe.getByteVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Boolean]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
                unsafe.getBooleanVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Double]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
                unsafe.getDoubleVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Float]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
                unsafe.getFloatVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x =:= typeOf[Short]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
                unsafe.getShortVolatile(ob, offset).asInstanceOf[T]
              }
            }
            case x if (x <:< typeOf[java.lang.Object]) => reify {
              {
                val (ob,offset) = getFieldBaseAndOffset(objRef.splice, name.splice)
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

