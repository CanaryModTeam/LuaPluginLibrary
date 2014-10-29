package net.canarymod.plugin.lang.lua

import net.canarymod.hook.Hook
import org.luaj.vm2.lib._
import org.luaj.vm2.lib.jse.CoerceJavaToLua
import org.luaj.vm2.{LuaError, LuaValue, Varargs}

/**
 * Canary Lua API
 *
 * @author Pwootage
 */
class LuaAPI(val plugin: LuaPlugin) {

  def luaMap(keys: (String, LuaValue)*) = {
    val res = LuaValue.tableOf()
    keys.foreach(m => res.set(m._1, m._2))
    res
  }

  private object luaFunc {
    def apply(func: () => LuaValue) = new ZeroArgFunction {
      override def call() = func()
    }

    def apply(func: (LuaValue) => LuaValue) = new OneArgFunction {
      override def call(arg1: LuaValue) = func(arg1)
    }

    def apply(func: (LuaValue, LuaValue) => LuaValue) = new TwoArgFunction {
      override def call(arg1: LuaValue, arg2: LuaValue) = func(arg1, arg2)
    }

    def apply(func: (LuaValue, LuaValue, LuaValue) => LuaValue) = new ThreeArgFunction {
      override def call(arg1: LuaValue, arg2: LuaValue, arg3: LuaValue) = func(arg1, arg2, arg3)
    }

    def vararg(func: (Varargs) => Varargs) = new VarArgFunction {
      override def onInvoke(arg1: Varargs) = func(arg1)
    }

  }

  def canaryLib = new TwoArgFunction {
    override def call(moduleName: LuaValue, env: LuaValue) = {
      val lib = luaMap(
        "plugin" -> CoerceJavaToLua.coerce(plugin),
        "hook" -> hook,
        "hooks" -> luaMap(),
        "log" -> log
      )
      env.set("canary", lib)
      lib
    }
  }

  val hook = luaFunc { (name, func) =>
    val hookName = name.tojstring()
    val hookCls = findClassForHook(hookName)
    plugin.hook(hookCls) { hook =>
      //Thread safety?
      try {
        val funcToCall = plugin.getThreadLocalGlobals.get("canary").get("hooks").get(hookCls.getName)
        if (funcToCall.isfunction()) {
          funcToCall.call(CoerceJavaToLua.coerce(hook))
        }
      } catch {
        case e: LuaError => plugin.getLogman.error("Error executing callback", e)
      }
    }
    plugin.getThreadLocalGlobals.get("canary").get("hooks").set(hookCls.getName, func)
    LuaValue.NIL
  }

  val log = luaMap(
    "info" -> luaFunc {
      msg =>
        plugin.getLogman.info(msg.tojstring())
        LuaValue.NIL
    },
    "warn" -> luaFunc {
      msg =>
        plugin.getLogman.warn(msg.tojstring())
        LuaValue.NIL
    },
    "error" -> luaFunc {
      msg =>
        plugin.getLogman.error(msg.tojstring())
        LuaValue.NIL
    }
  )


  private def findClassForHook(hookName: String): Class[_ <: Hook] = {
    tryClassNames(Seq(
      "",
      "net.canarymod.hook.",
      "net.canarymod.hook.command.",
      "net.canarymod.hook.entity.",
      "net.canarymod.hook.player.",
      "net.canarymod.hook.system.",
      "net.canarymod.hook.world."
    ).map(v => v + hookName)) match {
      case Some(x) => x
      case None => throw new ClassNotFoundException("Unknown hook type: " + hookName)
    }
  }

  private def tryClassNames(names: Seq[String]): Option[Class[_ <: Hook]] = {
    for (name <- names) {
      val found = try {
        Some(Class.forName(name).asInstanceOf[Class[_ <: Hook]])
      } catch {
        case e: ClassNotFoundException =>
          None
        case e: ClassCastException =>
          None
      }
      found match {
        case Some(x) => return Some(x)
        case _ =>
      }
    }
    None
  }
}
