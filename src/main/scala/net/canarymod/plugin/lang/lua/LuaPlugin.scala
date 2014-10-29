package net.canarymod.plugin.lang.lua

import java.io.InputStreamReader
import java.util

import net.canarymod.chat.MessageReceiver
import net.canarymod.commandsys.{CanaryCommand, DynamicCommandAnnotation, TabCompleteDispatch}
import net.canarymod.exceptions.InvalidPluginException
import net.canarymod.hook.{Dispatcher, Hook}
import net.canarymod.plugin.{Plugin, PluginDescriptor, PluginListener, Priority}
import net.canarymod.{Canary, CanaryClassLoader, Translator}
import org.luaj.vm2.lib.jse.JsePlatform
import org.luaj.vm2.{Globals, LuaValue}

import scala.collection.mutable

/**
 * Lua plugin base - handles all lua calls from here
 *
 * @author Pwootage
 */
class LuaPlugin(val desc: PluginDescriptor, val cl: CanaryClassLoader) extends Plugin {

  private lazy val hookListener = Canary.hooks()
  private lazy val commandManager = Canary.commands()
  private val mainFile: String = desc.getCanaryInf.getString("main-file", "").trim
  lazy val priority = Priority.NORMAL
  if (mainFile == "") {
    throw new InvalidPluginException("Main file for lua plugin not defined!")
  }
  private val luaAPI: LuaAPI = new LuaAPI(this)
  private val tlGlobals = new ThreadLocal[Globals] {
    override def initialValue = {
      val ret: Globals = JsePlatform.standardGlobals
      ret.load(luaAPI.canaryLib)
      val lv: LuaValue = ret.load(new InputStreamReader(cl.getResourceAsStream(mainFile)), mainFile)
      lv.call
      ret
    }
  }
  private val _globalStore: mutable.Map[String, String] = mutable.Map()

  def setGlobal(key: String, value: String) = _globalStore.synchronized {
    _globalStore.put(key, value)
  } match {
    case Some(x) => x
    case None => null
  }

  def getGlobal(key: String) = _globalStore.synchronized {
    _globalStore.get(key)
  } match {
    case Some(x) => x
    case None => null
  }

  def getThreadLocalGlobals: Globals = tlGlobals.get

  override def enable: Boolean = {
    val enableFunc = tlGlobals.get().get("canary").get("enable")
    if (enableFunc.isfunction()) {
      val res = enableFunc.call()
      if (res.isboolean()) {
        res.toboolean()
      } else {
        true
      }
    } else {
      getLogman.error("Lua plugins must define canary.enable")
      false
    }
  }

  override def disable(): Unit = {
    val enableFunc = tlGlobals.get().get("canary").get("disable")
    if (enableFunc.isfunction()) {
      enableFunc.call()
    }
  }

  /**
   * Regester a hook with Canary
   * @param h Hook method
   * @param clazz Class of hook
   * @tparam T Type of hook
   */
  def hook[T <: Hook](clazz: Class[T])(h: T => Unit): Unit = {
    hookListener.registerHook(
      new PluginListener {},
      this,
      clazz,
      new Dispatcher {
        override def execute(listener: PluginListener, hook: Hook): Unit = h(hook.asInstanceOf[T])
      },
      priority
    )
  }

  def command(aliases: Seq[String],
              permissions: Seq[String],
              description: String,
              toolTip: String,
              parent: String = "",
              helpLookup: String = "",
              searchTerms: Seq[String] = Seq(),
              min: Int = 1,
              max: Int = -1,
              version: Int = 1,
              tabComplete: (MessageReceiver, Seq[String]) => Seq[String] = (_, _) => Seq(),
              translator: Translator = Translator.getInstance()
               )(
               command: (MessageReceiver, Seq[String]) => Unit
               ): Unit = {

    val meta = new DynamicCommandAnnotation(
      aliases.toArray,
      permissions.toArray,
      description,
      toolTip,
      parent,
      helpLookup,
      searchTerms.toArray,
      min,
      max,
      "",
      version
    )
    val tcd = new TabCompleteDispatch() {

      import scala.collection.JavaConversions._

      override def complete(msgrec: MessageReceiver, args: Array[String]): util.List[String] = tabComplete(msgrec, args)
    }

    val cc = new CanaryCommand(meta, this, translator, tcd) {
      override protected def execute(caller: MessageReceiver, parameters: Array[String]): Unit = command(caller, parameters)
    }
    commandManager.registerCommand(cc, this, false)
  }
}