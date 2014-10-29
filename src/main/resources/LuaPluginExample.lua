canary.enable = function()
  canary.log.info("Enabling testplugin")
  canary.hook("ChatHook", chatHook)
  return true
end

function chatHook(hook)
  local player = hook:getPlayer()
  local lastMsg = canary.plugin:getGlobal("last-msg") or ""
  player:message("Last Message: " .. lastMsg)
  canary.plugin:setGlobal("last-msg", hook:getMessage())
end
