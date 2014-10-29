package net.canarymod.plugin.lang.lua;

import net.canarymod.CanaryClassLoader;
import net.canarymod.exceptions.PluginLoadFailedException;
import net.canarymod.plugin.Plugin;
import net.canarymod.plugin.PluginDescriptor;
import net.canarymod.plugin.lifecycle.PluginLifecycleBase;

import java.io.File;

/**
 * Lifecycle manager for a Lua plugin
 *
 * @author Pwootage
 */
public final class LuaPluginLifecycle extends PluginLifecycleBase {
    private CanaryClassLoader cl;

    public LuaPluginLifecycle(PluginDescriptor desc) {
        super(desc);
    }

    @Override
    protected void _load() throws PluginLoadFailedException {
        try {
            cl = new CanaryClassLoader(new File(desc.getPath()).toURI().toURL(), getClass().getClassLoader());
            //A hacky way of getting the name in during the constructor/initializer
            Plugin.threadLocalName.set(desc.getName());
            Plugin p = new LuaPlugin(desc, cl);
            //If it isn't called in initializer, gotta set it here.
            p.setName(desc.getName());
            p.setPriority(desc.getPriority());
            desc.setPlugin(p);
        }
        catch (Exception e) {
            throw new PluginLoadFailedException("Failed to load plugin", e);
        }
    }

    @Override
    protected void _unload() {
        if (cl != null) {
            cl.close();
        }
    }
}
