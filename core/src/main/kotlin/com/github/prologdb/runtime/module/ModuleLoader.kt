package com.github.prologdb.runtime.module

interface ModuleLoader {
    fun load(reference: ModuleReference): Module
}

