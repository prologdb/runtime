package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.runtime.builtin.ComparisonKt;
import com.github.prologdb.runtime.builtin.EqualityKt;
import com.github.prologdb.runtime.builtin.TypesafetyKt;
import com.github.prologdb.runtime.builtin.math.Math_libraryKt;
import com.github.prologdb.runtime.builtin.string.LibraryKt;
import com.github.prologdb.runtime.knowledge.library.Library;

public enum LoadableLibrary
{
    EQUALITY(EqualityKt.getEqualityLibrary()),
    TYPESAFETY(TypesafetyKt.getTypeSafetyLibrary()),
    COMPARE(ComparisonKt.getComparisonLibrary()),
    STRINGS(LibraryKt.getStringsLibrary()),
    MATH(Math_libraryKt.getMathLibrary()),
    LISTS(com.github.prologdb.runtime.builtin.lists.ModuleKt.getListsModule()),
    DYNAMICS(com.github.prologdb.runtime.builtin.dynamic.LibraryKt.getDynamicsLibrary()),
    DICTS(com.github.prologdb.runtime.builtin.dict.LibraryKt.getDictLibrary());

    private final Library library;

    LoadableLibrary(Library library)
    {
        this.library = library;
    }

    public Library getLibrary()
    {
        return library;
    }
}
