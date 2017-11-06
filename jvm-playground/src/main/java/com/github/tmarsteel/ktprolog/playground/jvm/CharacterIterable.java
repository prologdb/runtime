package com.github.tmarsteel.ktprolog.playground.jvm;

import org.jetbrains.annotations.NotNull;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class CharacterIterable implements Iterable<Character> {
    private String string;

    public CharacterIterable(String string) {
        this.string = string;
    }

    @NotNull
    @Override
    public Iterator<Character> iterator() {
        return new Iterator<Character>() {
            private int index = 0;

            @Override
            public boolean hasNext() {
                return index < string.length();
            }

            @Override
            public Character next() {
                try {
                    return string.charAt(index++);
                }
                catch (IndexOutOfBoundsException ex) {
                    throw new NoSuchElementException();
                }
            }
        };
    }
}
