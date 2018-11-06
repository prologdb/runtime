package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.knowledge.library.ClauseStore
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry

interface KnowledgeBase : ClauseStore, OperatorRegistry
interface MutableKnowledgeBase : KnowledgeBase {

}