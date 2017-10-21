if (typeof kotlin === 'undefined') {
  throw new Error("Error loading module 'kotlin-prolog-core'. Its dependency 'kotlin' was not found. Please, check whether 'kotlin' is loaded prior to 'kotlin-prolog-core'.");
}
this['kotlin-prolog-core'] = function (_, Kotlin) {
  'use strict';
  var IllegalStateException = Kotlin.kotlin.IllegalStateException;
  var Unit = Kotlin.kotlin.Unit;
  var CoroutineImpl = Kotlin.kotlin.coroutines.experimental.CoroutineImpl;
  var intrinsics = Kotlin.kotlin.coroutines.experimental.intrinsics;
  var buildSequence = Kotlin.kotlin.coroutines.experimental.buildSequence_of7nec$;
  var HashSet_init = Kotlin.kotlin.collections.HashSet_init_287e2$;
  var List = Kotlin.kotlin.collections.List;
  var map = Kotlin.kotlin.sequences.map_z5avom$;
  var get_lastIndex = Kotlin.kotlin.collections.get_lastIndex_55thoc$;
  var IllegalArgumentException = Kotlin.kotlin.IllegalArgumentException;
  var Regex = Kotlin.kotlin.text.Regex_61zpoe$;
  var emptySet = Kotlin.kotlin.collections.emptySet_287e2$;
  var emptyList = Kotlin.kotlin.collections.emptyList_287e2$;
  var toMutableSet = Kotlin.kotlin.collections.toMutableSet_7wnvza$;
  var joinToString = Kotlin.kotlin.collections.joinToString_fmv235$;
  var plus = Kotlin.kotlin.collections.plus_mydzjv$;
  var get_lastIndex_0 = Kotlin.kotlin.collections.get_lastIndex_m7z4lg$;
  var joinToString_0 = Kotlin.kotlin.collections.joinToString_cgipc5$;
  var contentDeepEquals = Kotlin.arrayDeepEquals;
  var toSet = Kotlin.kotlin.collections.toSet_7wnvza$;
  var setOf = Kotlin.kotlin.collections.setOf_mh5how$;
  var asSequence = Kotlin.kotlin.collections.asSequence_7wnvza$;
  var RuntimeException = Kotlin.kotlin.RuntimeException;
  var to = Kotlin.kotlin.to_ujzrz7$;
  RandomVariable.prototype = Object.create(Variable.prototype);
  RandomVariable.prototype.constructor = RandomVariable;
  AnonymousVariable.prototype = Object.create(Variable.prototype);
  AnonymousVariable.prototype.constructor = AnonymousVariable;
  UnificationException.prototype = Object.create(RuntimeException.prototype);
  UnificationException.prototype.constructor = UnificationException;
  NameError.prototype = Object.create(RuntimeException.prototype);
  NameError.prototype.constructor = NameError;
  VariableDiscrepancyException.prototype = Object.create(UnificationException.prototype);
  VariableDiscrepancyException.prototype.constructor = VariableDiscrepancyException;
  var LinkedHashMap_init = Kotlin.kotlin.collections.LinkedHashMap_init_q3lmfv$;
  function VariableMapping() {
    this.aToB_0 = LinkedHashMap_init();
    this.bToA_0 = LinkedHashMap_init();
  }
  VariableMapping.prototype.storeSubstitution_4ux96e$ = function (original, substitution) {
    if (this.aToB_0.containsKey_11rb$(original)) {
      throw new IllegalStateException();
    }
    this.aToB_0.put_xwzc9p$(original, substitution);
    this.bToA_0.put_xwzc9p$(substitution, original);
  };
  var Map = Kotlin.kotlin.collections.Map;
  VariableMapping.prototype.hasOriginal_itr7t1$ = function (original) {
    var $receiver = this.aToB_0;
    var tmp$;
    return (Kotlin.isType(tmp$ = $receiver, Map) ? tmp$ : Kotlin.throwCCE()).containsKey_11rb$(original);
  };
  VariableMapping.prototype.hasSubstitution_itr7t1$ = function (substitution) {
    var $receiver = this.bToA_0;
    var tmp$;
    return (Kotlin.isType(tmp$ = $receiver, Map) ? tmp$ : Kotlin.throwCCE()).containsKey_11rb$(substitution);
  };
  VariableMapping.prototype.getSubstitution_itr7t1$ = function (original) {
    return this.aToB_0.get_11rb$(original);
  };
  VariableMapping.prototype.getOriginal_itr7t1$ = function (substitution) {
    return this.bToA_0.get_11rb$(substitution);
  };
  VariableMapping.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'VariableMapping',
    interfaces: []
  };
  var LinkedHashSet_init = Kotlin.kotlin.collections.LinkedHashSet_init_287e2$;
  function DefaultKnowledgeBase() {
    this.predicates_0 = HashSet_init();
    this.rules_0 = LinkedHashSet_init();
  }
  function DefaultKnowledgeBase$fulfill$lambda(this$DefaultKnowledgeBase_0, closure$randomVarScope_0, closure$replaced_0, closure$termMappings_0, closure$predicate_0) {
    return function ($receiver_0, continuation_0, suspended) {
      var instance = new Coroutine$DefaultKnowledgeBase$fulfill$lambda(this$DefaultKnowledgeBase_0, closure$randomVarScope_0, closure$replaced_0, closure$termMappings_0, closure$predicate_0, $receiver_0, this, continuation_0);
      if (suspended)
        return instance;
      else
        return instance.doResume(null);
    };
  }
  function Coroutine$DefaultKnowledgeBase$fulfill$lambda(this$DefaultKnowledgeBase_0, closure$randomVarScope_0, closure$replaced_0, closure$termMappings_0, closure$predicate_0, $receiver_0, controller, continuation_0) {
    CoroutineImpl.call(this, continuation_0);
    this.$controller = controller;
    this.exceptionState_0 = 1;
    this.local$this$DefaultKnowledgeBase = this$DefaultKnowledgeBase_0;
    this.local$closure$randomVarScope = closure$randomVarScope_0;
    this.local$closure$replaced = closure$replaced_0;
    this.local$closure$termMappings = closure$termMappings_0;
    this.local$closure$predicate = closure$predicate_0;
    this.local$tmp$ = void 0;
    this.local$tmp$_0 = void 0;
    this.local$knownPredicate = void 0;
    this.local$knownPredicateReplaced = void 0;
    this.local$unification = void 0;
    this.local$resolvedBucket = void 0;
    this.local$knownRule = void 0;
    this.local$$receiver = $receiver_0;
  }
  Coroutine$DefaultKnowledgeBase$fulfill$lambda.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: null,
    interfaces: [CoroutineImpl]
  };
  Coroutine$DefaultKnowledgeBase$fulfill$lambda.prototype = Object.create(CoroutineImpl.prototype);
  Coroutine$DefaultKnowledgeBase$fulfill$lambda.prototype.constructor = Coroutine$DefaultKnowledgeBase$fulfill$lambda;
  Coroutine$DefaultKnowledgeBase$fulfill$lambda.prototype.doResume = function () {
    do
      try {
        switch (this.state_0) {
          case 0:
            this.local$tmp$ = this.local$this$DefaultKnowledgeBase.predicates_0.iterator();
            this.state_0 = 2;
            continue;
          case 1:
            throw this.exception_0;
          case 2:
            if (!this.local$tmp$.hasNext()) {
              this.state_0 = 5;
              continue;
            }

            this.local$knownPredicate = this.local$tmp$.next();
            this.local$knownPredicateReplaced = this.local$closure$randomVarScope.v.withRandomVariables_97438u$(this.local$knownPredicate, new VariableMapping());
            this.local$unification = this.local$knownPredicateReplaced.unify_278o73$(this.local$closure$replaced);
            if (this.local$unification != null) {
              this.local$resolvedBucket = this.local$unification.variableValues.withVariablesResolvedFrom_wy4b81$(this.local$closure$termMappings);
              this.local$resolvedBucket.retainAll_h547bq$(this.local$closure$predicate.variables);
              this.state_0 = 3;
              this.result_0 = this.local$$receiver.yield_11rb$(new Unification(this.local$resolvedBucket), this);
              if (this.result_0 === intrinsics.COROUTINE_SUSPENDED)
                return intrinsics.COROUTINE_SUSPENDED;
              break;
            }
             else {
              this.state_0 = 4;
              continue;
            }

          case 3:
            this.state_0 = 4;
            continue;
          case 4:
            this.state_0 = 2;
            continue;
          case 5:
            this.local$tmp$_0 = this.local$this$DefaultKnowledgeBase.rules_0.iterator();
            this.state_0 = 6;
            continue;
          case 6:
            if (!this.local$tmp$_0.hasNext()) {
              this.state_0 = 8;
              continue;
            }

            this.local$knownRule = this.local$tmp$_0.next();
            this.state_0 = 7;
            this.result_0 = this.local$$receiver.yieldAll_swo9gw$(this.local$knownRule.fulfill_z0h3pa$(this.local$closure$predicate, this.local$this$DefaultKnowledgeBase, this.local$closure$randomVarScope.v), this);
            if (this.result_0 === intrinsics.COROUTINE_SUSPENDED)
              return intrinsics.COROUTINE_SUSPENDED;
            break;
          case 7:
            this.state_0 = 6;
            continue;
          case 8:
            return Unit;
        }
      }
       catch (e) {
        if (this.state_0 === 1)
          throw e;
        else {
          this.state_0 = this.exceptionState_0;
          this.exception_0 = e;
        }
      }
     while (true);
  };
  DefaultKnowledgeBase.prototype.fulfill_yq3jvc$ = function (predicate) {
    var randomVarScope = {v: new RandomVariableScope()};
    var termMappings = new VariableMapping();
    var replaced = randomVarScope.v.withRandomVariables_97438u$(predicate, termMappings);
    return buildSequence(DefaultKnowledgeBase$fulfill$lambda(this, randomVarScope, replaced, termMappings, predicate));
  };
  DefaultKnowledgeBase.prototype.assert_yq3jvc$ = function (predicate) {
    this.predicates_0.add_11rb$(predicate);
  };
  DefaultKnowledgeBase.prototype.defineRule_khwii9$ = function (rule) {
    this.rules_0.add_11rb$(rule);
  };
  DefaultKnowledgeBase.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'DefaultKnowledgeBase',
    interfaces: [MutableKnowledgeBase]
  };
  function KnowledgeBase() {
    KnowledgeBase$Companion_getInstance();
  }
  KnowledgeBase.prototype.fulfill_9kpwdb$ = function (query) {
    return query.findProofWithin_dp0sxo$(this);
  };
  function KnowledgeBase$Companion() {
    KnowledgeBase$Companion_instance = this;
    this.EMPTY = new EmptyKnowledgeBase();
  }
  KnowledgeBase$Companion.$metadata$ = {
    kind: Kotlin.Kind.OBJECT,
    simpleName: 'Companion',
    interfaces: []
  };
  var KnowledgeBase$Companion_instance = null;
  function KnowledgeBase$Companion_getInstance() {
    if (KnowledgeBase$Companion_instance === null) {
      new KnowledgeBase$Companion();
    }
    return KnowledgeBase$Companion_instance;
  }
  KnowledgeBase.$metadata$ = {
    kind: Kotlin.Kind.INTERFACE,
    simpleName: 'KnowledgeBase',
    interfaces: []
  };
  function MutableKnowledgeBase() {
  }
  MutableKnowledgeBase.$metadata$ = {
    kind: Kotlin.Kind.INTERFACE,
    simpleName: 'MutableKnowledgeBase',
    interfaces: [KnowledgeBase]
  };
  function EmptyKnowledgeBase() {
  }
  EmptyKnowledgeBase.prototype.fulfill_yq3jvc$ = function (predicate) {
    return Unification$Companion_getInstance().NONE;
  };
  EmptyKnowledgeBase.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'EmptyKnowledgeBase',
    interfaces: [KnowledgeBase]
  };
  function RandomVariableScope() {
    this.randomCounter_0 = 0;
  }
  function RandomVariableScope$withRandomVariables$lambda(closure$mapping, this$RandomVariableScope) {
    return function (originalVariable) {
      var tmp$;
      if (!closure$mapping.hasOriginal_itr7t1$(originalVariable)) {
        var randomVariable = this$RandomVariableScope.createNewRandomVariable();
        closure$mapping.storeSubstitution_4ux96e$(originalVariable, randomVariable);
      }
      return (tmp$ = closure$mapping.getSubstitution_itr7t1$(originalVariable)) != null ? tmp$ : Kotlin.throwNPE();
    };
  }
  RandomVariableScope.prototype.withRandomVariables_97438u$ = function (term, mapping) {
    return term.substituteVariables_b4rspc$(RandomVariableScope$withRandomVariables$lambda(mapping, this));
  };
  RandomVariableScope.prototype.createNewRandomVariable = function () {
    var tmp$;
    return new RandomVariable((tmp$ = this.randomCounter_0, this.randomCounter_0 = tmp$ + 1 | 0, tmp$));
  };
  RandomVariableScope.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'RandomVariableScope',
    interfaces: []
  };
  function RandomVariable(counter) {
    Variable.call(this, '_G' + Kotlin.toString(counter));
    this.counter_0 = counter;
  }
  RandomVariable.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'RandomVariable',
    interfaces: [Variable]
  };
  function Rule(head, goals) {
    this.head = head;
    this.goals = goals;
  }
  function Rule$fulfill$lambda(closure$randomGoals_0, closure$kb_0, closure$ruleRandomVarsMapping_0, this$Rule_0) {
    return function ($receiver_0, continuation_0, suspended) {
      var instance = new Coroutine$Rule$fulfill$lambda(closure$randomGoals_0, closure$kb_0, closure$ruleRandomVarsMapping_0, this$Rule_0, $receiver_0, this, continuation_0);
      if (suspended)
        return instance;
      else
        return instance.doResume(null);
    };
  }
  function Coroutine$Rule$fulfill$lambda(closure$randomGoals_0, closure$kb_0, closure$ruleRandomVarsMapping_0, this$Rule_0, $receiver_0, controller, continuation_0) {
    CoroutineImpl.call(this, continuation_0);
    this.$controller = controller;
    this.exceptionState_0 = 1;
    this.local$closure$randomGoals = closure$randomGoals_0;
    this.local$closure$kb = closure$kb_0;
    this.local$closure$ruleRandomVarsMapping = closure$ruleRandomVarsMapping_0;
    this.local$this$Rule = this$Rule_0;
    this.local$$receiver = $receiver_0;
  }
  Coroutine$Rule$fulfill$lambda.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: null,
    interfaces: [CoroutineImpl]
  };
  Coroutine$Rule$fulfill$lambda.prototype = Object.create(CoroutineImpl.prototype);
  Coroutine$Rule$fulfill$lambda.prototype.constructor = Coroutine$Rule$fulfill$lambda;
  Coroutine$Rule$fulfill$lambda.prototype.doResume = function () {
    do
      try {
        switch (this.state_0) {
          case 0:
            this.state_0 = 2;
            this.result_0 = this.local$this$Rule.fulfill_0(this.local$$receiver, this.local$closure$randomGoals, this.local$closure$kb, this.local$closure$ruleRandomVarsMapping, void 0, this);
            if (this.result_0 === intrinsics.COROUTINE_SUSPENDED)
              return intrinsics.COROUTINE_SUSPENDED;
            break;
          case 1:
            throw this.exception_0;
          case 2:
            return this.result_0;
        }
      }
       catch (e) {
        if (this.state_0 === 1)
          throw e;
        else {
          this.state_0 = this.exceptionState_0;
          this.exception_0 = e;
        }
      }
     while (true);
  };
  function Rule$fulfill$lambda_0(closure$randomPredicate, closure$predicateAndHeadUnification, closure$predicateRandomVarsMapping) {
    return function (unification) {
      var tmp$, tmp$_0;
      var solutionVars = VariableBucket_init();
      tmp$ = closure$randomPredicate.variables.iterator();
      while (tmp$.hasNext()) {
        var randomPredicateVariable = tmp$.next();
        if (closure$predicateAndHeadUnification.variableValues.isInstantiated_itr7t1$(randomPredicateVariable)) {
          var value = closure$predicateAndHeadUnification.variableValues.get_itr7t1$(randomPredicateVariable).substituteVariables_b4rspc$(unification.variableValues.asSubstitutionMapper());
          solutionVars.instantiate_wyxs1m$(randomPredicateVariable, value);
        }
         else {
          var originalVar = (tmp$_0 = closure$predicateRandomVarsMapping.getOriginal_itr7t1$(randomPredicateVariable)) != null ? tmp$_0 : Kotlin.throwNPE();
          solutionVars.instantiate_wyxs1m$(originalVar, unification.variableValues.get_itr7t1$(randomPredicateVariable));
        }
      }
      return new Unification(solutionVars.withVariablesResolvedFrom_wy4b81$(closure$predicateRandomVarsMapping));
    };
  }
  var collectionSizeOrDefault = Kotlin.kotlin.collections.collectionSizeOrDefault_ba2ldo$;
  var ArrayList_init = Kotlin.kotlin.collections.ArrayList_init_ww73n8$;
  Rule.prototype.fulfill_z0h3pa$ = function (predicate, kb, randomVariableScope) {
    var tmp$;
    var predicateRandomVarsMapping = new VariableMapping();
    var randomPredicate = randomVariableScope.withRandomVariables_97438u$(predicate, predicateRandomVarsMapping);
    var ruleRandomVarsMapping = new VariableMapping();
    var randomHead = randomVariableScope.withRandomVariables_97438u$(this.head, ruleRandomVarsMapping);
    var predicateAndHeadUnification = randomHead.unify_278o73$(randomPredicate);
    if (predicateAndHeadUnification == null) {
      return Unification$Companion_getInstance().NONE;
    }
    var $receiver = this.goals;
    var destination = ArrayList_init(collectionSizeOrDefault($receiver, 10));
    var tmp$_0;
    tmp$_0 = $receiver.iterator();
    while (tmp$_0.hasNext()) {
      var item = tmp$_0.next();
      destination.add_11rb$(randomVariableScope.withRandomVariables_97438u$(item, ruleRandomVarsMapping));
    }
    var destination_0 = ArrayList_init(collectionSizeOrDefault(destination, 10));
    var tmp$_1;
    tmp$_1 = destination.iterator();
    while (tmp$_1.hasNext()) {
      var item_0 = tmp$_1.next();
      destination_0.add_11rb$(item_0.substituteVariables_b4rspc$(predicateAndHeadUnification.variableValues.asSubstitutionMapper()));
    }
    var randomGoals = Kotlin.isType(tmp$ = destination_0, List) ? tmp$ : Kotlin.throwCCE();
    return map(buildSequence(Rule$fulfill$lambda(randomGoals, kb, ruleRandomVarsMapping, this)), Rule$fulfill$lambda_0(randomPredicate, predicateAndHeadUnification, predicateRandomVarsMapping));
  };
  Rule.prototype.fulfill_0 = function ($receiver_0, goals_0, kb_0, ruleRandomVarsMapping_0, vars_0, continuation_0, suspended) {
    var instance = new Coroutine$fulfill_0(this, $receiver_0, goals_0, kb_0, ruleRandomVarsMapping_0, vars_0, continuation_0);
    if (suspended)
      return instance;
    else
      return instance.doResume(null);
  };
  function Coroutine$fulfill_0($this, $receiver_0, goals_0, kb_0, ruleRandomVarsMapping_0, vars_0, continuation_0) {
    CoroutineImpl.call(this, continuation_0);
    this.exceptionState_0 = 1;
    this.$this = $this;
    this.local$tmp$ = void 0;
    this.local$tmp$_0 = void 0;
    this.local$goal = void 0;
    this.local$goalUnification = void 0;
    this.local$goalVars = void 0;
    this.local$tmp$_1 = void 0;
    this.local$variable = void 0;
    this.local$value = void 0;
    this.local$substitutedValue = void 0;
    this.local$$receiver = $receiver_0;
    this.local$goals = goals_0;
    this.local$kb = kb_0;
    this.local$ruleRandomVarsMapping = ruleRandomVarsMapping_0;
    this.local$vars = vars_0;
  }
  Coroutine$fulfill_0.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: null,
    interfaces: [CoroutineImpl]
  };
  Coroutine$fulfill_0.prototype = Object.create(CoroutineImpl.prototype);
  Coroutine$fulfill_0.prototype.constructor = Coroutine$fulfill_0;
  Coroutine$fulfill_0.prototype.doResume = function () {
    do
      try {
        switch (this.state_0) {
          case 0:
            if (this.local$vars === void 0)
              this.local$vars = VariableBucket_init();
            this.local$goal = this.local$goals.get_za3lpa$(0).substituteVariables_b4rspc$(this.local$vars.asSubstitutionMapper());
            this.local$tmp$ = this.local$kb.fulfill_yq3jvc$(this.local$goal).iterator();
            this.state_0 = 2;
            continue;
          case 1:
            throw this.exception_0;
          case 2:
            if (!this.local$tmp$.hasNext()) {
              this.state_0 = 11;
              continue;
            }

            this.local$goalUnification = this.local$tmp$.next();
            this.local$goalVars = this.local$vars.copy();
            this.local$tmp$_0 = this.local$goalUnification.variableValues.values.iterator();
            this.state_0 = 3;
            continue;
          case 3:
            if (!this.local$tmp$_0.hasNext()) {
              this.state_0 = 7;
              continue;
            }

            this.local$tmp$_1 = this.local$tmp$_0.next();
            this.local$variable = this.local$tmp$_1.component1(), this.local$value = this.local$tmp$_1.component2();
            if (this.local$value != null) {
              this.local$substitutedValue = this.local$value.substituteVariables_b4rspc$(this.local$goalVars.asSubstitutionMapper());
              if (this.local$goalVars.isInstantiated_itr7t1$(this.local$variable)) {
                if (!Kotlin.equals(this.local$goalVars.get_itr7t1$(this.local$variable), this.local$substitutedValue) && !Kotlin.equals(this.local$goalVars.get_itr7t1$(this.local$variable), this.local$value)) {
                  this.state_0 = 2;
                  continue;
                }
                 else {
                  this.state_0 = 4;
                  continue;
                }
              }
               else {
                this.local$goalVars.instantiate_wyxs1m$(this.local$variable, this.local$substitutedValue);
                this.state_0 = 5;
                continue;
              }
            }
             else {
              this.state_0 = 6;
              continue;
            }

          case 4:
            this.state_0 = 5;
            continue;
          case 5:
            this.state_0 = 6;
            continue;
          case 6:
            this.state_0 = 3;
            continue;
          case 7:
            if (this.local$goals.size === 1) {
              this.state_0 = 9;
              this.result_0 = this.local$$receiver.yield_11rb$(new Unification(this.local$goalVars), this);
              if (this.result_0 === intrinsics.COROUTINE_SUSPENDED)
                return intrinsics.COROUTINE_SUSPENDED;
              break;
            }
             else {
              this.state_0 = 8;
              this.result_0 = this.$this.fulfill_0(this.local$$receiver, this.local$goals.subList_vux9f0$(1, get_lastIndex(this.local$goals)), this.local$kb, this.local$ruleRandomVarsMapping, this.local$goalVars, this);
              if (this.result_0 === intrinsics.COROUTINE_SUSPENDED)
                return intrinsics.COROUTINE_SUSPENDED;
              break;
            }

          case 8:
            this.state_0 = 10;
            continue;
          case 9:
            this.state_0 = 10;
            continue;
          case 10:
            this.state_0 = 2;
            continue;
          case 11:
            return;
        }
      }
       catch (e) {
        if (this.state_0 === 1)
          throw e;
        else {
          this.state_0 = this.exceptionState_0;
          this.exception_0 = e;
        }
      }
     while (true);
  };
  Rule.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'Rule',
    interfaces: []
  };
  function Query() {
  }
  Query.$metadata$ = {
    kind: Kotlin.Kind.INTERFACE,
    simpleName: 'Query',
    interfaces: []
  };
  function BooleanAndQuery(components) {
    if (components.length === 0) {
      throw new IllegalArgumentException('A boolean AND query must have at least one component.');
    }
  }
  function BooleanAndQuery$findProofWithin$lambda($receiver_0, continuation_0, suspended) {
    var instance = new Coroutine$BooleanAndQuery$findProofWithin$lambda($receiver_0, this, continuation_0);
    if (suspended)
      return instance;
    else
      return instance.doResume(null);
  }
  function Coroutine$BooleanAndQuery$findProofWithin$lambda($receiver_0, controller, continuation_0) {
    CoroutineImpl.call(this, continuation_0);
    this.$controller = controller;
    this.exceptionState_0 = 1;
    this.local$$receiver = $receiver_0;
  }
  Coroutine$BooleanAndQuery$findProofWithin$lambda.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: null,
    interfaces: [CoroutineImpl]
  };
  Coroutine$BooleanAndQuery$findProofWithin$lambda.prototype = Object.create(CoroutineImpl.prototype);
  Coroutine$BooleanAndQuery$findProofWithin$lambda.prototype.constructor = Coroutine$BooleanAndQuery$findProofWithin$lambda;
  Coroutine$BooleanAndQuery$findProofWithin$lambda.prototype.doResume = function () {
    do
      try {
        switch (this.state_0) {
          case 0:
            return Unit;
          case 1:
            throw this.exception_0;
        }
      }
       catch (e) {
        if (this.state_0 === 1)
          throw e;
        else {
          this.state_0 = this.exceptionState_0;
          this.exception_0 = e;
        }
      }
     while (true);
  };
  BooleanAndQuery.prototype.findProofWithin_dp0sxo$ = function (kb) {
    return buildSequence(BooleanAndQuery$findProofWithin$lambda);
  };
  BooleanAndQuery.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'BooleanAndQuery',
    interfaces: [Query]
  };
  function Atom(name) {
    this.name = name;
    this.variables_krllxd$_0 = emptySet();
  }
  Atom.prototype.unify_278o73$$default = function (rhs, randomVariableScope) {
    if (Kotlin.equals(rhs, this)) {
      return Unification$Companion_getInstance().TRUE;
    }
     else if (Kotlin.isType(rhs, Variable)) {
      return rhs.unify_278o73$(this);
    }
     else {
      return Unification$Companion_getInstance().FALSE;
    }
  };
  Object.defineProperty(Atom.prototype, 'variables', {
    get: function () {
      return this.variables_krllxd$_0;
    }
  });
  Atom.prototype.substituteVariables_b4rspc$ = function (mapper) {
    return this;
  };
  Atom.prototype.equals = function (other) {
    var tmp$, tmp$_0;
    if (this === other)
      return true;
    if (other == null || !((tmp$ = Kotlin.getKClassFromExpression(this)) != null ? tmp$.equals(Kotlin.getKClassFromExpression(other)) : null))
      return false;
    Kotlin.isType(tmp$_0 = other, Atom) ? tmp$_0 : Kotlin.throwCCE();
    if (!Kotlin.equals(this.name, other.name))
      return false;
    return true;
  };
  Atom.prototype.hashCode = function () {
    return Kotlin.hashCode(this.name);
  };
  var unboxChar = Kotlin.unboxChar;
  Atom.prototype.toString = function () {
    var firstChar = this.name.charCodeAt(0);
    var tmp$ = unboxChar(String.fromCharCode(firstChar).toLowerCase().charCodeAt(0)) === firstChar;
    if (!tmp$) {
      var $receiver = this.name;
      tmp$ = Regex('\\s').containsMatchIn_6bul2c$($receiver);
    }
    if (tmp$) {
      return "'" + this.name + "'";
    }
     else {
      return this.name;
    }
  };
  Atom.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'Atom',
    interfaces: [Term]
  };
  function List_0(givenElements, givenTail) {
    if (givenTail === void 0)
      givenTail = null;
    this.elements = null;
    this.tail = null;
    var tmp$;
    if (Kotlin.isType(givenTail, List_0)) {
      this.elements = plus(givenElements, givenTail.elements);
      this.tail = givenTail.tail;
    }
     else {
      this.elements = givenElements;
      this.tail = (tmp$ = givenTail) == null || Kotlin.isType(tmp$, Variable) ? tmp$ : Kotlin.throwCCE();
    }
  }
  List_0.prototype.unify_278o73$$default = function (rhs, randomVarsScope) {
    var tmp$, tmp$_0, tmp$_1, tmp$_2;
    if (Kotlin.isType(rhs, Variable)) {
      return rhs.unify_278o73$(this);
    }
     else if (Kotlin.isType(rhs, List_0)) {
      if (rhs.elements.size > this.elements.size) {
        return rhs.unify_278o73$(this, randomVarsScope);
      }
      if (this.elements.size > rhs.elements.size && rhs.tail == null) {
        return Unification$Companion_getInstance().FALSE;
      }
      var carryUnification = new Unification();
      tmp$ = get_lastIndex(this.elements);
      for (var index = 0; index <= tmp$; index++) {
        var iterationUnification;
        if (index > get_lastIndex(rhs.elements)) {
          var rhsTail = (tmp$_0 = rhs.tail) != null ? tmp$_0 : Kotlin.throwNPE();
          var vars = VariableBucket_init();
          vars.instantiate_wyxs1m$(rhsTail, new List_0(this.elements.subList_vux9f0$(index, this.elements.size), this.tail));
          try {
            return carryUnification.combineWith_qzv1eh$(new Unification(vars));
          }
           catch (ex) {
            if (Kotlin.isType(ex, VariableDiscrepancyException)) {
              return Unification$Companion_getInstance().FALSE;
            }
             else
              throw ex;
          }
        }
         else {
          var lhsElement = this.elements.get_za3lpa$(index);
          var rhsElement = rhs.elements.get_za3lpa$(index);
          iterationUnification = lhsElement.unify_278o73$(rhsElement, randomVarsScope);
        }
        if (iterationUnification == null) {
          return Unification$Companion_getInstance().FALSE;
        }
         else {
          try {
            carryUnification = carryUnification.combineWith_qzv1eh$(iterationUnification);
          }
           catch (ex) {
            if (Kotlin.isType(ex, VariableDiscrepancyException)) {
              return Unification$Companion_getInstance().FALSE;
            }
             else
              throw ex;
          }
        }
      }
      if (this.tail != null || rhs.tail != null) {
        var tailUnification;
        if (this.tail != null && rhs.tail != null) {
          tailUnification = this.tail.unify_278o73$((tmp$_1 = rhs.tail) != null ? tmp$_1 : Kotlin.throwNPE(), randomVarsScope);
        }
         else if (this.tail != null) {
          tailUnification = this.tail.unify_278o73$(new List_0(emptyList()));
        }
         else {
          tailUnification = ((tmp$_2 = rhs.tail) != null ? tmp$_2 : Kotlin.throwNPE()).unify_278o73$(new List_0(emptyList()));
        }
        try {
          carryUnification = carryUnification.combineWith_qzv1eh$(tailUnification);
        }
         catch (ex) {
          if (Kotlin.isType(ex, VariableDiscrepancyException)) {
            return Unification$Companion_getInstance().FALSE;
          }
           else
            throw ex;
        }
      }
      return carryUnification;
    }
     else {
      return Unification$Companion_getInstance().FALSE;
    }
  };
  var addAll = Kotlin.kotlin.collections.addAll_ipc267$;
  Object.defineProperty(List_0.prototype, 'variables', {
    get: function () {
      var $receiver = this.elements;
      var transform = Kotlin.getPropertyCallableRef('variables', 1, function ($receiver) {
        return $receiver.variables;
      });
      var destination = ArrayList_init();
      var tmp$;
      tmp$ = $receiver.iterator();
      while (tmp$.hasNext()) {
        var element = tmp$.next();
        var list = transform(element);
        addAll(destination, list);
      }
      var elements = toMutableSet(destination);
      if (this.tail != null) {
        elements.add_11rb$(this.tail);
      }
      return elements;
    }
  });
  List_0.prototype.substituteVariables_b4rspc$ = function (mapper) {
    var tmp$;
    var $receiver = this.elements;
    var destination = ArrayList_init(collectionSizeOrDefault($receiver, 10));
    var tmp$_0;
    tmp$_0 = $receiver.iterator();
    while (tmp$_0.hasNext()) {
      var item = tmp$_0.next();
      destination.add_11rb$(item.substituteVariables_b4rspc$(mapper));
    }
    return new List_0(destination, (tmp$ = this.tail) != null ? tmp$.substituteVariables_b4rspc$(mapper) : null);
  };
  List_0.prototype.toString = function () {
    var out = '[' + joinToString(this.elements, ',');
    if (this.tail != null) {
      out += '|' + Kotlin.toString(this.tail);
    }
    return out + ']';
  };
  List_0.prototype.equals = function (other) {
    if (this === other)
      return true;
    if (!Kotlin.isType(other, List_0))
      return false;
    if (!Kotlin.equals(this.elements, other.elements))
      return false;
    if (!Kotlin.equals(this.tail, other.tail))
      return false;
    return true;
  };
  List_0.prototype.hashCode = function () {
    var tmp$, tmp$_0;
    var result = Kotlin.hashCode(this.elements);
    result = (31 * result | 0) + ((tmp$_0 = (tmp$ = this.tail) != null ? tmp$.hashCode() : null) != null ? tmp$_0 : 0) | 0;
    return result;
  };
  List_0.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'List',
    interfaces: [Term]
  };
  function Predicate(name, arguments_0) {
    this.name = name;
    this.arguments = arguments_0;
    var $receiver = this.arguments;
    var transform = Kotlin.getPropertyCallableRef('variables', 1, function ($receiver) {
      return $receiver.variables;
    });
    var destination = ArrayList_init();
    var tmp$;
    for (tmp$ = 0; tmp$ !== $receiver.length; ++tmp$) {
      var element = $receiver[tmp$];
      var list = transform(element);
      addAll(destination, list);
    }
    this.variables_ub8xl$_0 = toSet(destination);
  }
  Predicate.prototype.unify_278o73$$default = function (rhs, randomVarsScope) {
    var tmp$, tmp$_0;
    if (Kotlin.isType(rhs, Predicate)) {
      if (!Kotlin.equals(this.name, rhs.name)) {
        return Unification$Companion_getInstance().FALSE;
      }
      if (this.arguments.length !== rhs.arguments.length) {
        return Unification$Companion_getInstance().FALSE;
      }
      if (this.arguments.length === 0) {
        return Unification$Companion_getInstance().TRUE;
      }
      var vars = VariableBucket_init();
      tmp$ = get_lastIndex_0(this.arguments);
      for (var argIndex = 0; argIndex <= tmp$; argIndex++) {
        var lhsArg = this.arguments[argIndex].substituteVariables_b4rspc$(vars.asSubstitutionMapper());
        var rhsArg = rhs.arguments[argIndex].substituteVariables_b4rspc$(vars.asSubstitutionMapper());
        var argUnification = lhsArg.unify_278o73$(rhsArg, randomVarsScope);
        if (argUnification == null) {
          return Unification$Companion_getInstance().FALSE;
        }
        tmp$_0 = argUnification.variableValues.values.iterator();
        while (tmp$_0.hasNext()) {
          var tmp$_1 = tmp$_0.next();
          var variable = tmp$_1.component1()
          , value = tmp$_1.component2();
          if (value != null) {
            var substitutedValue = value.substituteVariables_b4rspc$(vars.asSubstitutionMapper());
            if (vars.isInstantiated_itr7t1$(variable)) {
              if (!Kotlin.equals(vars.get_itr7t1$(variable), substitutedValue) && !Kotlin.equals(vars.get_itr7t1$(variable), value)) {
                return Unification$Companion_getInstance().FALSE;
              }
            }
             else {
              vars.instantiate_wyxs1m$(variable, substitutedValue);
            }
          }
        }
      }
      return new Unification(vars);
    }
     else if (Kotlin.isType(rhs, Variable)) {
      return rhs.unify_278o73$(this);
    }
     else {
      return Unification$Companion_getInstance().FALSE;
    }
  };
  Object.defineProperty(Predicate.prototype, 'variables', {
    get: function () {
      return this.variables_ub8xl$_0;
    }
  });
  var copyToArray = Kotlin.kotlin.collections.copyToArray;
  Predicate.prototype.substituteVariables_b4rspc$ = function (mapper) {
    var tmp$ = this.name;
    var $receiver = this.arguments;
    var destination = ArrayList_init($receiver.length);
    var tmp$_0;
    for (tmp$_0 = 0; tmp$_0 !== $receiver.length; ++tmp$_0) {
      var item = $receiver[tmp$_0];
      destination.add_11rb$(item.substituteVariables_b4rspc$(mapper));
    }
    return new Predicate(tmp$, copyToArray(destination));
  };
  Predicate.prototype.toString = function () {
    return this.name + '(' + joinToString_0(this.arguments, ',') + ')';
  };
  Predicate.prototype.equals = function (other) {
    if (this === other)
      return true;
    if (!Kotlin.isType(other, Predicate))
      return false;
    if (!Kotlin.equals(this.name, other.name))
      return false;
    if (contentDeepEquals(this.arguments, other.arguments))
      return true;
    return false;
  };
  Predicate.prototype.hashCode = function () {
    var result = Kotlin.hashCode(this.name);
    result = (31 * result | 0) + sensibleHashCode(this.arguments) | 0;
    return result;
  };
  Predicate.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'Predicate',
    interfaces: [Term]
  };
  function PredicateBuilder(predicateName) {
    this.predicateName_0 = predicateName;
  }
  PredicateBuilder.prototype.invoke_woqqnc$ = function (arguments_0) {
    return new Predicate(this.predicateName_0, arguments_0);
  };
  PredicateBuilder.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'PredicateBuilder',
    interfaces: []
  };
  function Term() {
  }
  Term.prototype.unify_278o73$ = function (rhs, randomVarsScope, callback$default) {
    if (randomVarsScope === void 0)
      randomVarsScope = new RandomVariableScope();
    return callback$default ? callback$default(rhs, randomVarsScope) : this.unify_278o73$$default(rhs, randomVarsScope);
  };
  Term.$metadata$ = {
    kind: Kotlin.Kind.INTERFACE,
    simpleName: 'Term',
    interfaces: []
  };
  function Variable(name) {
    Variable$Companion_getInstance();
    this.name = name;
  }
  Variable.prototype.unify_278o73$$default = function (rhs, randomVariableScope) {
    if (Kotlin.isType(rhs, Variable) && Kotlin.equals(rhs, this))
      return Unification$Companion_getInstance().TRUE;
    var vars = VariableBucket_init();
    vars.instantiate_wyxs1m$(this, rhs);
    return new Unification(vars);
  };
  Object.defineProperty(Variable.prototype, 'variables', {
    get: function () {
      return setOf(this);
    }
  });
  Variable.prototype.substituteVariables_b4rspc$ = function (mapper) {
    return mapper(this);
  };
  Variable.prototype.toString = function () {
    return this.name;
  };
  Variable.prototype.equals = function (other) {
    if (this === other)
      return true;
    if (!Kotlin.isType(other, Variable))
      return false;
    if (!Kotlin.equals(this.name, other.name))
      return false;
    return true;
  };
  Variable.prototype.hashCode = function () {
    return Kotlin.hashCode(this.name);
  };
  function Variable$Companion() {
    Variable$Companion_instance = this;
    this.ANONYMOUS = AnonymousVariable_getInstance();
  }
  Variable$Companion.$metadata$ = {
    kind: Kotlin.Kind.OBJECT,
    simpleName: 'Companion',
    interfaces: []
  };
  var Variable$Companion_instance = null;
  function Variable$Companion_getInstance() {
    if (Variable$Companion_instance === null) {
      new Variable$Companion();
    }
    return Variable$Companion_instance;
  }
  Variable.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'Variable',
    interfaces: [Term]
  };
  function AnonymousVariable() {
    AnonymousVariable_instance = this;
    Variable.call(this, '_');
  }
  AnonymousVariable.prototype.unify_278o73$$default = function (rhs, randomVariableScope) {
    var randomVar = randomVariableScope.createNewRandomVariable();
    var bucket = VariableBucket_init();
    bucket.instantiate_wyxs1m$(randomVar, rhs);
    return new Unification(bucket);
  };
  AnonymousVariable.$metadata$ = {
    kind: Kotlin.Kind.OBJECT,
    simpleName: 'AnonymousVariable',
    interfaces: [Variable]
  };
  var AnonymousVariable_instance = null;
  function AnonymousVariable_getInstance() {
    if (AnonymousVariable_instance === null) {
      new AnonymousVariable();
    }
    return AnonymousVariable_instance;
  }
  function Unification(variableValues) {
    Unification$Companion_getInstance();
    if (variableValues === void 0)
      variableValues = VariableBucket_init();
    this.variableValues = variableValues;
  }
  Unification.prototype.combineWith_qzv1eh$ = function (other) {
    return new Unification(this.variableValues.combineWith_85jbpu$(other.variableValues));
  };
  Unification.prototype.toString = function () {
    var $receiver = this.variableValues.values;
    var destination = ArrayList_init(collectionSizeOrDefault($receiver, 10));
    var tmp$;
    tmp$ = $receiver.iterator();
    while (tmp$.hasNext()) {
      var item = tmp$.next();
      var tmp$_0 = destination.add_11rb$;
      var variable = item.component1()
      , value = item.component2();
      tmp$_0.call(destination, variable + ' = ' + (value != null ? value : Variable$Companion_getInstance().ANONYMOUS));
    }
    return joinToString(destination, ', ');
  };
  function Unification$Companion() {
    Unification$Companion_instance = this;
    this.FALSE = null;
    this.TRUE = new Unification();
    this.NONE = asSequence(emptySet());
  }
  Unification$Companion.$metadata$ = {
    kind: Kotlin.Kind.OBJECT,
    simpleName: 'Companion',
    interfaces: []
  };
  var Unification$Companion_instance = null;
  function Unification$Companion_getInstance() {
    if (Unification$Companion_instance === null) {
      new Unification$Companion();
    }
    return Unification$Companion_instance;
  }
  Unification.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'Unification',
    interfaces: []
  };
  function UnificationException(message, cause) {
    if (cause === void 0)
      cause = null;
    RuntimeException.call(this, message);
    this.cause_lzcpef$_0 = cause;
    this.name = 'UnificationException';
  }
  Object.defineProperty(UnificationException.prototype, 'cause', {
    get: function () {
      return this.cause_lzcpef$_0;
    }
  });
  UnificationException.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'UnificationException',
    interfaces: [RuntimeException]
  };
  function VariableBucket(variableMap) {
    this.variableMap_0 = variableMap;
    this.substitutionMapper_0 = VariableBucket$substitutionMapper$lambda(this);
  }
  VariableBucket.prototype.asSubstitutionMapper = function () {
    return this.substitutionMapper_0;
  };
  VariableBucket.prototype.get_itr7t1$ = function (v) {
    var tmp$;
    if (this.isInstantiated_itr7t1$(v)) {
      return (tmp$ = this.variableMap_0.get_11rb$(v)) != null ? tmp$ : Kotlin.throwNPE();
    }
     else {
      throw new NameError('Variable ' + v + ' has not been instantiated yet.');
    }
  };
  VariableBucket.prototype.instantiate_wyxs1m$ = function (variable, value) {
    if (this.isInstantiated_itr7t1$(variable)) {
      throw new NameError('Variable ' + variable + ' is already instantiated in this bucket.');
    }
    this.variableMap_0.put_xwzc9p$(variable, value);
  };
  VariableBucket.prototype.isInstantiated_itr7t1$ = function (variable) {
    return this.variableMap_0.get_11rb$(variable) != null;
  };
  VariableBucket.prototype.combineWith_85jbpu$ = function (other) {
    var tmp$, tmp$_0;
    var copy = this.copy();
    tmp$ = other.variableMap_0.entries.iterator();
    while (tmp$.hasNext()) {
      var tmp$_1 = tmp$.next();
      var variableName = tmp$_1.key;
      var othersValue = tmp$_1.value;
      var $receiver = copy.variableMap_0;
      var tmp$_2;
      if (!(Kotlin.isType(tmp$_2 = $receiver, Map) ? tmp$_2 : Kotlin.throwCCE()).containsKey_11rb$(variableName)) {
        copy.variableMap_0.put_xwzc9p$(variableName, othersValue);
      }
       else {
        var thisValue = (tmp$_0 = copy.variableMap_0.get_11rb$(variableName)) != null ? tmp$_0 : Kotlin.throwNPE();
        if (thisValue != null && othersValue != null) {
          if (!Kotlin.equals(thisValue, othersValue)) {
            throw new VariableDiscrepancyException('Cannot combine: variable ' + variableName + ' is instantiated to unequal values: ' + thisValue + ' and ' + Kotlin.toString(othersValue));
          }
        }
         else if (othersValue != null) {
          copy.variableMap_0.put_xwzc9p$(variableName, othersValue);
        }
      }
    }
    return copy;
  };
  VariableBucket.prototype.copy = function () {
    var mapCopy = LinkedHashMap_init();
    mapCopy.putAll_a2k3zr$(this.variableMap_0);
    return new VariableBucket(mapCopy);
  };
  VariableBucket.prototype.retainAll_h547bq$ = function (variables) {
    var tmp$;
    var $receiver = this.variableMap_0.keys;
    var destination = ArrayList_init();
    var tmp$_0;
    tmp$_0 = $receiver.iterator();
    while (tmp$_0.hasNext()) {
      var element = tmp$_0.next();
      if (!variables.contains_11rb$(element))
        destination.add_11rb$(element);
    }
    var keysToRemove = destination;
    tmp$ = keysToRemove.iterator();
    while (tmp$.hasNext()) {
      var key = tmp$.next();
      this.variableMap_0.remove_11rb$(key);
    }
  };
  function VariableBucket$withVariablesResolvedFrom$resolve(closure$mapping) {
    return function (variable) {
      var tmp$;
      var pivot = variable;
      while (closure$mapping.hasSubstitution_itr7t1$(pivot)) {
        pivot = (tmp$ = closure$mapping.getOriginal_itr7t1$(pivot)) != null ? tmp$ : Kotlin.throwNPE();
      }
      return pivot;
    };
  }
  VariableBucket.prototype.withVariablesResolvedFrom_wy4b81$ = function (mapping) {
    var tmp$;
    var resolve = VariableBucket$withVariablesResolvedFrom$resolve(mapping);
    var newBucket = VariableBucket_init();
    tmp$ = this.values.iterator();
    while (tmp$.hasNext()) {
      var tmp$_0 = tmp$.next();
      var variable = tmp$_0.component1()
      , value = tmp$_0.component2();
      var resolved = resolve(variable);
      if (value != null)
        newBucket.instantiate_wyxs1m$(resolved, value);
    }
    return newBucket;
  };
  Object.defineProperty(VariableBucket.prototype, 'values', {
    get: function () {
      var $receiver = this.variableMap_0;
      var destination = ArrayList_init($receiver.size);
      var tmp$;
      tmp$ = $receiver.entries.iterator();
      while (tmp$.hasNext()) {
        var item = tmp$.next();
        destination.add_11rb$(to(item.key, item.value));
      }
      return destination;
    }
  });
  function VariableBucket$substitutionMapper$lambda(this$VariableBucket) {
    return function (variable) {
      if (this$VariableBucket.isInstantiated_itr7t1$(variable) && !Kotlin.equals(this$VariableBucket.get_itr7t1$(variable), variable)) {
        return this$VariableBucket.get_itr7t1$(variable).substituteVariables_b4rspc$(this$VariableBucket.asSubstitutionMapper());
      }
       else {
        return variable;
      }
    };
  }
  VariableBucket.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'VariableBucket',
    interfaces: []
  };
  function VariableBucket_init($this) {
    $this = $this || Object.create(VariableBucket.prototype);
    VariableBucket.call($this, LinkedHashMap_init());
    return $this;
  }
  function NameError(message, cause) {
    if (cause === void 0)
      cause = null;
    RuntimeException.call(this, message);
    this.cause_64orle$_0 = cause;
    this.name = 'NameError';
  }
  Object.defineProperty(NameError.prototype, 'cause', {
    get: function () {
      return this.cause_64orle$_0;
    }
  });
  NameError.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'NameError',
    interfaces: [RuntimeException]
  };
  function VariableDiscrepancyException(message, cause) {
    if (cause === void 0)
      cause = null;
    UnificationException.call(this, message, cause);
    this.name = 'VariableDiscrepancyException';
  }
  VariableDiscrepancyException.$metadata$ = {
    kind: Kotlin.Kind.CLASS,
    simpleName: 'VariableDiscrepancyException',
    interfaces: [UnificationException]
  };
  function sensibleHashCode($receiver) {
    var tmp$;
    if ($receiver == null)
      return 0;
    var result = 1;
    for (tmp$ = 0; tmp$ !== $receiver.length; ++tmp$) {
      var element = $receiver[tmp$];
      result = (31 * result | 0) + (element == null ? 0 : Kotlin.hashCode(element)) | 0;
    }
    return result;
  }
  var package$com = _.com || (_.com = {});
  var package$github = package$com.github || (package$com.github = {});
  var package$tmarsteel = package$github.tmarsteel || (package$github.tmarsteel = {});
  var package$ktprolog = package$tmarsteel.ktprolog || (package$tmarsteel.ktprolog = {});
  package$ktprolog.VariableMapping = VariableMapping;
  var package$knowledge = package$ktprolog.knowledge || (package$ktprolog.knowledge = {});
  package$knowledge.DefaultKnowledgeBase = DefaultKnowledgeBase;
  Object.defineProperty(KnowledgeBase, 'Companion', {
    get: KnowledgeBase$Companion_getInstance
  });
  package$knowledge.KnowledgeBase = KnowledgeBase;
  package$knowledge.MutableKnowledgeBase = MutableKnowledgeBase;
  package$knowledge.EmptyKnowledgeBase = EmptyKnowledgeBase;
  package$knowledge.RandomVariableScope = RandomVariableScope;
  package$knowledge.RandomVariable = RandomVariable;
  package$knowledge.Rule = Rule;
  var package$query = package$ktprolog.query || (package$ktprolog.query = {});
  package$query.Query = Query;
  package$query.BooleanAndQuery = BooleanAndQuery;
  var package$term = package$ktprolog.term || (package$ktprolog.term = {});
  package$term.Atom = Atom;
  package$term.List = List_0;
  package$term.Predicate = Predicate;
  package$term.PredicateBuilder = PredicateBuilder;
  package$term.Term = Term;
  Object.defineProperty(Variable, 'Companion', {
    get: Variable$Companion_getInstance
  });
  package$term.Variable = Variable;
  Object.defineProperty(Unification, 'Companion', {
    get: Unification$Companion_getInstance
  });
  var package$unification = package$ktprolog.unification || (package$ktprolog.unification = {});
  package$unification.Unification = Unification;
  package$unification.UnificationException = UnificationException;
  package$unification.VariableBucket_init = VariableBucket_init;
  package$unification.VariableBucket = VariableBucket;
  package$unification.NameError = NameError;
  package$unification.VariableDiscrepancyException = VariableDiscrepancyException;
  _.sensibleHashCode_tfvi98$ = sensibleHashCode;
  DefaultKnowledgeBase.prototype.fulfill_9kpwdb$ = KnowledgeBase.prototype.fulfill_9kpwdb$;
  MutableKnowledgeBase.prototype.fulfill_9kpwdb$ = KnowledgeBase.prototype.fulfill_9kpwdb$;
  EmptyKnowledgeBase.prototype.fulfill_9kpwdb$ = KnowledgeBase.prototype.fulfill_9kpwdb$;
  Variable.prototype.unify_278o73$ = Term.prototype.unify_278o73$;
  Atom.prototype.unify_278o73$ = Term.prototype.unify_278o73$;
  List_0.prototype.unify_278o73$ = Term.prototype.unify_278o73$;
  Predicate.prototype.unify_278o73$ = Term.prototype.unify_278o73$;
  Kotlin.defineModule('kotlin-prolog-core', _);
  return _;
}(typeof this['kotlin-prolog-core'] === 'undefined' ? {} : this['kotlin-prolog-core'], kotlin);

//# sourceMappingURL=kotlin-prolog-core.js.map
