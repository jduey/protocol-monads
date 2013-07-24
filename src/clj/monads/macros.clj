;   Copyright (c) Jim Duey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns monads.macros)

(defn- ensure-items [n steps]
  "Ensures there are at least n elements on a list, will fill up with nil
  values when list is not big enough."
  (take n (concat steps (repeat nil))))

(defn- each3-steps [steps]
  "Transforms a list in a list of triples following the form:
   [a b c] => [[a b c] [b c nil] [c nil nil]]."
  (let [n (count steps)]
    (map vector (ensure-items n steps)
         (ensure-items n (rest steps))
         (ensure-items n (rest (rest steps))))))

(def ^:private prepare-monadic-steps
  #(->> % (partition 2) reverse each3-steps))

(defmacro do
  "Monad comprehension. Takes the name of a monad (like vector, hash-set),
   a vector of steps given as binding-form/monadic-expression pairs, and
   a result value specified by expr. The monadic-expression terms can use
   the binding variables of the previous steps.
   If the monad contains a definition of m-zero, the step list can also
   contain conditions of the form :when p, where the predicate p can
   contain the binding variables from all previous steps.
   A clause of the form :let [binding-form expr ...], where the bindings
   are given as a vector as for the use in let, establishes additional
   bindings that can be used in the following steps."
  [result bindings expr]
  (let [steps (prepare-monadic-steps bindings)]
    `(monads.core/bind (~result nil)
                       (fn [_#]
                         ~(reduce (fn [expr [[sym mv] _ _]]
                                    (cond
                                     (= :when sym) `(if ~mv
                                                      ~expr
                                                      (monads.core/zero (~result nil)))
                                     (= :let sym) `(let ~mv
                                                     ~expr)
                                     :else `(monads.core/bind ~mv (fn [~sym]
                                                                    ~expr))))
                                  `(monads.core/do-result (~result nil) ~expr)
                                  steps)))))
