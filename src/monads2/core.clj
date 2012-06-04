(ns monads2.core
  (:refer-clojure :exclude [do seq])
  (:require [clojure.set :as set]))

(defprotocol Monad
  (do-result [_ v])
  (bind [mv f]))

(defprotocol MonadZero
  (zero [_])
  (plus-step [mv mvs]))

(defn plus [[mv & mvs]]
  (plus-step mv mvs))

(defmacro do [bindings expr]
  (let [steps (reverse (partition 2 bindings))
        example (->> steps
                  (remove (comp keyword? first))
                  first
                  second)]
    (reduce (fn [expr [sym mv]]
              (cond
                (= :when sym) `(if ~mv
                                 ~expr
                                 (monads2.core/zero ~example))
                (= :let sym) `(let ~mv
                                ~expr)
                :else `(monads2.core/bind ~mv (fn [~sym]
                                                ~expr))))
            `(monads2.core/do-result ~example ~expr) 
            steps)))

(defmacro seq [mvs]
  (let [steps (map (fn [x]
                     (let [sym (gensym)]
                       [sym x]))
                   mvs)
        syms (map first steps)]
    `(monads2.core/do [~@(apply concat steps)] [~@syms])))


(extend-type clojure.lang.PersistentList
  Monad
  (do-result [_ v]
          (list v))
  (bind [mv f]
        (mapcat f mv))
  
  MonadZero
  (zero [_]
        (list))
  (plus-step [mv mvs]
             (apply concat mv mvs)))

(extend-type clojure.lang.PersistentList$EmptyList
  Monad
  (do-result [_ v]
          (list v))
  (bind [mv f]
        (mapcat f mv))
  
  MonadZero
  (zero [_]
        (list))
  (plus-step [mv mvs]
             (apply concat mv mvs)))

(extend-type clojure.lang.PersistentVector
  Monad
  (do-result [_ v]
          [v])
  (bind [mv f]
        (vec (mapcat f mv)))
  
  MonadZero
  (zero [_]
        [])
  (plus-step [mv mvs]
             (vec (apply concat mv mvs))))

(extend-type clojure.lang.LazySeq
  Monad
  (do-result [_ v]
          (list v))
  (bind [mv f]
        (mapcat f mv))
  
  MonadZero
  (zero [_]
        [])
  (plus-step [mv mvs]
             ; TODO: make lazy
             (apply concat mv mvs)))

(extend-type clojure.lang.PersistentHashSet
  Monad
  (do-result [_ v]
          (hash-set v))
  (bind [mv f]
        (apply set/union
               (map f mv)))
  
  MonadZero
  (zero [_]
        #{})
  (plus-step [mv mvs]
             (apply set/union mv mvs)))


(declare state)

(deftype state-binder [mv f]
  clojure.lang.IFn
  (invoke [_ s]
          (let [[v ss] (mv s)]
            ((f v) ss)))
  
  Monad
  (do-result [_ v]
          (state v))
  (bind [mv f]
        (state-binder. mv f)))

(deftype state-monad [v]
  clojure.lang.IFn
  (invoke [_ s]
          [v s])
  
  Monad
  (do-result [_ v]
          (state v))
  (bind [mv f]
        (state-binder. mv f)))

(defn state [v]
  (state-monad. v))

(defn update-state [f]
  (reify
    clojure.lang.IFn
    (invoke [_ s]
            [s (f s)])

    Monad
    (do-result [_ v]
               (state v))
    (bind [mv f]
          (state-binder. mv f))))

(defn set-state [s]
  (update-state (constantly s)))

(defn get-state []
  (update-state identity))

(defn get-val [key]
  (monads2.core/do
    [s (get-state)]
    (get s key)))

(defn update-val [key f & args]
  (monads2.core/do
    [s (update-state #(apply update-in % [key] f args))]
    (get s key)))

(defn set-val [key val]
  (update-val key (constantly val)))


(declare cont)

(deftype cont-binder [mv f]
  clojure.lang.IFn
  (invoke [_ c]
          (mv (fn [v] ((f v) c))))
  
  Monad
  (do-result [_ v]
          (cont v))
  (bind [mv f]
        (cont-binder. mv f)))

(deftype cont-monad [v]
  clojure.lang.IFn
  (invoke [_ c]
          (c v))
  
  Monad
  (do-result [_ v]
          (cont-monad. v))
  (bind [mv f]
        (cont-binder. mv f)))

(defn cont [v]
  (cont-monad. v))

