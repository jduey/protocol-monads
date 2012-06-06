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

; for the writer monad
(defprotocol writer-monad-protocol
  "Accumulation of values into containers"
  (writer-m-empty [_]
  "return an empty container")
  (writer-m-add [container value]
  "add value to container, return new container")
  (writer-m-combine [container1 container2]
  "combine two containers, return new container"))

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
             (apply concat mv mvs))
  
  writer-monad-protocol
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

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
             (apply concat mv mvs))

  writer-monad-protocol 
  (writer-m-empty [_] (list)) 
  (writer-m-add [c v] (conj c v)) 
  (writer-m-combine [c1 c2] (concat c1 c2))) 

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
             (vec (apply concat mv mvs)))

  writer-monad-protocol
  (writer-m-empty [_] [])
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (vec (concat c1 c2))))

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
             (apply concat mv mvs))

  writer-monad-protocol 
  (writer-m-empty [_] (list)) 
  (writer-m-add [c v] (conj c v)) 
  (writer-m-combine [c1 c2] (concat c1 c2))) 

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
             (apply set/union mv mvs))

  writer-monad-protocol
  (writer-m-empty [_] #{})
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (clojure.set/union c1 c2)))


(deftype state-monad [v mv f]
  clojure.lang.IFn
  (invoke [_ s]
          (if f
            (let [[v ss] (mv s)]
              ((f v) ss)) 
            [v s]))
  
  Monad
  (do-result [_ v]
          (state-monad. v nil nil))
  (bind [mv f]
        (state-monad. nil mv f)))

(defn state [v]
  (state-monad. v nil nil))

(defn update-state [f]
  (reify
    clojure.lang.IFn
    (invoke [_ s]
            [s (f s)])

    Monad
    (do-result [_ v]
               (state-monad. v nil nil))
    (bind [mv f]
          (state-monad. nil mv f))))

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


(deftype cont-monad [v mv f]
  clojure.lang.IFn
  (invoke [_ c]
          (if f
            (mv (fn [v] ((f v) c))) 
            (c v)))
  
  Monad
  (do-result [_ v]
          (cont-monad. v nil nil))
  (bind [mv f]
        (cont-monad. nil mv f)))

(defn cont [v]
  (cont-monad. v nil nil))


(defprotocol writer-proto
  (writer-value [_]))

(extend-type java.lang.String
  writer-monad-protocol
  (writer-m-empty [_] "")
  (writer-m-add [c v] (str c v))
  (writer-m-combine [c1 c2] (str c1 c2)))

(deftype writer-monad [v accumulator mv f]
  writer-proto
  (writer-value [_]
                (if f
                  (let [[v1 a1] (writer-value mv) 
                        [v2 a2] (writer-value (f v1))]
                    [v2 (writer-m-combine a1 a2)])
                  [v accumulator]))

  Monad
  (do-result [_ v]
             (writer-monad. v (writer-m-empty accumulator)
                            nil nil))
  (bind [mv f]
        (writer-monad. nil nil mv f)))

(defn writer [empty-accumulator]
  (fn [v]
    (writer-monad. v empty-accumulator nil nil)))

(defn write [m-result val-to-write]
  (let [[_ a] (writer-value (m-result nil))]
    (writer-monad. nil (writer-m-add a val-to-write)
                   nil nil)))

(defn listen [mv]
  (let [[v a :as va] (writer-value mv)]
    (writer-monad. va a nil nil)))

(defn censor [f mv]
  (let [[v a] (writer-value mv)]
    (writer-monad. v (f a) nil nil)))
