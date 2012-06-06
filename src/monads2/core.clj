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

(defn seq [mvs]
  (let [rest-steps (reduce (fn [steps mv]
                             (fn [acc x]
                               (bind mv (partial steps (conj acc x)))))
                           (fn [acc x]
                             (do-result (first mvs) (conj acc x)))
                           (reverse (rest mvs)))]
    (bind (first mvs) (partial rest-steps []))))


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

(defprotocol maybe-proto
  (maybe-value [_]))

(deftype maybe-monad [v mv f alts]
  maybe-proto
  (maybe-value [_]
               (prn :maybe v mv f alts)
         (cond
           f (let [new-v (maybe-value mv)]
               (prn :new-v new-v :f f (f new-v))
               (if (= ::nothing new-v)
                 ::nothing
                 (f new-v)))
           alts (->> alts
                  (map maybe-value)
                  (drop-while #(= ::nothing))
                  first)
           :else v))
  
  Monad
  (do-result [_ v]
          (maybe-monad. v nil nil nil))
  (bind [mv f]
        (prn :binding mv f)
        (maybe-monad. nil mv f nil)) 
  
  MonadZero
  (zero [_]
        (maybe-monad. ::nothing nil nil nil)) 
  (plus-step [mv mvs]
             (maybe-monad. nil nil nil (cons mv mvs))))

(defn maybe [v]
  (prn :new-maybe v)
  (maybe-monad. v nil nil nil))


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


(deftype state-transformer [m v mv f alts]
  clojure.lang.IFn
  (invoke [_ s]
          
          (cond
            alts (plus (map #(% s) alts))
            f (bind (mv s)
                    (fn [[v ss]]
                      ((f v) ss)))
            :else (m [v s])))

  Monad
  (do-result [_ v]
          (state-transformer. m v nil nil nil))
  (bind [mv f]
        (state-transformer. m nil mv f nil)) 
  
  MonadZero
  (zero [_]
        (zero (m nil))) 
  (plus-step [mv mvs]
             (state-transformer. m nil nil nil (cons mv mvs)))) 

(defn state-t [m]
  (fn [v]
    (state-transformer. m v nil nil nil)))

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


(extend-type java.lang.String
  writer-monad-protocol
  (writer-m-empty [_] "")
  (writer-m-add [c v] (str c v))
  (writer-m-combine [c1 c2] (str c1 c2)))

(deftype writer-monad [v accumulator mv f]
  clojure.lang.IDeref
  (deref [_]
         (if f
           (let [[v1 a1] (deref mv) 
                 [v2 a2] (deref (f v1))]
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
  (let [[_ a] (deref (m-result nil))]
    (writer-monad. nil (writer-m-add a val-to-write)
                   nil nil)))

(defn listen [mv]
  (let [[v a :as va] (deref mv)]
    (writer-monad. va a nil nil)))

(defn censor [f mv]
  (let [[v a] (deref mv)]
    (writer-monad. v (f a) nil nil)))
