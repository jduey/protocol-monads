(ns monads2.core
  (:refer-clojure :exclude [do seq map])
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

(defn- comprehend [f mvs]
  (let [rest-steps (reduce (fn [steps mv]
                             (fn [acc x]
                               (bind mv (partial steps (conj acc x)))))
                           (fn [acc x]
                             (do-result (first mvs) (f (conj acc x))))
                           (reverse (rest mvs)))]
    (bind (first mvs) (partial rest-steps []))))

(defn seq
  ([mvs] (seq (first mvs) mvs))
  ([m-result mvs]
     (if (clojure.core/seq mvs)
       (comprehend identity mvs)
       (m-result []))))

(defn lift [f]
  (fn [& mvs]
    (comprehend (partial apply f) mvs)))

(defn join [mv]
  (bind mv identity))

(defn fmap [f mv]
  (bind mv (fn [x] (do-result mv (f x)))))

(defn map [f xs]
  (seq (clojure.core/map f xs)))

(defn chain [steps]
  (fn [x]
    (let [mv ((first steps) x)
          chain (reduce (fn [chain step]
                          (fn [x]
                            (bind (step x) chain)))
                        (partial do-result mv)
                        (reverse (rest steps)))]
      (bind mv chain))))

; for the writer monad
(defprotocol writer-monad-protocol
  "Accumulation of values into containers"
  (writer-m-empty [_]
  "return an empty container")
  (writer-m-add [container value]
  "add value to container, return new container")
  (writer-m-combine [container1 container2]
  "combine two containers, return new container"))

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

(defn- lazy-concat [l ls]
  (lazy-seq
   (cond
    (clojure.core/seq l) (cons (first l)
                               (lazy-concat (rest l) ls))
    (clojure.core/seq ls) (lazy-concat (first l) (rest ls))
    :else (list))))

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
             (lazy-concat mv mvs))

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
               (clojure.core/map f mv)))

  MonadZero
  (zero [_]
        #{})
  (plus-step [mv mvs]
             (apply set/union mv mvs))

  writer-monad-protocol
  (writer-m-empty [_] #{})
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (clojure.set/union c1 c2)))


(declare maybe-zero-val)

(deftype maybe-monad [v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (maybe-monad. v))
  (bind [mv f]
    (if (= mv maybe-zero-val)
      maybe-zero-val
      (f @mv)))

  MonadZero
  (zero [_]
    maybe-zero-val)
  (plus-step [mv mvs]
    (let [mv (->> (cons mv mvs)
                  (drop-while #(= maybe-zero-val %))
                  first)]
      (if (nil? mv)
        maybe-zero-val
        mv))))

(def maybe-zero-val (maybe-monad. ::nothing))

(defn maybe [v]
  (maybe-monad. v))


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
  clojure.lang.IDeref
  (deref [mv]
    (mv identity))

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

(defn call-cc [f]
  )


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


(deftype state-transformer [m v mv f alts]
  clojure.lang.IFn
  (invoke [_ s]
    (cond
     alts (plus (clojure.core/map #(% s) alts))
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


(deftype maybe-transformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
          (maybe-transformer. m v))
  (bind [mv f]
    (let [v (deref mv)]
      (maybe-transformer. m (bind v (fn [x]
                                      (if (= x maybe-zero-val)
                                        (m maybe-zero-val)
                                        (deref (f (deref x)))))))))

  MonadZero
  (zero [_]
        (maybe-transformer. m (m maybe-zero-val)))
  (plus-step [mv mvs]
    (maybe-transformer.
     m (bind (deref mv)
             (fn [x]
               (cond
                (and (= x maybe-zero-val) (empty? mvs)) (m maybe-zero-val)
                (= x maybe-zero-val) (deref (plus mvs))
                :else (m x)))))))

(defn maybe-t [m]
  (fn [v]
    (maybe-transformer. m (m (maybe v)))))
