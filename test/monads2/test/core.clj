(ns monads2.test.core
  (:use [clojure.test])
  (:require [monads2.core :as m])
  (:import [monads2.core writer-transformer state-transformer]))

(defn list-f [n]
  (list (inc n)))

(defn list-g [n]
  (list (+ n 5)))

(deftest first-law-list
  (is (= (m/bind (list 10) list-f)
         (list-f 10))))

(deftest second-law-list
  (is (= (m/bind '(10) list)
         '(10))))

(deftest third-law-list
  (is (= (m/bind (m/bind [4 9] list-f) list-g)
         (m/bind [4 9] (fn [x]
                         (m/bind (list-f x) list-g))))))

(deftest zero-law-list
  (is (= (m/bind '() list-f)
         '()))
  (is (= (m/bind '(4) (constantly '()))
         '()))
  (is (= (m/plus [(list 5 6) '()])
         (list 5 6)))
  (is (= (m/plus ['() (list 5 6)])
         (list 5 6))))


(defn vector-f [n]
  (vector (inc n)))

(defn vector-g [n]
  (vector (+ n 5)))

(deftest first-law-vector
  (is (= (m/bind [10] vector-f)
         (vector-f 10))))

(deftest second-law-vector
  (is (= (m/bind [10] vector)
         [10])))

(deftest third-law-vector
  (is (= (m/bind (m/bind [4 9] vector-f) vector-g)
         (m/bind [4 9] (fn [x]
                         (m/bind (vector-f x) vector-g))))))

(deftest zero-law-vector
  (is (= (m/bind [] vector-f)
         []))
  (is (= (m/bind '(4) (constantly []))
         []))
  (is (= (m/plus [(vector 5 6) []])
         (vector 5 6)))
  (is (= (m/plus [[] (vector 5 6)])
         (vector 5 6))))


(defn set-f [n]
  (hash-set (inc n)))

(defn set-g [n]
  (hash-set (+ n 5)))

(deftest first-law-set
  (is (= (m/bind #{10} set-f)
         (set-f 10))))

(deftest second-law-set
  (is (= (m/bind #{10} hash-set)
         #{10})))

(deftest third-law-set
  (is (= (m/bind (m/bind #{4 9} set-f) set-g)
         (m/bind #{4 9} (fn [x]
                          (m/bind (set-f x) set-g))))))

(deftest zero-law-set
  (is (= (m/bind #{} set-f)
         #{}))
  (is (= (m/bind #{4} (constantly #{}))
         #{}))
  (is (= (m/plus [(hash-set 5 6) #{}])
         (hash-set 5 6)))
  (is (= (m/plus [#{} (hash-set 5 6)])
         (hash-set 5 6))))


(def test-writer (m/writer #{}))

(defn writer-f [n]
  (test-writer (inc n)))

(defn writer-g [n]
  (test-writer (+ n 5)))

(deftest first-law-writer
  (is (= (deref (m/bind (test-writer 10) writer-f))
         (deref (writer-f 10)))))

(deftest second-law-writer
  (is (= (deref (m/bind (test-writer 10) test-writer))
         (deref (test-writer 10)))))

(deftest third-law-writer
  (is (= (deref (m/bind (m/bind (test-writer 3) writer-f) writer-g))
         (deref (m/bind (test-writer 3)
                        (fn [x]
                          (m/bind (writer-f x) writer-g)))))))

(deftest test-write
  (is (= [nil #{:written}]
         (deref (m/write test-writer :written)))))

(deftest test-listen
  (is (= [[nil #{:written}] #{:written}]
         @(m/listen (m/write test-writer :written)))))

(deftest test-censor
  (is (= [nil #{:new-written}]
         @(m/censor (constantly #{:new-written})
                    (m/write test-writer :written)))))


(defn state-f [n]
  (m/state (inc n)))

(defn state-g [n]
  (m/state (+ n 5)))

(deftest first-law-state
  (let [mv1 (m/bind (m/state 10) state-f)
        mv2 (state-f 10)]
    (is (= (mv1 {}) (mv2 {})))))

(deftest second-law-state
  (let [mv1 (m/bind (m/state 10) m/state)
        mv2 (m/state 10)]
    (is (= (mv1 :state) (mv2 :state)))))

(deftest third-law-state
  (let [mv1 (m/bind (m/bind (m/state 4) state-f) state-g)
        mv2 (m/bind (m/state 4)
                    (fn [x]
                      (m/bind (state-f x) state-g)))]
    (is (= (mv1 :state) (mv2 :state)))))

(deftest test-update-state
  (is (= [:state :new-state]
         ((m/update-state (constantly :new-state)) :state))))

(deftest test-get-val
  (is (= [17 {:a 17}]
         ((m/get-val :a) {:a 17}))))

(deftest test-set-val
  (is (= [17 {:a 12}]
         ((m/set-val :a 12) {:a 17}))))

(deftest test-update-val
  (is (= [5 {:a 19}]
         ((m/update-val :a + 14) {:a 5}))))

(deftest test-get-in-val
  (let [state {:a {:b 1} :c {:d {:e 2}}}]
    (are [expected args] (is (= expected ((apply m/get-in-val args) state)))
         [1 state]      [[:a :b]]
         [:def state]   [[:z] :def]
         [nil state]    [[:a :b :c]]
         [2 state]      [[:c :d :e]]
         [{:b 1} state] [[:a]])))

(deftest test-assoc-in-val
  (is (= [nil {:a {:b {:c 9}}}]
         ((m/assoc-in-val [:a :b :c] 9) {}))))

(deftest test-update-in-val
  (are [expected in-state path args] (is (= expected
                                            ((apply m/update-in-val path args) in-state)))
       [2 {:a {:b 4}}]      {:a {:b 2}}  [:a :b]  [* 2]
       [2 {:a {:b 3}}]      {:a {:b 2}}  [:a :b]  [inc]
       [nil {:a {:b [1]}}]  {:a nil}     [:a :b]  [(fnil conj []) 1]))

(defn cont-f [n]
  (m/cont (inc n)))

(defn cont-g [n]
  (m/cont (+ n 5)))

(deftest first-law-cont
  (let [mv1 (m/bind (m/cont 10) cont-f)
        mv2 (cont-f 10)]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest second-law-cont
  (let [mv1 (m/bind (m/cont 10) m/cont)
        mv2 (m/cont 10)]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest third-law-cont
  (let [mv1 (m/bind (m/bind (m/cont 4) cont-f) cont-g)
        mv2 (m/bind (m/cont 4)
                    (fn [x]
                      (m/bind (cont-f x) cont-g)))]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest deref-cont
  (is (= 10 @(m/cont 10))))


(def vect-state (m/state-t vector))
(defn state-t-f [n]
  (vect-state (inc n)))

(defn state-t-g [n]
  (vect-state (+ n 5)))

(deftest first-law-state-t
  (let [mv1 (m/bind (vect-state 10) state-t-f)
        mv2 (state-t-f 10)]
    (is (= (mv1 {}) (mv2 {})))))

(deftest second-law-state-t
  (let [mv1 (m/bind (vect-state 10) vect-state)
        mv2 (vect-state 10)]
    (is (= (mv1 :state-t) (mv2 :state-t)))))

(deftest third-law-state-t
  (let [mv1 (m/bind (m/bind (vect-state 4) state-t-f) state-t-g)
        mv2 (m/bind (vect-state 4)
                    (fn [x]
                      (m/bind (state-t-f x) state-t-g)))]
    (is (= (mv1 :state-t) (mv2 :state-t)))))

(deftest zero-law-state-t
  (is (= [] ((m/zero (vect-state nil)) :state)))
  (is (= ((m/bind (m/zero (vect-state nil)) state-t-f) :state)
         []))
  (is (= ((m/bind (vect-state 4) (constantly (m/zero (vect-state nil)))) :state)
         []))
  (is (= ((m/plus [(vect-state 5) (m/zero (vect-state nil))]) :state)
         ((vect-state 5) :state)))
  (is (= ((m/plus [(m/zero (vect-state nil)) (vect-state 4)]) :state)
         ((vect-state 4) :state))))

(deftest do-state-t
  (is (= []
         ((m/do vect-state
                [:when false]
                :something)
          :state)))

  (is (= [[[10 15] :state]]
         ((m/do vect-state
           [x (state-t-f 9)
            y (state-t-g x)]
           [x y])
          :state))))

;; TODO: these two tests demonstrate a shorcoming of the maybe monad
;; that needs to be fixed
#_(deftest maybe-plus
  (is (= :bogus
         @(m/plus [(m/maybe 10)
                   (m/do m/maybe
                         [_ (m/maybe 1)]
                         (throw (Exception. "Should not be thrown")))]))))

#_(deftest test-state-maybe
  (let [test-m (m/state-t m/maybe)]
    (is (= [:bogus :state]
           @((m/plus [(test-m :bogus)
                      (m/do test-m
                             [_ (test-m nil)]
                             (throw (Exception. "Should not be thrown")))])
             :state)))))


(defn maybe-f [n]
  (m/maybe (inc n)))

(defn maybe-g [n]
  (m/maybe (+ n 5)))

(deftest first-law-maybe
  (is (= @(m/bind (m/maybe 10) maybe-f)
         @(maybe-f 10))))

(deftest second-law-maybe
  (is (= @(m/bind (m/maybe 10) m/maybe)
         10)))

(deftest third-law-maybe
  (is (= @(m/bind (m/bind (m/maybe 5) maybe-f) maybe-g)
         @(m/bind (m/maybe 5) (fn [x]
                                (m/bind (maybe-f x) maybe-g))))))

(deftest zero-law-maybe
  (is (= (m/bind (m/zero (m/maybe nil)) maybe-f)
         (m/zero (m/maybe nil))))
  (is (= (m/bind (m/maybe 4) (constantly (m/zero (m/maybe nil))))
         (m/zero (m/maybe nil))))
  (is (= @(m/plus [(m/maybe 6) (m/zero (m/maybe nil))])
         @(m/maybe 6)))
  (is (= @(m/plus [(m/zero (m/maybe nil)) (m/maybe 6)])
         @(m/maybe 6))))


(deftest test-seq
  (is (= [[3 :a] [3 :b] [5 :a] [5 :b]]
         (m/seq [[3 5] [:a :b]])))
  (is (= [[]]
         (m/seq vector []))))

(deftest test-lift
  (let [lifted-+ (m/lift +)]
    (is (= [6]
           (apply lifted-+ (map vector (range 4)))))
    (is (= [6 :state]
           ((apply lifted-+ (map m/state (range 4))) :state)))))

(deftest test-chain
  (let [t (fn [x] (vector (inc x) (* 2 x)))
        u (fn [x] (vector (dec x)))
        st (fn [x] (m/state (inc x)))
        su (fn [x] (m/state (* 2 x)))]
    (is (= (map (fn [x] (m/do vector [y (t x) z (u y)] z)) (range 4))
           (map (m/chain [t u]) (range 4))))
    (is (= (m/do vector [x (range 4) y (t x) z (u y)] z)
           ((m/chain [range t u]) 4)))
    (is (= ((m/do m/state [x (st 8) y (su x)] y) :state)
           (((m/chain [st su]) 8) :state)))))


(def vect-maybe (m/maybe-t vector))
(defn maybe-t-f [n]
  (vect-maybe (inc n)))

(defn maybe-t-g [n]
  (vect-maybe (+ n 5)))

(deftest first-law-maybe-t
  (is (= @(first @(m/bind (vect-maybe 10) maybe-t-f))
           @(first @(maybe-t-f 10)))))

(deftest second-law-maybe-t
  (is (= @(first @(m/bind (vect-maybe 10) vect-maybe))
           @(first @(vect-maybe 10)))))

(deftest third-law-maybe-t
  (is (= @(first @(m/bind (m/bind (vect-maybe 4) maybe-t-f) maybe-t-g))
           @(first @(m/bind (vect-maybe 4)
                            (fn [x]
                              (m/bind (maybe-t-f x) maybe-t-g)))))))

(deftest zero-law-maybe-t
  (is (= m/maybe-zero-val (first @ (m/zero (vect-maybe nil)))))
  (is (= (first @(m/bind (m/zero (vect-maybe nil)) maybe-t-f))
         (first @(m/zero (vect-maybe nil)))))
  (is (= (first @(m/bind (vect-maybe 4) (constantly (m/zero (vect-maybe nil)))))
         (first @(m/zero (vect-maybe nil)))))
  (is (= @(first @(m/plus [(vect-maybe 4) (m/zero (vect-maybe nil))]))
         @(first @(vect-maybe 4))))
  (is (= @(first @(m/plus [(m/zero (vect-maybe nil)) (vect-maybe 4)]))
         @(first @(vect-maybe 4)))))

(deftest do-maybe-t
  (is (= [10 15]
         @(first @(m/do vect-maybe
                        [x (maybe-t-f 9)
                         y (maybe-t-g x)]
                        [x y])))))


(def set-list (m/list-t hash-set))
(defn list-t-f [n]
  (set-list (inc n)))

(defn list-t-g [n]
  (set-list (+ n 5)))

(deftest first-law-list-t
  (is (= @(m/bind (set-list 10) list-t-f)
         @(list-t-f 10))))

(deftest second-law-list-t
  (is (= @(m/bind (set-list 10) set-list)
         @(set-list 10))))

(deftest third-law-list-t
  (is (= @(m/bind (m/bind (set-list 4) list-t-f) list-t-g)
         @(m/bind (set-list 4)
                  (fn [x]
                    (m/bind (list-t-f x) list-t-g))))))

(deftest zero-law-list-t
  (is (= #{'()} @(m/zero (set-list nil))))
  (is (= @(m/bind (m/zero (set-list nil)) list-t-f)
         @(m/zero (set-list nil))))
  (is (= @(m/bind (set-list 4) (constantly (m/zero (set-list nil))))
         @(m/zero (set-list nil))))
  (is (= @(m/plus [(set-list 4) (m/zero (set-list nil))])
         @(set-list 4)))
  (is (= @(m/plus [(m/zero (set-list nil)) (set-list 4)])
         @(set-list 4))))

(deftest do-list-t
  (is (= #{(list [10 15])}
         @(m/do set-list
                [x (list-t-f 9)
                 y (list-t-g x)]
                [x y]))))


(def set-vect (m/vector-t hash-set))
(defn vector-t-f [n]
  (set-vect (inc n)))

(defn vector-t-g [n]
  (set-vect (+ n 5)))

(deftest first-law-vector-t
  (is (= @(m/bind (set-vect 10) vector-t-f)
         @(vector-t-f 10))))

(deftest second-law-vector-t
  (is (= @(m/bind (set-vect 10) set-vect)
         @(set-vect 10))))

(deftest third-law-vector-t
  (is (= @(m/bind (m/bind (set-vect 4) vector-t-f) vector-t-g)
         @(m/bind (set-vect 4)
                  (fn [x]
                    (m/bind (vector-t-f x) vector-t-g))))))

(deftest zero-law-vector-t
  (is (= #{[]} @(m/zero (set-vect nil))))
  (is (= @(m/bind (m/zero (set-vect nil)) vector-t-f)
         @(m/zero (set-vect nil))))
  (is (= @(m/bind (set-vect 4) (constantly (m/zero (set-vect nil))))
         @(m/zero (set-vect nil))))
  (is (= @(m/plus [(set-vect 4) (m/zero (set-vect nil))])
         @(set-vect 4)))
  (is (= @(m/plus [(m/zero (set-vect nil)) (set-vect 4)])
         @(set-vect 4))))

(deftest do-vector-t
  (is (= #{(vector [10 15])}
         @(m/do set-vect
                [x (vector-t-f 9)
                 y (vector-t-g x)]
                [x y]))))


(def vect-set (m/set-t vector))
(defn set-t-f [n]
  (vect-set (inc n)))

(defn set-t-g [n]
  (vect-set (+ n 5)))

(deftest first-law-set-t
  (is (= @(m/bind (vect-set 10) set-t-f)
         @(set-t-f 10))))

(deftest second-law-set-t
  (is (= @(m/bind (vect-set 10) vect-set)
         @(vect-set 10))))

(deftest third-law-set-t
  (is (= @(m/bind (m/bind (vect-set 4) set-t-f) set-t-g)
         @(m/bind (vect-set 4)
                  (fn [x]
                    (m/bind (set-t-f x) set-t-g))))))

(deftest zero-law-set-t
  (is (= [#{}] @(m/zero (vect-set nil))))
  (is (= @(m/bind (m/zero (vect-set nil)) set-t-f)
         @(m/zero (vect-set nil))))
  (is (= @(m/bind (vect-set 4) (constantly (m/zero (vect-set nil))))
         @(m/zero (vect-set nil))))
  (is (= @(m/plus [(vect-set 4) (m/zero (vect-set nil))])
         @(vect-set 4)))
  (is (= @(m/plus [(m/zero (vect-set nil)) (vect-set 4)])
         @(vect-set 4))))

(deftest do-set-t
  (is (= [(hash-set [10 15])]
         @(m/do vect-set
                [x (set-t-f 9)
                 y (set-t-g x)]
                [x y]))))


(def vect-writer (m/writer-t hash-set []))
(defn writer-t-f [n]
  (vect-writer (inc n)))

(defn writer-t-g [n]
  (vect-writer (+ n 5)))

(deftest first-law-writer-t
  (is (= @(first @(m/bind (vect-writer 10) writer-t-f))
         @(first @(writer-t-f 10)))))

(deftest second-law-writer-t
  (is (= @(first @(m/bind (vect-writer 10) vect-writer))
         @(first @(vect-writer 10)))))

(deftest third-law-writer-t
  (is (= @(first @(m/bind (m/bind (vect-writer 4) writer-t-f) writer-t-g))
         @(first @(m/bind (vect-writer 4)
                          (fn [x]
                            (m/bind (writer-t-f x) writer-t-g)))))))

(deftest zero-law-writer-t
  (is (= #{} @(m/zero (vect-writer nil))))
  (is (= @(m/bind (m/zero (vect-writer nil)) writer-t-f)
         @(m/zero (vect-writer nil))))
  (is (= @(m/bind (vect-writer 4) (constantly (m/zero (vect-writer nil))))
         @(m/zero (vect-writer nil))))
  (is (= @(first @(m/plus [(vect-writer 4) (m/zero (vect-writer nil))]))
         @(first @(vect-writer 4))))
  (is (= @(first @(m/plus [(m/zero (vect-writer nil)) (vect-writer 4)]))
         @(first @(vect-writer 4)))))

(deftest do-writer-t
  (is (= @(first @(vect-writer [10 15]))
         @(first @(m/do vect-writer
                        [x (writer-t-f 9)
                         y (writer-t-g x)]
                        [x y])))))

(def parse-m (m/state-t m/maybe))

(deftest test-do
  (is (= [19 {:val 19}]
         @((m/do parse-m
                 [_ (m/set-val :val 19)]
                 19)
           {})))

  (let [tinc #(vector (inc %))]
    (is ( = [[1 2 3] [3 4 5]]
            (m/do list
                  [a (range 5)
                   :when (odd? a)
                   x (tinc a)
                   y (tinc x)]
                  [a x y])))))

(deftest test-hash-set-writer
  (let [test-m (m/writer-t hash-set [])
        writer-m (m/writer [])
        write-msg (fn [msg]
                    (writer-transformer. hash-set
                                         (hash-set ((m/writer [msg]) nil))
                                         writer-m))
        listen-msgs (fn [mv]
                      (writer-transformer. hash-set
                                           (->> @mv
                                                (map #(m/listen %))
                                                set)
                                           writer-m))
        censor-msgs (fn [f mv]
                      (writer-transformer. hash-set
                                           (->> @mv
                                                (map #(m/censor f %))
                                                set)
                                           writer-m))]

    (is (= [nil [:msg1]] @(first @(write-msg :msg1))))

    (is (= [[nil [:msg3]] [:msg3]] @(first @(listen-msgs (write-msg :msg3)))))

    (is (= [[nil [nil [:msg3]] nil] [:msg1 :msg3 :msg2 :msg4]]
           (->> (m/seq [(write-msg :msg1)
                        (listen-msgs (write-msg :msg3))
                        (write-msg :msg2)])
                (censor-msgs #(conj % :msg4))
                deref
                first
                deref)))

    (is (= #{[5 [:msg3]] [nil [:msg1 :msg3]]}
           (->> (m/plus [(write-msg :msg1)
                         (m/zero (test-m nil))
                         (m/zero (write-msg :msg2))
                         (test-m 5)])
                (censor-msgs #(conj % :msg3))
                deref
                (map deref)
                set)))))

(deftest test-state-writer-maybe
  (let [test-m (m/state-t (m/writer-t m/maybe []))
        writer-m (m/writer-t m/maybe [])
        write-msg (fn [msg]
                    (state-transformer. writer-m
                                        nil
                                        ((m/state-t (m/writer-t m/maybe [msg])) nil)
                                        (constantly (test-m nil))
                                        nil))
        listen-msgs (fn [mv]
                      (state-transformer. writer-m
                                        nil
                                        (fn [s]
                                          (let [[[_ s] msgs] @@@(mv s)]
                                            ((m/writer-t m/maybe msgs) [msgs s])))
                                        (fn [v]
                                          (test-m v))
                                        nil))
        censor-msgs (fn [f mv]
                      (state-transformer. writer-m
                                        nil
                                        (fn [s]
                                          (let [[[v s] msgs] @@@(mv s)]
                                            ((m/writer-t m/maybe (f msgs)) [[v msgs] s])))
                                        (fn [v]
                                          (test-m v))
                                        nil))]
    (is (= [[nil :state] [:msg]]
           @@@((write-msg :msg) :state)))

    (is (= [[:result :state] [:msg]]
           @@@((m/bind (write-msg :msg)
                       (fn [x]
                         (is (nil? x))
                         (test-m :result)))
               :state)))
    (is (= [[[nil nil] :state] [:msg1 :msg2]]
           @@@((m/seq [(write-msg :msg1)
                       (write-msg :msg2)])
               :state)))
    (is (= [[nil :state] [:msg1]]
           @@@((m/plus [(write-msg :msg1)
                        (write-msg :msg2)])
               :state)))
    (is (= [[nil :state] [:msg2]]
             @@@((m/plus [(m/zero (test-m nil))
                          (write-msg :msg2)])
                 :state)))

    (is (= [[[:msg3] :state] [:msg3]]
           @@@((listen-msgs (write-msg :msg3)) :state)))

    (is (= [[[[nil [:msg3] nil] [:msg1 :msg3 :msg2]] :state] [:msg1 :msg3 :msg2 :msg4]]
             @@@((->> (m/seq [(write-msg :msg1)
                               (listen-msgs (write-msg :msg3))
                               (write-msg :msg2)])
                      (censor-msgs #(conj % :msg4)))
                 :state)))

    (is (= [[[nil [:msg1]] :state] [:msg1 :msg3]]
           @@@((->> (m/plus [(m/zero (test-m nil))
                             (m/zero (write-msg :msg2))
                             (write-msg :msg1)])
                    (censor-msgs #(conj % :msg3)))
               :state)))))
