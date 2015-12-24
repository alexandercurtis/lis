(ns lis.core
  (:gen-class))

(defn- bsearch
  [L X M i]
  (loop [lo 1
         hi L]
    (if (<= lo hi)
      (let [mid (int (/ (+ lo hi) 2.0))]
        (if (< (int (nth X (nth M mid))) (int (nth X i)))
          (recur (inc mid) hi)
          (recur lo (dec mid))))
      lo)))

(defn- lis'
  [X]
  (let [N (count X)]
    (loop [P (transient (into [] (repeat N nil)))
           M (transient (into [] (repeat (inc N) nil)))
           L 0
           i 0]
      (if (< i N)
        (let [newL (bsearch L X M i)]
          (recur
            (assoc! P i (nth M (dec newL)))
            (assoc! M newL i)
            (if (> newL L) newL L)
            (inc i)))
        [(nth M L) P L]))))

(defn- reconstruct
  [X k P L]
  (loop [S (list)
         k k
         i (dec L)]
    (if (>= i 0)
      (recur (cons (nth X k) S)
             (nth P k)
             (dec i))
      S)))

(defn lis [X]
  (let [[k P L] (lis' X)]
    (reconstruct X k P L)))


(defn -main
  "Longest Increasing Subsequence"
  [& args]
  (let [x [\P, \N, \O, \Q, \R, \B, \D, \C, \F, \E, \G, \M, \S, \A, \T]]
    (println "x=" x)
    (println "lis=" (lis x))))


