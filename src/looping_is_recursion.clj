(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                    acc
                    (recur (* acc base) (dec n))
                   ))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         loop-seq a-seq]
    (if (empty? loop-seq)
      nil
      (if (pred (first loop-seq))
        index
        (recur (inc index) (rest loop-seq))))))

(defn avg [a-seq]
  (loop [n 0
         acc 0
         loop-seq a-seq]
    (if (empty? loop-seq)
      (/ acc n)
      (recur (inc n) (+ acc (first loop-seq)) (rest loop-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (loop [return-set #{}
         loop-seq a-seq]
    (if (empty? loop-seq)
      return-set
      (recur (toggle return-set (first loop-seq)) (rest loop-seq)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (< n 3) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn fast-fibo [n]
  (cond
   (= n 0) 0
   (< n 3) 1
   :else (loop [loop-n 3
                f-n 1
                f-n-1 1]
           (if (< loop-n n)
             (recur (inc loop-n) (+ f-n f-n-1) f-n)
             (+ f-n f-n-1)))))


(defn cut-at-repetition [a-seq]
  (loop [retval []
         loop-seq a-seq]
    (if (empty? loop-seq)
      retval
      (let [next-retval (conj retval (first loop-seq))]
        (if (not (= (count next-retval) (count (set next-retval))))
          retval
          (recur next-retval (rest loop-seq)))))))

