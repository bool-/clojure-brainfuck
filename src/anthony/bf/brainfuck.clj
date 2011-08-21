(ns anthony.bf.brainfuck)

(defn go-back [pointer commands]
  (loop [bkt-count 1]
    (when-not (== bkt-count 0)
      (reset! pointer (dec @pointer))
      (condp = (nth commands @pointer)
        \[ (recur (dec bkt-count))
        \] (recur (inc bkt-count))
        (recur bkt-count)))))

(defn go-forward [pointer commands]
  (loop [bkt-count 1]
    (when-not (== bkt-count 0)
      (reset! pointer (inc @pointer))
      (condp = (nth commands @pointer)
        \] (recur (dec bkt-count))
        \[ (recur (inc bkt-count))
        (recur bkt-count)))))

(defn do-command [cell cells pointer commands]
  (condp = (nth commands @pointer)
    \> (reset! cell (inc @cell))
    \< (reset! cell (dec @cell))
    \+ (reset! cells (assoc @cells @cell (inc (get @cells @cell))))
    \- (reset! cells (assoc @cells @cell (dec (get @cells @cell))))
    \. (print (char (get @cells @cell)))
    \, (reset! cells (assoc @cells @cell (char (. System/in read))))
    \[ (if (== (get @cells @cell) 0)
         (go-forward pointer commands))
    \] (if-not (== (get @cells @cell) 0)
         (go-back pointer commands))
    0))


(defn interpret [& commands]
  (let [cell (atom 0)
        cells (atom [0 0 0 0 0 0 0 0 0 0 0 0 0])] ; need to make this dynamic
    (loop [pointer (atom 0)]
      (do-command cell cells pointer commands)
      (reset! pointer (inc @pointer))
      (if-not (= @pointer (count commands)) (recur pointer)))))

(apply interpret "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")

