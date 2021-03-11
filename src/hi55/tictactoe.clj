(ns hi55.tictactoe
  (:require [clojure.string :as s])
  (:gen-class))

(def empty-board [[:e :e :e]
                  [:e :e :e]
                  [:e :e :e]])

(defn rotate [b]
  (apply map vector (reverse b)))

(defn check-win-row [[a :as r]]
  (and (not= :e a)
       (apply = r)
       a))

(defn check-win-diagonal [b]
  (-> (map nth b (range 3))
      check-win-row))

(defn check-win [b]
  (or (some check-win-row b)
      (check-win-diagonal b)))

(defn analyse [b]
  (or (check-win b)
      (check-win (rotate b))
      (some #(and (= :e %) :ongoing) (apply concat b))
      :draw))

(defn next-moves [board mark]
  (loop [b (vec (apply concat board))
         i 0
         moves []]
    (if (= i 9)
      moves
      (recur b (inc i)
             (if (= :e (nth b i))
               (conj moves (mapv vec (partition 3 (assoc b i mark))))
               moves)))))

(defn move-win [b mark]
  (some #(and (= mark (analyse %)) %) (next-moves b mark)))

(defn swap-move [b b2]
  (->> (mapv (fn [m1 m2]
               (or (and (= m1 :e)
                        (= m2 :o)
                        :x)
                   m1))
             (apply concat b)
             (apply concat b2))
       (partition 3)
       (mapv vec)))

(defn move-block [b]
  (seq (swap-move b (move-win b :o))))

(defn move-fork [b mark]
  (some (fn [move]
          (and (< 1 (count (filter #(and (= mark (analyse %)) %) (next-moves move mark))))
               move))
        (next-moves b mark)))

(defn move-block-fork [b]
  (seq (swap-move b (:board (move-fork b :o)))))

(defn move-center [b]
  (and (= :e (get-in b [1 1]))
       (assoc-in b [1 1] :x)))

(defn move-opposite-corner [b]
  (or (and (= :o (get-in b [0 0]))
           (= :e (get-in b [2 2]))
           (assoc-in b [2 2] :x))
      (and (= :o (get-in b [2 2]))
           (= :e (get-in b [0 0]))
           (assoc-in b [0 0] :x))
      (and (= :o (get-in b [0 2]))
           (= :e (get-in b [2 0]))
           (assoc-in b [2 0] :x))
      (and (= :o (get-in b [2 0]))
           (= :e (get-in b [0 2]))
           (assoc-in b [0 2] :x))))

(defn move-empty-corner [b]
  (or (and (= :e (get-in b [2 2]))
           (assoc-in b [2 2] :x))
      (and (= :e (get-in b [0 0]))
           (assoc-in b [0 0] :x))
      (and (= :e (get-in b [2 0]))
           (assoc-in b [2 0] :x))
      (and (= :e (get-in b [0 2]))
           (assoc-in b [0 2] :x))))

(defn move-empty-side [b]
  (or (and (= :e (get-in b [0 1]))
           (assoc-in b [0 1] :x))
      (and (= :e (get-in b [1 0]))
           (assoc-in b [1 0] :x))
      (and (= :e (get-in b [2 1]))
           (assoc-in b [2 1] :x))
      (and (= :e (get-in b [1 2]))
           (assoc-in b [1 2] :x))))

(defn best-move 
  "from strategy listed in https://en.wikipedia.org/wiki/Tic-tac-toe"
  [b]
  (or (move-win b :x)
      (move-block b)
      (move-fork b :x)
      (move-block-fork b)
      (move-center b)
      (move-opposite-corner b)
      (move-empty-corner b)
      (move-empty-side b)))

(defn game
  "User plays Os and computer plays Xs.
   Returns {:board b :state s} where b is the board after next set of moves.
   If you have won, :state is :o
   If computer's next move has won, :state is :x
   If game is drawn, :state is :draw
   Otherwise :state is :ongoing"
  [board x y]
  (if (not= (get-in board [y x])
            :e)
    (do (println "bad move!")
        {:board board :state :ongoing})
    (let [b (assoc-in board [y x] :o)
          s (analyse b)]
      (if (not= s :ongoing)
        {:board b :state s}
        (let [b2 (vec (best-move b))]
          (when (seq b2) {:board b2 :state (analyse b2)}))))))

(defn print-board [b]
  (doseq [r b]
    (println r)))

(defn -main
  "Tic Tac Toe"
  [& args]
  (println "Enter your move by typing 'x y' i.e. to play top left enter '0 0'")
  (loop [b empty-board]
    (if (= (analyse b) :ongoing)
      (let [_ (print-board b)
            l (read-line)
            [x y] (map (fn [s] (Integer. s)) (s/split l #" "))]
        (recur (:board (game b x y))))
      (do (print-board b)
          (println (analyse b))))))

