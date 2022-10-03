(use 'clojure.walk)

(def board [["r", "h", "b", "q", "k", "b", "h", "t"]
            ["p", "p", "p", "p", "p", "p", "p", "p"]
            ["", "", "", "", "", "", "", ""]
            ["", "", "", "", "", "", "", ""]
            ["", "", "", "", "", "", "", ""]
            ["", "", "", "", "", "", "", ""]
            ["p", "p", "p", "p", "p", "p", "p", "p"]
            ["r", "h", "b", "q", "k", "b", "h", "t"]])

(def empty-board [["", "", "", "", "", "", "", ""]
                  ["", "", "", "", "", "", "", ""]
                  ["", "", "", "", "", "", "", ""]
                  ["", "", "", "", "", "", "", ""]
                  ["", "", "", "", "", "", "", ""]
                  ["", "", "", "", "", "", "", ""]
                  ["", "", "", "", "", "", "", ""]
                  ["", "", "", "", "", "", "", ""]])

(def files "abcdefgh")
(def rows "12345678")

(def initial_gamestate
  {;; The game context. Contains some game metadata such as the th
   :ctx {:turn 0}
   ;; A list of all the moves made. Each list's element is a 2-fold tuple, one for each player.
   ;; Matches the in-memory structure of the PGN file. Should be the only thing needed to recreate a game from scratch every time.
   :moves []
   :state {:pieces [{:x 1 :y 1 :symbol "p"}]}})

(defn find-piece [coordinates])

(def initial-state
  [{:x 4 :y 0 :symbol "p"}])

(defn initialize-state [] initial-state)

(defn initialize-board [] empty-board)

;; string => game
(defn parse-game
  "Parses a PGN related format (as a string) into an in memory structure that represents the steps that happened during a game."
  [pgn])

(defn has-piece? [coordinates piece]
  (and
   (= (get coordinates :x) (get piece :x))
   (= (get coordinates :y) (get piece :y))))

;; NOTE: This seems inneficient, see if there's a better way to build this eventually.
;; NOTE: It seems very doable to do this using (range) instead of a "static" board.
(defn generate-board
  "Generates the state of the board from the pieces information. It might only be used for display purposes, and as such, contains only a small amont of data about the pieces."
  [state]
  (let [board (initialize-board)]
    (map-indexed
     (fn [y-idx row]
       (map-indexed
        (fn [x-idx present]
          (let [piece (some #(when (has-piece? {:x x-idx :y y-idx} %) %) state)]
            (if (nil? piece)
              present
              (get piece :symbol))))
        row))
     board)))

(defn parse-move
  " Returns a tuple of coordinates from a tuple of string coordinates."
  [move]
  (let [[from to] move]
    [{:x (clojure.string/index-of files (first from)), :y (clojure.string/index-of rows (second from))}
     {:x (clojure.string/index-of files (first to)), :y (clojure.string/index-of rows (second to))}]))

(defn make-move
  "Creates a new board when provided with a move made."
  [move state]
  (let [[from to] (parse-move move)]
    (let [piece (some #(when (has-piece? from %) %) state)]
      (let [[n m] (split-with (partial not= piece) state)]
        (concat n (list (merge piece to)) (rest m))))))

(defn print-row [del row]
  (println-str
   (clojure.string/join
    del
    (map
     (fn [val]
       (if (clojure.string/blank? val) " " val))
     row))))

(defn print-row-delimiter [] (print-row "- +" [" " " " " " " " " " " " " " " " " "]))

(defn print-board [board]
  "Travel a board list containing the pieces and prints them in a makeshift board."
  (walk
   #(print-str (apply str [(print-row  " | " %) (print-row-delimiter)]))
   #(print %)
   board))

;; NOTE: We generate a board with a single pon, print the resulting board, then make a move and print the board again.
(defn -main [& _]
  (let [state (initialize-state)]
    (print-board (generate-board state))
    (let [new-state (make-move ["e1" "e3"] state)]
      (print-board (generate-board new-state)))))
