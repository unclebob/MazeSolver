(ns MazeSolver.core-test
  (:require [clojure.test :refer :all]
            [MazeSolver.core :refer :all]
            [clojure.pprint :refer :all]))

(defn- count-runs [c row state count counts]
  (case state
    :in (if (empty? row)
          (conj counts count)
          (if (= c (first row))
            (count-runs c (next row) :in (inc count) counts)
            (count-runs c (next row) :out 0 (conj counts count))))
    :out (if (empty? row)
           counts
           (if (= c (first row))
             (count-runs c (next row) :in 1 counts)
             (count-runs c (next row) :out 0 counts)))))

(defn- find-runs [c row]
  (count-runs c row :out 0 []))

(defn- get-row-widths [row]
  (let [bruns (find-runs "B" row)
        wruns (find-runs "W" row)]
    [(if (empty? bruns) 0 (apply min bruns))
     (if (empty? wruns) 0 (apply min wruns))
     ]))

(defn get-line-widths [maze]
  (if (= 0 (count maze))
    [0 0]
    (let [row-width (map get-row-widths maze)
          transposed-widths (apply mapv vector row-width)]
      [(apply min (first transposed-widths)) (apply min (second transposed-widths))])))

(defn compress-maze [maze]
  (let [[bw ww] (get-line-widths maze)
        cell-size (+ bw ww)
        half-cell (/ cell-size 2)
        mw (count (first maze))
        mh (count maze)
        w (/ (- mw bw) half-cell)
        h (/ (- mh bw) half-cell)
        ]
    (for [y (range 0 (inc h))]
      (for [x (range 0 (inc w))]
        (nth (nth maze (* y half-cell)) (* x half-cell))))))

(deftest test-runs
  (is (= [] (find-runs "B" [])))
  (is (= [1] (find-runs "B" ["B"])))
  (is (= [1] (find-runs "B" ["W" "B" "W"])))
  (is (= [1 2] (find-runs "B" ["W" "B" "W" "W" "B" "B"]))))

(deftest degenerate-maze
  (is (= [0 0] (get-line-widths [])))
  (is (= [1 0] (get-line-widths [["B"]])))
  (is (= [1 1] (get-line-widths [["B", "W"]]))))

(deftest multi-row-maze
  (is (= [2 3] (get-line-widths [["B" "B" "W" "W" "W" "B" "B"]
                                 ["W" "W" "W" "W" "B" "B" "B"]])))
  (is (= [2 3] (get-line-widths [["W" "W" "W" "W" "B" "B" "B"]
                                 ["B" "B" "W" "W" "W" "B" "B"]]))))

(deftest compress-maze-test
  (is (= [["B" "W" "B"]
          ["B" "W" "B"]
          ["B" "W" "B"]]
         (compress-maze [["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]])))
  (is (= [["B" "B" "W" "B" "B"]
          ["B" "B" "W" "B" "B"]
          ["B" "W" "B" "W" "B"]
          ["B" "B" "W" "B" "B"]]
         (compress-maze [["B" "B" "W" "B" "B"]
                         ["B" "B" "W" "B" "B"]
                         ["B" "W" "B" "W" "B"]
                         ["B" "B" "W" "B" "B"]])))

  (is (= '( ("B" "B" "B" "B" "B" "W" "B" "B" "B" "B" "B")
            ("B" "W" "W" "W" "B" "W" "W" "W" "B" "W" "B")
            ("B" "W" "B" "W" "B" "B" "B" "W" "B" "W" "B")
            ("B" "W" "B" "W" "W" "W" "B" "W" "W" "W" "B")
            ("B" "W" "B" "B" "B" "W" "B" "B" "B" "W" "B")
            ("B" "W" "W" "W" "B" "W" "B" "W" "W" "W" "B")
            ("B" "B" "B" "W" "B" "W" "B" "W" "B" "W" "B")
            ("B" "W" "W" "W" "B" "W" "W" "W" "B" "W" "B")
            ("B" "W" "B" "W" "B" "B" "B" "B" "B" "W" "B")
            ("B" "W" "B" "W" "W" "W" "B" "W" "W" "W" "B")
            ("B" "B" "B" "B" "B" "W" "B" "B" "B" "B" "B"))
         (compress-maze (getPixels "resources/maze.png")))))

(deftest real-maze
  (is (= [2 14] (get-line-widths (getPixels "resources/maze.png")))))
