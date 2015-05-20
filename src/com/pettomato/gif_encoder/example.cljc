(ns com.pettomato.gif-encoder.api
  (:require
   [com.pettomato.gif-encoder.api
    :refer [mk-image mk-frame mk-gif set-pixels scale-image gif->bytes]]))

(defn write-bytes [f byte-arr]
  (with-open [w (clojure.java.io/output-stream f)]
    (.write w byte-arr)))

(defn draw-rect [img x y w h color]
  (set-pixels img (for [x (range x (+ x w))
                        y (range y (+ y h))]
                    [x y color])))

(defn cycling-boxes [file size]
  (let [colors (cycle (repeatedly (/ size 2) (partial rand-int 0xFFFFFF)))
        rects  (for [i (range 0 size 2)]
                 [i i (- size (* i 2)) (- size (* i 2))])
        frames (reduce (fn [acc colors]
                         (let [img (reduce (fn [img [[x y w h] color]] (draw-rect img x y w h color))
                                           (mk-image size size 0x000000)
                                           (map vector rects colors))]
                           (conj acc (mk-frame img 0 0 1))))
                       []
                       (for [i (range (/ size 2))] (take (/ size 2) (drop i colors))))]
    (->> (mk-gif size size frames nil)
         gif->bytes
         byte-array
         (write-bytes file))))

(defn lazy-game-of-life [w h seed]
  (let [n       (* w h)
        idxs    (range n)
        f-adj   (fn [i]
                  (->> (list (dec (- i w)) (- i w) (inc (- i w))
                             (dec i)       i       (inc i)
                             (dec (+ i w)) (+ i w) (inc (+ i w)))
                       (filter #(<= 0 % n))))
        i->adj  (zipmap idxs (map f-adj idxs))
        adj-sum (fn [board i] (reduce + (map #(get board % 0) (i->adj i))))
        board   (vec seed)
        iter    (fn [board]
                  (reduce (fn [board' i]
                            (case (adj-sum board i)
                              3  (assoc board' i 1)
                              4  board'
                              (assoc board' i 0)))
                          board
                          idxs))]
    (iterate iter board)))

(defn game-of-life-gif [file w h iterations scale]
  (let [render (fn [board]
                 (mk-frame
                  (scale-image
                   {:width  w
                    :height h
                    :pixels (mapv #(case % 0 0xFFFFFF 1 0x000000) board)}
                   scale)
                  0
                  0
                  1))
        seed   (repeatedly (* w h) (partial rand-int 2))
        frames (->> (lazy-game-of-life w h seed)
                    (take iterations)
                    (map render))]
    (->> (mk-gif (* w scale) (* h scale) frames nil)
         gif->bytes
         byte-array
         (write-bytes file))))

;; (cycling-boxes "cycling-boxes.gif" 128)

;; (game-of-life-gif "game-of-life.gif" 256 8 200 2)
