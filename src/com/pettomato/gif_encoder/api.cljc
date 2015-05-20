(ns com.pettomato.gif-encoder.api
  (:require
   [com.pettomato.gif-encoder :as gif :refer [in]]))

;;; Higher-level, user-friendly API.

(defn mk-image [w h color]
  {:width  w
   :height h
   :pixels (vec (repeat (* w h) color))})

(defn set-pixel [img x y color]
  (let [{:keys [width height pixels]} img
        i (+ (* y width) x)]
    (assoc-in img [:pixels i] color)))

(defn set-pixels [img xs]
  (let [{:keys [width height pixels]} img
        pixels' (persistent!
                 (reduce (fn [pxs [x y color]]
                           (let [i (+ (* y width) x)]
                             (assoc! pxs i color)))
                         (transient pixels)
                         xs))]
    (assoc img :pixels pixels')))

(defn pixels->seq [img] (:pixels img))

(defn scale-image [img scale]
  ;; This is not a good image scaling algorithm.
  (assert (and (pos? scale) (integer? scale)))
  (let [{:keys [width height pixels]} img
        rows    (partition width pixels)
        rows-w  (map #(mapcat (partial repeat scale) %) rows)
        rows-y  (mapcat (partial repeat scale) rows-w)
        pixels' (vec (apply concat rows-y))]
    {:width  (* width scale)
     :height (* height scale)
     :pixels pixels'}))

(defn mk-frame [image left top delay-time]
  {:image      image
   :left       left
   :top        top
   :delay-time delay-time})

(defn mk-gif [width height frames background-color]
  {:width                  width
   :height                 height
   :frames                 frames
   :background-color       background-color
   :loop?                  true
   :color-bits-per-channel 8})

;;; High-level -> low-level.

(defn image->color-table [img]
  {:post [(< (count %) 256)]}
  (distinct (pixels->seq img)))

(defn images->color-table [images]
  {:post [(< (count %) 256)]}
  (distinct (mapcat pixels->seq images)))

(defn image->color-table->tabled-image [img table]
  (let [color->index (zipmap table (range))
        tabled-img   (map color->index (pixels->seq img))]
    tabled-img))

(defn instantiate-gif-with-global-color-table [gif]
  (let [{:keys [width height frames background-color loop? color-bits-per-channel]} gif
        global-color-table         (images->color-table (map :image frames))
        size-of-global-color-table (gif/calc-size-of-color-table (dec (count global-color-table)))
        padding                    (- (gif/size-of-color-table->actual-size size-of-global-color-table)
                                      (count global-color-table))
        padded-color-table         (concat global-color-table (repeat padding 0))
        color-resolution           (gif/calc-color-resolution color-bits-per-channel)
        background-color-index     (if background-color
                                     (.indexOf global-color-table background-color)
                                     0)
        logical-screen (in gif/logical-screen
                           :logical-screen-descriptor (in gif/logical-screen-descriptor
                                                          :logical-screen-width       width
                                                          :logical-screen-height      height
                                                          :size-of-global-color-table size-of-global-color-table
                                                          :color-resolution           color-resolution
                                                          :global-color-table-flag    1
                                                          :background-color-index     background-color-index)
                           :global-color-table        (in gif/color-table :rgbs padded-color-table))
        data (apply concat
                    (if loop?
                      (in gif/netscape-looping-application-extension)
                      [])
                    (for [f frames]
                      (let [{:keys [image left top delay-time]} f
                            {:keys [width height]} image
                            tabled-img            (image->color-table->tabled-image image global-color-table)
                            lzw-minimum-code-size (gif/calc-lzw-minimum-code-size size-of-global-color-table)
                            img-data              (gif/table-image->data lzw-minimum-code-size tabled-img)]
                        (in gif/graphic-block
                            :graphic-control-extension (in gif/graphic-control-extension :delay-time delay-time)
                            :graphic-rendering-block   (in gif/graphic-rendering-block
                                                           :table-based-image (in gif/table-based-image
                                                                                  :image-descriptor (in gif/image-descriptor
                                                                                                        :image-left-position       left
                                                                                                        :image-top-position        top
                                                                                                        :image-width               width
                                                                                                        :image-height              height
                                                                                                        :size-of-local-color-table 0
                                                                                                        :local-color-table-flag    0)
                                                                                  :image-data       (in gif/table-based-image-data
                                                                                                        :lzw-minimum-code-size lzw-minimum-code-size
                                                                                                        :image-data            img-data
                                                                                                        :image-data-terminator (in gif/image-data :size 0 :data []))))))))]
    (in gif/gif-data-stream
        :header         gif/header
        :logical-screen logical-screen
        :data           data
        :trailer        gif/trailer)))

(defn instantiate-gif [gif]
  (let [{:keys [width height frames loop? color-bits-per-channel]} gif
        color-resolution (gif/calc-color-resolution color-bits-per-channel)
        logical-screen   (in gif/logical-screen
                             :logical-screen-descriptor (in gif/logical-screen-descriptor
                                                            :logical-screen-width  width
                                                            :logical-screen-height height
                                                            :color-resolution      color-resolution))
        data             (apply concat
                                (if loop?
                                  (in gif/netscape-looping-application-extension)
                                  [])
                                (for [f frames]
                                  (let [{:keys [image left top delay-time]} f
                                        {:keys [width height]} image
                                        local-color-table          (image->color-table image)
                                        size-of-local-color-table  (gif/calc-size-of-color-table (dec (count local-color-table)))
                                        padding                    (- (gif/size-of-color-table->actual-size size-of-local-color-table)
                                                                      (count local-color-table))
                                        padded-color-table         (concat local-color-table (repeat padding 0))
                                        tabled-img                 (image->color-table->tabled-image image local-color-table)
                                        lzw-minimum-code-size      (gif/calc-lzw-minimum-code-size size-of-local-color-table)
                                        img-data                   (gif/table-image->data lzw-minimum-code-size tabled-img)]
                                    (in gif/graphic-block
                                        :graphic-control-extension (in gif/graphic-control-extension :delay-time delay-time)
                                        :graphic-rendering-block   (in gif/graphic-rendering-block
                                                                       :table-based-image (in gif/table-based-image
                                                                                              :image-descriptor (in gif/image-descriptor
                                                                                                                    :image-left-position       left
                                                                                                                    :image-top-position        top
                                                                                                                    :image-width               width
                                                                                                                    :image-height              height
                                                                                                                    :size-of-local-color-table size-of-local-color-table
                                                                                                                    :local-color-table-flag    1)
                                                                                              :local-color-table (in gif/color-table
                                                                                                                     :rgbs padded-color-table)
                                                                                              :image-data       (in gif/table-based-image-data
                                                                                                                    :lzw-minimum-code-size lzw-minimum-code-size
                                                                                                                    :image-data            img-data
                                                                                                                    :image-data-terminator (in gif/image-data :size 0 :data []))))))))]
    (in gif/gif-data-stream
        :header         gif/header
        :logical-screen logical-screen
        :data           data
        :trailer        gif/trailer)))

(defn gif->bytes [gif]
  (-> gif
      instantiate-gif
      gif/instantiation->pseudo-binary
      gif/pseudo-binary->bytes))
