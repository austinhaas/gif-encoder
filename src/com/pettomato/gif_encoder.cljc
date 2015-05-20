(ns com.pettomato.gif-encoder)

;;; Schema.

(def header
  [[:signature ['byte 3] "GIF"]
   [:version   ['byte 3] "89a"]])

(def logical-screen-descriptor
  [[:logical-screen-width       'unsigned ]
   [:logical-screen-height      'unsigned ]
   ;; If the Global Color Table Flag is set to 1, the value in this
   ;; field is used to calculate the number of bytes contained in the
   ;; Global Color Table. To determine the actual size of the color
   ;; table, raise 2 to (the-value-of-the-field + 1). Even if there is
   ;; no Global Color Table specified, set this field according to the
   ;; above forumla so that decoders can choose the best graphics mode
   ;; to display the stream in.
   [:size-of-global-color-table ['bit 3] 0]
   ;; Indicates whether the Global Color Table is sorted.
   ;; 0 - Not ordered.
   ;; 1 - Ordered by decreasing importance, most important color
   ;; first.
   [:sort-flag                  ['bit 1] 0]
   ;; Number of bits per primary color available in the original
   ;; image, minus 1.
   [:color-resolution           ['bit 3]  ]
   ;; 0 - No Global Color Table follows, the Background Color Index
   ;;     field is meaningless.
   ;; 1 - A Global Color Table will immediately follow, the Background
   ;;     Color Index is meaningful.
   [:global-color-table-flag    ['bit 1] 0]
   ;; Index into the Global Color Table for the Background Color. If
   ;; the Global Color Table Flag is set to 0, this field should be 0.
   [:background-color-index     'byte    0]
   [:pixel-aspect-ratio         'byte    0]])

(def color-table
  [[:rgbs ['rgb :*]]])

(def image-descriptor
  [[:image-separator           'byte     0X2C]
   [:image-left-position       'unsigned     ]
   [:image-top-position        'unsigned     ]
   [:image-width               'unsigned     ]
   [:image-height              'unsigned     ]
   [:size-of-local-color-table ['bit 3]      ]
   [:reserved                  ['bit 2]  0   ]
   [:sort-flag                 ['bit 1]  0   ]
   [:interlace-flag            ['bit 1]  0   ]
   [:local-color-table-flag    ['bit 1]      ]])

(def image-data
  [[:size 'byte     ]
   [:data ['byte :*]]])

(def table-based-image-data
  [[:lzw-minimum-code-size 'byte            ]
   [:image-data            ['image-data :*] ]
   [:image-data-terminator 'image-data      ]])

(def graphic-control-extension
  [[:extension-introducer    'byte     0x21]
   [:graphic-control-label   'byte     0xF9]
   [:block-size              'byte     4   ]
   [:transparent-color-flag  ['bit 1]  0   ]
   [:user-input-flag         ['bit 1]  0   ]
   ;; 0 - No disposal specified.
   ;; 1 - Do not dispose.
   ;; 2 - Restore to background color.
   ;; 3 - Restore to previous.
   [:disposal-method         ['bit 3]  0   ]
   [:reserved                ['bit 3]  0   ]
   [:delay-time              'unsigned 0   ]
   [:transparent-color-index 'byte     0   ]
   [:block-terminator        'byte     0   ]])

(def comment-data
  [[:size 'byte     ] ;; Must be >=1 and <= 255.
   [:data ['byte :+]]])

(def comment-extension
  [[:extension-introducer 'byte         0x21]
   [:comment-label        'byte         0xFE]
   [:comment-data         'comment-data     ]
   [:block-terminator     'byte         0   ]])

(def trailer
  [[:gif-trailer 'byte 0x3B]])

(def netscape-looping-application-extension
  ;; This extension must be the first data element.
  [[:extension-introducer            'byte     0x21      ]
   [:extension-label                 'byte     0xFF      ]
   [:block-size                      'byte     11        ]
   [:application-identifier          ['byte 8] "NETSCAPE"]
   [:application-authentication-code ['byte 3] "2.0"     ]
   [:sub-block-data-size             'byte     3         ]
   [:sub-block-id                    'byte     1         ]
   [:loop-count                      'unsigned 0         ]
   [:block-terminator                'byte     0         ]])

(def logical-screen
  [[:logical-screen-descriptor 'logical-screen-descriptor]
   [:global-color-table        ['color-table :?]         ]])

(def table-based-image
  [[:image-descriptor  'image-descriptor ]
   [:local-color-table ['color-table :?] ]
   [:image-data        'table-based-image-data]])

(def graphic-rendering-block
  [[:table-based-image 'table-based-image]])

(def graphic-block
  [[:graphic-control-extension ['graphic-control-extension :?]]
   [:graphic-rendering-block   'graphic-rendering-block       ]])

(def application-extension
  ;; Other application extensions are possible, but this is the only
  ;; one currently implemented.
  [[:val 'netscape-looping-application-extension]])

(def special-purpose-block
  [[:val [:or 'application-extension 'comment-extension]]])

(def data
  [[:val [:or 'graphic-block 'special-purpose-block]]])

(def gif-data-stream
  [[:header         'header]
   [:logical-screen 'logical-screen]
   [:data           ['data :*]]
   [:trailer        'trailer]])

;;; Instantiation from schema.

(defn in [spec & fields]
  (let [m (apply hash-map fields)]
    (mapv (fn [[label type value]]
            [label type (get m label value)])
          spec)))

;;; Instantiations -> pseudo-binary.

(declare instantiation->pseudo-binary)

(defn- field->pseudo-binary [[label type value]]
  (let [[type' count] (if (vector? type) type [type 1])
        count'        (case count
                        :? (if (nil? value) 0 1)
                        :+ (if (nil? value) (assert false (str "Must provide at least one value for " type)) (count value))
                        :* (count value)
                        count)]
    (case type'
      bit      (list [count' value])
      byte     (cond
                 (string? value)     (for [char value] [8 (#?(:clj int :cljs .charCodeAt) char)])
                 (integer? value)    (list [(* count' 8) value])
                 (sequential? value) (for [item value] [8 item]))
      unsigned (list [(* count' 8 2) value])
      rgb      (mapcat #(list [8 (unsigned-bit-shift-right (bit-and 0xFF0000 %) 16)]
                              [8 (unsigned-bit-shift-right (bit-and 0x00FF00 %) 8)]
                              [8 (bit-and 0x0000FF %)])
                       value)
      (instantiation->pseudo-binary value))))

(defn instantiation->pseudo-binary [block]
  (mapcat field->pseudo-binary block))

;;; Pseudo-binary -> bytes.

(def empty-octet-seq [[0] 0])

(defn eos-add-bits [eos size val]
  (let [[s i] eos]
    (loop [n size, v val, s s, i i]
      (cond
        (= n 0) [s i]
        (= i 8) (recur n v (conj s 0) 0)
        :else   (let [b  (peek s)
                      s' (pop s)
                      b' (if (bit-test v 0)
                           (bit-set b i)
                           b)]
                  (recur (dec n)
                         (unsigned-bit-shift-right v 1)
                         (conj s' b')
                         (inc i)))))))

(defn pseudo-binary->bytes [xs]
  (->> (reduce (fn [eos [size val]] (eos-add-bits eos size val))
               empty-octet-seq
               xs)
       first))

;;; LZW.

(defn expt-2 [exponent] (bit-shift-left 1 exponent))

(defn vlc-lzw-compress [xs code-size]
  ;; Variable-Length-Code LZW Compression.
  ;; Returns a psuedo-binary representation.
  (let [clear-code       (expt-2 code-size)
        end-of-info-code (+ clear-code 1)
        init-code-size   (+ code-size 1)
        init-code        (+ clear-code 2)]
    (loop [xs         (seq xs)
           patt       []
           patt->code (zipmap (map vector xs) xs)
           code       init-code
           code-size  init-code-size
           out        [[code-size clear-code]]]
      (cond
        (empty? xs)    (into out (map (partial vector code-size) (conj patt end-of-info-code)))
        (= code 0xFFF) (recur xs patt {} init-code init-code-size (conj out [code-size clear-code]))
        :else          (let [[x & xs'] xs
                             patt' (conj patt x)
                             code' (get patt->code patt')]
                         (if code'
                           (recur xs' [code'] patt->code code code-size out)
                           (recur xs'
                                  [x]
                                  (assoc patt->code patt' code)
                                  (inc code)
                                  (if (< code (expt-2 code-size)) code-size (inc code-size))
                                  (into out (map (partial vector code-size) patt)))))))))

(defn table-image->data [lzw-minimum-code-size tabled-img]
  ;; A tabled image is a sequence of indexes into a color table.
  (->> (vlc-lzw-compress tabled-img lzw-minimum-code-size)
       pseudo-binary->bytes
       (partition-all 255)
       (map (juxt count identity))
       (mapcat (fn [[size data]] (in image-data :size size :data data)))))

;;; Other.

(defn bits-needed-to-represent [n]
  (loop [n n
         c 0]
    (if (zero? n)
      c
      (recur (unsigned-bit-shift-right n 1)
             (inc c)))))

(defn calc-size-of-color-table [largest-index]
  (dec (max 1 (bits-needed-to-represent largest-index))))

(defn size-of-color-table->actual-size [n]
  (expt-2 (inc n)))

(defn calc-color-resolution [color-bits-per-channel]
  (dec color-bits-per-channel))

(defn calc-lzw-minimum-code-size [size-of-color-table]
  (max 2 (inc size-of-color-table)))
