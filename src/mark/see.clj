(ns mark.see
  (:import
    (javax.swing WindowConstants JFrame)
    (java.awt Dimension Graphics Image Color Graphics2D RenderingHints)
    (java.util.concurrent Executors TimeUnit)
    (java.awt.event WindowAdapter)
    [java.awt.image BufferedImage]))


(defn see
  "See a visual representation of an image in a java.awt Window.
  Returns a function to call when the image has been changed.

  Example usage:

  (require '[see :as s])

  (-> (s/definition-for my-java-awt-image)
      (s/with-title \"My Image\")
      (s/with-background-colour Color/YELLOW)
      (s/see))"
  [^Image image & {:keys [^String title
                          ^Color background-colour
                          ^Long fps
                          ^Boolean only-draw-when-updated?]
                   :or {title "See!"
                        background-colour (Color. 0 0 0 0)
                        fps 25
                        only-draw-when-updated? false}}]
  (let [frame ^JFrame (proxy [JFrame] []
                        (paint [^Graphics graphics]
                          (let [insets (-> this .getInsets)
                                container (-> this .getContentPane)]
                            #_(.setBackground ^Graphics2D graphics background-colour)
                            #_(.clearRect graphics
                                          (.left insets) (.top insets)
                                          (.getWidth container) (.getHeight container))
                            (.drawImage graphics image (.left insets) (.top insets) this))))
        changed? (volatile! true)
        executor (Executors/newSingleThreadScheduledExecutor)]
    (.scheduleAtFixedRate executor
                          #(when (or @changed? (not only-draw-when-updated?))
                             (vreset! changed? false)
                             (.repaint frame))
                          0 (long (/ 1000 fps)) TimeUnit/MILLISECONDS)
    (doto frame
      (.setTitle title)
      (-> .getContentPane (.setPreferredSize
                            (Dimension. (.getWidth image nil)
                                        (.getHeight image nil))))
      (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
      (.addWindowListener (proxy [WindowAdapter] []
                            (windowClosed [_window-event]
                              (.shutdown executor))))
      (.pack)
      (.setVisible true))
    #(vreset! changed? true)))

(defn window [title]
  (let [!image (volatile! nil)
        !scale (volatile! 1)
        frame ^JFrame (proxy [JFrame] []
                        (paint [^Graphics graphics]
                          (when @!image
                            (let [insets (-> this .getInsets)
                                  x1 (.left insets)
                                  y1 (.top insets)
                                  x2 (+ x1 (* (.getWidth @!image) @!scale))
                                  y2 (+ y1 (* (.getHeight @!image) @!scale))]
                              (.drawImage graphics @!image x1 y1 x2 y2 this)))))
        re-size! (fn []
                   (println "re-size!")
                   (.setPreferredSize (.getContentPane frame)
                                      (Dimension. (* (.getWidth @!image nil) @!scale)
                                                  (* (.getHeight @!image nil) @!scale)))
                   (.pack frame))]
    (doto frame
      (.setTitle title)
      (-> .getContentPane (.setPreferredSize (Dimension. 200 200)))
      (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
      (.pack)
      (.setVisible true))
    (fn this-fn
      ([image scale]
       (when (not= scale @!scale)
         (vreset! !scale scale)
         (re-size!))
       (this-fn image))
      ([image]
       (when (not= image @!image)
         (vreset! !image image)
         (re-size!))
       (.repaint frame)))))

(defn ^BufferedImage image
  ([rectangle]
   (image (.getWidth rectangle) (.getHeight rectangle)))
  ([width height]
   (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)))

(defn nice-graphics [image]
  (let [graphics (.getGraphics image)]
    (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint graphics RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
    graphics))
