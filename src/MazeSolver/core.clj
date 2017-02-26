(ns MazeSolver.core
  (:import (javax.imageio ImageIO)
           (java.io File)))

(defn getPixels [fileName]
  (let [file (File. fileName)
        img (ImageIO/read file)
        h (.getHeight img)
        w (.getWidth img)
        pixels (for [y (range h)]
                 (for [x (range w)]
                   (if (= -1 (.getRGB img x y))
                     "W"
                     "B")))]
    pixels))

