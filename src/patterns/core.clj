(ns patterns.core
  (:require [clojure.java.io :as io]
            [hiccup.core :as html]
            [patterns.utils :as utils]
            [patterns.utils.log :as u.log]
            [patterns.utils.svg :as svg]
            [taoensso.timbre :as log]
            [taoensso.tufte :as trace])
  (:import [java.awt RenderingHints]
           [java.io ByteArrayInputStream IOException]
           [java.nio.charset StandardCharsets]
           [java.util UUID]
           [org.apache.batik.anim.dom SAXSVGDocumentFactory]
           [org.apache.batik.transcoder SVGAbstractTranscoder
                                        TranscoderInput
                                        TranscoderOutput TranscoderException]
           [org.apache.batik.transcoder.image PNGTranscoder]))

(defn- src->string
  [src]
  (->> src
       svg/to-svg-doc
       html/html))

(defn- high-quality-png-transcoder []
  (proxy [PNGTranscoder] []
    (createRenderer []
      (let [add-hint (fn [hints k v] (.add hints (RenderingHints. k v)))
            renderer (proxy-super createRenderer)
            hints (RenderingHints. nil nil)]
        (doto hints
          (add-hint RenderingHints/KEY_ALPHA_INTERPOLATION
                    RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY)
          (add-hint RenderingHints/KEY_INTERPOLATION
                    RenderingHints/VALUE_INTERPOLATION_BICUBIC)
          (add-hint RenderingHints/KEY_ANTIALIASING
                    RenderingHints/VALUE_ANTIALIAS_ON)
          (add-hint RenderingHints/KEY_COLOR_RENDERING
                    RenderingHints/VALUE_COLOR_RENDER_QUALITY)
          (add-hint RenderingHints/KEY_DITHERING
                    RenderingHints/VALUE_DITHER_DISABLE)
          (add-hint RenderingHints/KEY_RENDERING
                    RenderingHints/VALUE_RENDER_QUALITY)
          (add-hint RenderingHints/KEY_STROKE_CONTROL
                    RenderingHints/VALUE_STROKE_PURE)
          (add-hint RenderingHints/KEY_FRACTIONALMETRICS
                    RenderingHints/VALUE_FRACTIONALMETRICS_ON)
          (add-hint RenderingHints/KEY_TEXT_ANTIALIASING
                    RenderingHints/VALUE_TEXT_ANTIALIAS_OFF))
        (.setRenderingHints renderer hints)
        renderer))))

(defn- render-document-to-png
  [filename svg-src & {:keys [attempt
                              previous-ex]
                       :or {attempt 0}}]
  (u.log/with-context {:attempt attempt}
    (if (< 3 attempt)
      (throw (ex-info "Too many attempts trying to render." {:filename filename
                                                             :svg-src svg-src}
                      previous-ex))
      (try
        (let [svg-document (with-open [in (ByteArrayInputStream. (.getBytes (src->string svg-src)
                                                                            StandardCharsets/UTF_8))]
                             (.createDocument (SAXSVGDocumentFactory. "org.apache.xerces.parsers.SAXParser")
                                              (str "file:///" filename ".svg")
                                              in))
              {:keys [width height]} (svg/dimensions svg-src)]
          (with-open [out-stream (io/output-stream (io/file filename))]
            (let [in (TranscoderInput. svg-document)
                  out (TranscoderOutput. out-stream)
                  trans (high-quality-png-transcoder)]
              (.addTranscodingHint trans SVGAbstractTranscoder/KEY_WIDTH (float width))
              (.addTranscodingHint trans SVGAbstractTranscoder/KEY_HEIGHT (float height))
              (.transcode trans in out))))
        (catch TranscoderException ex
          (log/warnf ex "`render-document-to-png` encountered exception. Retrying")
          (render-document-to-png filename svg-src
                                  :attempt (inc attempt) :previous-ex ex))))))

(defn- is-defs-element?
  [element]
  (and (vector? element)
       (-> element
           first
           #{:defs})
       true))

(defn- is-id-svg-element?
  [element]
  (and (vector? element)
       (-> element
           first
           #{:svg})
       (-> element
           second
           :id)
       true))

(defn tmp-resource
  []
  (str "/tmp/patterns.core." (UUID/randomUUID)))

(trace/defnp ^:private recursive-render-png
  [filename src]
  (u.log/with-context {:filename-path [filename]
                       :file-sizes [(count (str src))]}
    (let [pre-defs (take-while (comp not is-defs-element?)
                               src)
          [_def_tag def-attrs & def-body] (->> src
                                               (filter is-defs-element?)
                                               first)
          post-defs (drop (inc (count pre-defs))
                          src)
          def--svg-elements (filter is-id-svg-element?
                                    def-body)
          def--non-svg-elements (remove is-id-svg-element?
                                        def-body)
          tmp-filenames (repeatedly (count def--svg-elements) tmp-resource)
          svg-image-elements (mapv (fn [filename [_svg-tag attrs]]
                                     [:svg attrs
                                      [:image (assoc (svg/dimensions [:svg attrs])
                                                :xlink:href filename
                                                :x 0 :y 0)]])
                                   tmp-filenames
                                   def--svg-elements)]
      (try
        (when-let [tmp-files (seq (zipmap tmp-filenames
                                          def--svg-elements))]
          (doseq [[tmp-filename tmp-src] tmp-files]
            (recursive-render-png tmp-filename tmp-src)))
        (log/debug "Rendering...")
        (render-document-to-png
          filename
          (utils/veccat
            pre-defs
            [(utils/veccat
               [:defs def-attrs]
               def--non-svg-elements
               svg-image-elements)]
            post-defs))
        (catch Exception e
          (log/errorf e
                      "Exception encountered while trying to render %s. Snagged `src` as `%s`."
                      filename
                      (str (def snag-failing-src [123])))
          (throw e))
        (finally
          (doseq [tmp-filename tmp-filenames]
            (try
              (io/delete-file tmp-filename)
              (catch IOException e
                (log/warnf e
                           "Couldn't delete file %s"
                           tmp-filename)))))))))

(trace/defnp render
  "Given a `src` Hiccup SVG, return a string HTML representation.
   If provided a filename, place the rendered SVG representation there.
   If provided an extension [:svg :png] render the Hiccup SVG as the given extension.

   Returns the filename with extension."
  ([src]
   (src->string src))
  ([filename src]
   (render filename src :svg))
  ([filename src extension]
   (let [filename-w-extension (str filename "." (name extension))]
     (case extension
       :svg (spit filename-w-extension (render src))
       :png (recursive-render-png filename-w-extension
                                  src))
     filename-w-extension)))
