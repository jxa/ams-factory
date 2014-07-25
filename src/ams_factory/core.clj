(ns ams-factory.core
  (:require [clojure.tools.trace :refer (deftrace trace)]))

(defn ->payload
  [name m]
  {(keyword name) m
   :__root__ (keyword name)})

(defn payload-root
  [payload]
  (:__root__ payload))

(defn extract-model
  [payload]
  (get payload (payload-root payload)))

(defn pluralize
  [s]
  (str (name s) "s"))

(defn singularize
  [s]
  (clojure.string/replace (name s) #"s$" ""))

(defn uniq-by
  [f c]
  (letfn [(uniq-inner [seen items]
            (lazy-seq
             (if (seq items)
               (let [items (drop-while #(contains? seen (f %)) items)
                     item (first items)]
                 (when item
                   (cons item (uniq-inner (conj seen (f item))
                                          (rest items))))))))]
    (uniq-inner #{} c)))

(defn sideloaded-models
  [payload]
  (dissoc payload
          (payload-root payload)
          :__root__))

(defn model-collection-name
  [model-collection]
  (-> model-collection
      first
      payload-root))

(defn merge-sideload
  [payload key models]
  (assoc payload key
         (uniq-by :id
                  (concat (get payload key [])
                          models))))

(defn merge-sideloads
  [payload sideloads]
  (reduce (fn [final-payload [key model]]
            (merge-sideload final-payload key model))
          payload
          sideloads))

(defn merge-sideloaded-models
  [payload models]
  (reduce (fn [final-payload model]
            (merge-sideloads final-payload (sideloaded-models model)))
          payload
          models))

(defn sideload
  "Include model-collection in payload in the manner of side-loading"
  ([payload model-collection]
     (sideload payload model-collection (model-collection-name model-collection)))

  ([payload model-collection association-name]
     (let [model-name (payload-root payload)
           associated-models (map extract-model model-collection)
           associated-model-name (model-collection-name model-collection)
           associated-model-key (keyword (pluralize associated-model-name))
           existing-associated-models (get payload associated-model-key [])
           key-name (keyword (str (singularize association-name) "-ids"))]
       (-> payload
           (assoc-in [model-name key-name]
                     (map :id associated-models))
           (merge-sideload associated-model-key associated-models)
           (merge-sideloaded-models model-collection)))))

(defn sideload-one
  "TODO: ensure payload and model are PAYLOADs"
  ([payload model]
     (sideload-one payload model (payload-root model)))

  ([payload model association-name]
     (let [model-name (payload-root payload)
           key-name (keyword (str (singularize association-name) "-id"))]
       (-> payload
           (assoc-in [model-name key-name]
                     (:id (extract-model model)))
           (merge-sideload (keyword (pluralize (payload-root model)))
                           [(extract-model model)])
           (merge-sideloads (sideloaded-models model))))))

(comment
 (def primary-model (->payload :paper
                               {:id 2
                                :title "On the Origin of Species"}))

 (def author-task (->payload :task
                             {:id 5
                              :title "Assign Author"
                              :kind "AssignAuthorTask"}))

 (def some-guy (->payload :user
                          {:id 9
                           :name "Chris"}))

 (def creator (->payload :user
                         {:id 3
                          :name "Steve"}))

 (sideload primary-model [creator])
 (sideload-one primary-model creator)
 (sideload-one primary-model
               (sideload-one some-guy creator :father))

 (-> primary-model
      (sideload [creator] :collaborators)
      (sideload [(sideload author-task [some-guy] :assignee)])))
