(ns om-starter.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om-starter.util :as util]
            [om.dom :as dom]))

(enable-console-print!)

(def initial-state
  {:app/title "Twatter"
   :posts/items [{:id 0
                  :author {:id 0
                           :name "Jim Bob"}
                  :content "This is a #test"}
                 {:id 1
                  :author {:id 0
                           :name "Jim Bob"}
                  :content "This is another #test"}
                 {:id 2
                  :author {:id 1
                           :name "Jane Doe"}
                  :content "Yet another test"}
                 {:id 3
                  :author {:id 0
                           :name "Jim Bob"}
                  :content "Foo bar bar"}
                 {:id 4
                  :author {:id 1
                           :name "Jane Doe"}
                  :content "Baz Baz Baz"}]})

(defui Author
  static om/Ident
  (ident [this {:keys [id]}]
         [:author/by-id id])
  static om/IQuery
  (query [this]
         [:id :name])
  Object
  (render [this]
          (let [{:keys [name]} (om/props this)]
            (dom/h3 nil name))))

(def author (om/factory Author))

(defui Post
  static om/Ident
  (ident [this {:keys [id]}]
         [:post/by-id id])
  static om/IQuery
  (query [this]
         [:id :content {:author (om/get-query Author)}])
  Object
  (render [this]
          (let [the-post (om/props this)]
            (dom/div nil
                     (author (:author the-post))
                     (dom/p nil (:content the-post))))))

(def post (om/factory Post))

(defui PostList
  static om/IQuery
  (query [this]
         [{:posts/items (om/get-query Post)}]))

(defmulti mutate om/dispatch)

(defmethod mutate 'app/update-title
  [{:keys [state]} _ {:keys [new-title]}]
  {:remote true
   :value [:app/title]
   :action (fn [] (swap! state assoc :app/title new-title))})

(defmethod mutate 'app/loading?
  [{:keys [state]} _ _]
  {:value [:loading?]
   :action (fn [] (swap! state assoc :loading? true))})

(defmulti read om/dispatch)

(defmethod read :app/title
  [{:keys [state] :as env} _ {:keys [remote?]}]
  (let [st @state]
    (if-let [v (get st :app/title)]
      {:value v}
      {:value nil})))

(defn resolve-author [st v]
  (update v :author #(get-in st %)))

(defn get-post [st ref]
  (if-let [v (get-in st ref)]
    (resolve-author st v)))

(defn get-posts [st]
  (into [] (map #(get-post st %) (:posts/items st))))

(defmethod read :posts/items
  [{:keys [state selector parser] :as env} key params]
  (let [st @state]
    (if (contains? st key)
      {:value (get-posts st)}
      (println "You done fucked up"))))

(defmethod read :default
  [{:keys [state selector parser] :as env} key params]
  (println state)
  (println selector)
  (println key)
  (println params))

(defui Root
  static om/IQuery
  (query [this]
    (into [] (concat [:app/title] (om/get-query PostList))))
  Object
  (render [this]
    (let [{:keys [app/title posts/items]} (om/props this)]
      (dom/div nil
               (dom/h1 nil title)
               (dom/p nil "It's an awful knockoff of twitter")
               (apply dom/ul nil
                      (map #(dom/li nil (post %)) items))))))

(def parser (om/parser {:read read :mutate mutate}))

(def reconciler
  (om/reconciler
    {:state initial-state
     :normalize true
     :merge-tree (fn [a b] (println "|merge" a b) (merge a b))
     :parser parser
     :send (util/transit-post "/api")}))

(om/add-root! reconciler Root (gdom/getElement "app"))
