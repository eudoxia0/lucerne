(:docstring-markup-format :scriba
 :systems (:lucerne
           :lucerne-utweet
           :lucerne-auth)
 :documents ((:title "Lucerne"
              :authors ("Fernando Borretti")
              :output-format (:type :multi-html
                              :template :minima)
              :sources ("overview.scr"
                        "utweet.scr"
                        "project.scr"
                        "api.scr"
                        "extensions.scr"))))
