;; ln -s ${JV_ENV_HOME}/rc/profiles.clj ~/.lein/profiles.clj


{:repl {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]}

 ;; The Cider docs say make cider-repl a plugin only for the :repl
 ;; profile, or else the middleware will run all the time and delay
 ;; startup.  It says you only need it for :repl.  But that's not
 ;; really true.  If you want to run the project with "lein run"
 ;; and connect Cider to it, you need to get the middleware plugged
 ;; in.  The options are to include it in project.clj, or have it
 ;; more prominent in profiles.clj.  This seems like a better option.
 :user {:plugins [[cider/cider-nrepl "0.17.0"]
                  [jonase/eastwood "0.2.3"]]}}


